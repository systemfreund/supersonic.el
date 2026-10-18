;;; supersonic-music-assistant-player.el --- Music Assistant playback backend -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
;; Assisted-by: Claude:claude-opus-5
;; URL: https://github.com/systemfreund/supersonic.el
;; Keywords: multimedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The Music Assistant playback backend: plays on one of the
;; Music Assistant (MA) server's own players -- speakers, Chromecasts,
;; AirPlay devices, its web player -- the way `supersonic-jukebox.el'
;; plays on a Subsonic server's jukebox.  Registers itself with the
;; facade in `supersonic-playback.el' under the name `music-assistant',
;; for the `music-assistant' provider only: the player's queue takes
;; MA's item URIs, which only that provider hands out.  Loaded the
;; first time the backend is used -- see
;; `supersonic-playback-backend-libraries' -- and loading it loads the
;; provider too, so selecting both is enough:
;;
;;   (setq supersonic-provider 'music-assistant
;;         supersonic-playback-backend 'music-assistant)
;;
;; The player is `supersonic-music-assistant-player', picked with
;; `supersonic-music-assistant-select-player' -- or, with none picked
;; yet, offered when playing or enqueueing, taking the server's only
;; player without asking.  A player's queue has the player's id.
;;
;; MA's HTTP API pushes nothing, so the queue is polled through
;; `supersonic-poller.el' while `music-assistant' is the active backend.
;; Its entries, which carry whole media items, are only asked for when
;; the queue's state says they may have changed; see
;; `supersonic-music-assistant-player--items-key'.  Status, queue and
;; liveness answer from what the latest poll found.
;;
;; No scrobbling: MA records play history itself.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'aio)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)
(require 'supersonic-poller)
(require 'supersonic-music-assistant)

;;;
;;; The player
;;;

(defun supersonic-music-assistant-player--queue-id ()
  "Return the id of the selected player's queue, or signal a `user-error'."
  (or (plist-get supersonic-music-assistant-player :id)
      (user-error "No Music Assistant player selected; select one with M-x supersonic-music-assistant-select-player")))

(defun supersonic-music-assistant-player--target ()
  "Return (QUEUE-ID . CONNECTION) for the commands of an operation.
Call it before the first `aio-await', so that errors and prompts
reach the command."
  (let ((queue-id (supersonic-music-assistant-player--queue-id)))
    (cons queue-id (supersonic-music-assistant--connection))))

(aio-defun supersonic-music-assistant-player--send (target command &optional args)
  "Return a promise resolving to the result of queue COMMAND with ARGS.
TARGET is from `supersonic-music-assistant-player--target'."
  (pcase-let ((`(,queue-id . ,connection) target))
    (aio-await (supersonic-music-assistant--send connection command `(("queue_id" . ,queue-id) ,@args)))))

;;;
;;; Polling the queue
;;;

(defvar supersonic-music-assistant-player--poller)

(defconst supersonic-music-assistant-player--items-max-age 10
  "Seconds after which the queue entries are fetched even if unchanged.")

(defun supersonic-music-assistant-player--items-key (queue)
  "Return the parts of QUEUE that change when its entries do.
QUEUE is a `player_queues/get' result.  Entries moved by another
client go unnoticed for up to
`supersonic-music-assistant-player--items-max-age' seconds."
  (list (assoc-default "queue_id" queue)
        (assoc-default "items" queue)
        (assoc-default "queue_item_id" (assoc-default "current_item" queue))
        (assoc-default "queue_item_id" (assoc-default "next_item" queue))
        (assoc-default "shuffle_enabled" queue)))

(defun supersonic-music-assistant-player--entry-id (item)
  "Return the track id of queue entry ITEM, or nil."
  (assoc-default "uri" (assoc-default "media_item" item)))

(defun supersonic-music-assistant-player--entry-track (item)
  "Return the track plist of queue entry ITEM, or nil."
  (let ((media-item (assoc-default "media_item" item)))
    (and (consp media-item) (supersonic-music-assistant--track media-item))))

(defun supersonic-music-assistant-player--parse-snapshot (queue items polled-at items-polled-at)
  "Return the snapshot plist for QUEUE, a `player_queues/get' result.
ITEMS are its entries as (TRACK-ID . TRACK).  POLLED-AT is when QUEUE
was received, ITEMS-POLLED-AT when ITEMS were.  The position is taken
as of when the server last updated it."
  (let* ((current (assoc-default "current_item" queue))
         (updated (assoc-default "elapsed_time_last_updated" queue))
         (index (assoc-default "current_index" queue)))
    (list :items-key (supersonic-music-assistant-player--items-key queue)
          :items items
          :items-polled-at items-polled-at
          :entries (mapcar #'car items)
          :current-index (and (integerp index) index)
          :track-id (supersonic-music-assistant-player--entry-id current)
          :playing (equal (assoc-default "state" queue) "playing")
          :ended (and (assoc-default "ended" queue) t)
          :position (assoc-default "elapsed_time" queue)
          :duration (supersonic-music-assistant--positive-integer (assoc-default "duration" current))
          :polled-at (if (numberp updated)
                         (min (+ updated supersonic-music-assistant--clock-offset) polled-at)
                       polled-at))))

(aio-defun supersonic-music-assistant-player--fetch ()
  "Return a promise resolving to a snapshot of the player's queue.
The entries are only fetched if they may have changed."
  (pcase-let* ((`(,queue-id . ,connection) (supersonic-music-assistant-player--target))
               (queue (aio-await (supersonic-music-assistant-player--send
                                  (cons queue-id connection) "player_queues/get")))
               (polled-at (float-time))
               (previous (supersonic-poller-snapshot supersonic-music-assistant-player--poller))
               (fetch-items (not (and previous
                                      (equal (plist-get previous :items-key)
                                             (supersonic-music-assistant-player--items-key queue))
                                      (numberp (plist-get previous :items-polled-at))
                                      (< (- polled-at (plist-get previous :items-polled-at))
                                         supersonic-music-assistant-player--items-max-age)))))
    (unless (consp queue)
      (user-error "Music Assistant has no player %s; select one with M-x supersonic-music-assistant-select-player"
                  (or (plist-get supersonic-music-assistant-player :name) queue-id)))
    (supersonic-music-assistant-player--parse-snapshot
     queue
     (cond
      ((eql 0 (assoc-default "items" queue))
       nil)
      ((not fetch-items)
       (plist-get previous :items))
      (t
       (mapcar (lambda (item)
                 (cons (supersonic-music-assistant-player--entry-id item)
                       (supersonic-music-assistant-player--entry-track item)))
               (aio-await (supersonic-music-assistant--all-items
                           "player_queues/items" `(("queue_id" . ,queue-id)) connection)))))
     polled-at
     (if fetch-items polled-at (plist-get previous :items-polled-at)))))

(defun supersonic-music-assistant-player--summarize (snapshot)
  "Return the summary of SNAPSHOT that `supersonic-poller-announce' compares."
  (list :track-id (plist-get snapshot :track-id)
        :paused (not (plist-get snapshot :playing))
        :entries (plist-get snapshot :entries)))

(defun supersonic-music-assistant-player--make-poller ()
  "Return a poller for the selected player that has not polled yet."
  (supersonic-poller-create
   :backend 'music-assistant
   :interval 'supersonic-music-assistant-poll-interval
   :fetch #'supersonic-music-assistant-player--fetch
   :summarize #'supersonic-music-assistant-player--summarize
   :description "poll the Music Assistant player"))

;; `defvar', so that re-evaluating this file keeps the poller and with
;; it what polling found.  After changing the slots of
;; `supersonic-poller', set this to
;; (supersonic-music-assistant-player--make-poller) by hand before
;; re-evaluating this file.
(defvar supersonic-music-assistant-player--poller (supersonic-music-assistant-player--make-poller)
  "The poller of the selected player's queue.
Its snapshot is a plist of `:items', the entries as (TRACK-ID . TRACK);
`:entries', their track ids; `:current-index'; `:track-id'; `:playing';
`:ended', non-nil if the queue was played to its end; `:position' in
seconds as of `:polled-at'; `:duration'; `:items-key'; and
`:items-polled-at'.")

(defun supersonic-music-assistant-player--snapshot ()
  "Return the player's latest snapshot, or nil."
  (supersonic-poller-snapshot supersonic-music-assistant-player--poller))

(defun supersonic-music-assistant-player--position ()
  "Return the player's estimated current position."
  (let ((snapshot (supersonic-music-assistant-player--snapshot)))
    (supersonic-poller-position snapshot (plist-get snapshot :playing) (plist-get snapshot :duration))))

(defun supersonic-music-assistant-player--poll ()
  "Return a promise of polling the player."
  (supersonic-poller-poll supersonic-music-assistant-player--poller))

;;;
;;; The facade's status/queue/live-p operations
;;;

(defun supersonic-music-assistant-player-live-p ()
  "Return non-nil if the last poll of the player succeeded."
  (supersonic-poller-live supersonic-music-assistant-player--poller))

(aio-defun supersonic-music-assistant-player-status (key)
  "Return a promise resolving to the player's current KEY."
  (pcase key
    ('track-id (plist-get (supersonic-music-assistant-player--snapshot) :track-id))
    ('position (supersonic-music-assistant-player--position))
    ('paused (not (plist-get (supersonic-music-assistant-player--snapshot) :playing)))))

(aio-defun supersonic-music-assistant-player-queue ()
  "Return a promise resolving to the player's queue."
  (let* ((snapshot (supersonic-music-assistant-player--snapshot))
         (current-index (plist-get snapshot :current-index)))
    (seq-map-indexed
     (lambda (item index)
       (append (list :track-id (car item) :current (eql index current-index))
               (and (cdr item) (list :track (cdr item)))))
     (plist-get snapshot :items))))

;;;
;;; The facade's playing operations
;;;

;; Each is a plain function the facade fires and forgets, settling
;; the player and the connection before handing the work to an
;; `aio-defun' that reports its own errors, as the jukebox's do.

(aio-defun supersonic-music-assistant-player--run (target description command &optional args)
  "Run queue COMMAND with ARGS on TARGET, then poll.
Report a failure as failing to DESCRIPTION."
  (supersonic--with-async-error-handling nil description
    (aio-await (supersonic-music-assistant-player--send target command args))
    (aio-await (supersonic-music-assistant-player--poll))))

(defun supersonic-music-assistant-player--with-chosen-target (description sole-format action)
  "Call ACTION with the target of a play command.
If no player is selected, let the user choose one first, see
`supersonic-music-assistant-player--choose' for SOLE-FORMAT.  Report
a failure as failing to DESCRIPTION."
  (let ((connection (supersonic-music-assistant--connection))
        (queue-id (plist-get supersonic-music-assistant-player :id)))
    (ignore
     (if queue-id
         (funcall action (cons queue-id connection))
       (supersonic-music-assistant-player--choose-then connection description sole-format action)))))

(defun supersonic-music-assistant-player-start (ids)
  "Replace the player's queue with IDS and start playing."
  (supersonic-music-assistant-player--with-chosen-target
   "play on the Music Assistant player" "Playing on %s"
   (lambda (target)
     (supersonic-music-assistant-player--run
      target "play on the Music Assistant player"
      "player_queues/play_media"
      `(("media" . ,(vconcat ids)) ("option" . "replace"))))))

(aio-defun supersonic-music-assistant-player--enqueue (target ids)
  "Add IDS to the queue of TARGET.
Start playing if the queue was empty or played to its end."
  (supersonic--with-async-error-handling nil "enqueue on the Music Assistant player"
    (aio-await (supersonic-music-assistant-player--poll))
    (let* ((snapshot (supersonic-music-assistant-player--snapshot))
           (idle (or (null (plist-get snapshot :entries)) (plist-get snapshot :ended))))
      (aio-await (supersonic-music-assistant-player--send
                  target "player_queues/play_media"
                  `(("media" . ,(vconcat ids)) ("option" . "add"))))
      (when idle
        (aio-await (supersonic-music-assistant-player--send target "player_queues/play"))))
    (aio-await (supersonic-music-assistant-player--poll))))

(defun supersonic-music-assistant-player-enqueue (ids)
  "Add IDS to the player's queue.
Start playing if the queue was empty or played to its end."
  (supersonic-music-assistant-player--with-chosen-target
   "enqueue on the Music Assistant player" "Enqueueing on %s"
   (lambda (target) (supersonic-music-assistant-player--enqueue target ids))))

(defun supersonic-music-assistant-player-toggle-play ()
  "Pause or resume the player."
  (ignore
   (supersonic-music-assistant-player--run
    (supersonic-music-assistant-player--target) "toggle Music Assistant playback" "player_queues/play_pause")))

(defun supersonic-music-assistant-player-next ()
  "Skip to the next entry in the player's queue."
  (ignore
   (supersonic-music-assistant-player--run
    (supersonic-music-assistant-player--target) "skip to the next Music Assistant track" "player_queues/next")))

(defun supersonic-music-assistant-player-prev ()
  "Go back to the previous entry, or to the start of the current one."
  (ignore
   (supersonic-music-assistant-player--run
    (supersonic-music-assistant-player--target) "skip to the previous Music Assistant track" "player_queues/previous")))

(defun supersonic-music-assistant-player-stop ()
  "Stop the player, if one is selected."
  (let ((target (ignore-error user-error (supersonic-music-assistant-player--target))))
    (when target
      (ignore
       (supersonic-music-assistant-player--run target "stop the Music Assistant player" "player_queues/stop")))))

(aio-defun supersonic-music-assistant-player--seek-to (target position)
  "Seek TARGET's current entry to POSITION seconds."
  (supersonic--with-async-error-handling nil "seek the Music Assistant player"
    (let ((duration (plist-get (supersonic-music-assistant-player--snapshot) :duration)))
      (aio-await (supersonic-music-assistant-player--send
                  target "player_queues/seek"
                  `(("position" . ,(max 0 (round (if duration (min position duration) position))))))))
    (aio-await (supersonic-poller-finish-seek supersonic-music-assistant-player--poller))))

(defun supersonic-music-assistant-player-seek (offset)
  "Seek OFFSET seconds from the player's current position."
  (let ((target (supersonic-music-assistant-player--target))
        (position (supersonic-music-assistant-player--position)))
    (unless position
      (user-error "The Music Assistant player has reported no position to seek from yet"))
    (ignore (supersonic-music-assistant-player--seek-to target (+ position offset)))))

(defun supersonic-music-assistant-player-seek-fraction (fraction)
  "Seek the player to FRACTION, between 0.0 and 1.0, of the current entry."
  (let ((target (supersonic-music-assistant-player--target))
        (duration (plist-get (supersonic-music-assistant-player--snapshot) :duration)))
    (unless duration
      (user-error "The Music Assistant player has reported no duration to seek within yet"))
    (ignore (supersonic-music-assistant-player--seek-to target (* fraction duration)))))

;;;
;;; Selecting the player
;;;

(defun supersonic-music-assistant-player--selectable (players)
  "Return the enabled and available ones of PLAYERS."
  (seq-filter (lambda (player)
                (and (assoc-default "enabled" player)
                     (assoc-default "available" player)))
              players))

(defun supersonic-music-assistant-player--name (player)
  "Return the display name of PLAYER."
  (or (assoc-default "display_name" player) (assoc-default "name" player) (assoc-default "player_id" player)))

(defun supersonic-music-assistant-player--choices (players)
  "Return a completion alist of PLAYERS' names to (:id ID :name NAME).
Names shared by several players get the id appended."
  (let ((names (mapcar #'supersonic-music-assistant-player--name players)))
    (mapcar (lambda (player)
              (let ((name (supersonic-music-assistant-player--name player))
                    (id (assoc-default "player_id" player)))
                (cons (if (> (seq-count (lambda (other) (equal other name)) names) 1)
                          (format "%s (%s)" name id)
                        name)
                      (list :id id :name name))))
            players)))

(aio-defun supersonic-music-assistant-player--choose (connection then &optional sole-format)
  "Let the user choose a player at CONNECTION, then call THEN with its target.
The choice is set for this session; saving it is offered afterwards.
If SOLE-FORMAT is non-nil and there is only one player, take it
without asking and show SOLE-FORMAT with its name.  Resolve to the
chosen (:id ID :name NAME), or nil if none was chosen."
  (let ((choices (supersonic-music-assistant-player--choices
                  (supersonic-music-assistant-player--selectable
                   (aio-await (supersonic-music-assistant--send connection "players/all"))))))
    (unless choices
      (user-error "Music Assistant at %s has no player available" (car connection)))
    (let* ((sole (and sole-format (null (cdr choices))))
           (player (if sole
                       (cdar choices)
                     (cdr (assoc (completing-read "Music Assistant player: " choices nil t) choices))))
           (name (plist-get player :name)))
      (if (not player)
          (ignore (message "No Music Assistant player chosen"))
        (customize-set-variable 'supersonic-music-assistant-player player)
        ;; Before THEN, so that a failure it reports is what shows last.
        (when sole
          (message sole-format name))
        (aio-await (funcall then (cons (plist-get player :id) connection)))
        (unless sole
          (supersonic-playback-offer-to-save 'supersonic-music-assistant-player "Music Assistant player" name))
        player))))

(aio-defun supersonic-music-assistant-player--choose-then (connection description sole-format action)
  "Choose a player at CONNECTION, then call ACTION with its target.
See `supersonic-music-assistant-player--choose' for SOLE-FORMAT.
Report a failure as failing to DESCRIPTION."
  (supersonic--with-async-error-handling nil description
    (aio-await (supersonic-music-assistant-player--choose connection action sole-format))))

;; An explicit cookie: the autoloads generator does not know `aio-defun'
;; and would copy the whole definition, which fails to load without aio.
;;;###autoload (autoload 'supersonic-music-assistant-select-player "supersonic-music-assistant-player" nil t)
(aio-defun supersonic-music-assistant-select-player ()
  "Select the Music Assistant player the `music-assistant' backend plays on.
The choice applies to this session; you are asked whether to save it."
  (interactive)
  (supersonic--with-async-error-handling nil "select a Music Assistant player"
    (aio-await (supersonic-music-assistant-player--choose
                (supersonic-music-assistant--connection)
                (aio-lambda (_target)
                  (when (eq supersonic-playback-backend 'music-assistant)
                    (aio-await (supersonic-music-assistant-player--poll))))))))

(supersonic-poller-register 'supersonic-music-assistant-player--poller)

;; Only for the `music-assistant' provider: the queue takes MA's item
;; URIs, and nothing but a Music Assistant server has its players.
(supersonic-playback-register-backend
 'music-assistant
 '((start . supersonic-music-assistant-player-start)
   (enqueue . supersonic-music-assistant-player-enqueue)
   (toggle-play . supersonic-music-assistant-player-toggle-play)
   (next . supersonic-music-assistant-player-next)
   (prev . supersonic-music-assistant-player-prev)
   (stop . supersonic-music-assistant-player-stop)
   (seek . supersonic-music-assistant-player-seek)
   (seek-fraction . supersonic-music-assistant-player-seek-fraction)
   (live-p . supersonic-music-assistant-player-live-p)
   (status . supersonic-music-assistant-player-status)
   (queue . supersonic-music-assistant-player-queue))
 :providers '(music-assistant))

(provide 'supersonic-music-assistant-player)
;;; supersonic-music-assistant-player.el ends here
