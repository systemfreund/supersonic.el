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

;; The Music Assistant playback backend (#60): plays on one of the
;; Music Assistant (MA) server's own players -- speakers, Chromecasts,
;; AirPlay devices, its web player -- the way `supersonic-jukebox.el'
;; plays on a Subsonic server's jukebox.  Registers itself with the
;; facade in `supersonic-playback.el' under the name `music-assistant',
;; for the `music-assistant' provider only: the player's queue takes
;; MA's item URIs, which only that provider hands out.  Opt-in, and
;; loading it loads the provider too:
;;
;;   (require 'supersonic-music-assistant-player)
;;   (setq supersonic-playback-backend 'music-assistant)
;;
;; The player is `supersonic-music-assistant-player', which
;; `supersonic-music-assistant-select-player' picks from the players
;; the server has.  A player's queue has the player's id, so that is
;; what every command names.
;;
;; The provider talks to MA over its plain HTTP API, which tells a
;; client nothing on its own, so this polls the queue through the
;; machinery in `supersonic-poller.el', exactly while `music-assistant'
;; is the active backend.  A poll asks `player_queues/get' for the
;; queue's state, and `player_queues/items' for its entries only when
;; that state says they may have changed -- see
;; `supersonic-music-assistant-player--items-key' -- since an entry
;; carries the whole media item and its stream details, and a queue of
;; a few albums would otherwise be a fair bit of JSON every couple of
;; seconds.  The status, queue and liveness operations answer from
;; what the latest poll found, as the jukebox's do.
;;
;; No scrobbling: MA records play history itself, and the provider
;; does not implement `scrobble', which the poller asks before it
;; would.

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
  "Return the id of the selected player's queue.
Signals a `user-error' saying how to select a player when none is."
  (or (plist-get supersonic-music-assistant-player :id)
      (user-error "No Music Assistant player selected; select one with M-x supersonic-music-assistant-select-player")))

(defun supersonic-music-assistant-player--target ()
  "Return what a playback command is sent to, as (QUEUE-ID . CONNECTION).
CONNECTION is as `supersonic-music-assistant--connection' returns it.
For an operation to call before going asynchronous, so that no player
selected, or no server set, is reported to the command that asked,
and anything auth-source asks of the user is asked by that command."
  (let ((queue-id (supersonic-music-assistant-player--queue-id)))
    (cons queue-id (supersonic-music-assistant--connection))))

(aio-defun
 supersonic-music-assistant-player--send (target command &optional args)
 "Return a promise of the result of running queue COMMAND with ARGS on TARGET.
TARGET is as returned by `supersonic-music-assistant-player--target';
its queue id goes with ARGS as `queue_id'."
 (pcase-let ((`(,queue-id . ,connection) target))
   (aio-await (supersonic-music-assistant--send connection command `(("queue_id" . ,queue-id) ,@args)))))

;;;
;;; Polling the queue
;;;

(defvar supersonic-music-assistant-player--poller)

(defun supersonic-music-assistant-player--items-key (queue)
  "Return what tells whether QUEUE's entries may have changed since a poll.
QUEUE is what `player_queues/get' answered.  Its id, the number of
entries, the current and the next entry -- an MA queue entry has an
id of its own, made up afresh whenever it is queued -- and whether it
is shuffled.  Entries added, removed or replaced change one of those;
only an entry moved further down the queue, by another client, goes
unnoticed until something else changes."
  (list (assoc-default "queue_id" queue)
        (assoc-default "items" queue)
        (assoc-default "queue_item_id" (assoc-default "current_item" queue))
        (assoc-default "queue_item_id" (assoc-default "next_item" queue))
        (assoc-default "shuffle_enabled" queue)))

(defun supersonic-music-assistant-player--entry-id (item)
  "Return the track id of queue entry ITEM: its media item's URI, or nil."
  (assoc-default "uri" (assoc-default "media_item" item)))

(defun supersonic-music-assistant-player--entry-track (item)
  "Return queue entry ITEM's media item as a facade track plist, or nil.
An entry carries the whole media item, so the queue buffer need not
ask `music/item_by_uri' for each entry again."
  (let ((media-item (assoc-default "media_item" item)))
    (and (consp media-item) (supersonic-music-assistant--track media-item))))

(defun supersonic-music-assistant-player--parse-snapshot (queue items polled-at)
  "Turn QUEUE, as `player_queues/get' answered, into a snapshot plist.
ITEMS are its entries, in order, as (TRACK-ID . TRACK) each; TRACK is
the entry's track plist, or nil.  POLLED-AT is a
`float-time' timestamp of when QUEUE was received.

The position is the queue's `elapsed_time' as of its
`elapsed_time_last_updated', the server's own timestamp, so that
`supersonic-poller-position' counts on from when the server last
looked rather than from when the answer arrived -- as MA's own client
does.  The timestamp is read on the local clock, shifted by
`supersonic-music-assistant--clock-offset': a server clock running
behind would otherwise put the position ahead by as much, and a
relative seek with it.  Never later than POLLED-AT."
  (let* ((current (assoc-default "current_item" queue))
         (updated (assoc-default "elapsed_time_last_updated" queue))
         (index (assoc-default "current_index" queue)))
    (list :items-key (supersonic-music-assistant-player--items-key queue)
          :items items
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

(aio-defun
 supersonic-music-assistant-player--fetch ()
 "Ask the selected player's queue for its state, as a fresh snapshot.
Its entries are asked for too, unless it has none, or the snapshot
polled last found the same `supersonic-music-assistant-player--items-key'.  See
`supersonic-music-assistant-player--parse-snapshot'."
 (let* ((queue-id (supersonic-music-assistant-player--queue-id))
        (connection (supersonic-music-assistant--connection))
        (queue (aio-await (supersonic-music-assistant--send
                           connection "player_queues/get" `(("queue_id" . ,queue-id)))))
        (polled-at (float-time))
        (previous (supersonic-poller-snapshot supersonic-music-assistant-player--poller)))
   (unless (consp queue)
     (user-error "Music Assistant has no player %s; select one with M-x supersonic-music-assistant-select-player"
                 (or (plist-get supersonic-music-assistant-player :name) queue-id)))
   (supersonic-music-assistant-player--parse-snapshot
    queue
    (cond
     ((eql 0 (assoc-default "items" queue))
      nil)
     ((and previous
           (equal (plist-get previous :items-key) (supersonic-music-assistant-player--items-key queue)))
      (plist-get previous :items))
     (t
      (mapcar (lambda (item)
                (cons (supersonic-music-assistant-player--entry-id item)
                      (supersonic-music-assistant-player--entry-track item)))
              (aio-await (supersonic-music-assistant--all-items
                          "player_queues/items" `(("queue_id" . ,queue-id)) connection)))))
    polled-at)))

(defun supersonic-music-assistant-player--summarize (snapshot)
  "Return what the facade's hooks announce of SNAPSHOT.
See `supersonic-poller-announce'."
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
  "Polls the selected player's queue, and holds what the latest poll found.
Its snapshot is a plist: `:items', the queue's entries, in order, as
\(TRACK-ID . TRACK) each, TRACK being the entry's track plist or nil;
`:entries', their track ids; `:current-index', the 0-based index into `:entries'
of the current entry, or nil; `:track-id', the current entry's track
id; `:playing', non-nil if the player is actually playing; `:ended',
non-nil if the queue was played to its end; `:position', the current
entry's position in seconds as of `:polled-at', a `float-time'
timestamp; `:duration', the current entry's duration in seconds, or
nil; and `:items-key', see `supersonic-music-assistant-player--items-key'.")

(defun supersonic-music-assistant-player--snapshot ()
  "Return the queue state the latest poll found, or nil before any landed."
  (supersonic-poller-snapshot supersonic-music-assistant-player--poller))

(defun supersonic-music-assistant-player--position ()
  "Return the player's position, counted on from the latest poll.
See `supersonic-poller-position'."
  (let ((snapshot (supersonic-music-assistant-player--snapshot)))
    (supersonic-poller-position snapshot (plist-get snapshot :playing) (plist-get snapshot :duration))))

(defun supersonic-music-assistant-player--poll ()
  "Return a promise of polling the player; see `supersonic-poller-poll'."
  (supersonic-poller-poll supersonic-music-assistant-player--poller))

;;;
;;; The facade's status/queue/live-p operations
;;;

(defun supersonic-music-assistant-player-live-p ()
  "Return non-nil if the last poll of the player got an answer."
  (supersonic-poller-live supersonic-music-assistant-player--poller))

(aio-defun
 supersonic-music-assistant-player-status (key)
 "Resolve to the player's current KEY, read from what the latest poll found."
 (pcase key
   ('track-id (plist-get (supersonic-music-assistant-player--snapshot) :track-id))
   ('position (supersonic-music-assistant-player--position))
   ('paused (not (plist-get (supersonic-music-assistant-player--snapshot) :playing)))))

(aio-defun
 supersonic-music-assistant-player-queue ()
 "Resolve to the player's queue, read from what the latest poll found.
Each entry brings its `:track' along, as the poll found it."
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
;;; Each is a plain function the facade fires and forgets, settling
;;; the player and the connection before handing the work to an
;;; `aio-defun' that reports its own errors, as the jukebox's do.
;;;

(aio-defun
 supersonic-music-assistant-player--run (target description command &optional args)
 "Run queue COMMAND with ARGS on TARGET, then poll for its effect.
A failure is reported as failing to DESCRIPTION."
 (supersonic--with-async-error-handling nil description
   (aio-await (supersonic-music-assistant-player--send target command args))
   (aio-await (supersonic-music-assistant-player--poll))))

(defun supersonic-music-assistant-player--media (ids)
  "Return IDS as the `media' argument of `player_queues/play_media'."
  (vconcat ids))

(defun supersonic-music-assistant-player-start (ids)
  "Replace the player's queue with IDS and start playing."
  (ignore
   (supersonic-music-assistant-player--run
    (supersonic-music-assistant-player--target) "play on the Music Assistant player"
    "player_queues/play_media"
    `(("media" . ,(supersonic-music-assistant-player--media ids)) ("option" . "replace")))))

(aio-defun
 supersonic-music-assistant-player--enqueue (target ids)
 "Add IDS to the queue of TARGET, playing them if nothing is left to play.
MA's `add' never starts playback, so this also asks the queue to play
when it was empty or had been played to its end, as
`supersonic-playback-enqueue' promises.  An ended queue has already
moved on to the first of IDS by then, so playing starts there.  A
queue merely stopped partway through is left stopped.  Polls first
rather than trusting the latest poll, which may be a few seconds old.
A server older than MA's `ended' flag looks merely stopped once its
queue has ended, and is left so."
 (supersonic--with-async-error-handling nil "enqueue on the Music Assistant player"
   (aio-await (supersonic-music-assistant-player--poll))
   (let* ((snapshot (supersonic-music-assistant-player--snapshot))
          (idle (or (null (plist-get snapshot :entries)) (plist-get snapshot :ended))))
     (aio-await (supersonic-music-assistant-player--send
                 target "player_queues/play_media"
                 `(("media" . ,(supersonic-music-assistant-player--media ids)) ("option" . "add"))))
     (when idle
       (aio-await (supersonic-music-assistant-player--send target "player_queues/play"))))
   (aio-await (supersonic-music-assistant-player--poll))))

(defun supersonic-music-assistant-player-enqueue (ids)
  "Add IDS to the player's queue, playing them if nothing is left to play."
  (ignore (supersonic-music-assistant-player--enqueue (supersonic-music-assistant-player--target) ids)))

(defun supersonic-music-assistant-player-toggle-play ()
  "Toggle the player between playing and paused."
  (ignore
   (supersonic-music-assistant-player--run
    (supersonic-music-assistant-player--target) "toggle Music Assistant playback" "player_queues/play_pause")))

(defun supersonic-music-assistant-player-next ()
  "Skip to the next entry in the player's queue."
  (ignore
   (supersonic-music-assistant-player--run
    (supersonic-music-assistant-player--target) "skip to the next Music Assistant track" "player_queues/next")))

(defun supersonic-music-assistant-player-prev ()
  "Go back to the previous entry in the player's queue.
As MA's own previous: more than a few seconds into an entry, it starts
that entry over instead."
  (ignore
   (supersonic-music-assistant-player--run
    (supersonic-music-assistant-player--target) "skip to the previous Music Assistant track" "player_queues/previous")))

(defun supersonic-music-assistant-player-stop ()
  "Stop the player.
Nothing to stop with no player selected, or no server to reach it on,
which is no reason to keep `supersonic-playback-switch-backend' from
switching away."
  (let ((target (ignore-error user-error (supersonic-music-assistant-player--target))))
    (when target
      (ignore
       (supersonic-music-assistant-player--run target "stop the Music Assistant player" "player_queues/stop")))))

(aio-defun
 supersonic-music-assistant-player--seek-to (target position)
 "Seek TARGET's current entry to POSITION seconds, then say it moved.
POSITION is clamped to the current entry's duration, past which MA
refuses to seek."
 (supersonic--with-async-error-handling nil "seek the Music Assistant player"
   (let ((duration (plist-get (supersonic-music-assistant-player--snapshot) :duration)))
     (aio-await (supersonic-music-assistant-player--send
                 target "player_queues/seek"
                 `(("position" . ,(max 0 (round (if duration (min position duration) position))))))))
   (aio-await (supersonic-poller-finish-seek supersonic-music-assistant-player--poller))))

(defun supersonic-music-assistant-player-seek (offset)
  "Seek OFFSET seconds relative to the player's current position.
MA's `seek' takes an absolute position, which this adds OFFSET to the
position counted on from the latest poll to get -- rather than
leaving that to `skip', which adds it to whatever position the server
last recorded, seconds old while the player is playing.  Signals a
`user-error' before any poll found a position to seek from."
  (let ((target (supersonic-music-assistant-player--target))
        (position (supersonic-music-assistant-player--position)))
    (unless position
      (user-error "The Music Assistant player has reported no position to seek from yet"))
    (ignore (supersonic-music-assistant-player--seek-to target (+ position offset)))))

(defun supersonic-music-assistant-player-seek-fraction (fraction)
  "Seek to FRACTION (0.0 to 1.0) of the way through the player's current entry.
Of the duration the latest poll found, or the start of the entry if it
found none."
  (ignore
   (supersonic-music-assistant-player--seek-to
    (supersonic-music-assistant-player--target)
    (* fraction (or (plist-get (supersonic-music-assistant-player--snapshot) :duration) 0)))))

;;;
;;; Selecting the player
;;;

(defun supersonic-music-assistant-player--selectable (players)
  "Return those of PLAYERS, as `players/all' answers, a queue can play on.
The enabled and available ones.  Those MA's own client hides are kept:
that includes the web player of a browser, which MA's web UI hides
from every other browser, and which may well be what is listening."
  (seq-filter (lambda (player)
                (and (assoc-default "enabled" player)
                     (assoc-default "available" player)))
              players))

(defun supersonic-music-assistant-player--name (player)
  "Return the name MA shows for PLAYER, as `players/all' answers it."
  (or (assoc-default "display_name" player) (assoc-default "name" player) (assoc-default "player_id" player)))

(defun supersonic-music-assistant-player--choices (players)
  "Return PLAYERS as a completion alist of label to (:id ID :name NAME).
The label is the player's name, followed by its id where two players
share a name."
  (let ((names (mapcar #'supersonic-music-assistant-player--name players)))
    (mapcar (lambda (player)
              (let ((name (supersonic-music-assistant-player--name player))
                    (id (assoc-default "player_id" player)))
                (cons (if (> (seq-count (lambda (other) (equal other name)) names) 1)
                          (format "%s (%s)" name id)
                        name)
                      (list :id id :name name))))
            players)))

;; An explicit cookie: the autoloads generator does not know `aio-defun'
;; and would copy the whole definition, which fails to load without aio.
;;;###autoload (autoload 'supersonic-music-assistant-select-player "supersonic-music-assistant-player" nil t)
(aio-defun
 supersonic-music-assistant-select-player ()
 "Select the Music Assistant player the `music-assistant' backend plays on.
Offers the players the server has by name.  The choice becomes
`supersonic-music-assistant-player' for this session, and is saved
for future ones only if confirmed: saving writes to `custom-file', or
to the init file without one, which not everyone wants touched."
 (interactive)
 (supersonic--with-async-error-handling nil "select a Music Assistant player"
   (let* ((connection (supersonic-music-assistant--connection))
          (choices (supersonic-music-assistant-player--choices
                    (supersonic-music-assistant-player--selectable
                     (aio-await (supersonic-music-assistant--send connection "players/all"))))))
     (unless choices
       (user-error "Music Assistant at %s has no player available" (car connection)))
     (let* ((player (cdr (assoc (completing-read "Music Assistant player: " choices nil t) choices)))
            (name (plist-get player :name)))
       (customize-set-variable 'supersonic-music-assistant-player player)
       (when (eq supersonic-playback-backend 'music-assistant)
         (aio-await (supersonic-music-assistant-player--poll)))
       (if (y-or-n-p (format "Selected Music Assistant player %s; save it for future sessions? " name))
           (progn
             (customize-save-variable 'supersonic-music-assistant-player supersonic-music-assistant-player)
             (message "Saved Music Assistant player %s" name))
         (message "Selected Music Assistant player %s for this session" name))))))

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
