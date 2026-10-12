;;; supersonic-jukebox.el --- Subsonic jukeboxControl playback backend -*- lexical-binding: t; -*-

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

;; Optional playback backend that plays on the Subsonic server's own
;; jukebox (the `jukeboxControl' endpoint), out of the server's
;; speakers rather than this machine's.  Load it and set
;; `supersonic-playback-backend' to `jukebox'.
;;
;; The Subsonic API cannot push jukebox state, so `action=get', which
;; returns playlist and status in one request, is polled through
;; `supersonic-poller.el'.  Status, queue and liveness answer from what
;; the latest poll found, and the position is counted on in between.
;; No server scrobbles jukebox playback itself, so a track change from
;; one poll to the next is scrobbled here.
;;
;; Actions some servers reject, such as `skip' with an `offset'
;; (Ampache), are sent anyway and the server's error is surfaced.

;;; Code:
(require 'seq)
(require 'aio)
(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-provider)
(require 'supersonic-playback)
(require 'supersonic-poller)

;;;
;;; Talking to jukeboxControl
;;;

(defun supersonic-jukebox--url (action &optional extra-query)
  "Build a jukeboxControl.view URL for ACTION, with EXTRA-QUERY appended."
  (supersonic-build-url "/jukeboxControl.view" (cons (cons "action" action) extra-query)))

(aio-defun
 supersonic-jukebox--request (action &optional extra-query)
 "Issue jukeboxControl ACTION with EXTRA-QUERY.
Returns a promise resolving to the parsed \"subsonic-response\" body."
 (aio-await (supersonic-get-json (supersonic-jukebox--url action extra-query))))

(defun supersonic-jukebox--id-query (ids)
  "Return a query alist with one (\"id\" . ID) pair per id in IDS.
jukeboxControl's `set' and `add' take one id parameter per song."
  (mapcar (lambda (id) (cons "id" id)) ids))

;;;
;;; The cached snapshot
;;;

(defun supersonic-jukebox--parse-snapshot (playlist polled-at)
  "Turn PLAYLIST, a parsed jukeboxPlaylist (action=get), into a snapshot plist.
POLLED-AT is the `float-time' PLAYLIST was received at.  See
`supersonic-jukebox--poller' for the plist.  \"playing\" is compared
against t, since `json-read' turns false into the non-nil `:json-false'."
  (let* ((entries (assoc-default "entry" playlist))
         (current-index (truncate (or (assoc-default "currentIndex" playlist) -1))))
    (list
     :entries (mapcar (lambda (entry) (assoc-default "id" entry)) entries)
     :current-index current-index
     :playing (eq (assoc-default "playing" playlist) t)
     :position (assoc-default "position" playlist)
     :duration (and (>= current-index 0) (assoc-default "duration" (nth current-index entries)))
     :polled-at polled-at)))

(defun supersonic-jukebox--current-track (snapshot)
  "Return the supersonic track id SNAPSHOT's `:current-index' points at, or nil.
nil both when SNAPSHOT itself is nil (no poll has landed yet) and when
its `:current-index' is -1 (nothing loaded)."
  (let ((index (plist-get snapshot :current-index)))
    (and index (>= index 0) (nth index (plist-get snapshot :entries)))))

(defun supersonic-jukebox--summarize (snapshot)
  "Return what the facade's hooks announce of SNAPSHOT.
The plist `supersonic-poller-announce' compares: the playing entry's
track id, whether the jukebox is paused, and the playlist's entries,
so that an `add' is noticed too."
  (list :track-id (supersonic-jukebox--current-track snapshot)
        :paused (not (plist-get snapshot :playing))
        :entries (plist-get snapshot :entries)))

(aio-defun
 supersonic-jukebox--fetch ()
 "Ask jukeboxControl for its playlist and status, as a fresh snapshot.
See `supersonic-jukebox--parse-snapshot'."
 (let* ((response (aio-await (supersonic-jukebox--request "get")))
        (polled-at (float-time))
        (playlist (supersonic-recursive-assoc response '("subsonic-response" "jukeboxPlaylist"))))
   (supersonic-jukebox--parse-snapshot playlist polled-at)))

(defun supersonic-jukebox--make-poller ()
  "Return a poller for the jukebox that has not polled yet."
  (supersonic-poller-create
   :backend 'jukebox
   :interval 'supersonic-jukebox-poll-interval
   :fetch #'supersonic-jukebox--fetch
   :summarize #'supersonic-jukebox--summarize
   :description "poll the jukebox"))

;; `defvar', so that re-evaluating this file keeps the poller and with
;; it what polling found.  After changing the slots of `supersonic-poller',
;; set this to (supersonic-jukebox--make-poller) by hand before re-evaluating
;; this file: the old poller no longer fits the new accessors, and
;; `supersonic-poller-register' below would trip over it.
(defvar supersonic-jukebox--poller (supersonic-jukebox--make-poller)
  "Polls the jukebox, and holds what the latest poll found.
Its snapshot is a plist: `:entries', the supersonic track ids in the
jukebox's playlist, in order; `:current-index', the 0-based index into
`:entries' of the playing entry, or -1 for none; `:playing', non-nil if
the jukebox is actually playing rather than paused; `:position', the
current track's position in seconds as of `:polled-at', a `float-time'
timestamp of when this snapshot was taken; `:duration', the current
track's duration in seconds, or nil if there is no current track or the
server left it out.")

(defun supersonic-jukebox--snapshot ()
  "Return the jukebox state the latest poll found, or nil before any landed."
  (supersonic-poller-snapshot supersonic-jukebox--poller))

(defun supersonic-jukebox--position ()
  "Return the jukebox's position, counted on from the latest poll.
See `supersonic-poller-position'."
  (let ((snapshot (supersonic-jukebox--snapshot)))
    (supersonic-poller-position snapshot (plist-get snapshot :playing))))

(defun supersonic-jukebox--poll ()
  "Return a promise of polling the jukebox; see `supersonic-poller-poll'."
  (supersonic-poller-poll supersonic-jukebox--poller))

(defun supersonic-jukebox-live-p ()
  "Return non-nil if the jukebox backend's last poll succeeded."
  (supersonic-poller-live supersonic-jukebox--poller))

;;;
;;; The facade's status/queue/live-p operations
;;;

(aio-defun
 supersonic-jukebox-status (key)
 "Resolve to the jukebox backend's current KEY, read from the cached snapshot.
Never issues a request of its own -- see `supersonic-jukebox--poll'."
 (pcase key
   ('track-id (supersonic-jukebox--current-track (supersonic-jukebox--snapshot)))
   ('position (supersonic-jukebox--position))
   ('paused (not (plist-get (supersonic-jukebox--snapshot) :playing)))))

(aio-defun
 supersonic-jukebox-queue ()
 "Resolve to the jukebox's current playlist, read from the cached poll snapshot.
Never issues a request of its own -- see `supersonic-jukebox--poll'."
 (let ((current-index (plist-get (supersonic-jukebox--snapshot) :current-index)))
   (seq-map-indexed
    (lambda (id index) (list :track-id id :current (eql index current-index)))
    (plist-get (supersonic-jukebox--snapshot) :entries))))

;;;
;;; The facade's start/enqueue/toggle-play/next operations
;;;

;; The facade fires these and forgets them, so each does its work in an
;; `aio-defun' helper wrapped in `supersonic--with-async-error-handling':
;; an error would otherwise reject a promise nobody awaits, and vanish.

(aio-defun
 supersonic-jukebox--start (ids) "Replace the jukebox playlist with IDS and start playing."
 (supersonic--with-async-error-handling
  nil
  "start jukebox playback"
  (aio-await (supersonic-jukebox--request "set" (supersonic-jukebox--id-query ids)))
  (aio-await (supersonic-jukebox--request "start"))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-start (ids)
  "Replace the jukebox playlist with IDS and start playing immediately."
  (ignore (supersonic-jukebox--start ids)))

(aio-defun
 supersonic-jukebox--enqueue (ids)
 "Append IDS to the jukebox playlist, playing them if nothing is left to play.
That is, when the playlist was empty or played to its end, which
Navidrome reports as the last entry, not playing, at position 0.  A
jukebox stopped partway is left stopped, since `start' would only
resume that entry (#39).  Polls first, since the cached snapshot may
be stale.  `skip' does not start a stopped jukebox, hence the `start'
after it."
 (supersonic--with-async-error-handling
  nil "enqueue on the jukebox"
  (aio-await (supersonic-jukebox--poll))
  (let* ((snapshot (supersonic-jukebox--snapshot))
         (length (length (plist-get snapshot :entries)))
         (finished (and (not (plist-get snapshot :playing))
                        (eql (plist-get snapshot :current-index) (1- length))
                        (eql (plist-get snapshot :position) 0))))
    (aio-await (supersonic-jukebox--request "add" (supersonic-jukebox--id-query ids)))
    (cond
     ((zerop length)
      (aio-await (supersonic-jukebox--request "start")))
     (finished
      (aio-await (supersonic-jukebox--request "skip" `(("index" . ,(number-to-string length)))))
      (aio-await (supersonic-jukebox--request "start")))))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-enqueue (ids)
  "Append IDS to the jukebox playlist, playing them if nothing is left to play."
  (ignore (supersonic-jukebox--enqueue ids)))

(aio-defun
 supersonic-jukebox--toggle-play ()
 "Toggle the jukebox between playing and stopped.
Based on the cached snapshot's `:playing'."
 (supersonic--with-async-error-handling
  nil "toggle jukebox playback"
  (aio-await
   (supersonic-jukebox--request
    (if (plist-get (supersonic-jukebox--snapshot) :playing)
        "stop"
      "start")))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-toggle-play ()
  "Toggle between playing and paused on the jukebox."
  (ignore (supersonic-jukebox--toggle-play)))

(aio-defun
 supersonic-jukebox--next ()
 "Skip the jukebox to the entry after the cached snapshot's current one.
A no-op past the last entry: `skip' has no bounds of its own, so asking
it for an out-of-range index there just replays the last entry instead
of stopping (#50)."
 (supersonic--with-async-error-handling
  nil "skip to the next jukebox track"
  (let* ((snapshot (supersonic-jukebox--snapshot))
         (index (1+ (or (plist-get snapshot :current-index) -1)))
         (length (length (plist-get snapshot :entries))))
    (when (< index length)
      (aio-await (supersonic-jukebox--request "skip" `(("index" . ,(number-to-string index)))))))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-next ()
  "Skip to the next track in the jukebox playlist."
  (ignore (supersonic-jukebox--next)))

(aio-defun
 supersonic-jukebox--prev ()
 "Skip the jukebox to the entry before the cached snapshot's current one.
jukeboxControl has no \"previous track\" action, only `skip' to an
index.  A no-op before the first entry: `skip' has no bounds of its
own, and a negative index replays the last entry (#50)."
 (supersonic--with-async-error-handling
  nil "skip to the previous jukebox track"
  (let ((index (1- (or (plist-get (supersonic-jukebox--snapshot) :current-index) -1))))
    (when (>= index 0)
      (aio-await (supersonic-jukebox--request "skip" `(("index" . ,(number-to-string index)))))))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-prev ()
  "Go to the previous track in the jukebox playlist."
  (ignore (supersonic-jukebox--prev)))

(aio-defun
 supersonic-jukebox--seek (offset)
 "Seek OFFSET seconds relative to the jukebox's current position.
jukeboxControl's `skip' only takes an absolute `offset' within the
track at `index', so this skips to the current index at the counted-on
position plus OFFSET, no earlier than the start."
 (supersonic--with-async-error-handling
  nil "seek the jukebox"
  (let* ((index (or (plist-get (supersonic-jukebox--snapshot) :current-index) -1))
         (position (or (supersonic-jukebox--position) 0))
         (target (max 0 (round (+ position offset)))))
    (aio-await
     (supersonic-jukebox--request
      "skip" `(("index" . ,(number-to-string index)) ("offset" . ,(number-to-string target))))))
  (aio-await (supersonic-poller-finish-seek supersonic-jukebox--poller))))

(defun supersonic-jukebox-seek (offset)
  "Seek OFFSET seconds relative to the current position on the jukebox."
  (ignore (supersonic-jukebox--seek offset)))

(aio-defun
 supersonic-jukebox--seek-fraction (fraction)
 "Seek the jukebox to FRACTION (0.0 to 1.0) of the way through the current track.
`skip' wants seconds, so FRACTION is multiplied by the cached
`:duration', or by 0 if the server left that out."
 (supersonic--with-async-error-handling
  nil "seek the jukebox"
  (let* ((snapshot (supersonic-jukebox--snapshot))
         (index (or (plist-get snapshot :current-index) -1))
         (duration (or (plist-get snapshot :duration) 0))
         (target (round (* fraction duration))))
    (aio-await
     (supersonic-jukebox--request
      "skip" `(("index" . ,(number-to-string index)) ("offset" . ,(number-to-string target))))))
  (aio-await (supersonic-poller-finish-seek supersonic-jukebox--poller))))

(defun supersonic-jukebox-seek-fraction (fraction)
  "Seek to FRACTION (0.0 to 1.0) of the way through the jukebox's current track."
  (ignore (supersonic-jukebox--seek-fraction fraction)))

(aio-defun
 supersonic-jukebox--stop ()
 "Stop the jukebox.
Unlike `supersonic-jukebox--toggle-play', always sends `stop', whatever
the cached snapshot says."
 (supersonic--with-async-error-handling
  nil "stop the jukebox"
  (aio-await (supersonic-jukebox--request "stop"))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-stop ()
  "Stop playback on the jukebox."
  (ignore (supersonic-jukebox--stop)))

(supersonic-poller-register 'supersonic-jukebox--poller)

;; Only for the `subsonic' provider: only a Subsonic server has a
;; jukebox, and it takes Subsonic ids.
(supersonic-playback-register-backend
 'jukebox
 '((start . supersonic-jukebox-start)
   (enqueue . supersonic-jukebox-enqueue)
   (toggle-play . supersonic-jukebox-toggle-play)
   (next . supersonic-jukebox-next)
   (prev . supersonic-jukebox-prev)
   (stop . supersonic-jukebox-stop)
   (seek . supersonic-jukebox-seek)
   (seek-fraction . supersonic-jukebox-seek-fraction)
   (live-p . supersonic-jukebox-live-p)
   (status . supersonic-jukebox-status)
   (queue . supersonic-jukebox-queue))
 :providers '(subsonic))

(provide 'supersonic-jukebox)

;;; supersonic-jukebox.el ends here
