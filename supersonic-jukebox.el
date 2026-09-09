;;; supersonic-jukebox.el --- Subsonic jukeboxControl playback backend -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
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

;; Optional playback backend that hands playback to the Subsonic
;; server's own remote jukebox (the `jukeboxControl' endpoint) instead
;; of a local mpv process, so tracks play out of the server's own
;; speakers rather than being streamed to this machine.  Registers
;; itself with the facade in `supersonic-playback.el' under the name
;; `jukebox', the same way `supersonic-mpv.el' registers `mpv' -- so it
;; is used by setting `supersonic-playback-backend' to `jukebox'.
;;
;; The Subsonic API has no push/event mechanism for jukebox state, so
;; this runs its own poll timer against `action=get' -- which returns
;; both the jukebox's playlist and its status (current index, playing,
;; position) in a single request -- caches the result, and fires the
;; facade's generalized track-change/state-change hooks whenever the
;; cached snapshot changes.  Every other jukebox operation this file
;; implements -- the status accessor, the queue listing, and liveness
;; -- answers from that cache instead of issuing a request of its own.
;; The timer runs only while `jukebox' is the active backend; see
;; `supersonic-jukebox--watch-backend'.
;;
;; `position' is the one exception to "answers from the cache
;; verbatim": raising the poll interval to ease server load would
;; otherwise directly stall the now-playing buffer's own once-a-second
;; position display, so it is instead interpolated forward from the
;; cached position by the wall-clock time elapsed since the poll that
;; produced it -- see `supersonic-jukebox--interpolated-position' --
;; letting that display keep counting up smoothly between polls
;; without the poll interval itself needing to be anywhere near 1s.
;;
;; This file is deliberately one-directional, the same way
;; `supersonic-mpv.el' and `supersonic-mpris.el' are: it only knows the
;; facade's operation names and the Subsonic API, never any buffer
;; supersonic.el renders.  Not loaded or activated automatically --
;; `(require 'supersonic-jukebox)' and setting
;; `supersonic-playback-backend' to `jukebox' are both opt-in, the same
;; as enabling `supersonic-mpris-mode'.
;;
;; No capability pre-checking is done for actions some servers may not
;; support: the action is sent and whatever error the server returns is
;; surfaced, the same way `supersonic-mpv-command' does for "mpv not
;; running".
;;
;; No Subsonic server scrobbles jukebox playback on its own, so this
;; file has to do what `supersonic-mpv.el' does off mpv's own
;; start-file/end-file events, but off a poll tick instead: whenever a
;; poll's current track id differs from the previous poll's, the
;; previous id is scrobbled as a submission and the new one as
;; now-playing -- see `supersonic-jukebox--scrobble-track-change'.

;;; Code:
(require 'seq)
(require 'aio)
(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-playback)

(defvar supersonic-jukebox--timer nil
  "Poll timer driving `supersonic-jukebox--poll'.
Runs only while `jukebox' is the active backend -- see
`supersonic-jukebox--watch-backend' -- so polling never spends a
request against a server nobody is currently asking to hear from.")

(defvar supersonic-jukebox--snapshot nil
  "The most recently polled jukebox state, or nil before any poll has landed.
A plist: `:entries', the supersonic track ids in the jukebox's
playlist, in order; `:current-index', the 0-based index into `:entries'
of the playing entry, or -1 for none; `:playing', non-nil if the
jukebox is actually playing rather than paused; `:position', the
current track's position in seconds as of `:polled-at', a `float-time'
timestamp of when this snapshot was taken. `supersonic-jukebox-status',
`-queue' and `-live-p' all answer from this rather than issuing a
fresh request -- see `supersonic-jukebox--poll'; `:position' itself is
interpolated forward from `:polled-at' rather than read verbatim, see
`supersonic-jukebox--interpolated-position'.")

(defvar supersonic-jukebox--live nil
  "Non-nil if the jukebox backend's last poll got an answer.
There is no local process to ask, the way `supersonic-mpv-live-p' asks
whether mpv is still running -- this is the polling backend's
equivalent of that same fact: whether the last poll succeeded.")

(defvar supersonic-jukebox--poll-failing nil
  "Non-nil once a poll has failed without a later poll succeeding since.
Distinguishes a fresh failure -- including the very first poll ever,
which has no earlier success to fall from -- from one a server outage
already reported, so `supersonic-jukebox--poll' can report exactly
once per outage: never leaving even the first failure silent, and
never narrating a server that stays unreachable once per poll on top
of that.")

(defun supersonic-jukebox-live-p ()
  "Return non-nil if the jukebox backend's last poll succeeded."
  supersonic-jukebox--live)

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
jukeboxControl's `set'/`add' actions each take the id parameter more
than once for more than one song, and `supersonic-alist->query' emits
one \"id=...\" per alist entry regardless of key uniqueness -- so
repeating the parameter is all this takes."
  (mapcar (lambda (id) (cons "id" id)) ids))

;;;
;;; The cached snapshot
;;;

(defun supersonic-jukebox--parse-snapshot (playlist polled-at)
  "Turn PLAYLIST, a parsed jukeboxPlaylist (action=get), into a snapshot plist.
POLLED-AT is a `float-time' timestamp of when PLAYLIST was received,
stored as `:polled-at' -- see `supersonic-jukebox--interpolated-position'.

`entry' -- the song list -- is unique to jukeboxPlaylist; the status
attributes it is built from (currentIndex/playing/position) are shared
with the bare jukeboxStatus other actions return, which this file
never parses on its own -- see the commentary at the top for why a
poll always re-fetches the whole playlist instead.

`:playing' is compared against `t' explicitly rather than taken as any
non-nil value, the same as `supersonic--mpv-handle-message' has to for
mpv's own \"pause\" property: `json-read' turns JSON's false into
`:json-false', which is itself non-nil in Lisp."
  (list
   :entries (mapcar (lambda (entry) (assoc-default "id" entry)) (assoc-default "entry" playlist))
   :current-index (truncate (or (assoc-default "currentIndex" playlist) -1))
   :playing (eq (assoc-default "playing" playlist) t)
   :position (assoc-default "position" playlist)
   :polled-at polled-at))

(defun supersonic-jukebox--current-track (snapshot)
  "Return the supersonic track id SNAPSHOT's `:current-index' points at, or nil.
nil both when SNAPSHOT itself is nil (no poll has landed yet) and when
its `:current-index' is -1 (nothing loaded)."
  (let ((index (plist-get snapshot :current-index)))
    (and index (>= index 0) (nth index (plist-get snapshot :entries)))))

(defun supersonic-jukebox--interpolated-position (snapshot)
  "Return SNAPSHOT's `:position', advanced by the time elapsed since `:polled-at'.
A poll only lands every `supersonic-jukebox-poll-interval' seconds, so
the cached `:position' on its own would otherwise sit still between
polls instead of counting up by the second the way the now-playing
buffer's own tick expects -- interpolating against the wall clock is
what lets it do that without polling the server any more often than
`supersonic-jukebox-poll-interval' calls for. Only while `:playing' --
nothing is elapsing towards the position while paused. The next poll's
real position is authoritative and quietly corrects whatever this
guessed in the meantime, typically by a fraction of a second, bounded
by network latency and the jukebox's own position granularity."
  (let ((position (plist-get snapshot :position)))
    (if (and position (plist-get snapshot :playing))
        (+ position (- (float-time) (plist-get snapshot :polled-at)))
      position)))

(defun supersonic-jukebox--scrobble-track-change (previous-track current-track)
  "Scrobble PREVIOUS-TRACK and CURRENT-TRACK across a detected track change.
No push/event mechanism means this file, unlike `supersonic-mpv.el',
never sees an exact end-of-track moment -- so the previous poll's track
id stands in for \"the track that just finished\" and the current
poll's for \"the track that just started\", the same way mpv's own
end-file/start-file pair does it. PREVIOUS-TRACK is only submitted when
there was one (nothing to submit the very first time a track starts,
with no prior poll to have seen it in), and CURRENT-TRACK is only
announced as now-playing when there is one (nothing to announce once
the jukebox runs out of queue). `supersonic-scrobble' itself gates on
`supersonic-scrobble-plays', so this needs no gate of its own."
  (when previous-track
    (supersonic-scrobble previous-track))
  (when current-track
    (supersonic-scrobble current-track t)))

(defun supersonic-jukebox--announce-changes (previous current)
  "Run the facade's hooks for whatever changed between PREVIOUS and CURRENT.
Track-change when the identity of the playing entry moved -- including
between nothing and something, the same as any other backend going
live or not-live counts as a track change -- state-change when only
play/pause did. Never fires the position-change hook: that one is for
the sudden jump a seek makes, and this file does not implement seeking
\(see #9\); a position simply creeping on between polls needs no signal
of its own, the same as it needs none from mpv.

A track change is also what scrobbling keys off of -- see
`supersonic-jukebox--scrobble-track-change' -- since a poll tick is all
this backend ever gets to notice one."
  (let ((previous-track (supersonic-jukebox--current-track previous))
        (current-track (supersonic-jukebox--current-track current)))
    (if (not (equal previous-track current-track))
        (progn
          (supersonic-jukebox--scrobble-track-change previous-track current-track)
          (run-hooks 'supersonic-playback-track-change-hook))
      (unless (eq (plist-get previous :playing) (plist-get current :playing))
        (run-hooks 'supersonic-playback-state-change-hook)))))

(aio-defun
 supersonic-jukebox--poll ()
 "Poll jukeboxControl for the current playlist/status, caching the result.
Marks the backend live on success and not live on failure -- see
`supersonic-jukebox-live-p' -- and, on failure, also runs the
track-change hook so consumers stop showing a now-stale snapshot as
current. Reports a failure to the user exactly once per outage --
see `supersonic-jukebox--poll-failing' -- rather than never (the very
first poll's failure has no earlier success to fall from) or once per
`supersonic-jukebox-poll-interval' for as long as the server stays
unreachable."
 (let ((previous supersonic-jukebox--snapshot))
   (condition-case err
       (let* ((response (aio-await (supersonic-jukebox--request "get")))
              (polled-at (float-time))
              (playlist (supersonic-recursive-assoc response '("subsonic-response" "jukeboxPlaylist"))))
         (setq supersonic-jukebox--snapshot (supersonic-jukebox--parse-snapshot playlist polled-at))
         (setq supersonic-jukebox--live t)
         (setq supersonic-jukebox--poll-failing nil)
         (supersonic-jukebox--announce-changes previous supersonic-jukebox--snapshot))
     (error
      (setq supersonic-jukebox--live nil)
      (unless supersonic-jukebox--poll-failing
        (setq supersonic-jukebox--poll-failing t)
        (supersonic--report-async-error "poll the jukebox" err)
        (run-hooks 'supersonic-playback-track-change-hook))))))

;;;
;;; The facade's status/queue/live-p operations
;;;

(aio-defun
 supersonic-jukebox-status (key)
 "Resolve to the jukebox backend's current KEY, read from the cached snapshot.
Never issues a request of its own -- see `supersonic-jukebox--poll'."
 (pcase key
   ('track-id (supersonic-jukebox--current-track supersonic-jukebox--snapshot))
   ('position (supersonic-jukebox--interpolated-position supersonic-jukebox--snapshot))
   ('paused (not (plist-get supersonic-jukebox--snapshot :playing)))))

(aio-defun
 supersonic-jukebox-queue ()
 "Resolve to the jukebox's current playlist, read from the cached poll snapshot.
Never issues a request of its own -- see `supersonic-jukebox--poll'."
 (let ((current-index (plist-get supersonic-jukebox--snapshot :current-index)))
   (seq-map-indexed
    (lambda (id index) (list :track-id id :current (eql index current-index)))
    (plist-get supersonic-jukebox--snapshot :entries))))

;;;
;;; The facade's start/enqueue/toggle-play/next operations
;;;
;;; Each of these is a plain function fired and forgotten by the
;;; facade -- see `supersonic-playback.el' -- so the actual async work
;;; happens in an `aio-defun' helper wrapped in
;;; `supersonic--with-async-error-handling', the same pattern
;;; `supersonic-now-playing-fetch-and-render' uses for the same reason:
;;; nothing here awaits the promise, so an error raised inside it would
;;; otherwise reject a promise nobody is listening to and vanish.
;;;

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
 "Append IDS to the jukebox playlist, starting playback if it was idle.
`add' alone leaves the jukebox's play state untouched, which would
leave `supersonic-playback-enqueue''s contract -- \"if nothing is
[playing], playback starts\" -- broken for this backend; whether it
was idle is read from the cached snapshot from before this call, since
`add' does not report it back itself."
 (supersonic--with-async-error-handling
  nil "enqueue on the jukebox"
  (let ((was-playing (plist-get supersonic-jukebox--snapshot :playing)))
    (aio-await (supersonic-jukebox--request "add" (supersonic-jukebox--id-query ids)))
    (unless was-playing
      (aio-await (supersonic-jukebox--request "start"))))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-enqueue (ids)
  "Append IDS to the end of the jukebox playlist."
  (ignore (supersonic-jukebox--enqueue ids)))

(aio-defun
 supersonic-jukebox--toggle-play ()
 "Toggle the jukebox between playing and stopped.
Based on the cached snapshot's `:playing', the same way every other
jukebox operation avoids a request just to learn current state."
 (supersonic--with-async-error-handling
  nil "toggle jukebox playback"
  (aio-await
   (supersonic-jukebox--request
    (if (plist-get supersonic-jukebox--snapshot :playing)
        "stop"
      "start")))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-toggle-play ()
  "Toggle between playing and paused on the jukebox."
  (ignore (supersonic-jukebox--toggle-play)))

(aio-defun
 supersonic-jukebox--next () "Skip the jukebox to the entry after the cached snapshot's current one."
 (supersonic--with-async-error-handling
  nil "skip to the next jukebox track"
  (let ((index (1+ (or (plist-get supersonic-jukebox--snapshot :current-index) -1))))
    (aio-await (supersonic-jukebox--request "skip" `(("index" . ,(number-to-string index))))))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-next ()
  "Skip to the next track in the jukebox playlist."
  (ignore (supersonic-jukebox--next)))

;;;
;;; Polling only while `jukebox' is the active backend
;;;

(defun supersonic-jukebox--start-polling ()
  "Start the jukebox poll timer, unless it is already running.
Also polls once immediately, rather than waiting out the first
`supersonic-jukebox-poll-interval', so the cached snapshot -- and
hence `supersonic-jukebox-live-p' -- reflects real state as soon as
possible after `jukebox' becomes the active backend."
  (unless supersonic-jukebox--timer
    (setq supersonic-jukebox--timer
          (run-at-time supersonic-jukebox-poll-interval supersonic-jukebox-poll-interval #'supersonic-jukebox--poll))
    (ignore (supersonic-jukebox--poll))))

(defun supersonic-jukebox--stop-polling ()
  "Stop the jukebox poll timer and discard whatever it last knew.
Run whenever `jukebox' stops being the active backend, so neither a
stale snapshot nor a timer still spending requests on a server nothing
is asking about lingers past that."
  (when supersonic-jukebox--timer
    (cancel-timer supersonic-jukebox--timer)
    (setq supersonic-jukebox--timer nil))
  (setq supersonic-jukebox--snapshot nil)
  (setq supersonic-jukebox--live nil)
  (setq supersonic-jukebox--poll-failing nil))

(defun supersonic-jukebox--watch-backend (_symbol new-value _operation _where)
  "Start or stop polling as `supersonic-playback-backend' becomes/stops `jukebox'.
Registered on `supersonic-playback-backend' with `add-variable-watcher'
as this file loads, and invoked once by hand right after with the
variable's current value, so a `jukebox' selection already in place
before this file was required is picked up too instead of only ones
that happen afterwards."
  (if (eq new-value 'jukebox)
      (supersonic-jukebox--start-polling)
    (supersonic-jukebox--stop-polling)))

;; `remove-variable-watcher' first so re-evaluating this file (e.g. via
;; `eval-buffer' while developing) cannot stack a second watcher and
;; end up polling twice as often.
(remove-variable-watcher 'supersonic-playback-backend #'supersonic-jukebox--watch-backend)
(add-variable-watcher 'supersonic-playback-backend #'supersonic-jukebox--watch-backend)
(supersonic-jukebox--watch-backend 'supersonic-playback-backend supersonic-playback-backend 'set nil)

;; Announce jukebox to the playback facade as we are loaded, so that the
;; generic `supersonic-playback-*' functions resolve to the wrappers
;; above as soon as `supersonic-playback-backend' selects `jukebox' --
;; see `supersonic-playback.el'. `prev', `stop', `seek' and
;; `seek-fraction' are left out, same as `supersonic-playback-operations'
;; allows any backend to do; see #9 for `prev'/`seek'/`seek-fraction'.
(supersonic-playback-register-backend
 'jukebox
 '((start . supersonic-jukebox-start)
   (enqueue . supersonic-jukebox-enqueue)
   (toggle-play . supersonic-jukebox-toggle-play)
   (next . supersonic-jukebox-next)
   (live-p . supersonic-jukebox-live-p)
   (status . supersonic-jukebox-status)
   (queue . supersonic-jukebox-queue)))

(provide 'supersonic-jukebox)

;;; supersonic-jukebox.el ends here
