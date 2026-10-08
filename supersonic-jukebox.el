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

;; Optional playback backend that hands playback to the Subsonic
;; server's own remote jukebox (the `jukeboxControl' endpoint) instead
;; of a local mpv process, so tracks play out of the server's own
;; speakers rather than being streamed to this machine.  Registers
;; itself with the facade in `supersonic-playback.el' under the name
;; `jukebox', the same way `supersonic-mpv.el' registers `mpv' -- so it
;; is used by setting `supersonic-playback-backend' to `jukebox'.
;;
;; The Subsonic API has no push/event mechanism for jukebox state, so
;; this polls `action=get' -- which returns both the jukebox's playlist
;; and its status (current index, playing, position) in a single
;; request -- through the machinery in `supersonic-poller.el', which
;; caches the result and fires the facade's generalized
;; track-change/state-change hooks whenever it changes.  Every other
;; jukebox operation this file implements -- the status accessor, the
;; queue listing, and liveness -- answers from that cache instead of
;; issuing a request of its own.  Polling runs only while `jukebox' is
;; the active backend; see `supersonic-poller-register'.
;;
;; `position' is the one exception to "answers from the cache
;; verbatim": raising the poll interval to ease server load would
;; otherwise directly stall the now-playing buffer's own once-a-second
;; position display, so it is instead interpolated forward from the
;; cached position by the wall-clock time elapsed since the poll that
;; produced it -- see `supersonic-poller-position' -- letting that
;; display keep counting up smoothly between polls without the poll
;; interval itself needing to be anywhere near 1s.
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
;; running".  This matters in particular for seeking (see
;; `supersonic-jukebox--seek'/`-seek-fraction'): jukeboxControl's `skip'
;; takes an `offset' parameter some servers accept and others (e.g.
;; Ampache) reject outright, and such a rejection is left to surface
;; the same way any other action's would.
;;
;; jukeboxControl has no dedicated "previous track" action either, only
;; `skip' to an arbitrary index -- so `supersonic-jukebox--prev' skips
;; to the cached snapshot's `:current-index' minus one, and seeking
;; skips to the current index with an `offset', the position within the
;; track `skip' wants in place of mpv's relative seconds -- see
;; `supersonic-jukebox--seek' for the conversion.
;;
;; No Subsonic server scrobbles jukebox playback on its own, so this
;; file has to do what `supersonic-mpv.el' does off mpv's own
;; start-file/end-file events, but off a poll tick instead: whenever a
;; poll's current track id differs from the previous poll's, the
;; previous id is scrobbled as a submission and the new one as
;; now-playing -- see `supersonic-poller-announce'.

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
stored as `:polled-at' -- see `supersonic-poller-position'.

`entry' -- the song list -- is unique to jukeboxPlaylist; the status
attributes it is built from (currentIndex/playing/position) are shared
with the bare jukeboxStatus other actions return, which this file
never parses on its own -- see the commentary at the top for why a
poll always re-fetches the whole playlist instead.  `:duration' is
pulled from the same `entry' list, off whichever one `currentIndex'
points at -- a song entry carries its own \"duration\" the same way any
other Subsonic song listing does -- rather than tracked per-entry,
since the only use for it is seeking within the track currently
playing.

`:playing' is compared against t explicitly rather than taken as any
non-nil value, the same as `supersonic--mpv-handle-message' has to for
mpv's own \"pause\" property: `json-read' turns JSON's false into
`:json-false', which is itself non-nil in Lisp."
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
track id, whether the jukebox is not playing, and the playlist's
entries.  Entries count too because an `add' neither moves the current
entry nor touches play/pause -- that covers another client adding to
the jukebox's playlist as well."
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
server left it out -- see `supersonic-jukebox--seek-fraction', the only
reader of this field, for why.  `supersonic-jukebox-status', `-queue'
and `-live-p' all answer from it rather than issuing a fresh request.")

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
  "Return non-nil if the jukebox backend's last poll succeeded.
There is no local process to ask, the way `supersonic-mpv-live-p' asks
whether mpv is still running -- this is the polling backend's
equivalent of that same fact: whether the last poll succeeded."
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
 "Append IDS to the jukebox playlist, playing them if nothing is left to play.
`add' alone leaves the jukebox's play state untouched, so this also
starts playback on the first of IDS when the playlist was empty or had
been played to its end -- `supersonic-playback-enqueue''s contract.  A
jukebox merely stopped partway through its playlist is left stopped:
`start' would only resume the stopped entry, not play IDS -- see #39.

Polls first rather than trusting the cached snapshot, which can be up
to `supersonic-jukebox-poll-interval' seconds stale -- long enough to
still show the last entry playing after it has in fact ended.  Played
to its end is read the way Navidrome reports it: `currentIndex' left on
the last entry, not playing, at position 0.  `skip' does not start a
stopped jukebox by itself, hence the `start' after it."
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
Based on the cached snapshot's `:playing', the same way every other
jukebox operation avoids a request just to learn current state."
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
jukeboxControl has no \"previous track\" action of its own, only `skip'
to an arbitrary index -- see #9 -- so this is `supersonic-jukebox--next'
with the cached `:current-index' decremented instead of incremented.  A
no-op before the first entry: `skip' has no bounds of its own, so asking
it for a negative index there just replays the last entry instead of
doing nothing (#50)."
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
jukeboxControl's `skip' has no relative seek of its own: its `offset'
parameter is an absolute position, in seconds, within the song named by
its `index' parameter -- so this adds OFFSET to the cached snapshot's
interpolated current position (see
`supersonic-poller-position') to get the absolute
position `skip' wants, clamped to never go below the start of the
track. Re-skipping the current index rather than a neighbouring one is
what makes this a seek rather than a track change."
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
`skip''s `offset' parameter wants a position in seconds, not a
fraction, so this multiplies FRACTION by the cached snapshot's
`:duration' -- see `supersonic-jukebox--parse-snapshot' -- to get it;
the caller (a click in the waveform seekbar image) has no other way to
know the track's duration itself. Falls back to an offset of 0 if the
server left `:duration' out of the current track's entry -- the
Subsonic API marks a song's \"duration\" optional, see
`supersonic--format-duration' for the same caveat elsewhere in this
package -- rather than erroring on the arithmetic."
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
 "Stop the jukebox, matching #11's teardown when switching away from it.
Sends the same `stop' jukeboxControl action `-toggle-play' already
sends when the cached snapshot says something is playing; unlike
`-toggle-play' this always sends it regardless of that snapshot, since
a caller of `supersonic-playback-stop' -- #11's backend switch, or the
Stop/Quit commands in `supersonic-mpris.el' -- wants the server to
actually stop, not to have its current state toggled."
 (supersonic--with-async-error-handling
  nil "stop the jukebox"
  (aio-await (supersonic-jukebox--request "stop"))
  (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-stop ()
  "Stop playback on the jukebox."
  (ignore (supersonic-jukebox--stop)))

(supersonic-poller-register 'supersonic-jukebox--poller)

;; Announce jukebox to the playback facade as we are loaded, so that the
;; generic `supersonic-playback-*' functions resolve to the wrappers
;; above as soon as `supersonic-playback-backend' selects `jukebox' --
;; see `supersonic-playback.el'.  Only for the `subsonic' provider:
;; jukeboxControl takes Subsonic ids, and nothing but a Subsonic server
;; has a jukebox to control.
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
