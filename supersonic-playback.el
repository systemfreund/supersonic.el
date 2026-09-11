;;; supersonic-playback.el --- Playback backend facade for supersonic.el -*- lexical-binding: t; -*-

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

;; The single door through which every part of supersonic.el asks for
;; something to be played.  A backend -- currently only mpv, via
;; `supersonic-mpv.el' -- registers the functions implementing a fixed
;; set of playback operations under a name, and the generic
;; `supersonic-playback-*' functions here dispatch to whichever backend
;; `supersonic-playback-backend' currently selects.
;;
;; Traffic runs the other way too: a backend says *that* something about
;; playback may have changed by running the two hooks defined here, and
;; whoever is interested pulls what it needs back through
;; `supersonic-playback-status'.  Signal on the hook, pull on the
;; accessor -- no payload is carried, so a consumer never has to care
;; which backend woke it, and a backend never has to know who is
;; listening.
;;
;; This file therefore knows nothing about mpv, HTTP streams, or the
;; Subsonic API: it only knows the operation names.  The dependency runs
;; the other way round, each backend requiring this file and registering
;; itself on load, so that adding a backend never means touching the
;; buffers and commands that drive playback.

;;; Code:

(require 'aio)
(require 'seq)

(require 'supersonic-custom)

(defconst supersonic-playback-operations
  '(start enqueue toggle-play next prev stop seek seek-fraction live-p status queue)
  "The playback operations a backend can implement.
`start' and `enqueue' each take a list of supersonic track ids; `seek'
takes an offset in seconds, which may be negative; `seek-fraction'
takes a position in the current track as a fraction between 0.0 and
1.0; `status' takes one of `supersonic-playback-status-keys'; `queue'
and the rest take no arguments.  `status' and `queue' each return a
promise, the same as their `supersonic-playback-*' counterparts below.
A backend need not implement all of these -- an operation its player
has no equivalent for is simply left out of the alist passed to
`supersonic-playback-register-backend', and calling the corresponding
`supersonic-playback-*' function then reports that rather than failing
silently.")

(defconst supersonic-playback-status-keys '(track-id position paused)
  "The pieces of current playback state `supersonic-playback-status' answers.
`track-id' is the supersonic id of whatever is playing, `position' its
playback position in seconds, and `paused' non-nil if playback is
paused.  Deliberately the facade's own vocabulary rather than any
backend's property names, so that a consumer asking what is playing
never has to know how the active backend keeps track of it.")

(defvar supersonic-playback-track-change-hook nil
  "Hook run whenever the identity of what is playing may have changed.
Run by whichever backend is active, with no payload: it says only that
something may be different now, and a consumer pulls what it actually
needs via `supersonic-playback-status'.  Buffers hang their refreshes
off this from the outside, so no backend ever has to know they exist.")

(defvar supersonic-playback-state-change-hook nil
  "Hook run whenever playback was paused or resumed.
Only that: a change in the identity of what is playing runs
`supersonic-playback-track-change-hook' instead.")

(defvar supersonic-playback-position-change-hook nil
  "Hook run whenever the playback position jumped rather than crept on.
Seeking is what this is for.  A position that only advances by itself
needs no signal -- whoever shows it re-reads it on a timer of its own
choosing -- but a seek makes the value they last read wrong at once,
and waiting out the rest of a tick to notice is what makes a click on
the waveform seekbar look like it did not land.  So a backend says so
as soon as the seek has actually taken effect, and consumers pull the
new position back through `supersonic-playback-status'.")

(defvar supersonic-playback--backends (make-hash-table :test #'eq)
  "Map of backend name (a symbol) to that backend's operation alist.
Populated by `supersonic-playback-register-backend', which every
backend calls as it is loaded, and read by
`supersonic-playback--implementation' when dispatching.")

(defun supersonic-playback-register-backend (name operations)
  "Register NAME as a playback backend implementing OPERATIONS.
NAME is the symbol users select with `supersonic-playback-backend'.
OPERATIONS is an alist mapping operation symbols from
`supersonic-playback-operations' to the functions implementing them.
Registering a name that is already registered replaces it, so
re-loading a backend file is harmless."
  (dolist (operation operations)
    (unless (memq (car operation) supersonic-playback-operations)
      (error "Unknown playback operation `%s' for backend `%s'" (car operation) name))
    (unless (functionp (cdr operation))
      (error "Implementation of `%s' for backend `%s' is not a function" (car operation) name)))
  (puthash name operations supersonic-playback--backends))

(defun supersonic-playback--implementation (operation)
  "Return the active backend's implementation of OPERATION.
Signals a `user-error' if `supersonic-playback-backend' names a backend
that was never registered -- typically because the file providing it
has not been loaded -- or one that does not implement OPERATION.  Both
are configuration problems the user can act on, hence `user-error'
rather than a backtrace."
  (let ((operations (gethash supersonic-playback-backend supersonic-playback--backends)))
    (unless operations
      (user-error "No playback backend named `%s' is registered" supersonic-playback-backend))
    (or (alist-get operation operations)
        (user-error "The `%s' playback backend cannot %s" supersonic-playback-backend operation))))

(defun supersonic-playback--call (operation &rest args)
  "Call the active backend's implementation of OPERATION with ARGS."
  (apply (supersonic-playback--implementation operation) args))

(defun supersonic-playback-backend-names ()
  "Return the names of every backend currently registered.
That is, everything `supersonic-playback-register-backend' has been
called for so far -- not necessarily every backend a user could select,
since a backend not yet loaded has never registered itself."
  (let (names)
    (maphash (lambda (name _operations) (push name names)) supersonic-playback--backends)
    (nreverse names)))

(defun supersonic-playback-start (ids)
  "Replace the play queue with IDS and start playing immediately."
  (supersonic-playback--call 'start ids))

(defun supersonic-playback-enqueue (ids)
  "Append IDS to the end of the play queue.
Whatever is already playing is left undisturbed; if nothing is,
playback starts."
  (supersonic-playback--call 'enqueue ids))

(defun supersonic-playback-toggle-play ()
  "Toggle between playing and paused."
  (supersonic-playback--call 'toggle-play))

(defun supersonic-playback-next ()
  "Skip to the next track in the play queue."
  (supersonic-playback--call 'next))

(defun supersonic-playback-prev ()
  "Go back to the previous track in the play queue."
  (supersonic-playback--call 'prev))

(defun supersonic-playback-stop ()
  "Stop whatever the active backend currently has playing."
  (supersonic-playback--call 'stop))

(defun supersonic-playback-seek (offset)
  "Seek OFFSET seconds relative to the current position.
A negative OFFSET seeks backwards."
  (supersonic-playback--call 'seek offset))

(defun supersonic-playback--seek-step (arg)
  "Return the number of seconds a seek command called with ARG should move.
ARG is a raw prefix argument or nil, nil meaning `supersonic-seek-step'.
The magnitude only: which way the seek goes is the command's business,
so a negative prefix does not turn a forward seek into a backward one."
  (if arg
      (abs (prefix-numeric-value arg))
    supersonic-seek-step))

(defun supersonic-playback-seek-fraction (fraction)
  "Seek to FRACTION of the way through the current track.
FRACTION is between 0.0 and 1.0.  Separate from
`supersonic-playback-seek' because a fraction is what the waveform
seekbar has to work with -- a click position within an image whose
width stands for the whole track -- and because backends express the
two differently: mpv seeks by percentage natively, whereas a backend
that can only seek to a number of seconds has to multiply by the
running track's duration, which it knows and its callers do not."
  (supersonic-playback--call 'seek-fraction fraction))

(defun supersonic-playback-live-p ()
  "Return non-nil if the active backend currently has playback to report on.
Synchronous, unlike `supersonic-playback-status', because for every
backend this is a locally known fact and never a round-trip: mpv knows
whether its process is running, and a polling backend knows whether its
last poll got an answer.  Callers use it as a plain guard -- whether to
render anything, whether to keep a timer ticking -- where waiting on a
promise would buy nothing."
  (supersonic-playback--call 'live-p))

(aio-defun
 supersonic-playback-status (key)
 "Return a promise resolving to the active backend's current KEY.
KEY is one of `supersonic-playback-status-keys'.  Resolves to nil when
nothing is playing, rather than leaving the caller waiting on a promise
that can never be resolved, so a caller needs no liveness guard of its
own before asking.

Answers one key per call rather than a whole snapshot of state: the
callers that want several values want them at different moments (the
now-playing render deliberately asks for the position last, once its
cover-art fetch is done, so that it is as fresh as possible), and the
one that ticks every second wants only the position.  A backend that
holds all of its state in one polled snapshot answers every key from it
without issuing anything."
 (unless (memq key supersonic-playback-status-keys)
   (error "Unknown playback status key `%s'" key))
 (when (supersonic-playback-live-p)
   (aio-await (supersonic-playback--call 'status key))))

(aio-defun
 supersonic-playback-queue ()
 "Return a promise resolving to the active backend's current play queue.
Each entry is a plist with `:track-id', a supersonic track id, and
`:current', non-nil for whichever entry is currently playing.  Resolves
to nil when nothing is live, the same way `supersonic-playback-status'
does, so a caller needs no liveness guard of its own before asking --
an empty queue and no backend to ask look the same from here."
 (when (supersonic-playback-live-p)
   (aio-await (supersonic-playback--call 'queue))))

(aio-defun
 supersonic-playback--wait-until-live (&optional timeout)
 "Poll `supersonic-playback-live-p' until it answers non-nil.
Gives up and returns nil after TIMEOUT seconds, two by default.
Starting a backend is fire-and-forget at the facade level -- see
`supersonic-playback-start' -- so there is no promise to await for
\"the track has actually begun\"; a caller that needs to know, such as
`supersonic-playback-switch-backend' seeking into a track it just
started on the incoming backend, polls for it instead."
 (let ((deadline (+ (float-time) (or timeout 2))))
   (while (and (not (supersonic-playback-live-p)) (< (float-time) deadline))
     (aio-await (aio-sleep 0.05)))
   (supersonic-playback-live-p)))

;;;###autoload
(defun supersonic-toggle-playing ()
  "Toggle playing/paused state."
  (interactive)
  (supersonic-playback-toggle-play))

;;;###autoload
(defun supersonic-skip-track ()
  "Skip to the next track."
  (interactive)
  (supersonic-playback-next))

;;;###autoload
(defun supersonic-prev-track ()
  "Go to the previous track."
  (interactive)
  (supersonic-playback-prev))

;;;###autoload
(aio-defun
 supersonic-playback-switch-backend (backend)
 "Make BACKEND the active playback backend.
Interactively, prompts among the names
`supersonic-playback-register-backend' has been called for.

Before `supersonic-playback-backend' actually changes, this calls
`supersonic-playback-stop' against whichever backend is still active --
which is why the switch has to happen here rather than via a plain
`setq': mpv's `stop' kills its process, and the jukebox backend's
`stop' sends the server a `stop' action, so nothing each backend's own
`stop' mapping already knows how to tear down keeps running
unsupervised just because Emacs stopped pointing at it.

With `supersonic-playback-sync-queue-on-switch' non-nil, the outgoing
backend's queue -- current track plus whatever is upcoming -- and its
position within the current track are read before it is stopped, then
replayed onto BACKEND once that is active: the queue via
`supersonic-playback-start', the position via `supersonic-playback-seek'
once BACKEND reports itself live (starting a backend is itself
fire-and-forget, so there is otherwise nothing to say the track has
actually begun and can be seeked into).  Reading or replaying either is
best-effort: a failure along the way is reported rather than left to
fail silently, but never stops the switch itself from going through,
since by the time replay would run the old backend is already torn
down.  With the default nil, neither the queue nor the position is
carried over at all -- each backend starts from whatever state it is
independently in."
 (interactive
  (list
   (intern
    (completing-read
     "Switch to playback backend: " (mapcar #'symbol-name (supersonic-playback-backend-names)) nil t))))
 (unless (eq backend supersonic-playback-backend)
   (let (resume-ids resume-position read-error position-error)
     (when supersonic-playback-sync-queue-on-switch
       (condition-case err
           (setq resume-ids
                 (mapcar
                  (lambda (entry) (plist-get entry :track-id))
                  (seq-drop-while
                   (lambda (entry) (not (plist-get entry :current))) (aio-await (supersonic-playback-queue)))))
         (error (setq read-error err)))
       ;; Position is carried best-effort on top of the queue: a backend
       ;; that cannot report it -- or any other failure reading it --
       ;; must not stop the queue itself from being replayed below.
       (when (and resume-ids (not read-error))
         (condition-case err
             (setq resume-position (aio-await (supersonic-playback-status 'position)))
           (error (setq position-error err)))))
     (supersonic-playback-stop)
     (setq supersonic-playback-backend backend)
     (when read-error
       (message "[Supersonic] Failed to read the outgoing queue to carry over: %s" (error-message-string read-error)))
     (when position-error
       (message
        "[Supersonic] Failed to read the outgoing position to carry over: %s" (error-message-string position-error)))
     (when resume-ids
       (condition-case err
           (progn
             (supersonic-playback-start resume-ids)
             (when (and resume-position (> resume-position 0) (aio-await (supersonic-playback--wait-until-live)))
               (condition-case err
                   (supersonic-playback-seek resume-position)
                 (error
                  (message
                   "[Supersonic] Failed to seek `%s' to the outgoing position: %s" backend
                   (error-message-string err))))))
         (error
          (message
           "[Supersonic] Failed to carry the queue over to `%s': %s" backend (error-message-string err))))))))

;;;###autoload
(defun supersonic-seek-forward (&optional seconds)
  "Seek forward by SECONDS, `supersonic-seek-step' by default.
Interactively SECONDS is the numeric prefix argument."
  (interactive "P")
  (supersonic-playback-seek (supersonic-playback--seek-step seconds)))

;;;###autoload
(defun supersonic-seek-back (&optional seconds)
  "Seek back by SECONDS, `supersonic-seek-step' by default.
Interactively SECONDS is the numeric prefix argument."
  (interactive "P")
  (supersonic-playback-seek (- (supersonic-playback--seek-step seconds))))

(provide 'supersonic-playback)
;;; supersonic-playback.el ends here
