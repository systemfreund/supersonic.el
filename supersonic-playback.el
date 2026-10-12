;;; supersonic-playback.el --- Playback backend facade for supersonic.el -*- lexical-binding: t; -*-

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

;; The single door through which every part of supersonic.el asks for
;; something to be played.  A backend, such as mpv in
;; `supersonic-mpv.el', registers the functions implementing a fixed set
;; of playback operations under a name, and the `supersonic-playback-*'
;; functions here dispatch to whichever backend
;; `supersonic-playback-backend' selects.
;;
;; A backend says that something may have changed by running the hooks
;; defined here, without payload; whoever is interested pulls what it
;; needs through `supersonic-playback-status'.
;;
;; A backend also says which providers it plays for, and an
;; incompatible pairing is kept from being selected or used; see
;; `supersonic-playback-register-backend'.

;;; Code:

(require 'aio)
(require 'seq)

(require 'supersonic-custom)
(require 'supersonic-provider)

(defconst supersonic-playback-operations
  '(start enqueue toggle-play next prev stop seek seek-fraction live-p status queue)
  "The playback operations a backend can implement.
`start' and `enqueue' each take a list of supersonic track ids, and
may finish asynchronously, so a backend reports their failures itself;
`seek' takes an offset in seconds, which may be negative;
`seek-fraction' takes a position in the current track as a fraction
between 0.0 and 1.0; `status' takes one of
`supersonic-playback-status-keys'; the rest take no arguments.
`status' and `queue' each return a promise.  A backend may leave
operations out; calling one then reports that.")

(defconst supersonic-playback-status-keys '(track-id position paused)
  "The pieces of current playback state `supersonic-playback-status' answers.
`track-id' is the supersonic id of whatever is playing, `position' its
playback position in seconds, and `paused' non-nil if playback is
paused.")

(defvar supersonic-playback-track-change-hook nil
  "Hook run whenever the identity of what is playing may have changed.
Run by the active backend, with no payload; a consumer pulls what it
needs via `supersonic-playback-status'.")

(defvar supersonic-playback-state-change-hook nil
  "Hook run whenever playback was paused or resumed.
Only that: a change in the identity of what is playing runs
`supersonic-playback-track-change-hook' instead.")

(defvar supersonic-playback-queue-change-hook nil
  "Hook run whenever the play queue's entries may have changed.
Only while what is playing stays the same, e.g. tracks enqueued behind
it; otherwise `supersonic-playback-track-change-hook' runs instead.  A
consumer pulls the queue via `supersonic-playback-queue'.")

(defvar supersonic-playback-position-change-hook nil
  "Hook run whenever the playback position jumped rather than crept on.
Run once a seek has taken effect.  A position that only creeps on is
re-read on a timer anyway, but waiting for its tick after a seek makes
a click on the waveform seekbar look like it did not land.")

(defvar supersonic-playback--backends (make-hash-table :test #'eq)
  "Map of backend name (a symbol) to that backend's operation alist.")

(defvar supersonic-playback--compatibility (make-hash-table :test #'eq)
  "Map of backend name (a symbol) to the providers it can play for.
Each value is the plist of `:providers' and `:requires' passed to
`supersonic-playback-register-backend'; a backend registered with
neither has no entry and plays for every provider.")

(defconst supersonic-playback--unchecked-operations '(stop live-p status queue)
  "Operations the active backend answers even for an incompatible provider.
None of them plays anything, and refusing them would break the
now-playing buffer, MPRIS, and switching away from the backend.")

(defun supersonic-playback-register-backend (name operations &rest compatibility)
  "Register NAME as a playback backend implementing OPERATIONS.
NAME is the symbol users select with `supersonic-playback-backend'.
OPERATIONS is an alist mapping operation symbols from
`supersonic-playback-operations' to the functions implementing them.
Registering a name that is already registered replaces it, so
re-loading a backend file is harmless.

COMPATIBILITY is a plist saying which library providers -- see
`supersonic-provider' -- the backend can play for:

- `:providers', a list of provider names: the backend plays for these
  and no others.  For a backend tied to one kind of server, such as
  the Subsonic jukebox.
- `:requires', a list of operations from
  `supersonic-provider-operations': the backend plays for any
  provider implementing them all.  For a backend that only needs to be
  told where a track is, such as mpv with `stream-url'.

Given both, a provider must satisfy both.  Given neither, the backend
plays for every provider."
  (dolist (operation operations)
    (unless (memq (car operation) supersonic-playback-operations)
      (error "Unknown playback operation `%s' for backend `%s'" (car operation) name))
    (unless (functionp (cdr operation))
      (error "Implementation of `%s' for backend `%s' is not a function" (car operation) name)))
  (let ((providers (plist-get compatibility :providers))
        (requires (plist-get compatibility :requires)))
    (dolist (operation requires)
      (unless (memq operation supersonic-provider-operations)
        (error "Unknown provider operation `%s' required by backend `%s'" operation name)))
    (puthash name operations supersonic-playback--backends)
    (if (or providers requires)
        (puthash name (list :providers providers :requires requires) supersonic-playback--compatibility)
      (remhash name supersonic-playback--compatibility))))

(defun supersonic-playback-compatible-p (backend &optional provider)
  "Return non-nil if BACKEND can play for PROVIDER.
PROVIDER defaults to the active one, `supersonic-provider'.  Only says
what BACKEND declared when it registered, and so is nil for a backend
never registered at all."
  (let ((provider (or provider supersonic-provider))
        (compatibility (gethash backend supersonic-playback--compatibility)))
    (and (gethash backend supersonic-playback--backends)
         (let ((providers (plist-get compatibility :providers)))
           (or (null providers) (memq provider providers)))
         (seq-every-p
          (lambda (operation) (supersonic-provider-supports-p operation provider))
          (plist-get compatibility :requires))
         t)))

(defun supersonic-playback--incompatible (backend)
  "Signal a `user-error' saying BACKEND cannot play for the active provider."
  (user-error "The `%s' playback backend cannot play for the `%s' provider" backend supersonic-provider))

(defun supersonic-playback--implementation (operation)
  "Return the active backend's implementation of OPERATION.
Signals a `user-error' if `supersonic-playback-backend' names a backend
that was never registered -- typically because the file providing it
has not been loaded -- or one that does not implement OPERATION, or
one that cannot play for the active provider (unless OPERATION is one
of `supersonic-playback--unchecked-operations')."
  (let ((operations (gethash supersonic-playback-backend supersonic-playback--backends)))
    (unless operations
      (user-error "No playback backend named `%s' is registered" supersonic-playback-backend))
    (unless (or (memq operation supersonic-playback--unchecked-operations)
                (supersonic-playback-compatible-p supersonic-playback-backend))
      (supersonic-playback--incompatible supersonic-playback-backend))
    (or (alist-get operation operations)
        (user-error "The `%s' playback backend cannot %s" supersonic-playback-backend operation))))

(defun supersonic-playback--call (operation &rest args)
  "Call the active backend's implementation of OPERATION with ARGS."
  (apply (supersonic-playback--implementation operation) args))

(defun supersonic-playback-backend-names ()
  "Return the names of every backend currently registered.
A backend not loaded yet has not registered itself."
  (let (names)
    (maphash (lambda (name _operations) (push name names)) supersonic-playback--backends)
    (nreverse names)))

(defun supersonic-playback-compatible-backend-names (&optional provider)
  "Return the names of the registered backends that fit PROVIDER.
That is, every backend `supersonic-playback-compatible-p' accepts.
PROVIDER defaults to the active one, `supersonic-provider'."
  (seq-filter (lambda (backend) (supersonic-playback-compatible-p backend provider))
              (supersonic-playback-backend-names)))

(defvar supersonic-playback--last-backend nil
  "Alist of provider name to the backend last active with it this session.
Kept by `supersonic-playback-select', so that
`supersonic-playback-backend-for-provider' can return to it.")

(defun supersonic-playback-select (backend &optional provider)
  "Make BACKEND the active backend, and PROVIDER, if given, the active provider.
Does nothing if neither changes.  Otherwise first stops whatever the
active backend is playing -- even when only the provider changes,
since what it has queued are the old provider's ids -- unless that
backend was never registered and so has nothing to stop.  Remembers
both the pairing left and the one entered, for
`supersonic-playback-backend-for-provider'.

Checks nothing: whether BACKEND can play for PROVIDER is the caller's
business, settled before calling this."
  (let ((provider (or provider supersonic-provider)))
    (unless (and (eq provider supersonic-provider) (eq backend supersonic-playback-backend))
      (setf (alist-get supersonic-provider supersonic-playback--last-backend) supersonic-playback-backend)
      (when (memq supersonic-playback-backend (supersonic-playback-backend-names))
        (supersonic-playback-stop))
      (setq supersonic-provider provider)
      (setq supersonic-playback-backend backend)
      (setf (alist-get provider supersonic-playback--last-backend) backend))))

(defun supersonic-playback-backend-for-provider (provider)
  "Return the backend to play on once PROVIDER is active.
The active backend, if it can play for PROVIDER -- see
`supersonic-playback-compatible-p'.  Otherwise the one backend that
can, or of several, the one last active with PROVIDER; failing both,
prompts among them.  A provider no backend can play for, and an empty
answer to the prompt, are refused with a `user-error', so that
declining leaves everything as it was."
  (if (supersonic-playback-compatible-p supersonic-playback-backend provider)
      supersonic-playback-backend
    (let ((backends (supersonic-playback-compatible-backend-names provider))
          (last (alist-get provider supersonic-playback--last-backend)))
      (cond
       ((null backends)
        (user-error "No registered playback backend can play for the `%s' provider" provider))
       ((null (cdr backends)) (car backends))
       ((memq last backends) last)
       (t
        (let ((answer (completing-read
                       (format "The `%s' backend cannot play for `%s'; switch backend to: "
                               supersonic-playback-backend provider)
                       (mapcar #'symbol-name backends) nil t)))
          (when (equal answer "")
            (user-error "Kept the `%s' provider and the `%s' backend"
                        supersonic-provider supersonic-playback-backend))
          (intern answer)))))))

(defun supersonic-playback-start (ids)
  "Replace the play queue with IDS and start playing immediately."
  (supersonic-playback--call 'start ids))

(defun supersonic-playback-enqueue (ids)
  "Append IDS to the end of the play queue.
Whatever is already playing is left undisturbed, and so is a queue
merely paused or stopped partway through; if there is nothing left to
play -- the queue is empty or has been played to its end -- playback
starts on the first of IDS."
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
FRACTION is between 0.0 and 1.0, as the waveform seekbar has it; a
backend seeking in seconds knows the track's duration to convert."
  (supersonic-playback--call 'seek-fraction fraction))

(defun supersonic-playback-live-p ()
  "Return non-nil if the active backend currently has playback to report on.
Synchronous, unlike `supersonic-playback-status': every backend knows
this locally, so it can serve as a plain guard."
  (supersonic-playback--call 'live-p))

(aio-defun
 supersonic-playback-status (key)
 "Return a promise resolving to the active backend's current KEY.
KEY is one of `supersonic-playback-status-keys'.  Resolves to nil when
nothing is playing, so a caller needs no liveness guard of its own.
One key per call, since callers want them at different moments: the
now-playing render asks for the position last, so that it is fresh."
 (unless (memq key supersonic-playback-status-keys)
   (error "Unknown playback status key `%s'" key))
 (when (supersonic-playback-live-p)
   (aio-await (supersonic-playback--call 'status key))))

(defun supersonic-playback-offer-to-save (variable what name)
  "Offer to save VARIABLE, just set to the WHAT called NAME, for future sessions.
For a command selecting the player a backend plays on, such as a UPnP
renderer: the selection is in effect already, and saving it writes to
`custom-file', or to the init file without one, which not everyone
wants touched -- their configuration may set it itself."
  (if (y-or-n-p (format "Selected %s %s; save it for future sessions? " what name))
      (progn
        (customize-save-variable variable (symbol-value variable))
        (message "Saved %s %s" what name))
    (message "Selected %s %s for this session" what name)))

(aio-defun
 supersonic-playback-queue ()
 "Return a promise resolving to the active backend's current play queue.
Each entry is a plist with `:track-id', a supersonic track id, and
`:current', non-nil for whichever entry is currently playing.  A
backend whose player hands out what it plays along with the queue may
add `:track', the entry's track as `supersonic-provider-track' would
resolve it, so that showing the queue need not look each entry up
again.  Resolves to nil when nothing is live, as
`supersonic-playback-status' does."
 (when (supersonic-playback-live-p)
   (aio-await (supersonic-playback--call 'queue))))

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
(defun supersonic-playback-switch-backend (backend)
  "Make BACKEND the active playback backend.
Interactively, prompts among the registered backends that can play for
the active provider -- see `supersonic-playback-compatible-p'.  A
BACKEND that is not registered, or cannot play for the active
provider, is refused with a `user-error' before anything is stopped.

Stops the backend switched away from first, which a plain `setq' of
`supersonic-playback-backend' would not.  No queue is carried over."
  (interactive
   (list
    (intern
     (completing-read
      "Switch to playback backend: " (mapcar #'symbol-name (supersonic-playback-compatible-backend-names)) nil t))))
  (unless (supersonic-playback-compatible-p backend)
    (if (gethash backend supersonic-playback--backends)
        (supersonic-playback--incompatible backend)
      (user-error "No playback backend named `%s' is registered" backend)))
  (supersonic-playback-select backend))

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
