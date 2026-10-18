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

;; The playback side of supersonic.el.  A backend, such as mpv in
;; `supersonic-mpv.el', registers functions for the operations in
;; `supersonic-playback-operations', and the `supersonic-playback-*'
;; functions call those of the backend `supersonic-playback-backend'
;; selects.  Backends run the hooks defined here when something may
;; have changed; listeners then ask `supersonic-playback-status'.

;;; Code:

(require 'aio)
(require 'seq)

(require 'supersonic-custom)
(require 'supersonic-provider)

(defconst supersonic-playback-operations
  '(start enqueue toggle-play next prev stop seek seek-fraction live-p status queue)
  "The playback operations a backend can implement.
`start' and `enqueue' take a list of track ids and report their own
failures.  `seek' takes an offset in seconds, `seek-fraction' a
fraction of the track between 0.0 and 1.0, `status' one of
`supersonic-playback-status-keys'.  The others take no arguments.
`status' and `queue' return promises.")

(defconst supersonic-playback-status-keys '(track-id position paused)
  "The keys `supersonic-playback-status' accepts.
`track-id' is the id of the current track, `position' the position in
seconds, `paused' non-nil if paused.")

(defvar supersonic-playback-track-change-hook nil
  "Hook run when the current track may have changed.
Use `supersonic-playback-status' to get the new state.")

(defvar supersonic-playback-state-change-hook nil
  "Hook run when playback is paused or resumed.")

(defvar supersonic-playback-queue-change-hook nil
  "Hook run when the play queue may have changed but the track has not.
Use `supersonic-playback-queue' to get the new queue.")

(defvar supersonic-playback-position-change-hook nil
  "Hook run when the playback position jumped, after a seek.")

(defvar supersonic-playback--backends (make-hash-table :test #'eq)
  "Map of backend name to its operation alist.")

(defvar supersonic-playback--compatibility (make-hash-table :test #'eq)
  "Map of backend name to its compatibility plist.
See `supersonic-playback-register-backend'.  A backend without an
entry plays for every provider.")

(defconst supersonic-playback--unchecked-operations '(stop live-p status queue)
  "Operations allowed even when the backend can't play for the provider.")

(defun supersonic-playback-register-backend (name operations &rest compatibility)
  "Register NAME as a playback backend implementing OPERATIONS.
NAME is the symbol `supersonic-playback-backend' selects it by.
OPERATIONS is an alist of operations from
`supersonic-playback-operations' to their functions.  Registering NAME
again replaces it.

COMPATIBILITY is a plist of the providers the backend can play for:

- `:providers': a list of provider names it plays for.
- `:requires': a list of `supersonic-provider-operations' a provider
  must implement, such as `stream-url'.

With both, a provider must match both.  With neither, the backend
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
  "Return non-nil if BACKEND is registered and can play for PROVIDER.
PROVIDER defaults to `supersonic-provider'."
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
  "Return the active backend's function for OPERATION.
Signal a `user-error' if the backend isn't registered, doesn't
implement OPERATION, or can't play for the active provider."
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
  "Return the names of the registered backends."
  (let (names)
    (maphash (lambda (name _operations) (push name names)) supersonic-playback--backends)
    (nreverse names)))

(defun supersonic-playback-compatible-backend-names (&optional provider)
  "Return the names of the registered backends that can play for PROVIDER.
PROVIDER defaults to `supersonic-provider'."
  (seq-filter (lambda (backend) (supersonic-playback-compatible-p backend provider))
              (supersonic-playback-backend-names)))

(defvar supersonic-playback--last-backend nil
  "Alist of provider to the backend last used with it.")

(defun supersonic-playback-select (backend &optional provider)
  "Make BACKEND the active backend, and PROVIDER the active provider.
PROVIDER defaults to the active one.  If either changes, stop playback
first.  Doesn't check that BACKEND can play for PROVIDER."
  (let ((provider (or provider supersonic-provider)))
    (unless (and (eq provider supersonic-provider) (eq backend supersonic-playback-backend))
      (setf (alist-get supersonic-provider supersonic-playback--last-backend) supersonic-playback-backend)
      (when (memq supersonic-playback-backend (supersonic-playback-backend-names))
        (supersonic-playback-stop))
      (setq supersonic-provider provider)
      (setq supersonic-playback-backend backend)
      (setf (alist-get provider supersonic-playback--last-backend) backend))))

(defun supersonic-playback-backend-for-provider (provider)
  "Return the backend to use with PROVIDER.
This is the active backend if it can play for PROVIDER, else the only
one that can, else the one last used with PROVIDER, else one the user
picks.  Signal a `user-error' if there is none."
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
  "Append IDS to the play queue.
Start playing the first of them if nothing is left to play."
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
  "Stop playback."
  (supersonic-playback--call 'stop))

(defun supersonic-playback-seek (offset)
  "Seek OFFSET seconds relative to the current position.
A negative OFFSET seeks backwards."
  (supersonic-playback--call 'seek offset))

(defun supersonic-playback--seek-step (arg)
  "Return the seconds a seek command with prefix argument ARG moves by.
Without ARG, return `supersonic-seek-step'.  The result is never
negative."
  (if arg
      (abs (prefix-numeric-value arg))
    supersonic-seek-step))

(defun supersonic-playback-seek-fraction (fraction)
  "Seek to FRACTION, between 0.0 and 1.0, of the current track."
  (supersonic-playback--call 'seek-fraction fraction))

(defun supersonic-playback-live-p ()
  "Return non-nil if the active backend has playback to report on."
  (supersonic-playback--call 'live-p))

(aio-defun supersonic-playback-status (key)
  "Return a promise resolving to the active backend's current KEY.
KEY is one of `supersonic-playback-status-keys'.  Resolve to nil when
nothing is playing."
  (unless (memq key supersonic-playback-status-keys)
    (error "Unknown playback status key `%s'" key))
  (when (supersonic-playback-live-p)
    (aio-await (supersonic-playback--call 'status key))))

(defun supersonic-playback-offer-to-save (variable what name)
  "Ask whether to save VARIABLE, just set to the WHAT called NAME."
  (if (y-or-n-p (format "Selected %s %s; save it for future sessions? " what name))
      (progn
        (customize-save-variable variable (symbol-value variable))
        (message "Saved %s %s" what name))
    (message "Selected %s %s for this session" what name)))

(aio-defun supersonic-playback-queue ()
  "Return a promise resolving to the play queue.
Each entry is a plist with `:track-id' and `:current', non-nil for the
current entry, and possibly `:track', the track plist.  Resolve to nil
when nothing is playing."
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
  "Make BACKEND the active playback backend, stopping the current one.
Interactively, offer the backends that can play for the active
provider.  The queue is not carried over."
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
