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
;; This file therefore knows nothing about mpv, HTTP streams, or the
;; Subsonic API: it only knows the operation names.  The dependency runs
;; the other way round, each backend requiring this file and registering
;; itself on load, so that adding a backend never means touching the
;; buffers and commands that drive playback.

;;; Code:

;; fix byte-compiler complaints; the defcustom lives in supersonic.el
;; alongside the package's other user options
(defvar supersonic-playback-backend)

(defconst supersonic-playback-operations '(start enqueue toggle-play next prev seek seek-fraction)
  "The playback operations a backend can implement.
`start' and `enqueue' each take a list of supersonic track ids; `seek'
takes an offset in seconds, which may be negative; `seek-fraction'
takes a position in the current track as a fraction between 0.0 and
1.0; the rest take no arguments.  A backend need not implement all of
these -- an operation its player has no equivalent for is simply left
out of the alist passed to `supersonic-playback-register-backend', and
calling the corresponding `supersonic-playback-*' function then reports
that rather than failing silently.")

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

(defun supersonic-playback-seek (offset)
  "Seek OFFSET seconds relative to the current position.
A negative OFFSET seeks backwards."
  (supersonic-playback--call 'seek offset))

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
(defun supersonic-seek-forward ()
  "Seek 30 seconds forward."
  (interactive)
  (supersonic-playback-seek 30))

;;;###autoload
(defun supersonic-seek-back ()
  "Seek 30 seconds back."
  (interactive)
  (supersonic-playback-seek -30))

(provide 'supersonic-playback)
;;; supersonic-playback.el ends here
