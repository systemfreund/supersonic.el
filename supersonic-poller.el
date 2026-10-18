;;; supersonic-poller.el --- Polling machinery shared by polled playback backends -*- lexical-binding: t; -*-

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

;; Polling for the playback backends whose player reports nothing on
;; its own: the jukebox, UPnP renderers and Music Assistant players.
;; A backend describes how to poll its player in a `supersonic-poller';
;; this file polls on a timer while that backend is active, runs the
;; playback hooks for what changed, and estimates the position between
;; polls.

;;; Code:
(require 'cl-lib)
(require 'aio)
(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(cl-defstruct (supersonic-poller
               (:constructor supersonic-poller-create)
               (:copier nil))
  "A polled playback backend.
Set by the backend:

- `backend': its name in `supersonic-playback-backend'.
- `interval': the variable holding the seconds between polls.
- `fetch': a function returning a promise of a snapshot, a plist with
  at least `:position' and `:polled-at'.
- `summarize': a function of a snapshot returning
  (:track-id ID :paused PAUSED :entries IDS).
- `description': what a failed poll failed to do, e.g. \"poll the
  jukebox\".
- `on-failure': nil or a function run when a poll fails.
- `on-landed': nil or a function of the previous and new snapshot,
  run when a poll succeeds.  It may return a function to call once
  the poll is done, which must report its own errors.

Kept here: `snapshot', `live', `failing', `in-flight' (a promise),
`generation' (to ignore stale polls), `timer' and `announced'."
  backend interval fetch summarize description on-failure on-landed
  snapshot live failing in-flight (generation 0) timer announced)

;;;
;;; Polling
;;;

(defun supersonic-poller--scrobble-track-change (previous-track current-track)
  "Scrobble PREVIOUS-TRACK as played and CURRENT-TRACK as now playing.
Either may be nil.  A polled player doesn't report a track ending, so
a track that is no longer current counts as played."
  (when previous-track
    (supersonic-provider-scrobble previous-track))
  (when current-track
    (supersonic-provider-scrobble current-track t)))

(defun supersonic-poller-announce (poller &optional went-live)
  "Run the playback hooks for what changed since POLLER last announced.
A new track runs the track-change hook and scrobbles; otherwise a
changed queue or pause state runs the queue- or state-change hook.
WENT-LIVE non-nil runs the track-change hook, for a player that
answers again after an outage."
  (let ((previous (supersonic-poller-announced poller))
        (current (funcall (supersonic-poller-summarize poller) (supersonic-poller-snapshot poller))))
    (setf (supersonic-poller-announced poller) current)
    (cond
     ((not (equal (plist-get previous :track-id) (plist-get current :track-id)))
      (supersonic-poller--scrobble-track-change (plist-get previous :track-id) (plist-get current :track-id))
      (run-hooks 'supersonic-playback-track-change-hook))
     (went-live
      (run-hooks 'supersonic-playback-track-change-hook))
     (t
      (unless (equal (plist-get previous :entries) (plist-get current :entries))
        (run-hooks 'supersonic-playback-queue-change-hook))
      (unless (eq (plist-get previous :paused) (plist-get current :paused))
        (run-hooks 'supersonic-playback-state-change-hook))))))

(aio-defun supersonic-poller-poll (poller)
  "Poll POLLER's player, store the snapshot and announce what changed.
If the player doesn't answer, mark the backend as not live, run
`on-failure' and, at the start of an outage, report it.  Wait for a
poll already in flight first, so polls never overlap.  The answer to
a poll sent before polling stopped is ignored."
  (while (supersonic-poller-in-flight poller)
    (aio-await (supersonic-poller-in-flight poller)))
  (let ((done (aio-promise))
        (generation (supersonic-poller-generation poller))
        (previous (supersonic-poller-snapshot poller))
        (was-live (supersonic-poller-live poller))
        (snapshot nil)
        (failure nil)
        (then nil))
    (setf (supersonic-poller-in-flight poller) done)
    (unwind-protect
        (progn
          (condition-case err
              (setq snapshot (aio-await (funcall (supersonic-poller-fetch poller))))
            (error (setq failure err)))
          (when (eql generation (supersonic-poller-generation poller))
            (supersonic--with-async-error-handling nil "run the playback hooks"
                                                   (if failure
                                                       (progn
                                                         (setf (supersonic-poller-live poller) nil)
                                                         (when (supersonic-poller-on-failure poller)
                                                           (funcall (supersonic-poller-on-failure poller)))
                                                         (unless (supersonic-poller-failing poller)
                                                           (setf (supersonic-poller-failing poller) t)
                                                           (supersonic--report-async-error (supersonic-poller-description poller) failure)
                                                           (run-hooks 'supersonic-playback-track-change-hook)))
                                                     (setf (supersonic-poller-snapshot poller) snapshot)
                                                     (setf (supersonic-poller-live poller) t)
                                                     (setf (supersonic-poller-failing poller) nil)
                                                     (when (supersonic-poller-on-landed poller)
                                                       (setq then (funcall (supersonic-poller-on-landed poller) previous snapshot)))
                                                     (supersonic-poller-announce poller (not was-live))))))
      (setf (supersonic-poller-in-flight poller) nil)
      (aio-resolve done #'ignore))
    (when then
      (aio-await (funcall then)))))

(defun supersonic-poller--tick (poller)
  "Poll POLLER's player unless a poll is already in flight."
  (unless (supersonic-poller-in-flight poller)
    (ignore (supersonic-poller-poll poller))))

(defun supersonic-poller-replace-snapshot (poller snapshot)
  "Make SNAPSHOT POLLER's latest, as if a poll had found it, and announce it."
  (setf (supersonic-poller-snapshot poller) snapshot)
  (supersonic-poller-announce poller))

(aio-defun supersonic-poller-finish-seek (poller)
  "Poll POLLER's player after a seek, then run the position-change hook."
  (aio-await (supersonic-poller-poll poller))
  (run-hooks 'supersonic-playback-position-change-hook))

(defun supersonic-poller-position (snapshot playing &optional duration)
  "Return SNAPSHOT's `:position', advanced to now if PLAYING.
Never past DURATION, if given.  Return nil if SNAPSHOT has no position."
  (let ((position (plist-get snapshot :position)))
    (when position
      (let ((position (if playing
                          (+ position (- (float-time) (plist-get snapshot :polled-at)))
                        position)))
        (if duration
            (min position duration)
          position)))))

;;;
;;; Polling only while the backend is the active one
;;;

(defun supersonic-poller--start (poller)
  "Start POLLER's timer unless it is running, and poll right away."
  (unless (supersonic-poller-timer poller)
    (let ((interval (symbol-value (supersonic-poller-interval poller))))
      (setf (supersonic-poller-timer poller) (run-at-time interval interval #'supersonic-poller--tick poller)))
    (supersonic-poller--tick poller)))

(defun supersonic-poller--stop (poller)
  "Stop POLLER's timer and forget its state.
The answer to a poll still in flight will be ignored."
  (cl-incf (supersonic-poller-generation poller))
  (when (supersonic-poller-timer poller)
    (cancel-timer (supersonic-poller-timer poller))
    (setf (supersonic-poller-timer poller) nil))
  (setf (supersonic-poller-snapshot poller) nil)
  (setf (supersonic-poller-live poller) nil)
  (setf (supersonic-poller-failing poller) nil)
  (setf (supersonic-poller-announced poller) nil))

(defun supersonic-poller--follow (poller backend)
  "Start POLLER if BACKEND is its backend, stop it otherwise."
  (if (eq backend (supersonic-poller-backend poller))
      (supersonic-poller--start poller)
    (supersonic-poller--stop poller)))

(defvar supersonic-poller--registered nil
  "Variables holding the pollers of the polled backends.")

(defun supersonic-poller--watch-backend (_symbol new-value _operation _where)
  "Start or stop each registered poller for NEW-VALUE, the new backend."
  (dolist (variable supersonic-poller--registered)
    (supersonic-poller--follow (symbol-value variable) new-value)))

;; Removed first so that reloading this file doesn't add it twice.
(remove-variable-watcher 'supersonic-playback-backend #'supersonic-poller--watch-backend)
(add-variable-watcher 'supersonic-playback-backend #'supersonic-poller--watch-backend)

(defun supersonic-poller-register (variable)
  "Poll with the poller in VARIABLE while its backend is active.
Start polling now if it already is."
  (cl-pushnew variable supersonic-poller--registered)
  (supersonic-poller--follow (symbol-value variable) supersonic-playback-backend))

(provide 'supersonic-poller)

;;; supersonic-poller.el ends here
