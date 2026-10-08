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

;; What a playback backend needs when its player reports nothing on
;; its own and has to be asked, on a timer, what it is doing: the
;; jukebox in `supersonic-jukebox.el' and UPnP renderers in
;; `supersonic-upnp.el' (#79).  mpv tells `supersonic-mpv.el' about
;; every change itself, and needs none of this.
;;
;; A backend describes itself with a `supersonic-poller': how to ask
;; its player for a fresh snapshot, how to read the current track,
;; pause state and queue entries off one, and which backend name it
;; polls for.  The poller keeps the rest: the latest snapshot, whether
;; the player answered, the poll in flight and the timer.  From there
;; this file
;;
;; - polls, never two at once, marking the backend live or not and
;;   reporting an outage once (`supersonic-poller-poll');
;; - runs the facade's hooks and scrobbles for whatever a poll, or the
;;   backend itself, changed (`supersonic-poller-announce');
;; - polls on a timer exactly while the backend is the active one
;;   (`supersonic-poller-register');
;; - counts the position on between polls (`supersonic-poller-position').
;;
;; What differs stays with each backend: how it talks to its player,
;; how an answer becomes a snapshot, and anything it does on top, such
;; as the UPnP backend's client-side queue.

;;; Code:
(require 'cl-lib)
(require 'aio)
(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(cl-defstruct (supersonic-poller
               (:constructor supersonic-poller-create)
               (:copier nil))
  "A polled playback backend: how it is polled, and what polling found.
Described by the backend:

`backend', the name `supersonic-playback-backend' has while this one is
active; `interval', the variable holding the seconds between polls;
`fetch', a function of no arguments returning a promise of a fresh
snapshot, a plist the backend makes up but for `:position' and
`:polled-at', which `supersonic-poller-position' reads; `summarize', a
function of a snapshot returning a plist (:track-id ID :paused PAUSED
:entries IDS) of what the facade's hooks announce -- read off the
snapshot, or off state the backend keeps itself, such as the UPnP
backend's client-side queue; `description', what a failed poll is
reported as having failed to do, such as \"poll the jukebox\";
`on-failure', nil or a function run whenever a poll fails; and
`on-landed', nil or a function of the previous and the new snapshot,
run whenever a poll gets an answer, before the facade's hooks; the
previous snapshot is nil before any poll landed, and after a failed
one, since what the player did in between is unknown (#81).  It may
return a function of no arguments, which is called once the poll is no
longer in flight, and whatever promise that returns is awaited.  That
function has to report its own errors: on a timer's poll, nobody
awaits the poll's promise.

Kept by this file:

`snapshot', the latest poll's, or nil before any landed; `live',
non-nil if the last poll got an answer; `failing', non-nil once a poll
failed without a later one succeeding since; `in-flight', a promise
resolved once the poll in flight is done, or nil; `generation',
counting how often polling stopped, so that a poll can tell it outlived
the polling it was sent for; `timer', the poll timer while the backend
is active; and `announced', what `supersonic-poller-announce' last ran
the hooks for."
  backend interval fetch summarize description on-failure on-landed
  snapshot live failing in-flight (generation 0) timer announced)

;;;
;;; Polling
;;;

(defun supersonic-poller--scrobble-track-change (previous-track current-track)
  "Scrobble PREVIOUS-TRACK as played and CURRENT-TRACK as playing now.
Either may be nil.  A polled backend never sees the moment a track
ends, the way mpv's end-file and start-file events show it, so the
track a poll no longer finds stands in for the one that just finished.
`supersonic-provider-scrobble' itself checks whether to scrobble."
  (when previous-track
    (supersonic-provider-scrobble previous-track))
  (when current-track
    (supersonic-provider-scrobble current-track t)))

(defun supersonic-poller-announce (poller &optional went-live)
  "Run the facade's hooks for whatever changed since POLLER's last ran.
Compares what POLLER's `summarize' makes of its snapshot with what the
hooks last ran for: a different track runs the track-change hook, and
scrobbles; otherwise different entries run the queue-change hook (an
`add' moves neither the track nor the pause state, see #38), and a
different pause state the state-change hook.  WENT-LIVE non-nil runs
the track-change hook regardless, without scrobbling, since a player
that answers again after an outage is news to whoever showed it gone.

Never runs the position-change hook: a poll cannot tell a seek from a
position that crept on, so a seek runs it itself, see
`supersonic-poller-finish-seek'."
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

(aio-defun
 supersonic-poller-poll (poller)
 "Poll POLLER's player, keep the snapshot and announce what changed.
A poll that gets an answer marks the backend live and runs the
facade's hooks, see `supersonic-poller-announce'; one that does not
marks it not live, runs POLLER's `on-failure', and the first time in
an outage also reports the failure and runs the track-change hook, so
that nothing goes on showing the stale snapshot as current.  Reported
once per outage: never leaving the first failure silent, even of the
very first poll, and never once per poll for as long as it lasts.

Never runs alongside another poll: with one in flight, this waits for
it and then polls itself.  An action polling for its own effect cannot
make do with the answer to a poll sent before it, and two polls in
flight could land out of order, an older snapshot overwriting a newer
one.  The timer polls through `supersonic-poller--tick' instead, which
skips rather than waits.  A poll stays in flight until its request
settles, which `supersonic-request-timeout' bounds.

A poll that gets an answer runs POLLER's `on-landed' before the hooks,
and whatever function that returns once it is no longer in flight.
The first answer after a failed poll is not compared with the snapshot
from before the outage: a renderer switched off mid-track and back on
did not play the track to its end in between (#81).

Only the player failing to answer counts as a failed poll.  An error
from a facade hook, `on-failure' or `on-landed' is reported as failing
to run the playback hooks, and leaves the backend live.  A poll still
waiting for its answer when polling stops changes nothing once the
answer arrives, see `supersonic-poller--stop'."
 (while (supersonic-poller-in-flight poller)
   (aio-await (supersonic-poller-in-flight poller)))
 (let ((done (aio-promise))
       (generation (supersonic-poller-generation poller))
       (previous (and (not (supersonic-poller-failing poller)) (supersonic-poller-snapshot poller)))
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
  "Poll POLLER's player, unless a poll is still waiting for its answer.
What POLLER's timer runs.  A tick used to poll regardless, so a player
that stopped answering piled up one more open connection per tick
until Emacs ran out of file descriptors (#70).  Skipping loses
nothing: the poll in flight is about to report the same state."
  (unless (supersonic-poller-in-flight poller)
    (ignore (supersonic-poller-poll poller))))

(defun supersonic-poller-replace-snapshot (poller snapshot)
  "Make SNAPSHOT POLLER's latest, as if a poll had found it, and announce it.
For a backend that knows what its player is about to report before a
poll can find it -- the UPnP backend, having just loaded a track --
and wants the facade's hooks to show it now.  See
`supersonic-poller-announce'."
  (setf (supersonic-poller-snapshot poller) snapshot)
  (supersonic-poller-announce poller))

(aio-defun
 supersonic-poller-finish-seek (poller)
 "Poll POLLER's player for where a seek landed, then say it moved.
The position-change hook is for the sudden jump of a seek, which only
whoever asked for the seek can tell apart from a position that crept
on between polls -- see `supersonic-poller-announce'."
 (aio-await (supersonic-poller-poll poller))
 (run-hooks 'supersonic-playback-position-change-hook))

(defun supersonic-poller-position (snapshot playing &optional duration)
  "Return SNAPSHOT's `:position', counted on to now if PLAYING.
A poll only lands every so many seconds, but the now-playing buffer
expects the position to advance by the second, so while PLAYING the
time elapsed since SNAPSHOT's `:polled-at' is added; the next poll's
real position quietly corrects whatever that guessed.  Never past
DURATION, if given.  nil if SNAPSHOT has no position."
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
  "Start POLLER's timer, unless it is running, and poll once right away.
Right away rather than after the first interval, so that the backend
shows real state as soon as it becomes the active one."
  (unless (supersonic-poller-timer poller)
    (let ((interval (symbol-value (supersonic-poller-interval poller))))
      (setf (supersonic-poller-timer poller) (run-at-time interval interval #'supersonic-poller--tick poller)))
    (supersonic-poller--tick poller)))

(defun supersonic-poller--stop (poller)
  "Stop POLLER's timer and forget what polling found.
So that neither a stale snapshot nor a timer spending requests on a
player nobody asks about outlives the backend being the active one.
A poll still in flight is not cancelled, but its answer is dropped:
otherwise it would mark the backend live again, run the hooks, and
for UPnP even load the next track, after the user switched away."
  (cl-incf (supersonic-poller-generation poller))
  (when (supersonic-poller-timer poller)
    (cancel-timer (supersonic-poller-timer poller))
    (setf (supersonic-poller-timer poller) nil))
  (setf (supersonic-poller-snapshot poller) nil)
  (setf (supersonic-poller-live poller) nil)
  (setf (supersonic-poller-failing poller) nil)
  (setf (supersonic-poller-announced poller) nil))

(defun supersonic-poller--follow (poller backend)
  "Poll with POLLER exactly while BACKEND, the one about to be active, is its."
  (if (eq backend (supersonic-poller-backend poller))
      (supersonic-poller--start poller)
    (supersonic-poller--stop poller)))

(defvar supersonic-poller--registered nil
  "Variables holding the poller of a polled backend each.
Variables rather than the pollers themselves, so that a poller bound
in their place, by a test say, is the one that follows the backend.")

(defun supersonic-poller--watch-backend (_symbol new-value _operation _where)
  "Start or stop each registered poller as NEW-VALUE becomes the backend.
Watches `supersonic-playback-backend'."
  (dolist (variable supersonic-poller--registered)
    (supersonic-poller--follow (symbol-value variable) new-value)))

;; `remove-variable-watcher' first so that re-evaluating this file
;; cannot stack a second watcher.
(remove-variable-watcher 'supersonic-playback-backend #'supersonic-poller--watch-backend)
(add-variable-watcher 'supersonic-playback-backend #'supersonic-poller--watch-backend)

(defun supersonic-poller-register (variable)
  "Poll with the poller in VARIABLE exactly while its backend is active.
Starts polling right away if it already is, so that a selection made
before the backend's file was loaded is picked up too."
  (cl-pushnew variable supersonic-poller--registered)
  (supersonic-poller--follow (symbol-value variable) supersonic-playback-backend))

(provide 'supersonic-poller)

;;; supersonic-poller.el ends here
