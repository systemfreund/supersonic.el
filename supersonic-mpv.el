;;; supersonic-mpv.el --- mpv IPC/process layer for supersonic.el -*- lexical-binding: t; -*-

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

;; mpv process/IPC layer for supersonic.el: starting and talking to an
;; idle mpv instance over its JSON IPC socket, dispatching replies and
;; events, and the handful of playback operations that are thin wrappers
;; around it, which it registers with the backend facade in
;; `supersonic-playback.el' under the name `mpv'.
;;
;; This file knows nothing about the Subsonic list/now-playing buffers
;; that supersonic.el renders -- it only knows *that* the identity of
;; what mpv is playing, or its pause state, may have changed, and says
;; so by running the backend-neutral
;; `supersonic-playback-track-change-hook' /
;; `supersonic-playback-state-change-hook' rather than calling into
;; supersonic.el directly.  supersonic.el hangs its buffer refreshes off
;; those hooks from the outside, the same way `supersonic-mpris.el'
;; observes and drives this file purely via `advice-add' rather than
;; being depended on by it.
;;
;; Nor does it know how to reach a track: it streams whatever URL the
;; active provider's `supersonic-provider-stream-url' resolves to, and
;; reports what it played through `supersonic-provider-scrobble'.  mpv
;; therefore plays for any provider that implements `stream-url', and
;; says so when it registers.  Since those URLs arrive asynchronously,
;; starting or enqueueing tracks returns before they reach mpv; loads
;; are applied strictly in the order they were requested -- see
;; `supersonic-mpv--pending-load'.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'seq)
(require 'aio)
(require 'subr-x)
(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(defvar supersonic-mpv--process nil)
(defvar supersonic-mpv--socket nil
  "The network process connected to mpv's `--input-ipc-server' socket.
Commands are written to it directly with `process-send-string'; replies
are matched by hand via `supersonic-mpv--pending-requests', so nothing
here queues or waits for a response the way `tq.el' does.")
(defvar supersonic-mpv--socket-buffer ""
  "Bytes read from the mpv IPC socket that don't yet form a complete line.
Process filters aren't guaranteed to see whole, newline-terminated JSON
messages in a single call, so `supersonic--mpv-socket-filter' carries
any trailing partial message over between invocations here.")

(defvar supersonic-mpv--entry-counter 0
  "Client-side mirror of the mpv playlist entry id mpv will assign next.
mpv assigns each `loadfile' a playlist entry id that is unique for the
lifetime of the mpv core instance and increments strictly in the order
commands are sent; since we are the only writer on the IPC socket, we
can predict it instead of reading it back from mpv.")

(defvar supersonic--playlist (make-hash-table)
  "Map of mpv playlist entry id (integer) to supersonic track id (string).
Populated as tracks are loaded into mpv via `supersonic--mpv-load-track',
and consulted by scrobbling and MPRIS to resolve `playlist_entry_id'
values reported by mpv back to supersonic track ids.")

(defvar supersonic-mpv--pending-load nil
  "Promise of the most recently requested load into mpv, or nil.
Stream URLs are resolved asynchronously, so a `supersonic-mpv-start'
or `-enqueue' does not reach mpv right away.  Each one waits for the
load requested before it to be done before touching mpv's playlist,
so loads land in the order they were asked for -- an album played and
then another enqueued behind it ends up as exactly that, however long
either one's URLs took.  See `supersonic-mpv--load'.")

(defvar supersonic-mpv--load-generation 0
  "Bumped by `supersonic-mpv-kill' to drop loads still resolving URLs.
A load that finds this changed since it was requested gives up rather
than starting mpv all over again right after it was stopped -- by the
user, or by `supersonic-playback-switch-backend' switching away.")

(defvar supersonic-mpv--request-counter 0
  "Counter for `supersonic-mpv-command-with-callback' request_ids.")

(defvar supersonic-mpv--pending-requests (make-hash-table)
  "Map of in-flight request_id (integer) to the callback awaiting its reply.")

(defvar supersonic--paused nil
  "Non-nil if mpv currently reports its \"pause\" property as true.
Kept in sync via an `observe_property' registered once per mpv process
in `supersonic-mpv-ensure-running'; consulted by the now-playing buffer
so it never has to query mpv for this on every render.")

;;;###autoload
(defun supersonic-mpv-kill ()
  "Kill the mpv process."
  (interactive)
  (when (process-live-p supersonic-mpv--socket)
    (delete-process supersonic-mpv--socket))
  (when (supersonic-mpv-live-p)
    (kill-process supersonic-mpv--process))
  (with-timeout (supersonic-mpv-timeout (error "Failed to kill mpv"))
    (while (supersonic-mpv-live-p)
      (accept-process-output nil 0.05)))
  (setq supersonic-mpv--load-generation (1+ supersonic-mpv--load-generation))
  (setq supersonic-mpv--process nil)
  (setq supersonic-mpv--socket nil)
  (setq supersonic-mpv--socket-buffer "")
  (setq supersonic-mpv--entry-counter 0)
  (clrhash supersonic--playlist)
  (setq supersonic-mpv--request-counter 0)
  (clrhash supersonic-mpv--pending-requests)
  (setq supersonic--paused nil)
  (run-hooks 'supersonic-playback-track-change-hook))

(defun supersonic-mpv-live-p ()
  "Return non-nil if inferior mpv is running."
  (and supersonic-mpv--process (eq (process-status supersonic-mpv--process) 'run)))

(defun supersonic-mpv-ensure-running ()
  "Make sure mpv is running as an idle player, starting it if necessary.
Does nothing if mpv is already running, so it is safe to call before
every play/enqueue action."
  (when (eq system-type 'windows-nt)
    (user-error
     "Supersonic talks to mpv over a Unix-domain socket, which native
Windows does not support; this is not implemented for windows-nt"))
  (unless (supersonic-mpv-live-p)
    (supersonic-mpv-kill)
    (let ((socket (make-temp-name (expand-file-name "supersonic-mpv-" temporary-file-directory))))
      (setq supersonic-mpv--process
            (start-process "supersonic-player" nil supersonic-mpv
                           "--no-terminal"
                           "--really-quiet"
                           "--no-video"
                           "--no-config"
                           "--idle=once"
                           (format "--volume=%d" supersonic-default-volume)
                           (concat "--input-ipc-server=" socket)))
      (set-process-query-on-exit-flag supersonic-mpv--process nil)
      (set-process-sentinel
       supersonic-mpv--process
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (supersonic-mpv-kill)
           (when (file-exists-p socket)
             (with-demoted-errors "%S"
               (delete-file socket))))))
      (with-timeout (supersonic-mpv-timeout (supersonic-mpv-kill) (error "Failed to connect to mpv"))
        (while (not (file-exists-p socket))
          (accept-process-output nil 0.05)))
      (setq supersonic-mpv--socket
            (make-network-process
             :name "supersonic-mpv-socket"
             :family 'local
             :service socket
             :filter #'supersonic--mpv-socket-filter
             ;; mpv (`--idle=once') can exit and tear down its end of the
             ;; socket at any time, racing `supersonic-mpv-command'/
             ;; `-with-callback' writing to it -- see #15.  This sentinel is
             ;; the fast path for noticing that: it lets those commands
             ;; guard with `process-live-p' right before every
             ;; `process-send-string', narrowing (though not eliminating --
             ;; Emacs can still deliver a raw SIGPIPE for a write that races
             ;; the close itself) the window where they'd write to a socket
             ;; whose peer already hung up.
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (setq supersonic-mpv--socket nil)))))
      ;; Have mpv tell us about pause/resume, whoever triggered it, so the now-playing buffer can follow along.  mpv
      ;; answers an `observe_property' with the property's current value right away, which also seeds
      ;; `supersonic--paused'.  Observer id 2 rather than 1 so it cannot collide with the one supersonic-mpris.el
      ;; registers.
      (supersonic-mpv-command "observe_property" 2 "pause")))
  t)

(defun supersonic--mpv-load-track (id url flag)
  "Load supersonic track ID, streamed from URL, into mpv using loadfile FLAG.
Registers the mpv playlist entry id this load will be assigned in
`supersonic--playlist', so it can later be resolved back to ID for
scrobbling and MPRIS metadata.  If the `loadfile' command could not
actually be sent (e.g. mpv died between `supersonic-mpv-ensure-running'
and this call), the tentative registration is rolled back and an error
is signalled instead, so `supersonic-mpv--entry-counter' never runs
ahead of the playlist entries mpv has actually seen.

URL travels over the IPC socket rather than on mpv's command line,
which is what keeps any credentials in it out of the process list."
  (let ((entry-id (1+ supersonic-mpv--entry-counter)))
    (puthash entry-id id supersonic--playlist)
    (if (supersonic-mpv-command "loadfile" url flag)
        (setq supersonic-mpv--entry-counter entry-id)
      (progn
        (remhash entry-id supersonic--playlist)
        (error "Failed to load track %s: mpv is not running" id)))))

(aio-defun
 supersonic-mpv--stream-urls (ids)
 "Return a promise resolving to the stream URLs of IDS, in the same order.
All are asked for at once rather than one after the other, so a
provider that has to ask its server waits for its answers side by
side rather than for one round-trip per track in turn."
 (let ((promises (mapcar #'supersonic-provider-stream-url ids))
       (urls nil))
   (dolist (promise promises)
     (push (aio-await promise) urls))
   (nreverse urls)))

(aio-defun
 supersonic-mpv--load-after (previous ids flags then)
 "Load IDS into mpv once PREVIOUS, the load before, is done.
FLAGS is the `loadfile' flag for the first of IDS consed onto the flag
for the rest; THEN is called with no arguments once all are loaded.
Resolves the stream URLs first, concurrently with PREVIOUS, so a
provider that cannot name one leaves mpv untouched -- not started,
nothing registered.  Every failure is reported in the echo area rather
than signalled: nobody awaits this, and PREVIOUS must never reject for
whichever load comes after it."
 (let ((generation supersonic-mpv--load-generation))
   (condition-case err
       (let ((urls (aio-await (supersonic-mpv--stream-urls ids))))
         (when previous
           (aio-await previous))
         (when (= generation supersonic-mpv--load-generation)
           (supersonic-mpv-ensure-running)
           (supersonic--mpv-load-track (car ids) (car urls) (car flags))
           (cl-mapc
            (lambda (id url) (supersonic--mpv-load-track id url (cdr flags)))
            (cdr ids) (cdr urls))
           (funcall then)))
     (error
      (message "[Supersonic] Failed to play: %s" (error-message-string err))))))

(defun supersonic-mpv--load (ids flags then)
  "Queue up loading IDS into mpv behind every load requested before.
FLAGS and THEN are as for `supersonic-mpv--load-after'.  Returns a
promise resolving once the load is done, or has failed and been
reported -- see `supersonic-mpv--pending-load'."
  (setq supersonic-mpv--pending-load (supersonic-mpv--load-after supersonic-mpv--pending-load ids flags then)))

(defun supersonic-mpv-start (ids)
  "Replace the current mpv queue with IDS and start playing.
Returns before they reach mpv, once their stream URLs have been asked
for -- see `supersonic-mpv--load'.
`loadfile ... replace' swaps out the playlist but leaves mpv's `pause'
property untouched, so if playback was paused before this call it
would otherwise stay paused; explicitly unpause since the caller asked
to start playing now."
  (when ids
    (supersonic-mpv--load
     ids '("replace" . "append")
     (lambda ()
       (supersonic-mpv-command "set_property" "pause" :json-false)
       (run-hooks 'supersonic-playback-track-change-hook))))
  nil)

;;;###autoload
(defun supersonic-mpv-enqueue (ids)
  "Append IDS to the end of the current mpv queue.
Starts playback if mpv is currently idle; otherwise leaves whatever is
already playing undisturbed and simply queues IDS after it.  Returns
before they reach mpv, the same as `supersonic-mpv-start'."
  (when ids
    (supersonic-mpv--load
     ids '("append-play" . "append-play")
     ;; If mpv was idle, `append-play' starts the first of IDS, and the
     ;; `start-file' that follows runs the track-change hook by itself.
     (lambda () (run-hooks 'supersonic-playback-queue-change-hook))))
  nil)

(defun supersonic--mpv-handle-message (parsed-response)
  "Handle PARSED-RESPONSE, one message parsed from mpv's IPC socket."
  (let* ((request-id (alist-get 'request_id parsed-response))
         (callback (and request-id (gethash request-id supersonic-mpv--pending-requests))))
    (cond
     (callback
      (remhash request-id supersonic-mpv--pending-requests)
      (funcall callback parsed-response))
     (t
      (let ((event (alist-get 'event parsed-response)))
        (when (member event '("start-file" "end-file"))
          (run-hooks 'supersonic-playback-track-change-hook))
        ;; mpv reports a seek twice: `seek' when one is requested, and
        ;; `playback-restart' once it has actually taken effect.  The
        ;; latter is the one worth passing on, because it is only by
        ;; then that `time-pos' reads the position sought to instead of
        ;; the one left behind.  It also fires when a file starts
        ;; playing, which is harmless: the position is then simply read
        ;; once more alongside the track change.
        (when (string-equal event "playback-restart")
          (run-hooks 'supersonic-playback-position-change-hook))
        ;; mpv reports booleans as JSON true/false, which `json-read'
        ;; turns into t and `:json-false' -- the latter being non-nil in
        ;; Lisp, so this has to compare against t explicitly.
        (when (and (string-equal event "property-change") (string-equal (alist-get 'name parsed-response) "pause"))
          (setq supersonic--paused (eq (alist-get 'data parsed-response) t))
          (run-hooks 'supersonic-playback-state-change-hook))
        ;; `supersonic-provider-scrobble' does its own gating on
        ;; `supersonic-enable-scrobbling', on the provider being able
        ;; to scrobble at all, and on the entry resolving to a track.
        (cond
         ((string-equal event "end-file")
          (supersonic-provider-scrobble
           (gethash (alist-get 'playlist_entry_id parsed-response) supersonic--playlist)))
         ((string-equal event "start-file")
          (supersonic-provider-scrobble
           (gethash (alist-get 'playlist_entry_id parsed-response) supersonic--playlist) t))))))))

(defun supersonic--mpv-socket-filter (_ output)
  "Filter the mpv socket connection.
OUTPUT is the latest chunk read from mpv's IPC socket.  A single call
is not guaranteed to see whole, newline-terminated JSON messages, so
any trailing partial message is carried over in
`supersonic-mpv--socket-buffer' until the rest of it arrives."
  (setq supersonic-mpv--socket-buffer (concat supersonic-mpv--socket-buffer output))
  (let ((lines (split-string supersonic-mpv--socket-buffer "\n")))
    ;; The last element of LINES is whatever follows the final newline in
    ;; the buffer so far -- an empty string if it ends cleanly on one, or
    ;; an incomplete message otherwise.  Either way, hold it back and only
    ;; hand complete lines to `json-read-from-string'.
    (setq supersonic-mpv--socket-buffer (car (last lines)))
    (dolist (parsed-response (mapcar #'json-read-from-string (seq-remove #'string-empty-p (butlast lines))))
      (supersonic--mpv-handle-message parsed-response))))

(defun supersonic-mpv--send (string)
  "Write STRING to the mpv IPC socket, returning non-nil on success.
Guards with `process-live-p' immediately beforehand and catches the
`file-error' Emacs normally raises for a write to an already-closed
socket, treating either as mpv having gone away: the socket is torn
down and nil is returned instead of the write being attempted.  This
narrows, but per #15 cannot fully close, the race against mpv
\(`--idle=once') exiting mid-command -- a raw SIGPIPE landing inside
the write itself is a signal, not a Lisp error, and kills Emacs before
`condition-case' ever sees it."
  (and (process-live-p supersonic-mpv--socket)
       (condition-case nil
           (progn
             (process-send-string supersonic-mpv--socket string)
             t)
         (file-error
          (delete-process supersonic-mpv--socket)
          (setq supersonic-mpv--socket nil)
          nil))))

(defun supersonic-mpv-command (&rest args)
  "Generate a mpv ipc command using ARGS.
Returns non-nil if the command was actually sent to mpv over the IPC
socket; nil (after printing a \"MPV not running\" message) if there is
no live connection, so callers that must stay in sync with mpv's
actual state -- like `supersonic--mpv-load-track' -- can tell the
difference instead of assuming the command went through."
  (if (supersonic-mpv--send (concat (json-encode (list (cons 'command (apply #'vector args)))) "\n"))
      t
    (message "MPV not running")
    nil))

(defun supersonic-mpv-command-with-callback (callback &rest args)
  "Send an mpv IPC command built from ARGS, calling CALLBACK with its reply.
Unlike `supersonic-mpv-command', this expects an actual answer: the
command is tagged with a fresh request_id, and CALLBACK is invoked
with the full parsed JSON reply once `supersonic--mpv-socket-filter'
sees a response carrying that same request_id.

Returns non-nil if the command was sent, nil if there was no live
connection to send it on -- in which case CALLBACK is dropped rather
than left in `supersonic-mpv--pending-requests' waiting for a reply
that can never arrive, and the caller can tell that no answer is
coming instead of waiting for one forever."
  (let ((request-id (1+ supersonic-mpv--request-counter)))
    (puthash request-id callback supersonic-mpv--pending-requests)
    (if (supersonic-mpv--send
         (concat (json-encode (list (cons 'command (apply #'vector args)) (cons 'request_id request-id))) "\n"))
        (progn
          (setq supersonic-mpv--request-counter request-id)
          t)
      (remhash request-id supersonic-mpv--pending-requests)
      (message "MPV not running")
      nil)))

(aio-defun
 supersonic-mpv-get-property (name)
 "Return a promise resolving to mpv's current value of property NAME.
The `aio' counterpart of `supersonic-mpv-command-with-callback', for
callers that want to keep reading mpv state in a straight line instead
of nesting callbacks.  Resolves to nil if the request could not be sent
at all, so that a caller awaiting this never ends up waiting on a reply
that by then can never arrive."
 (let ((promise (aio-promise)))
   (if (supersonic-mpv-command-with-callback (lambda (response)
                                               (aio-resolve promise (lambda () (alist-get 'data response))))
                                             "get_property" name)
       (aio-await promise)
     nil)))

(defun supersonic-mpv-toggle-play ()
  "Toggle playing/paused state in mpv."
  (supersonic-mpv-command "cycle" "pause"))

(defun supersonic-mpv-next ()
  "Skip to the next track in mpv."
  (supersonic-mpv-command "playlist-next"))

(defun supersonic-mpv-prev ()
  "Go to the previous track in mpv."
  (supersonic-mpv-command "playlist-prev"))

(defun supersonic-mpv-seek (offset)
  "Seek OFFSET seconds relative to mpv's current position."
  (supersonic-mpv-command "seek" (number-to-string offset) "relative"))

(defun supersonic-mpv-seek-fraction (fraction)
  "Seek mpv to FRACTION (0.0 to 1.0) of the way through the current track."
  (supersonic-mpv-command "seek" (number-to-string (* fraction 100)) "absolute-percent"))

(aio-defun
 supersonic-mpv-queue ()
 "Return a promise resolving to mpv's playlist as generic queue entries.
The mpv side of `supersonic-playback-queue'.  Fetches mpv's raw
\"playlist\" property via `supersonic-mpv-get-property' and translates
each entry's private mpv playlist id back to a supersonic track id
through `supersonic--playlist', the same table
`supersonic--mpv-load-track' populates as tracks are loaded."
 (mapcar
  (lambda (item)
    (list :track-id (gethash (alist-get 'id item) supersonic--playlist) :current (and (alist-get 'current item) t)))
  (aio-await (supersonic-mpv-get-property "playlist"))))

(aio-defun
 supersonic-mpv-status (key)
 "Return a promise resolving to mpv's current value for status KEY.
The mpv side of `supersonic-playback-status'.  `paused' is answered
from `supersonic--paused', which mpv keeps up to date on its own via
the `observe_property' registered in `supersonic-mpv-ensure-running',
so it costs no round-trip; `position' is read off mpv as it is only
mpv that knows it.  `track-id' reuses `supersonic-mpv-queue' -- which
already fetches the playlist and resolves each entry's mpv id back to
a supersonic track id -- and simply picks out whichever entry it marks
current."
 (pcase key
   ('paused supersonic--paused)
   ('position (aio-await (supersonic-mpv-get-property "time-pos")))
   ('track-id
    (let ((entry (seq-find (lambda (e) (plist-get e :current)) (aio-await (supersonic-mpv-queue)))))
      (and entry (plist-get entry :track-id))))))

;; Announce mpv to the playback facade as we are loaded, so that the
;; generic `supersonic-playback-*' functions resolve to the wrappers
;; above (and to the queueing entry points further up) as soon as this
;; file is on the feature list -- see `supersonic-playback.el'.
;; Registered as symbols rather than function values, so that dispatch
;; goes through each symbol's function cell and `supersonic-mpris.el''s
;; `advice-add' on `supersonic-mpv-start'/`-enqueue' still runs.
;; Compatible with whichever provider can say where to stream from.
(supersonic-playback-register-backend
 'mpv
 '((start . supersonic-mpv-start)
   (enqueue . supersonic-mpv-enqueue)
   (toggle-play . supersonic-mpv-toggle-play)
   (next . supersonic-mpv-next)
   (prev . supersonic-mpv-prev)
   (stop . supersonic-mpv-kill)
   (seek . supersonic-mpv-seek)
   (seek-fraction . supersonic-mpv-seek-fraction)
   (live-p . supersonic-mpv-live-p)
   (status . supersonic-mpv-status)
   (queue . supersonic-mpv-queue))
 :requires '(stream-url))

(provide 'supersonic-mpv)
;;; supersonic-mpv.el ends here
