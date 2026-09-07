;;; supersonic-mpv.el --- mpv IPC/process layer for supersonic.el -*- lexical-binding: t; -*-

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

;; mpv process/IPC layer for supersonic.el: starting and talking to an
;; idle mpv instance over its JSON IPC socket, dispatching replies and
;; events, and the handful of playback commands that are thin wrappers
;; around it.
;;
;; This file knows nothing about the Subsonic list/now-playing buffers
;; that supersonic.el renders -- it only knows *that* the identity of
;; what mpv is playing, or its pause state, may have changed, and says
;; so via `supersonic-mpv-track-change-hook' /
;; `supersonic-mpv-playback-state-change-hook' rather than calling into
;; supersonic.el directly.  supersonic.el hangs its buffer refreshes off
;; those hooks from the outside, the same way `supersonic-mpris.el'
;; observes and drives this file purely via `advice-add' rather than
;; being depended on by it.

;;; Code:
(require 'json)
(require 'url)
(require 'seq)
(require 'aio)
(require 'supersonic-api)

;; fix byte-compiler complaints
(defvar supersonic-mpv)
(defvar supersonic-default-volume)
(defvar supersonic-mpv-timeout)
(defvar supersonic-scrobble-plays)

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

(defvar supersonic-mpv--request-counter 0
  "Counter for `supersonic-mpv-command-with-callback' request_ids.")

(defvar supersonic-mpv--pending-requests (make-hash-table)
  "Map of in-flight request_id (integer) to the callback awaiting its reply.")

(defvar supersonic--paused nil
  "Non-nil if mpv currently reports its \"pause\" property as true.
Kept in sync via an `observe_property' registered once per mpv process
in `supersonic-mpv-ensure-running'; consulted by the now-playing buffer
so it never has to query mpv for this on every render.")

(defvar supersonic-mpv-track-change-hook nil
  "Hook run whenever the identity of what mpv is playing may have changed
(mpv killed/(re)started, the queue replaced/enqueued, or mpv reporting a
start-file/end-file event).  supersonic.el hangs its queue/now-playing
buffer refreshes off this from the outside, mirroring how
`supersonic-mpris.el' observes this file via `advice-add' instead of this
file depending on either of them.")

(defvar supersonic-mpv-playback-state-change-hook nil
  "Hook run whenever mpv reports its pause state changed, without the
track identity itself changing.")

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
  (setq supersonic-mpv--process nil)
  (setq supersonic-mpv--socket nil)
  (setq supersonic-mpv--socket-buffer "")
  (setq supersonic-mpv--entry-counter 0)
  (clrhash supersonic--playlist)
  (setq supersonic-mpv--request-counter 0)
  (clrhash supersonic-mpv--pending-requests)
  (setq supersonic--paused nil)
  (run-hooks 'supersonic-mpv-track-change-hook))

(defun supersonic-mpv-live-p ()
  "Return non-nil if inferior mpv is running."
  (and supersonic-mpv--process (eq (process-status supersonic-mpv--process) 'run)))

(defun supersonic-mpv-ensure-running ()
  "Make sure mpv is running as an idle player, starting it if necessary.
Does nothing if mpv is already running, so it is safe to call before
every play/enqueue action."
  (when (eq system-type 'windows-nt)
    (user-error
     "supersonic.el talks to mpv over a Unix-domain socket, which native
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
             :sentinel (lambda (process _event)
                         (unless (process-live-p process)
                           (setq supersonic-mpv--socket nil)))))
      ;; Have mpv tell us about pause/resume, whoever triggered it, so the now-playing buffer can follow along.  mpv
      ;; answers an `observe_property' with the property's current value right away, which also seeds
      ;; `supersonic--paused'.  Observer id 2 rather than 1 so it cannot collide with the one supersonic-mpris.el
      ;; registers.
      (supersonic-mpv-command "observe_property" 2 "pause")))
  t)

(defun supersonic--mpv-load-track (id flag)
  "Load supersonic track ID into the running mpv instance using loadfile FLAG.
Registers the mpv playlist entry id this load will be assigned in
`supersonic--playlist', so it can later be resolved back to ID for
scrobbling and MPRIS metadata.  If the `loadfile' command could not
actually be sent (e.g. mpv died between `supersonic-mpv-ensure-running'
and this call), the tentative registration is rolled back and an error
is signalled instead, so `supersonic-mpv--entry-counter' never runs
ahead of the playlist entries mpv has actually seen."
  (let ((entry-id (1+ supersonic-mpv--entry-counter)))
    (puthash entry-id id supersonic--playlist)
    (if (supersonic-mpv-command "loadfile" (supersonic-build-url "/stream.view" `(("id" . ,id))) flag)
        (setq supersonic-mpv--entry-counter entry-id)
      (progn
        (remhash entry-id supersonic--playlist)
        (error "Failed to load track %s: mpv is not running" id)))))

(defun supersonic-mpv-start (ids)
  "Replace the current mpv queue with IDS and start playing immediately.
`loadfile ... replace' swaps out the playlist but leaves mpv's `pause'
property untouched, so if playback was paused before this call it
would otherwise stay paused; explicitly unpause since the caller asked
to start playing now."
  (supersonic-mpv-ensure-running)
  (supersonic--mpv-load-track (car ids) "replace")
  (dolist (id (cdr ids))
    (supersonic--mpv-load-track id "append"))
  (supersonic-mpv-command "set_property" "pause" :json-false)
  (run-hooks 'supersonic-mpv-track-change-hook))

;;;###autoload
(defun supersonic-mpv-enqueue (ids)
  "Append IDS to the end of the current mpv queue.
Starts playback if mpv is currently idle; otherwise leaves whatever is
already playing undisturbed and simply queues IDS after it."
  (supersonic-mpv-ensure-running)
  (dolist (id ids)
    (supersonic--mpv-load-track id "append-play"))
  (run-hooks 'supersonic-mpv-track-change-hook))

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
          (run-hooks 'supersonic-mpv-track-change-hook))
        ;; mpv reports booleans as JSON true/false, which `json-read'
        ;; turns into t and `:json-false' -- the latter being non-nil in
        ;; Lisp, so this has to compare against t explicitly.
        (when (and (string-equal event "property-change") (string-equal (alist-get 'name parsed-response) "pause"))
          (setq supersonic--paused (eq (alist-get 'data parsed-response) t))
          (run-hooks 'supersonic-mpv-playback-state-change-hook))
        (when supersonic-scrobble-plays
          (cond
           ((string-equal event "end-file")
            (supersonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response) supersonic--playlist)))
           ((string-equal event "start-file")
            (supersonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response) supersonic--playlist)
                                 t)))))))))

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

(defun supersonic-scrobble (id &optional now-playing)
  "Scrobble ID and optionally use a NOW-PLAYING request."
  (when supersonic-scrobble-plays
    (url-retrieve
     (supersonic-build-url
      "/scrobble.view"
      `(("id" . ,id)
        ;; send a submission by default
        ("submission" .
         ,(if now-playing
              "false"
            "true"))))
     ;; Nothing here reads the reply, but `url-retrieve' still hands
     ;; the callback a response buffer and then forgets about it --
     ;; without this every scrobble leaves one ` *http host:port*'
     ;; buffer behind for the rest of the session.  Killing it from
     ;; inside the callback is safe: url-http has already handed the
     ;; connection back to its keep-alive pool before calling us (see
     ;; `url-http-activate-callback').
     (lambda (_status) (kill-buffer (current-buffer))))))

(defun supersonic-mpv--send (string)
  "Write STRING to the mpv IPC socket, returning non-nil on success.
Guards with `process-live-p' immediately beforehand and catches the
`file-error' Emacs normally raises for a write to an already-closed
socket, treating either as mpv having gone away: the socket is torn
down and nil is returned instead of the write being attempted.  This
narrows, but per #15 cannot fully close, the race against mpv
(`--idle=once') exiting mid-command -- a raw SIGPIPE landing inside
the write itself is a signal, not a Lisp error, and kills Emacs before
`condition-case' ever sees it."
  (and (process-live-p supersonic-mpv--socket)
       (condition-case nil
           (progn (process-send-string supersonic-mpv--socket string) t)
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
sees a response carrying that same request_id."
  (let ((request-id (1+ supersonic-mpv--request-counter)))
    (puthash request-id callback supersonic-mpv--pending-requests)
    (if (supersonic-mpv--send
         (concat (json-encode (list (cons 'command (apply #'vector args)) (cons 'request_id request-id))) "\n"))
        (setq supersonic-mpv--request-counter request-id)
      (remhash request-id supersonic-mpv--pending-requests)
      (message "MPV not running"))))

(aio-defun
 supersonic-mpv-get-property (name)
 "Return a promise resolving to mpv's current value of property NAME.
The `aio' counterpart of `supersonic-mpv-command-with-callback', for
callers that want to keep reading mpv state in a straight line instead
of nesting callbacks.  Only call this with mpv running: without a live
IPC connection no reply can ever arrive, and the promise stays
unresolved forever."
 (let ((promise (aio-promise)))
   (supersonic-mpv-command-with-callback (lambda (response)
                                           (aio-resolve promise (lambda () (alist-get 'data response))))
                                         "get_property" name)
   (aio-await promise)))

;;;###autoload
(defun supersonic-toggle-playing ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (supersonic-mpv-command "cycle" "pause"))

;;;###autoload
(defun supersonic-skip-track ()
  "Skip to the next track in mpv."
  (interactive)
  (supersonic-mpv-command "playlist-next"))

;;;###autoload
(defun supersonic-prev-track ()
  "Go to the previous track in mpv."
  (interactive)
  (supersonic-mpv-command "playlist-prev"))

;;;###autoload
(defun supersonic-seek-forward ()
  "Seek 30 seconds forward in mpv."
  (interactive)
  (supersonic-mpv-command "seek" "30" "relative"))

;;;###autoload
(defun supersonic-seek-back ()
  "Seek 30 seconds back in mpv."
  (interactive)
  (supersonic-mpv-command "seek" "-30" "relative"))

(provide 'supersonic-mpv)
;;; supersonic-mpv.el ends here
