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
;; This file knows nothing about supersonic.el's buffers: it reports
;; changes through the facade's hooks.  Nor does it know
;; how to reach a track: it streams whatever URL
;; `supersonic-provider-stream-url' resolves to, so it plays for any
;; provider that implements `stream-url'.

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
Replies are matched to requests via `supersonic-mpv--pending-requests'.")
(defvar supersonic-mpv--socket-buffer ""
  "Bytes read from the mpv IPC socket that don't yet form a complete line.
See `supersonic--mpv-socket-filter'.")

(defvar supersonic-mpv--entry-counter 0
  "Client-side mirror of the mpv playlist entry id mpv will assign next.
mpv numbers `loadfile's in the order they are sent, and nothing else
writes to the socket, so the id can be predicted rather than read back.")

(defvar supersonic--playlist (make-hash-table)
  "Map of mpv playlist entry id (integer) to supersonic track id (string).
Filled by `supersonic--mpv-load-track'.")

(defvar supersonic-mpv--pending-load nil
  "Promise of the most recently requested load into mpv, or nil.
Stream URLs are resolved asynchronously, so each load waits for the
one before it, and loads land in the order they were asked for.")

(defvar supersonic-mpv--load-generation 0
  "Bumped by `supersonic-mpv-kill' to drop loads still resolving URLs.
A load that finds this changed gives up rather than starting mpv again
right after it was stopped.")

(defvar supersonic-mpv--request-counter 0
  "Counter for `supersonic-mpv-command-with-callback' request_ids.")

(defvar supersonic-mpv--pending-requests (make-hash-table)
  "Map of in-flight request_id (integer) to the callback awaiting its reply.")

(defvar supersonic--paused nil
  "Non-nil if mpv currently reports its \"pause\" property as true.
Kept in sync by the `observe_property' in
`supersonic-mpv-ensure-running', so it costs no round trip.")

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
             ;; Notice mpv hanging up as early as possible, see
             ;; `supersonic-mpv--send'.
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (setq supersonic-mpv--socket nil)))))
      ;; Follow pause/resume, whoever triggered it.  mpv answers with the
      ;; current value right away, which also seeds `supersonic--paused'.
      (supersonic-mpv-command "observe_property" 2 "pause")))
  t)

(defun supersonic--mpv-load-track (id url flag)
  "Load supersonic track ID, streamed from URL, into mpv using loadfile FLAG.
Records the entry id mpv will assign in `supersonic--playlist'.  If
the command cannot be sent, that is rolled back and an error signalled,
so `supersonic-mpv--entry-counter' never runs ahead of mpv.  URL goes
over the socket, not the command line, keeping credentials out of the
process list."
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
All are asked for at once, not one after the other."
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
Resolves the stream URLs first, so a failure leaves mpv untouched.
Failures are reported rather than signalled: nobody awaits this, and
the load after it must not see it reject."
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
promise resolving once the load is done or has failed."
  (setq supersonic-mpv--pending-load (supersonic-mpv--load-after supersonic-mpv--pending-load ids flags then)))

(defun supersonic-mpv-start (ids)
  "Replace the current mpv queue with IDS and start playing.
Returns before they reach mpv.  Unpauses, since `loadfile ... replace'
leaves mpv's `pause' as it was."
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
Starts playback if mpv is idle.  Returns before they reach mpv."
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
        ;; Only once `playback-restart' comes, not `seek', does
        ;; `time-pos' read the position sought to.
        (when (string-equal event "playback-restart")
          (run-hooks 'supersonic-playback-position-change-hook))
        ;; Compare against t: `json-read' turns false into `:json-false'.
        (when (and (string-equal event "property-change") (string-equal (alist-get 'name parsed-response) "pause"))
          (setq supersonic--paused (eq (alist-get 'data parsed-response) t))
          (run-hooks 'supersonic-playback-state-change-hook))
        (cond
         ((string-equal event "end-file")
          (supersonic-provider-scrobble
           (gethash (alist-get 'playlist_entry_id parsed-response) supersonic--playlist)))
         ((string-equal event "start-file")
          (supersonic-provider-scrobble
           (gethash (alist-get 'playlist_entry_id parsed-response) supersonic--playlist) t))))))))

(defun supersonic--mpv-socket-filter (_ output)
  "Filter the mpv socket connection.
OUTPUT is the latest chunk read from mpv's IPC socket, which need not
end on a whole message; the rest is kept in
`supersonic-mpv--socket-buffer' until it does."
  (setq supersonic-mpv--socket-buffer (concat supersonic-mpv--socket-buffer output))
  (let ((lines (split-string supersonic-mpv--socket-buffer "\n")))
    (setq supersonic-mpv--socket-buffer (car (last lines)))
    (dolist (parsed-response (mapcar #'json-read-from-string (seq-remove #'string-empty-p (butlast lines))))
      (supersonic--mpv-handle-message parsed-response))))

(defun supersonic-mpv--send (string)
  "Write STRING to the mpv IPC socket, returning non-nil on success.
A dead socket or a `file-error' on writing counts as mpv having gone
away.  That narrows, but cannot close, the race against mpv exiting
mid-command: a SIGPIPE during the write itself is not a Lisp error
\(#15)."
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
Returns non-nil if the command was sent, nil if mpv is not running."
  (if (supersonic-mpv--send (concat (json-encode (list (cons 'command (apply #'vector args)))) "\n"))
      t
    (message "MPV not running")
    nil))

(defun supersonic-mpv-command-with-callback (callback &rest args)
  "Send an mpv IPC command built from ARGS, calling CALLBACK with its reply.
CALLBACK gets the full parsed reply.  Returns non-nil if the command
was sent, nil if mpv is not running, in which case CALLBACK is dropped."
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
Resolves to nil if the request could not be sent."
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
The mpv side of `supersonic-playback-queue'."
 (mapcar
  (lambda (item)
    (list :track-id (gethash (alist-get 'id item) supersonic--playlist) :current (and (alist-get 'current item) t)))
  (aio-await (supersonic-mpv-get-property "playlist"))))

(aio-defun
 supersonic-mpv-status (key)
 "Return a promise resolving to mpv's current value for status KEY.
The mpv side of `supersonic-playback-status'."
 (pcase key
   ('paused supersonic--paused)
   ('position (aio-await (supersonic-mpv-get-property "time-pos")))
   ('track-id
    (let ((entry (seq-find (lambda (e) (plist-get e :current)) (aio-await (supersonic-mpv-queue)))))
      (and entry (plist-get entry :track-id))))))

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
