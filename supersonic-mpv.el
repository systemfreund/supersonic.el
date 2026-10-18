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

;; The `mpv' playback backend: plays in a local mpv process, driven
;; over its JSON IPC socket.  It streams the URLs from
;; `supersonic-provider-stream-url', so it plays for any provider
;; implementing `stream-url'.

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
  "The connection to mpv's IPC socket.")
(defvar supersonic-mpv--socket-buffer ""
  "Output from mpv's IPC socket not yet ending in a newline.")

(defvar supersonic-mpv--entry-counter 0
  "The id of the playlist entry mpv created last.
mpv numbers entries in the order they are loaded, so the next id is
known in advance.")

(defvar supersonic--playlist (make-hash-table)
  "Map of mpv playlist entry id to track id.")

(defvar supersonic-mpv--pending-load nil
  "Promise of the last requested load into mpv, or nil.
Each load waits for the previous one, so they happen in order.")

(defvar supersonic-mpv--load-generation 0
  "Counter incremented by `supersonic-mpv-kill'.
A pending load that sees it change is dropped.")

(defvar supersonic-mpv--request-counter 0
  "The request_id of the last mpv command sent with a callback.")

(defvar supersonic-mpv--pending-requests (make-hash-table)
  "Map of request_id to the callback awaiting its reply.")

(defvar supersonic--paused nil
  "Non-nil if mpv is paused.")

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
  "Return non-nil if mpv is running."
  (and supersonic-mpv--process (eq (process-status supersonic-mpv--process) 'run)))

(defun supersonic-mpv-ensure-running ()
  "Start mpv as an idle player unless it is running."
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
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (setq supersonic-mpv--socket nil)))))
      ;; Keeps `supersonic--paused' up to date.
      (supersonic-mpv-command "observe_property" 2 "pause")))
  t)

(defun supersonic--mpv-load-track (id url flag)
  "Load track ID into mpv from URL with `loadfile' FLAG.
Record the new entry in `supersonic--playlist'.  Signal an error if
mpv isn't running.  The URL is sent over the socket so that the
credentials in it stay out of the process list."
  (let ((entry-id (1+ supersonic-mpv--entry-counter)))
    (puthash entry-id id supersonic--playlist)
    (if (supersonic-mpv-command "loadfile" url flag)
        (setq supersonic-mpv--entry-counter entry-id)
      (progn
        (remhash entry-id supersonic--playlist)
        (error "Failed to load track %s: mpv is not running" id)))))

(aio-defun supersonic-mpv--stream-urls (ids)
  "Return a promise resolving to the stream URLs of IDS."
  (let ((promises (mapcar #'supersonic-provider-stream-url ids))
        (urls nil))
    (dolist (promise promises)
      (push (aio-await promise) urls))
    (nreverse urls)))

(aio-defun supersonic-mpv--load-after (previous ids flags then)
  "Load IDS into mpv once PREVIOUS, the previous load, is done.
FLAGS is a cons of the `loadfile' flags for the first and the other
ids.  THEN is called after loading.  Errors are reported, not
signaled."
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
  "Load IDS into mpv after all loads requested before.
FLAGS and THEN are as for `supersonic-mpv--load-after'.  Return a
promise resolving when done."
  (setq supersonic-mpv--pending-load (supersonic-mpv--load-after supersonic-mpv--pending-load ids flags then)))

(defun supersonic-mpv-start (ids)
  "Replace mpv's playlist with IDS and start playing."
  (when ids
    (supersonic-mpv--load
     ids '("replace" . "append")
     (lambda ()
       (supersonic-mpv-command "set_property" "pause" :json-false)
       (run-hooks 'supersonic-playback-track-change-hook))))
  nil)

;;;###autoload
(defun supersonic-mpv-enqueue (ids)
  "Append IDS to mpv's playlist, starting playback if mpv is idle."
  (when ids
    (supersonic-mpv--load
     ids '("append-play" . "append-play")
     ;; If mpv was idle, `append-play' starts the first of IDS, and the
     ;; `start-file' that follows runs the track-change hook by itself.
     (lambda () (run-hooks 'supersonic-playback-queue-change-hook))))
  nil)

(defun supersonic--mpv-handle-message (parsed-response)
  "Handle PARSED-RESPONSE, a message from mpv: a reply or an event."
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
        ;; Not `seek': only after `playback-restart' is `time-pos' updated.
        (when (string-equal event "playback-restart")
          (run-hooks 'supersonic-playback-position-change-hook))
        ;; false is read as `:json-false'.
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
  "Process OUTPUT from mpv's IPC socket, one line per message."
  (setq supersonic-mpv--socket-buffer (concat supersonic-mpv--socket-buffer output))
  (let ((lines (split-string supersonic-mpv--socket-buffer "\n")))
    (setq supersonic-mpv--socket-buffer (car (last lines)))
    (dolist (parsed-response (mapcar #'json-read-from-string (seq-remove #'string-empty-p (butlast lines))))
      (supersonic--mpv-handle-message parsed-response))))

(defun supersonic-mpv--send (string)
  "Send STRING to mpv's IPC socket.  Return non-nil on success."
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
  "Send the mpv command ARGS.
Return non-nil if it was sent, nil if mpv isn't running."
  (if (supersonic-mpv--send (concat (json-encode (list (cons 'command (apply #'vector args)))) "\n"))
      t
    (message "MPV not running")
    nil))

(defun supersonic-mpv-command-with-callback (callback &rest args)
  "Send the mpv command ARGS, calling CALLBACK with the reply.
Return non-nil if it was sent, nil if mpv isn't running."
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

(aio-defun supersonic-mpv-get-property (name)
  "Return a promise resolving to the value of mpv's property NAME.
Resolve to nil if mpv isn't running."
  (let ((promise (aio-promise)))
    (if (supersonic-mpv-command-with-callback (lambda (response)
                                                (aio-resolve promise (lambda () (alist-get 'data response))))
                                              "get_property" name)
        (aio-await promise)
      nil)))

(defun supersonic-mpv-toggle-play ()
  "Pause or resume mpv."
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
  "Seek mpv to FRACTION, between 0.0 and 1.0, of the current track."
  (supersonic-mpv-command "seek" (number-to-string (* fraction 100)) "absolute-percent"))

(aio-defun supersonic-mpv-queue ()
  "Return a promise resolving to mpv's playlist as queue entries."
  (mapcar
   (lambda (item)
     (list :track-id (gethash (alist-get 'id item) supersonic--playlist) :current (and (alist-get 'current item) t)))
   (aio-await (supersonic-mpv-get-property "playlist"))))

(aio-defun supersonic-mpv-status (key)
  "Return a promise resolving to mpv's value for the status KEY."
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
