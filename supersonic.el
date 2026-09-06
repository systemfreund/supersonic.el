;;; supersonic.el --- Browse and play music from subsonic servers with mpv  -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
;; URL: https://github.com/systemfreund/supersonic.el
;; Version: 0.1.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "27.1") (transient "0.2") (aio "1.0"))

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

;; This package is meant to act as a simple subsonic frontend that
;; uses mpv for playing the actual music.

;;; Code:
(require 'json)
(require 'url)
(require 'tq)
(require 'seq)
(require 'aio)

(require 'transient)

(defgroup supersonic nil "Customization group for mpv." :prefix "supersonic-" :group 'external)

(defcustom supersonic-host ""
  "URL of the supersonic service, e.g. \"http://host:4533\".
Must match the \"machine\" field of the corresponding authinfo
entry verbatim; that entry's host is what is actually used to
build request URLs.  May be given without a scheme (\"http://\" or
\"https://\"), in which case \"https://\" is assumed."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-mpv (executable-find "mpv")
  "Path to the mpv executable."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-default-volume 100
  "Default  volume for mpv to use."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-enable-art nil
  "Enable displaying album art in supported frames."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-art-size 100
  "Set size for the album art download query.
Applies where art is not shown in one of supersonic's own buffers;
those download the art at exactly the size they display it at, see
`supersonic-list-art-size' and `supersonic-now-playing-art-size'."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-list-art-size 100
  "Height in pixels of the cover art in the album and podcast lists.
The art is also downloaded at this size, so raising it costs a
re-download of anything already cached at the old size."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-now-playing-art-size 300
  "Height in pixels of the cover art in the now-playing buffer.
The art is also downloaded at this size, so raising it costs a
re-download of anything already cached at the old size."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-now-playing-interval 1
  "Seconds between playback position updates in the now-playing buffer.
Each update is a single query to the local mpv socket, and only runs
while that buffer is both open and on display."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-art-cache-path (expand-file-name "supersonic-cache" user-emacs-directory)
  "Path to store cached art."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-album-list-count 50
  "Number of albums to display in random/newest albums etc."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-browse-by-tags t
  "Browse by folder or by idv3 tags."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-scrobble-plays nil
  "Request that the supersonic server scrobble played tracks."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-mpv-timeout 0.5
  "How long to wait when starting or killing the mpv process."
  :type 'float
  :group 'supersonic)

(defvar supersonic-mpv--process nil)
(defvar supersonic-mpv--queue nil)

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

(defun supersonic-mpv-kill ()
  "Kill the mpv process."
  (interactive)
  (when supersonic-mpv--queue
    (tq-close supersonic-mpv--queue))
  (when (supersonic-mpv-live-p)
    (kill-process supersonic-mpv--process))
  (with-timeout (supersonic-mpv-timeout (error "Failed to kill mpv"))
    (while (supersonic-mpv-live-p)
      (sleep-for 0.05)))
  (setq supersonic-mpv--process nil)
  (setq supersonic-mpv--queue nil)
  (setq supersonic-mpv--entry-counter 0)
  (clrhash supersonic--playlist)
  (setq supersonic-mpv--request-counter 0)
  (clrhash supersonic-mpv--pending-requests)
  (setq supersonic--paused nil)
  (supersonic-queue-maybe-refresh)
  (supersonic-now-playing-maybe-refresh))

(defun supersonic-mpv-live-p ()
  "Return non-nil if inferior mpv is running."
  (and supersonic-mpv--process (eq (process-status supersonic-mpv--process) 'run)))

(defun supersonic-mpv-ensure-running ()
  "Make sure mpv is running as an idle player, starting it if necessary.
Does nothing if mpv is already running, so it is safe to call before
every play/enqueue action."
  (unless (supersonic-mpv-live-p)
    (supersonic-mpv-kill)
    (let ((socket (make-temp-name (expand-file-name "supersonic-mpv-" temporary-file-directory))))
      (setq supersonic-mpv--process
        (start-process
          "supersonic-player" nil supersonic-mpv
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
              (with-demoted-errors "%S" (delete-file socket))))))
      (with-timeout (supersonic-mpv-timeout (supersonic-mpv-kill) (error "Failed to connect to mpv"))
        (while (not (file-exists-p socket))
          (sleep-for 0.05)))
      (setq supersonic-mpv--queue
        (tq-create
         (make-network-process :name "supersonic-mpv-socket"
							   :family 'local
							   :service socket)))
      (set-process-filter (tq-process supersonic-mpv--queue) #'supersonic--mpv-socket-filter)
      ;; Have mpv tell us about pause/resume, whoever triggered it, so the
      ;; now-playing buffer can follow along.  mpv answers an
      ;; `observe_property' with the property's current value right away,
      ;; which also seeds `supersonic--paused'.  Observer id 2 rather than 1
      ;; so it cannot collide with the one supersonic-mpris.el registers.
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
  "Replace the current mpv queue with IDS and start playing immediately."
  (supersonic-mpv-ensure-running)
  (supersonic--mpv-load-track (car ids) "replace")
  (dolist (id (cdr ids))
    (supersonic--mpv-load-track id "append"))
  (supersonic-queue-maybe-refresh)
  (supersonic-now-playing-maybe-refresh))

;;;###autoload
(defun supersonic-mpv-enqueue (ids)
  "Append IDS to the end of the current mpv queue.
Starts playback if mpv is currently idle; otherwise leaves whatever is
already playing undisturbed and simply queues IDS after it."
  (supersonic-mpv-ensure-running)
  (dolist (id ids)
    (supersonic--mpv-load-track id "append-play"))
  (supersonic-queue-maybe-refresh)
  (supersonic-now-playing-maybe-refresh))

(defun supersonic--mpv-socket-filter (_ output)
  "Filter the mpv socket connection.
OUTPUT is the stdout read from mpv"
  (dolist (parsed-response (mapcar #'json-read-from-string
									(split-string output "\n" t)))
	(let* ((request-id (alist-get 'request_id parsed-response))
		   (callback (and request-id (gethash request-id supersonic-mpv--pending-requests))))
	  (cond
	   (callback
		(remhash request-id supersonic-mpv--pending-requests)
		(funcall callback parsed-response))
	   (t
		(let ((event (alist-get 'event parsed-response)))
		  (when (member event '("start-file" "end-file"))
			(supersonic-queue-maybe-refresh)
			(supersonic-now-playing-maybe-refresh))
		  ;; mpv reports booleans as JSON true/false, which `json-read'
		  ;; turns into t and `:json-false' -- the latter being non-nil in
		  ;; Lisp, so this has to compare against t explicitly.
		  (when (and (string-equal event "property-change")
					 (string-equal (alist-get 'name parsed-response) "pause"))
			(setq supersonic--paused (eq (alist-get 'data parsed-response) t))
			(supersonic-now-playing-maybe-refresh))
		  (when supersonic-scrobble-plays
			(cond
			 ((string-equal event "end-file")
			  (supersonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response)
										  supersonic--playlist)))
			 ((string-equal event "start-file")
			  (supersonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response)
										  supersonic--playlist)
								 t))))))))))

(defun supersonic-scrobble (id &optional now-playing)
  "Scrobble ID and optionally use a NOW-PLAYING request."
  (when supersonic-scrobble-plays
	(url-retrieve (supersonic-build-url "/scrobble.view"
									  `(("id" . ,id)
										;; send a submission by default
										("submission" . ,(if now-playing "false" "true"))))
				  (lambda (_)))))

(defun supersonic-auth ()
  "Return the auth-source entry for the current `supersonic-host'.
Calls `auth-source-search' fresh every time rather than memoizing the
result ourselves -- `auth-source-search' already caches internally
(see `auth-source-do-cache'), but that cache is invalidated by
`auth-source-forget-all-cached' and expires on its own, so deferring
to it means both a `supersonic-host' change and a corrected
authinfo entry (after forgetting the cache) take effect on the next
request instead of being frozen in for the rest of the Emacs
session."
  (car (auth-source-search :host supersonic-host)))

(defun supersonic-alist->query (al)
  "Convert an alist -- AL to a set of url query parameters."
  (seq-reduce
    (lambda (accu q)
      (if (string-empty-p accu)
        (concat "?" (car q) "=" (cdr q))
        (concat accu "&" (car q) "=" (cdr q))))
    al ""))

;; fix byte-compiler complaints
(defvar url-http-end-of-headers)

(aio-defun supersonic-get-json (url)
  "Return a promise resolving to the parsed json response from URL."
  (pcase-let ((`(,status . ,buffer) (aio-await (aio-url-retrieve url))))
    (unwind-protect
      (progn
        (when (plist-get status :error)
          (error "Failed to fetch %s: %S" url (plist-get status :error)))
        (with-current-buffer buffer
          (let*
            (
              (json-array-type 'list)
              (json-key-type 'string)
              (data
                (condition-case nil
                  (json-read-from-string
                    ;; The Subsonic API always returns UTF-8 JSON (per RFC
                    ;; 8259); `aio-url-retrieve' doesn't reliably decode the
                    ;; body for us across Emacs versions, so decode explicitly.
                    ;; Safe even if it's already decoded: `decode-coding-string'
                    ;; is a no-op on text that isn't raw undecoded bytes.
                    (decode-coding-string
                      (buffer-substring (1+ url-http-end-of-headers) (point-max))
                      'utf-8))
                  (json-readtable-error (error "Failed to read json")))))
            (supersonic--signal-if-failed data)
            data)))
      (kill-buffer buffer))))

(defun supersonic--signal-if-failed (data)
  "Signal an `error' if the parsed Subsonic response DATA reports failure.
The Subsonic API reports application-level failures (e.g. a wrong
username/password from auth-source) inside a 200 OK response body,
as a \"subsonic-response\" with status \"failed\" and an \"error\"
object, rather than via the HTTP status -- so without this check
`supersonic-get-json' would return that body as if it were a normal,
empty result instead of raising anything the user can see."
  (let ((response (supersonic-recursive-assoc data '("subsonic-response"))))
    (when (equal (assoc-default "status" response) "failed")
      (let ((err (assoc-default "error" response)))
        (error "%s" (or (assoc-default "message" err) "Subsonic request failed"))))))

(defun supersonic-art-cache-file (id size)
  "Return the path cover art ID is cached under when fetched at SIZE.
The size is part of the file name because the same art is shown at
different sizes in different buffers (see `supersonic-list-art-size'
and `supersonic-now-playing-art-size'): sharing one file per art id
would hand whichever buffer asked second the other one's resolution,
and would silently keep serving the old resolution after either
setting is changed."
  (expand-file-name (format "%s-%d" id size) supersonic-art-cache-path))

(defun supersonic-image-propertize (id size)
  "Generate a property displaying cover art ID at SIZE pixels high."
  (propertize
    " "
    'display
    (create-image (supersonic-art-cache-file id size) nil nil :height size)))

(aio-defun supersonic--fetch-art (id size)
  "Ensure cover art ID is cached on disk at SIZE, fetching it if necessary.
Returns a promise that resolves once the fetch has settled; callers
should re-check `file-exists-p' afterwards rather than assume success,
since a failed fetch resolves without signalling here."
  (unless (file-exists-p (supersonic-art-cache-file id size))
    (unless (file-exists-p supersonic-art-cache-path)
      (mkdir supersonic-art-cache-path))
    (pcase-let ((`(,status . ,buffer)
                  (aio-await
                    (aio-url-retrieve
                      (supersonic-build-url
                        "/getCoverArt.view"
                        `(("id" . ,id) ("size" . ,(int-to-string size))))))))
      (unwind-protect
        (unless (plist-get status :error)
          (with-current-buffer buffer
            ;; Cover art is arbitrary binary image data, not text -- write
            ;; the bytes as-is instead of letting Emacs guess (and
            ;; possibly prompt for) a coding system.
            (let ((coding-system-for-write 'no-conversion))
              (write-region
                (1+ url-http-end-of-headers)
                (point-max)
                (supersonic-art-cache-file id size)
                nil
                'no-message))))
        (kill-buffer buffer)))))

(aio-defun supersonic-get-images (entries n buff)
  "Fetch/cache cover art for ENTRIES and paint it into column N of BUFF.
Fetches concurrently (fired up front, before anything is awaited) and
tolerates individual failures via `aio-catch', leaving those entries
without art rather than aborting the rest.  BUFF is (re)printed once
every fetch has settled, so callers don't need to print again
themselves."
  (if (or (not supersonic-enable-art) (not (display-graphic-p)))
    (dolist (entry entries)
      (aset (nth 1 entry) n ""))
    (let
      (
        (pending
          (mapcar
            (lambda (entry)
              (cons entry (aio-catch (supersonic--fetch-art (car entry) supersonic-list-art-size))))
            entries)))
      (dolist (item pending)
        (aio-await (cdr item))
        (let ((entry (car item)))
          (when (file-exists-p (supersonic-art-cache-file (car entry) supersonic-list-art-size))
            (aset
              (nth 1 entry)
              n
              (supersonic-image-propertize (car entry) supersonic-list-art-size)))))))
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (when (derived-mode-p 'tabulated-list-mode)
        (tabulated-list-print t)))))


(defun supersonic-recursive-assoc (data keys)
  "Recursively assoc DATA from a list of KEYS."
  (if keys
    (supersonic-recursive-assoc (assoc-default (car keys) data) (cdr keys))
    data))

(defun supersonic--random-salt ()
  "Generate a random alphanumeric salt for Subsonic token authentication.
12 hex characters, well above the API's 6-character minimum."
  (mapconcat (lambda (_) (format "%x" (random 16))) (make-list 12 nil) ""))

(defun supersonic--auth-query ()
  "Build the \"u\"/\"t\"/\"s\" token-auth query parameters for one request.
Uses Subsonic's token authentication (t = md5(password + salt), s = a
fresh salt per request) instead of sending the plaintext password, so
it never ends up in a URL -- which, depending on how that URL is used
elsewhere (e.g. handed to curl as an argument), could otherwise be
visible to any local user via `ps' or in a subprocess's argv."
  (let* ((auth (supersonic-auth))
          (password (funcall (plist-get auth :secret)))
          (salt (supersonic--random-salt)))
    `(("u" . ,(plist-get auth :user)) ("t" . ,(md5 (concat password salt))) ("s" . ,salt))))

(defun supersonic-build-url (endpoint extra-query)
  "Build a valid supersonic url for a given ENDPOINT.
EXTRA-QUERY is used for any extra query parameters"
  (let ((auth (supersonic-auth)))
    (if auth
      (let ((host (plist-get auth :host)))
        (concat
          (unless (string-match-p "\\`https?://" host) "https://")
          host "/rest" endpoint
          (supersonic-alist->query
            (append
              (supersonic--auth-query)
              `(("c" . "ElSonic") ("v" . "1.16.0") ("f" . "json"))
              extra-query))))
      (error
        "Failed to load .authinfo, please provide auth configuration for
supersonic, and ensure supersonic-host is set correctly"))))

(defun supersonic--report-async-error (description err)
  "Tell the user that DESCRIPTION failed with ERR via the echo area.
DESCRIPTION is a short present-tense phrase, e.g. \"fetch tracks\"."
  (message "[Supersonic] Failed to %s: %s" description (error-message-string err)))

(defun supersonic--handle-async-error (buffer description err)
  "Report that DESCRIPTION failed with ERR, both in BUFFER and the echo area.
BUFFER is the tabulated-list buffer whose refresh failed; its contents
are replaced with the error and configuration hints.  The same failure
is also echoed via `supersonic--report-async-error'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Error: Failed to %s: %s\n\n" description (error-message-string err)))
        (insert "Configuration hint:\n")
        (insert "  - Check that supersonic-host is configured correctly\n")
        (insert "  - Ensure the scheme (http:// or https://) matches your server\n")
        (insert "  - Verify .authinfo has the correct host (must match supersonic-host exactly)\n"))))
  (supersonic--report-async-error description err))

(defmacro supersonic--with-async-error-handling (buff description &rest body)
  "Run BODY, reporting any error via DESCRIPTION instead of propagating it.
BUFF, if non-nil, is a tabulated-list buffer whose contents are replaced
with the error and configuration hints, in addition to an echo-area
message; if BUFF is nil, only the echo-area message is shown.
DESCRIPTION is a short present-tense phrase, e.g. \"fetch tracks\",
combined into \"Failed to DESCRIPTION: ERR\".

Wraps BODY in a `condition-case'.  Safe to use inside an `aio-defun':
generator.el fully macroexpands a function body -- including calls to
this macro -- before transforming it, and the `condition-case' this
expands to is itself transform-aware."
  (declare (indent 2))
  `(condition-case err
     (progn ,@body)
     (error
       (if ,buff
         (supersonic--handle-async-error ,buff ,description err)
         (supersonic--report-async-error ,description err)))))

(defun supersonic--init-list-buffer (buff mode-fn placeholder)
  "Ready BUFF as a fresh tabulated-list buffer while an async refresh runs.
Turns on MODE-FN (a derived tabulated-list mode) and shows PLACEHOLDER
text (e.g. \"Loading tracks...\") until the refresh that follows
replaces it with real entries."
  (with-current-buffer buff
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert placeholder "\n")
    (setq buffer-read-only t)
    (funcall mode-fn)))

(defun supersonic-mpv-command (&rest args)
  "Generate a mpv ipc command using ARGS.
Returns non-nil if the command was actually handed to mpv's IPC queue;
nil (after printing a \"MPV not running\" message) if there is no live
connection, so callers that must stay in sync with mpv's actual state
-- like `supersonic--mpv-load-track' -- can tell the difference instead
of assuming the command went through."
  (if supersonic-mpv--queue
	  (progn
	    (tq-enqueue
         supersonic-mpv--queue
         (concat (json-serialize (list 'command (apply #'vector args))) "\n")
         ""
         nil
         (lambda (_x _y)))
	    t)
	(progn
	  (message "MPV not running")
	  nil)))

(defun supersonic-mpv-command-with-callback (callback &rest args)
  "Send an mpv IPC command built from ARGS, calling CALLBACK with its reply.
Unlike `supersonic-mpv-command', this expects an actual answer: the
command is tagged with a fresh request_id, and CALLBACK is invoked
with the full parsed JSON reply once `supersonic--mpv-socket-filter'
sees a response carrying that same request_id."
  (if supersonic-mpv--queue
	  (let ((request-id (setq supersonic-mpv--request-counter
							  (1+ supersonic-mpv--request-counter))))
		(puthash request-id callback supersonic-mpv--pending-requests)
		(tq-enqueue
		 supersonic-mpv--queue
		 (concat
		  (json-serialize (list 'command (apply #'vector args) 'request_id request-id))
		  "\n")
		 ""
		 nil
		 (lambda (_x _y))))
	(message "MPV not running")))

(aio-defun supersonic-mpv-get-property (name)
  "Return a promise resolving to mpv's current value of property NAME.
The `aio' counterpart of `supersonic-mpv-command-with-callback', for
callers that want to keep reading mpv state in a straight line instead
of nesting callbacks.  Only call this with mpv running: without a live
IPC connection no reply can ever arrive, and the promise stays
unresolved forever."
  (let ((promise (aio-promise)))
    (supersonic-mpv-command-with-callback
      (lambda (response) (aio-resolve promise (lambda () (alist-get 'data response))))
      "get_property" name)
    (aio-await promise)))

;;;###autoload
(defun supersonic-toggle-playing ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (supersonic-mpv-command "cycle" "pause"))

;;;###autoload
(defun supersonic-skip-track ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (supersonic-mpv-command "playlist-next"))

;;;###autoload
(defun supersonic-prev-track ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (supersonic-mpv-command "playlist-prev"))

;;;###autoload
(defun supersonic-seek-forward ()
  "Seek 30 seconds forward in mpv."
  (interactive)
  (supersonic-mpv-command "seek" "30" "relative"))

(defun supersonic-seek-back ()
  "Seek 30 seconds back in mpv."
  (interactive)
  (supersonic-mpv-command "seek" "-30" "relative"))

;;;
;;; Queue
;;;

(defconst supersonic-queue-buffer-name "*supersonic-queue*"
  "Name of the buffer used by `supersonic-show-queue'.")

(defun supersonic-queue-buffer ()
  "Return the play queue buffer if it is currently live, else nil."
  (let ((buff (get-buffer supersonic-queue-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-queue-maybe-refresh ()
  "Refresh the play queue buffer from mpv's playlist, if it is open.
Called whenever the queue is likely to have changed: after
starting/enqueueing tracks and whenever mpv reports a track
starting or ending."
  (let ((buff (supersonic-queue-buffer)))
    (when buff
      (supersonic-queue-fetch-and-render buff))))

(aio-defun supersonic-queue-parse (playlist)
  "Turn mpv's PLAYLIST (from a \"get_property playlist\" reply) into
tabulated-list entries.
Fetches each entry's song metadata concurrently (fired up front, below,
before anything is awaited) and tolerates individual lookup failures
via `aio-catch', falling back to the \"?\" placeholder row instead of
aborting the whole render."
  (let*
    (
      (pending
        (mapcar
          (lambda (entry)
            (let* ((mpv-id (alist-get 'id entry))
                   (track-id (gethash mpv-id supersonic--playlist)))
              (list entry mpv-id track-id
                (and track-id
                  (aio-catch
                    (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,track-id)))))))))
          playlist)))
    ;; A plain `mapcar' lambda would call `aio-await' through an ordinary
    ;; `funcall', outside of this function's own generator machinery, which
    ;; `generator.el' cannot transform -- so this collects results via a
    ;; `dolist', which (like `while') stays inline and awaits correctly.
    (let (rows)
      (dolist (item pending)
        (pcase-let ((`(,entry ,mpv-id ,track-id ,promise) item))
          (let* ((outcome (and promise (aio-await promise)))
                 (song
                   (and outcome (eq (car outcome) :success)
                     (supersonic-recursive-assoc (cdr outcome) '("subsonic-response" "song")))))
            (push
              (list
                (or track-id (format "%s" mpv-id))
                (vector
                  (if (alist-get 'current entry) "▶" "")
                  (if song (assoc-default "title" song) "?")
                  (if song (or (assoc-default "artist" song) "") "")
                  (if song (or (assoc-default "album" song) "") "")))
              rows))))
      (nreverse rows))))

(defun supersonic-queue-fetch-and-render (buff)
  "Query mpv for its current playlist and render it into BUFF."
  (if (supersonic-mpv-live-p)
    (supersonic-mpv-command-with-callback
      (aio-lambda (response)
        (when (buffer-live-p buff)
          (let ((entries (aio-await (supersonic-queue-parse (alist-get 'data response)))))
            (when (buffer-live-p buff)
              (with-current-buffer buff
                (setq tabulated-list-entries entries)
                (tabulated-list-print t))))))
      "get_property" "playlist")
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries nil)
        (tabulated-list-print t)))))

(defun supersonic-queue-refresh ()
  "Refresh the play queue buffer from mpv's current playlist."
  (interactive)
  (supersonic-queue-fetch-and-render (current-buffer)))

(defvar supersonic-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-queue-refresh)
    map))

(define-derived-mode
  supersonic-queue-mode
  tabulated-list-mode
  "Supersonic Queue"
  (setq tabulated-list-format [("" 2 nil) ("Title" 40 t) ("Artist" 25 t) ("Album" 25 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun supersonic-show-queue ()
  "Open a buffer showing mpv's current play queue."
  (interactive)
  (let ((buff (get-buffer-create supersonic-queue-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-queue-mode)
        (supersonic-queue-mode)))
    (supersonic-queue-fetch-and-render buff)
    (pop-to-buffer-same-window buff)))

;;;
;;; Now playing
;;;

(defconst supersonic-now-playing-buffer-name "*supersonic-now-playing*"
  "Name of the buffer used by `supersonic-show-now-playing'.")

(defvar supersonic-now-playing--timer nil
  "Timer ticking the playback position shown in the now-playing buffer.")

(defvar-local supersonic-now-playing--duration nil
  "Duration in seconds of the track the now-playing buffer is showing.
Kept around so the position can be re-rendered on its own tick, without
another round of metadata lookups just to learn what to count towards.")

(defun supersonic-now-playing-buffer ()
  "Return the now-playing buffer if it is currently live, else nil."
  (let ((buff (get-buffer supersonic-now-playing-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-now-playing--start-timer ()
  "Start ticking the playback position, unless that is already happening."
  (unless supersonic-now-playing--timer
    (setq supersonic-now-playing--timer
      (run-at-time
        supersonic-now-playing-interval
        supersonic-now-playing-interval
        #'supersonic-now-playing--tick))))

(defun supersonic-now-playing--stop-timer ()
  "Stop ticking the playback position."
  (when supersonic-now-playing--timer
    (cancel-timer supersonic-now-playing--timer)
    (setq supersonic-now-playing--timer nil)))

(defun supersonic-now-playing--tick ()
  "Update the playback position in the now-playing buffer.
Asks mpv where it is rather than counting seconds locally, so seeking
and pausing need no special handling here.  Stops itself once there is
nothing left to update, and keeps quiet while the buffer is not on
display."
  (let ((buff (supersonic-now-playing-buffer)))
    (cond
      ((or (not buff) (not (supersonic-mpv-live-p)))
        (supersonic-now-playing--stop-timer))
      ((not (get-buffer-window buff t)))
      (t
        (supersonic-mpv-command-with-callback
          (lambda (response)
            (supersonic-now-playing--update-field
              buff
              'duration
              (supersonic-now-playing--position
                (alist-get 'data response)
                (buffer-local-value 'supersonic-now-playing--duration buff))))
          "get_property" "time-pos")))))

(defun supersonic-now-playing-maybe-refresh ()
  "Refresh the now-playing buffer from mpv's state, if it is open.
Called at the same points as `supersonic-queue-maybe-refresh', plus
whenever mpv reports that playback was paused or resumed."
  (let ((buff (supersonic-now-playing-buffer)))
    (when buff
      (supersonic-now-playing-fetch-and-render buff))))

(defun supersonic-now-playing--insert-field (label value &optional field)
  "Insert one \"LABEL: VALUE\" metadata row, skipping it if VALUE is nil.
FIELD, if given, tags VALUE so `supersonic-now-playing--update-field'
can replace it later without re-rendering the whole buffer."
  (when value
    (insert
      (propertize (format "%-10s" (concat label ":")) 'face 'shadow)
      (if field (propertize value 'supersonic-now-playing-field field) value)
      "\n")))

(defun supersonic-now-playing--update-field (buff field value)
  "Replace the text tagged FIELD in BUFF with VALUE, leaving the rest alone.
The playback position ticks once a second; re-rendering everything that
often would rebuild the cover art image and yank point back to the top
of the buffer each time."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (save-excursion
        (goto-char (point-min))
        (let ((match (text-property-search-forward 'supersonic-now-playing-field field t)))
          (when match
            (let ((inhibit-read-only t))
              (delete-region (prop-match-beginning match) (prop-match-end match))
              (goto-char (prop-match-beginning match))
              (insert (propertize value 'supersonic-now-playing-field field)))))))))

(defun supersonic-now-playing--insert-button (label command)
  "Insert a button reading LABEL that runs COMMAND when activated."
  (insert-text-button label 'action (lambda (_button) (call-interactively command)) 'follow-link t))

(defun supersonic-now-playing--format-time (seconds longest)
  "Format SECONDS as a clock string no longer than it has to be.
LONGEST is the longest value the same line will show, so that a
position and the duration it counts towards stay the same shape: hours
only appear once LONGEST reaches an hour, e.g. \"05:01\" for a five
minute track but \"1:05:01\" once an hour is on the clock."
  (let ((seconds (max 0 (truncate (or seconds 0)))))
    (if (>= (or longest 0) 3600)
      (format-seconds "%h:%.2m:%.2s" seconds)
      (format-seconds "%.2m:%.2s" seconds))))

(defun supersonic-now-playing--position (position duration)
  "Format POSITION and DURATION (seconds, either may be nil) for display."
  (cond
    ((and position duration)
      (format "%s / %s"
        (supersonic-now-playing--format-time position duration)
        (supersonic-now-playing--format-time duration duration)))
    (duration (supersonic-now-playing--format-time duration duration))
    (position (supersonic-now-playing--format-time position position))))

(defun supersonic-now-playing--format (song)
  "Return SONG's file format as \"MP3 (audio/mpeg)\", or nil if unknown."
  (let ((suffix (assoc-default "suffix" song))
        (type (assoc-default "contentType" song)))
    (cond
      ((and suffix type) (format "%s (%s)" (upcase suffix) type))
      (suffix (upcase suffix))
      (type type))))

(defun supersonic-now-playing--art (song)
  "Return a display string for SONG's cover art, or nil if there is none.
Expects the art to be cached already, which
`supersonic-now-playing-fetch-and-render' takes care of before it
renders."
  (let ((art-id (assoc-default "coverArt" song)))
    (when (and supersonic-enable-art
               (display-graphic-p)
               art-id
               (file-exists-p (supersonic-art-cache-file art-id supersonic-now-playing-art-size)))
      (supersonic-image-propertize art-id supersonic-now-playing-art-size))))

(defun supersonic-now-playing--render (buff song paused position)
  "Render SONG into BUFF, marked as paused or playing according to PAUSED.
POSITION is how many seconds into SONG playback currently is.  SONG is a
\"song\" alist as returned by getSong.view; if it is nil, BUFF shows a
placeholder saying that nothing is playing."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (let ((inhibit-read-only t)
            (art (and song (supersonic-now-playing--art song)))
            (duration (and song (assoc-default "duration" song)))
            (size (and song (assoc-default "size" song))))
        (setq supersonic-now-playing--duration duration)
        (if song
          (supersonic-now-playing--start-timer)
          (supersonic-now-playing--stop-timer))
        (erase-buffer)
        (if (not song)
          (insert "Nothing is playing.\n")
          (progn
            (when art
              (insert art "\n\n"))
            (insert
              (propertize (or (assoc-default "title" song) "?") 'face 'bold)
              "  "
              (propertize (if paused "(paused)" "(playing)") 'face 'shadow)
              "\n\n")
            (supersonic-now-playing--insert-button "|◀◀" #'supersonic-prev-track)
            (insert "  ")
            (supersonic-now-playing--insert-button (if paused " ▶ " " ⏸ ") #'supersonic-toggle-playing)
            (insert "  ")
            (supersonic-now-playing--insert-button "▶▶|" #'supersonic-skip-track)
            (insert "  ")
            (supersonic-now-playing--insert-button "◀◀" #'supersonic-seek-back)
            (insert "  ")
            (supersonic-now-playing--insert-button "▶▶" #'supersonic-seek-forward)
            (insert "\n\n")
            (supersonic-now-playing--insert-field "Title" (assoc-default "title" song))
            (supersonic-now-playing--insert-field "Artist" (assoc-default "artist" song))
            (supersonic-now-playing--insert-field "Album" (assoc-default "album" song))
            (supersonic-now-playing--insert-field "Format" (supersonic-now-playing--format song))
            (supersonic-now-playing--insert-field
              "Duration"
              (supersonic-now-playing--position position duration)
              'duration)
            (supersonic-now-playing--insert-field
              "Size"
              (and size (format "%.2f MB" (/ size 1048576.0))))))
        (goto-char (point-min))))))

(aio-defun supersonic-now-playing-fetch-and-render (buff)
  "Query mpv for the track it is currently on and render it into BUFF.
Tolerates a failing metadata lookup the way `supersonic-queue-parse'
does: rather than blanking a view that refreshes on every track change,
it falls back to showing the bare track id."
  (if (not (supersonic-mpv-live-p))
    (supersonic-now-playing--render buff nil nil nil)
    (let* ((playlist (aio-await (supersonic-mpv-get-property "playlist")))
           (entry (seq-find (lambda (item) (alist-get 'current item)) playlist))
           (track-id (and entry (gethash (alist-get 'id entry) supersonic--playlist))))
      (if (not track-id)
        (supersonic-now-playing--render buff nil nil nil)
        (let* ((outcome
                 (aio-await
                   (aio-catch
                     (supersonic-get-json
                       (supersonic-build-url "/getSong.view" `(("id" . ,track-id)))))))
               (song
                 (if (eq (car outcome) :success)
                   (supersonic-recursive-assoc (cdr outcome) '("subsonic-response" "song"))
                   `(("title" . ,track-id)))))
          (when (and supersonic-enable-art (assoc-default "coverArt" song))
            (aio-await
              (aio-catch
                (supersonic--fetch-art
                  (assoc-default "coverArt" song)
                  supersonic-now-playing-art-size))))
          ;; Asked for last, so the position is as fresh as possible: the
          ;; art fetch above can take a while on a cold cache.
          (supersonic-now-playing--render
            buff
            song
            supersonic--paused
            (aio-await (supersonic-mpv-get-property "time-pos"))))))))

(defun supersonic-now-playing-refresh ()
  "Refresh the now-playing buffer from mpv's current state."
  (interactive)
  (supersonic-now-playing-fetch-and-render (current-buffer)))

(defvar supersonic-now-playing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-now-playing-refresh)
    (define-key map (kbd "SPC") #'supersonic-toggle-playing)
    (define-key map (kbd "n") #'supersonic-skip-track)
    (define-key map (kbd "p") #'supersonic-prev-track)
    (define-key map (kbd "f") #'supersonic-seek-forward)
    (define-key map (kbd "b") #'supersonic-seek-back)
    map))

(define-derived-mode
  supersonic-now-playing-mode
  special-mode
  "Supersonic Now Playing"
  "Major mode for the buffer opened by `supersonic-show-now-playing'.")

;;;###autoload
(defun supersonic-show-now-playing ()
  "Open a buffer showing the track mpv is currently on.
The buffer follows mpv on its own -- track changes, pausing and
resuming are all reflected without a manual refresh, the same way the
play queue buffer keeps itself current."
  (interactive)
  (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-now-playing-mode)
        (supersonic-now-playing-mode)))
    (ignore (supersonic-now-playing-fetch-and-render buff))
    (pop-to-buffer-same-window buff)))

(defun supersonic-get-id-as-string (data)
  (let ((id (assoc-default "id" data)))
	(if (numberp id)
	  (number-to-string id)
	  id)))

;;;
;;; Search
;;;
(defun supersonic-search-parse (data)
  "Retrieve a list of search results from some parsed json DATA."
  (let*
    (
      (search-results (supersonic-recursive-assoc data '("subsonic-response" "searchResult3")))
      (result
        (append
          (mapcar
           (lambda (artist)
             (list
                `(,(supersonic-get-id-as-string artist) . "artist")
                (vector "Artist" (assoc-default "name" artist))))
            (assoc-default "artist" search-results))
          (mapcar
            (lambda (album)
              (list
                `(,(supersonic-get-id-as-string album) . "album")
                (vector "Album" (assoc-default "name" album))))
            (assoc-default "album" search-results))
          (mapcar
            (lambda (song)
              (list
                `(,(supersonic-get-id-as-string song) . "song")
                (vector "Song" (assoc-default "title" song))))
            (assoc-default "song" search-results)))))
    result))

(aio-defun supersonic-search-refresh (query buff)
  "Refresh the list of search results from QUERY into BUFF."
  (supersonic--with-async-error-handling buff "search"
    (let ((data (aio-await (supersonic-get-json (supersonic-build-url "/search3.view" `(("query" . ,query)))))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (setq tabulated-list-entries (supersonic-search-parse data))
          (tabulated-list-print t))))))

(defun supersonic-open-search-appropriate-result (result)
  "Opens the RESULT from a search in the appropriate buffer."
  (let ((type (cdr result)))
    (cond
      ((string-equal type "artist")
        (supersonic-albums (car result)))
      ((string-equal type "album")
        (supersonic-tracks (car result)))
      ((string-equal type "song")
       (supersonic-mpv-start (list (car result)))))))

(defun supersonic-open-search-result ()
  "Open a view of the result from the result at point."
  (interactive)
  (supersonic-open-search-appropriate-result (tabulated-list-get-id)))

(defvar supersonic-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-search-result)
    map))

;;;###autoload
(defun supersonic-search ()
  "List supersonic search results."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-search*")))
    (supersonic--init-list-buffer new-buff #'supersonic-search-mode "Searching...")
    (ignore (supersonic-search-refresh (url-hexify-string (read-string "Query: ")) new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
  supersonic-search-mode
  tabulated-list-mode
  "Supersonic search mode"
  ;;  type: artist|album|track
  (setq tabulated-list-format [("Type" 10 t) ("Name" 30 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))


;;;
;;; Tracks
;;;

(defun supersonic-get-tracklist-id (id)
  "Get a tracklist for a given ID."
  (reverse
    (seq-reduce
      (lambda (accu current)
        (if (equal (car current) id)
          (list (car current))
          (if (null accu)
            '()
            (cons (car current) accu))))
      tabulated-list-entries '())))

(defun supersonic--tracks-extract-path ()
  "Return the json path to a track list, per current `supersonic-browse-by-tags'.
Computed fresh on every call rather than cached, so that toggling
`supersonic-browse-by-tags' at runtime stays consistent with
`supersonic-tracks-json', which also reads it live to pick the
endpoint -- a cached path here would otherwise go stale and parse
the response at the wrong key."
  (if supersonic-browse-by-tags
    '("subsonic-response" "album" "song")
    '("subsonic-response" "directory" "child")))

(defun supersonic-tracks-parse (data)
  "Parse tracks from json DATA."
  (let*
    (
      (tracks (supersonic-recursive-assoc data (supersonic--tracks-extract-path)))
      (result
        (mapcar
          (lambda (track)
            (let* ((duration (assoc-default "duration" track)))
              (list
                (supersonic-get-id-as-string track)
                (vector
                  (assoc-default "title" track)
                  (format-seconds "%m:%.2s" duration)
                  (format "%d" (or (assoc-default "track" track) 0))))))
          tracks)))
    result))

(aio-defun supersonic-tracks-json (id)
  "Fetch the raw getAlbum/getMusicDirectory json response for ID."
  (aio-await
    (supersonic-get-json (if supersonic-browse-by-tags
						   (supersonic-build-url "/getAlbum.view" `(("id" . ,id)))
						 (supersonic-build-url "/getMusicDirectory.view" `(("id" . ,id)))))))

(aio-defun supersonic-get-album-track-ids (id)
  "Return the list of track ids for album/directory ID."
  (mapcar #'car (supersonic-tracks-parse (aio-await (supersonic-tracks-json id)))))

(aio-defun supersonic-tracks-refresh (id buff)
  "Refresh the list of tracks from ID into BUFF."
  (supersonic--with-async-error-handling buff "fetch tracks"
    (let ((data (aio-await (supersonic-tracks-json id))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (setq tabulated-list-entries (supersonic-tracks-parse data))
          (tabulated-list-print t))))))

(defun supersonic-play-tracks ()
  "Play all the tracks after the point in the list."
  (interactive)
  (supersonic-mpv-start (supersonic-get-tracklist-id (tabulated-list-get-id))))

(defun supersonic-enqueue-tracks ()
  "Add all the tracks after the point in the list to the play queue."
  (interactive)
  (let ((ids (supersonic-get-tracklist-id (tabulated-list-get-id))))
    (supersonic-mpv-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids))))

(defvar supersonic-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-play-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-tracks)
    map))

(defun supersonic-tracks (id)
  "Create a buffer with a list of tracks from ID."
  (let ((new-buff (get-buffer-create "*supersonic-tracks*")))
    (supersonic--init-list-buffer new-buff #'supersonic-tracks-mode "Loading tracks...")
    (ignore (supersonic-tracks-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
  supersonic-tracks-mode
  tabulated-list-mode
  "Supersonic Tracks"
  (setq tabulated-list-format [("Title" 30 t) ("Duration" 10 t) ("Track" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Albums
;;;

(defun supersonic-albums-parse (data)
  "Retrieve a list of albums from some parsed json DATA."
  (let*
    (
      (albums (supersonic-recursive-assoc data '("subsonic-response" "artist" "album")))
      (result
        (mapcar
          (lambda (album)
            (list
              (supersonic-get-id-as-string album)
              (vector
                (format "%d" (or (assoc-default "year" album) 0))
                (assoc-default "name" album)
                "")))
          albums)))
    result))

(defun supersonic-albums-type-parse (data)
  "Retrieve a list of albums from some parsed json DATA."
  (let*
    (
      (albums (supersonic-recursive-assoc data '("subsonic-response" "albumList2" "album")))
      (result
        (mapcar
          (lambda (album)
            (list
              (supersonic-get-id-as-string album)
              (vector (assoc-default "name" album) (assoc-default "artist" album) "")))
          albums)))
    result))

(aio-defun supersonic-albums-refresh (id buff)
  "Refresh the albums list for a given artist ID into BUFF."
  (supersonic--with-async-error-handling buff "fetch albums"
    (let ((data (aio-await (supersonic-get-json (supersonic-build-url "/getArtist.view" `(("id" . ,id)))))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (setq tabulated-list-entries (supersonic-albums-parse data))
          (tabulated-list-print t)
          (supersonic-get-images tabulated-list-entries 2 buff))))))


(aio-defun supersonic-albums-refresh-type (type buff)
  "Refresh the albums list for a given albumlist TYPE into BUFF."
  (supersonic--with-async-error-handling buff "fetch albums"
    (let ((data
            (aio-await
              (supersonic-get-json
                (supersonic-build-url
                  "/getAlbumList2.view"
                  `(("type" . ,type) ("size" . ,(number-to-string supersonic-album-list-count))))))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (setq tabulated-list-entries (supersonic-albums-type-parse data))
          (tabulated-list-print t)
          (supersonic-get-images tabulated-list-entries 2 buff))))))

(defun supersonic-open-tracks ()
  "Open a list of tracks at point."
  (interactive)
  (supersonic-tracks (tabulated-list-get-id)))

(aio-defun supersonic-enqueue-album ()
  "Add all the tracks of the album at point to the play queue."
  (interactive)
  (supersonic--with-async-error-handling nil "enqueue album"
    (let* ((track-id (tabulated-list-get-id))
           (ids (aio-await (supersonic-get-album-track-ids track-id))))
      (supersonic-mpv-enqueue ids)
      (message "Added %d track(s) to the queue" (length ids)))))

(defvar supersonic-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map))

(defvar supersonic-album-type-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map))

(defun supersonic-recent-albums ()
  "Show a list of recently played albums."
  (interactive)
  (supersonic-albums nil "recent"))

(defun supersonic-random-albums ()
  "Show a list of random albums."
  (interactive)
  (supersonic-albums nil "random"))

(defun supersonic-newest-albums ()
  "Show a list of recently added albums."
  (interactive)
  (supersonic-albums nil "newest"))

(defun supersonic-albums (&optional id type)
  "Open a buffer of albums for artist ID or list TYPE."
  (cond
   (id
	(let ((new-buff (get-buffer-create "*supersonic-artist-albums*")))
	  (supersonic--init-list-buffer new-buff #'supersonic-album-mode "Loading albums...")
	  (ignore (supersonic-albums-refresh id new-buff))
	  (pop-to-buffer-same-window new-buff)))
   (type
    (let ((new-buff (get-buffer-create "*supersonic-albums*")))
	  (supersonic--init-list-buffer new-buff #'supersonic-album-type-mode "Loading albums...")
	  (ignore (supersonic-albums-refresh-type type new-buff))
	  (pop-to-buffer-same-window new-buff)))))

(define-derived-mode
  supersonic-album-type-mode
  tabulated-list-mode
  "Supersonic Album List"
  (setq tabulated-list-format [("Albums" 30 t) ("Artists" 30 t) ("Art" 30 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(define-derived-mode
  supersonic-album-mode
  tabulated-list-mode
  "Supersonic Albums"
  (setq tabulated-list-format [("Year" 5 t) ("Albums" 40 t) ("Art" 30 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Artists
;;;
(defun supersonic-open-album ()
  "Open the albums for the artist at point."
  (interactive)
  (supersonic-albums (tabulated-list-get-id)))

(defun supersonic-artists-parse (data)
  "Retrieve a list of artists from some parsed json DATA."
  (let*
    (
      (artists (supersonic-recursive-assoc data '("subsonic-response" "artists" "index")))
      (result
        (seq-reduce
          (lambda (accu artist-index)
            (append
              accu
              (mapcar
                (lambda (artist)
                  (list (supersonic-get-id-as-string artist) (vector (assoc-default "name" artist))))
                (assoc-default "artist" artist-index))))
          artists '())))
    result))

(aio-defun supersonic-artists-refresh (buff)
  "Refresh the list of artists into BUFF."
  (supersonic--with-async-error-handling buff "fetch artists"
    (let ((data (aio-await (supersonic-get-json (supersonic-build-url "/getArtists.view" '())))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (setq tabulated-list-entries (supersonic-artists-parse data))
          (tabulated-list-print t))))))

(defun supersonic-artists-revert ()
  "Refresh the artists buffer from the Subsonic server."
  (interactive)
  (supersonic-artists-refresh (current-buffer)))

(defvar supersonic-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-album)
    (define-key map (kbd "g") #'supersonic-artists-revert)
    map))

;;;###autoload
(defun supersonic-artists ()
  "List artists."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-artists*")))
    (supersonic--init-list-buffer new-buff #'supersonic-artist-mode "Loading artists...")
    (ignore (supersonic-artists-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
  supersonic-artist-mode
  tabulated-list-mode
  "Supersonic Artists"
  (setq tabulated-list-format [("Artist" 30 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Podcasts
;;;
(defun supersonic-podcasts-parse (data)
  "Retrieve a list of podcasts from some parsed json DATA."
  (let*
    (
      (podcasts (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel")))
      (result
        (mapcar
          (lambda (channel)
            (list (supersonic-get-id-as-string channel) (vector (assoc-default "title" channel) "")))
          podcasts)))
    result))

(aio-defun supersonic-podcasts-refresh (buff)
  "Refresh the list of podcasts into BUFF."
  (supersonic--with-async-error-handling buff "fetch podcasts"
    (let ((data
            (aio-await
              (supersonic-get-json
                (supersonic-build-url "/getPodcasts.view" '(("includeEpisodes" . "false")))))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (setq tabulated-list-entries (supersonic-podcasts-parse data))
          (tabulated-list-print t)
          (supersonic-get-images tabulated-list-entries 1 buff))))))


(defun supersonic-open-podcast-episodes ()
  "Open a view of podcasts episodes from the podcast at point."
  (interactive)
  (supersonic-podcast-episodes (tabulated-list-get-id)))

(aio-defun supersonic-add-podcast ()
  "Add a new podcast."
  (interactive)
  (supersonic--with-async-error-handling nil "add podcast"
    (aio-await
      (supersonic-get-json
        (supersonic-build-url
          "/createPodcastChannel.view"
          `(("url" . ,(url-hexify-string (read-string "feed url: ")))))))
    (message "Podcast added")))

(transient-define-prefix
  supersonic-podcast-help () "Help transient for podcasts."
  ["Supersonic podcast help"
    ("a" "Add a podcast" supersonic-add-podcast)
    ("RET" "Open a podcast" supersonic-open-podcast-episodes)])

(defvar supersonic-podcast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-podcast-episodes)
    (define-key map (kbd "?") #'supersonic-podcast-help)
    (define-key map (kbd "a") #'supersonic-add-podcast)
    map))

;;;###autoload
(defun supersonic-podcasts ()
  "List podcasts."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-podcasts*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-mode "Loading podcasts...")
    (ignore (supersonic-podcasts-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
  supersonic-podcast-mode
  tabulated-list-mode
  "Supersonic Podcasts"
  (setq tabulated-list-format [("Podcasts" 30 t) ("Art" 20 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Podcast episodes
;;;

(defun supersonic-podcast-episodes-parse (data)
  "Retrieve a list of podcast episodes from some parsed json DATA."
  (let*
    (
      (episodes
        (assoc-default
          "episode"
          (car (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel")))))
      (result
        (mapcar
          (lambda (episode)
            (list
              (supersonic-get-id-as-string episode)
              (vector
                (assoc-default "title" episode)
                (format-seconds "%h:%.2m:%.2s" (assoc-default "duration" episode))
                (assoc-default "status" episode))))
          episodes)))
    result))

(defun supersonic-play-podcast ()
  "Play a podcast episode at point."
  (interactive)
  (supersonic-mpv-start (list (tabulated-list-get-id))))

(aio-defun supersonic-podcasts-episode-refresh (id buff)
  "Refresh the list of podcast episodes for a podcast ID into BUFF."
  (supersonic--with-async-error-handling buff "fetch episodes"
    (let ((data
            (aio-await
              (supersonic-get-json
                (supersonic-build-url "/getPodcasts.view" `(("id" . ,id) ("includeEpisodes" . "true")))))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (setq tabulated-list-entries (supersonic-podcast-episodes-parse data))
          (tabulated-list-print t))))))

(aio-defun supersonic-download-podcast-episode ()
  "Tell the supersonic server to download an episode at point."
  (interactive)
  (supersonic--with-async-error-handling nil "download episode"
    (let ((id (tabulated-list-get-id)))
      (aio-await
        (supersonic-get-json
          (supersonic-build-url "/downloadPodcastEpisode.view" `(("id" . ,id)))))
      (message "Episode download started"))))

(transient-define-prefix
  supersonic-podcast-episode-help
  ()
  "Help transient for podcast episodes."
  ["Supersonic podcast episode help"
    ("d" "Download" supersonic-download-podcast-episode)
    ("RET" "Start playing" supersonic-play-podcast)])


(defvar supersonic-podcast-episodes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'supersonic-podcast-episode-help)
    (define-key map (kbd "RET") #'supersonic-play-podcast)
    (define-key map (kbd "d") #'supersonic-download-podcast-episode)
    map))

(defun supersonic-podcast-episodes (id)
  "Open a buffer with a list of podcast episodes from podcast ID."
  (let ((new-buff (get-buffer-create "*supersonic-podcast-episodes*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-episodes-mode "Loading episodes...")
    (ignore (supersonic-podcasts-episode-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
  supersonic-podcast-episodes-mode
  tabulated-list-mode
  "Supersonic Podcast Episodes"
  (setq tabulated-list-format [("Title" 50 t) ("Duration" 10 t) ("Status" 24 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload (autoload 'supersonic "supersonic" nil t)
(transient-define-prefix
  supersonic () "Help transient for supersonic."
  ["Supersonic"
   ("a" "Artists" supersonic-artists)
   ("r" "Random Albums" supersonic-random-albums)
   ("n" "Newest Albums" supersonic-newest-albums)   
   ("s" "Search supersonic" supersonic-search)
   ("p" "Podcasts" supersonic-podcasts)]
  ["Controls"
   ("Q" "Show queue" supersonic-show-queue)
   ("N" "Now playing" supersonic-show-now-playing)
   ("t" "Toggle playing" supersonic-toggle-playing)
   ("f" "Skip track" supersonic-skip-track)
   ("b" "Previous track" supersonic-prev-track)
   ;; Seeking is the one control worth repeating in a row, so these two
   ;; keep the transient open instead of dismissing it.  That used to be
   ;; done by having the commands themselves re-invoke this prefix, which
   ;; also popped it up when they were called from outside it, e.g. from
   ;; the now-playing buffer.
   ("F" "Seek forward" supersonic-seek-forward :transient t)
   ("B" "Seek back" supersonic-seek-back :transient t)])

(provide 'supersonic)

;;; supersonic.el ends here
