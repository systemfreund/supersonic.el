;;; subsonic.el --- Browse and play music from subsonic servers with mpv  -*- lexical-binding: t; -*-

;; Author: Alex McGrath <amk@amk.ie>
;; URL: https://git.sr.ht/~amk/subsonic.el
;; Version: 0.3.0
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
;; uses mpv for playing the actual music.  Use a ~/.authinfo.gpg file with
;; contents like the following to setup auth
;;
;; machine SUBSONIC_HOST login USERNAME password PASSWORD

;;; Code:
(require 'json)
(require 'url)
(require 'tq)
(require 'seq)
(require 'aio)

(require 'transient)

;; Credit & thanks to the mpv.el and docker-mode projects for examples
;; and much of the code here :)

(defgroup subsonic nil "Customization group for mpv." :prefix "subsonic-" :group 'external)

(defcustom subsonic-host ""
  "Hostname for the subsonic service.
Used to find the correct authinfo entry."
  :type 'string
  :group 'subsonic)

(defcustom subsonic-mpv (executable-find "mpv")
  "Path to the mpv executable."
  :type 'string
  :group 'subsonic)

(defcustom subsonic-default-volume 100
  "Default  volume for mpv to use."
  :type 'integer
  :group 'subsonic)

(defcustom subsonic-enable-art nil
  "Enable displaying album art in supported frames."
  :type 'boolean
  :group 'subsonic)

(defcustom subsonic-art-size 100
  "Set size for the album art download query."
  :type 'integer
  :group 'subsonic)

(defcustom subsonic-art-cache-path (expand-file-name "subsonic-cache" user-emacs-directory)
  "Path to store cached art."
  :type 'string
  :group 'subsonic)

(defcustom subsonic-ssl t
  "Choose either a https or http connection to subsonic."
  :type 'boolean
  :group 'subsonic)

(defcustom subsonic-album-list-count 50
  "Number of albums to display in random/newest albums etc."
  :type 'integer
  :group 'subsonic)

(defcustom subsonic-browse-by-tags t
  "Browse by folder or by idv3 tags."
  :type 'boolean
  :group 'subsonic)

(defcustom subsonic-scrobble-plays nil
  "Request that the subsonic server scrobble played tracks."
  :type 'boolean
  :group 'subsonic)

(defcustom subsonic-mpv-timeout 0.5
  "How long to wait when starting or killing the mpv process."
  :type 'float
  :group 'subsonic)

(defvar subsonic-mpv--volume subsonic-default-volume)
(defvar subsonic-mpv--process nil)
(defvar subsonic-mpv--queue nil)

(defvar subsonic-mpv--entry-counter 0
  "Client-side mirror of the mpv playlist entry id mpv will assign next.
mpv assigns each `loadfile' a playlist entry id that is unique for the
lifetime of the mpv core instance and increments strictly in the order
commands are sent; since we are the only writer on the IPC socket, we
can predict it instead of reading it back from mpv.")

(defvar subsonic--playlist (make-hash-table)
  "Map of mpv playlist entry id (integer) to subsonic track id (string).
Populated as tracks are loaded into mpv via `subsonic--mpv-load-track',
and consulted by scrobbling and MPRIS to resolve `playlist_entry_id'
values reported by mpv back to subsonic track ids.")

(defvar subsonic-mpv--request-counter 0
  "Counter for `subsonic-mpv-command-with-callback' request_ids.")

(defvar subsonic-mpv--pending-requests (make-hash-table)
  "Map of in-flight request_id (integer) to the callback awaiting its reply.")

(defun subsonic-mpv-kill ()
  "Kill the mpv process."
  (interactive)
  (when subsonic-mpv--queue
    (tq-close subsonic-mpv--queue))
  (when (subsonic-mpv-live-p)
    (kill-process subsonic-mpv--process))
  (with-timeout (subsonic-mpv-timeout (error "Failed to kill mpv"))
    (while (subsonic-mpv-live-p)
      (sleep-for 0.05)))
  (setq subsonic-mpv--process nil)
  (setq subsonic-mpv--queue nil)
  (setq subsonic-mpv--entry-counter 0)
  (clrhash subsonic--playlist)
  (setq subsonic-mpv--request-counter 0)
  (clrhash subsonic-mpv--pending-requests)
  (subsonic-queue-maybe-refresh))

(defun subsonic-mpv-live-p ()
  "Return non-nil if inferior mpv is running."
  (and subsonic-mpv--process (eq (process-status subsonic-mpv--process) 'run)))

(defun subsonic-mpv-ensure-running ()
  "Make sure mpv is running as an idle player, starting it if necessary.
Does nothing if mpv is already running, so it is safe to call before
every play/enqueue action."
  (unless (subsonic-mpv-live-p)
    (subsonic-mpv-kill)
    (let ((socket (make-temp-name (expand-file-name "subsonic-mpv-" temporary-file-directory))))
      (setq subsonic-mpv--process
        (start-process
          "supersonic-player" nil subsonic-mpv
          "--no-terminal"
          "--really-quiet"
          "--no-video"
          "--no-config"
          "--idle=once"
          (format "--volume=%d" subsonic-mpv--volume)
          (concat "--input-ipc-server=" socket)))
      (set-process-query-on-exit-flag subsonic-mpv--process nil)
      (set-process-sentinel
        subsonic-mpv--process
        (lambda (process _event)
          (when (memq (process-status process) '(exit signal))
            (subsonic-mpv-kill)
            (when (file-exists-p socket)
              (with-demoted-errors "%S" (delete-file socket))))))
      (with-timeout (subsonic-mpv-timeout (subsonic-mpv-kill) (error "Failed to connect to mpv"))
        (while (not (file-exists-p socket))
          (sleep-for 0.05)))
      (setq subsonic-mpv--queue
        (tq-create
         (make-network-process :name "subsonic-mpv-socket"
							   :family 'local
							   :service socket)))
      (set-process-filter (tq-process subsonic-mpv--queue) #'subsonic--mpv-socket-filter)))
  t)

(defun subsonic--mpv-load-track (id flag)
  "Load subsonic track ID into the running mpv instance using loadfile FLAG.
Registers the mpv playlist entry id this load will be assigned in
`subsonic--playlist', so it can later be resolved back to ID for
scrobbling and MPRIS metadata."
  (setq subsonic-mpv--entry-counter (1+ subsonic-mpv--entry-counter))
  (puthash subsonic-mpv--entry-counter id subsonic--playlist)
  (subsonic-mpv-command "loadfile" (subsonic-build-url "/stream.view" `(("id" . ,id))) flag))

(defun subsonic-mpv-start (ids)
  "Replace the current mpv queue with IDS and start playing immediately."
  (subsonic-mpv-ensure-running)
  (subsonic--mpv-load-track (car ids) "replace")
  (dolist (id (cdr ids))
    (subsonic--mpv-load-track id "append"))
  (subsonic-queue-maybe-refresh))

;;;###autoload
(defun subsonic-mpv-enqueue (ids)
  "Append IDS to the end of the current mpv queue.
Starts playback if mpv is currently idle; otherwise leaves whatever is
already playing undisturbed and simply queues IDS after it."
  (subsonic-mpv-ensure-running)
  (dolist (id ids)
    (subsonic--mpv-load-track id "append-play"))
  (subsonic-queue-maybe-refresh))

(defun subsonic--mpv-socket-filter (_ output)
  "Filter the mpv socket connection.
OUTPUT is the stdout read from mpv"
  (dolist (parsed-response (mapcar #'json-read-from-string
									(split-string output "\n" t)))
	(let* ((request-id (alist-get 'request_id parsed-response))
		   (callback (and request-id (gethash request-id subsonic-mpv--pending-requests))))
	  (cond
	   (callback
		(remhash request-id subsonic-mpv--pending-requests)
		(funcall callback parsed-response))
	   (t
		(let ((event (alist-get 'event parsed-response)))
		  (when (member event '("start-file" "end-file"))
			(subsonic-queue-maybe-refresh))
		  (when subsonic-scrobble-plays
			(cond
			 ((string-equal event "end-file")
			  (subsonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response)
										  subsonic--playlist)))
			 ((string-equal event "start-file")
			  (subsonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response)
										  subsonic--playlist)
								 t))))))))))

(defun subsonic-scrobble (id &optional now-playing)
  "Scrobble ID and optionally use a NOW-PLAYING request."
  (when subsonic-scrobble-plays
	(url-retrieve (subsonic-build-url "/scrobble.view"
									  `(("id" . ,id)
										;; send a submission by default
										("submission" . ,(if now-playing "false" "true"))))
				  (lambda (_)))))

(defvar subsonic-auth
  (let ((auth (auth-source-search :host subsonic-host)))
    (when auth
      (car auth))))

(defun subsonic-alist->query (al)
  "Convert an alist -- AL to a set of url query parameters."
  (seq-reduce
    (lambda (accu q)
      (if (string-empty-p accu)
        (concat "?" (car q) "=" (cdr q))
        (concat accu "&" (car q) "=" (cdr q))))
    al ""))

;; fix byte-compiler complaints
(defvar url-http-end-of-headers)

(aio-defun subsonic-get-json (url)
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
              (json-key-type 'string))
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
              (json-readtable-error (error "Failed to read json"))))))
      (kill-buffer buffer))))

(defun subsonic-image-propertize (id)
  "Generate a property for a subsonic ID."
  (propertize
    " "
    'display
    (create-image (expand-file-name id subsonic-art-cache-path) nil nil :height 100)))

(aio-defun subsonic--fetch-art (id)
  "Ensure cover art ID is cached on disk, fetching it if necessary.
Returns a promise that resolves once the fetch has settled; callers
should re-check `file-exists-p' afterwards rather than assume success,
since a failed fetch resolves without signalling here."
  (unless (file-exists-p (expand-file-name id subsonic-art-cache-path))
    (pcase-let ((`(,status . ,buffer)
                  (aio-await
                    (aio-url-retrieve
                      (subsonic-build-url
                        "/getCoverArt.view"
                        `(("id" . ,id) ("size" . ,(int-to-string subsonic-art-size))))))))
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
                (expand-file-name id subsonic-art-cache-path)
                nil
                'no-message))))
        (kill-buffer buffer)))))

(aio-defun subsonic-get-images (entries n buff)
  "Fetch/cache cover art for ENTRIES and paint it into column N of BUFF.
Fetches concurrently (fired up front, before anything is awaited) and
tolerates individual failures via `aio-catch', leaving those entries
without art rather than aborting the rest.  BUFF is (re)printed once
every fetch has settled, so callers don't need to print again
themselves."
  (if (or (not subsonic-enable-art) (not (display-graphic-p)))
    (dolist (entry entries)
      (aset (nth 1 entry) n ""))
    (progn
      (unless (file-exists-p subsonic-art-cache-path)
        (mkdir subsonic-art-cache-path))
      (let
        (
          (pending
            (mapcar
              (lambda (entry) (cons entry (aio-catch (subsonic--fetch-art (car entry)))))
              entries)))
        (dolist (item pending)
          (aio-await (cdr item))
          (let ((entry (car item)))
            (when (file-exists-p (expand-file-name (car entry) subsonic-art-cache-path))
              (aset (nth 1 entry) n (subsonic-image-propertize (car entry)))))))))
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (when (derived-mode-p 'tabulated-list-mode)
        (tabulated-list-print t)))))


(defun subsonic-recursive-assoc (data keys)
  "Recursively assoc DATA from a list of KEYS."
  (if keys
    (subsonic-recursive-assoc (assoc-default (car keys) data) (cdr keys))
    data))

(defun subsonic--random-salt ()
  "Generate a random alphanumeric salt for Subsonic token authentication.
12 hex characters, well above the API's 6-character minimum."
  (mapconcat (lambda (_) (format "%x" (random 16))) (make-list 12 nil) ""))

(defun subsonic--auth-query ()
  "Build the \"u\"/\"t\"/\"s\" token-auth query parameters for one request.
Uses Subsonic's token authentication (t = md5(password + salt), s = a
fresh salt per request) instead of sending the plaintext password, so
it never ends up in a URL -- which, depending on how that URL is used
elsewhere (e.g. handed to curl as an argument), could otherwise be
visible to any local user via `ps' or in a subprocess's argv."
  (let* ((password (funcall (plist-get subsonic-auth :secret)))
          (salt (subsonic--random-salt)))
    `(("u" . ,(plist-get subsonic-auth :user)) ("t" . ,(md5 (concat password salt))) ("s" . ,salt))))

(defun subsonic-build-url (endpoint extra-query)
  "Build a valid subsonic url for a given ENDPOINT.
EXTRA-QUERY is used for any extra query parameters"
  (if subsonic-auth
    (concat
      (if subsonic-ssl
        "https://"
        "http://")
      (plist-get subsonic-auth :host) "/rest" endpoint
      (subsonic-alist->query
        (append
          (subsonic--auth-query)
          `(("c" . "ElSonic") ("v" . "1.16.0") ("f" . "json"))
          extra-query)))
    (error
      "Failed to load .authinfo, please provide auth configuration for
subsonic, and ensure subsonic-host is set correctly")))

(defun subsonic-mpv-command (&rest args)
  "Generate a mpv ipc command using ARGS."
  (if subsonic-mpv--queue
	  (tq-enqueue
       subsonic-mpv--queue
       (concat (json-serialize (list 'command (apply #'vector args))) "\n")
       ""
       nil
       (lambda (_x _y)))
	(message "MPV not running")))

(defun subsonic-mpv-command-with-callback (callback &rest args)
  "Send an mpv IPC command built from ARGS, calling CALLBACK with its reply.
Unlike `subsonic-mpv-command', this expects an actual answer: the
command is tagged with a fresh request_id, and CALLBACK is invoked
with the full parsed JSON reply once `subsonic--mpv-socket-filter'
sees a response carrying that same request_id."
  (if subsonic-mpv--queue
	  (let ((request-id (setq subsonic-mpv--request-counter
							  (1+ subsonic-mpv--request-counter))))
		(puthash request-id callback subsonic-mpv--pending-requests)
		(tq-enqueue
		 subsonic-mpv--queue
		 (concat
		  (json-serialize (list 'command (apply #'vector args) 'request_id request-id))
		  "\n")
		 ""
		 nil
		 (lambda (_x _y))))
	(message "MPV not running")))

;;;###autoload
(defun subsonic-toggle-playing ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (subsonic-mpv-command "cycle" "pause"))

;;;###autoload
(defun subsonic-skip-track ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (subsonic-mpv-command "playlist-next"))

;;;###autoload
(defun subsonic-prev-track ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (subsonic-mpv-command "playlist-prev"))

;;;###autoload
(defun subsonic-seek-forward ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (subsonic-mpv-command "seek" "30" "relative")
  (subsonic))

(defun subsonic-seek-back ()
  "Toggle playing/paused state in mpv."
  (interactive)
  (subsonic-mpv-command "seek" "-30" "relative")
  (subsonic))

;;;
;;; Queue
;;;

(defconst subsonic-queue-buffer-name "*subsonic-queue*"
  "Name of the buffer used by `subsonic-show-queue'.")

(defun subsonic-queue-buffer ()
  "Return the play queue buffer if it is currently live, else nil."
  (let ((buff (get-buffer subsonic-queue-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun subsonic-queue-maybe-refresh ()
  "Refresh the play queue buffer from mpv's playlist, if it is open.
Called whenever the queue is likely to have changed: after
starting/enqueueing tracks and whenever mpv reports a track
starting or ending."
  (let ((buff (subsonic-queue-buffer)))
    (when buff
      (subsonic-queue-fetch-and-render buff))))

(aio-defun subsonic-queue-parse (playlist)
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
                   (track-id (gethash mpv-id subsonic--playlist)))
              (list entry mpv-id track-id
                (and track-id
                  (aio-catch
                    (subsonic-get-json (subsonic-build-url "/getSong.view" `(("id" . ,track-id)))))))))
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
                     (subsonic-recursive-assoc (cdr outcome) '("subsonic-response" "song")))))
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

(defun subsonic-queue-fetch-and-render (buff)
  "Query mpv for its current playlist and render it into BUFF."
  (if (subsonic-mpv-live-p)
    (subsonic-mpv-command-with-callback
      (aio-lambda (response)
        (when (buffer-live-p buff)
          (let ((entries (aio-await (subsonic-queue-parse (alist-get 'data response)))))
            (when (buffer-live-p buff)
              (with-current-buffer buff
                (setq tabulated-list-entries entries)
                (tabulated-list-print t))))))
      "get_property" "playlist")
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries nil)
        (tabulated-list-print t)))))

(defun subsonic-queue-refresh ()
  "Refresh the play queue buffer from mpv's current playlist."
  (interactive)
  (subsonic-queue-fetch-and-render (current-buffer)))

(defvar subsonic-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'subsonic-queue-refresh)
    map))

(define-derived-mode
  subsonic-queue-mode
  tabulated-list-mode
  "Subsonic Queue"
  (setq tabulated-list-format [("" 2 nil) ("Title" 40 t) ("Artist" 25 t) ("Album" 25 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun subsonic-show-queue ()
  "Open a buffer showing mpv's current play queue."
  (interactive)
  (let ((buff (get-buffer-create subsonic-queue-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'subsonic-queue-mode)
        (subsonic-queue-mode)))
    (subsonic-queue-fetch-and-render buff)
    (pop-to-buffer-same-window buff)))

(defun subsonic-get-id-as-string (data)
  (let ((id (assoc-default "id" data)))
	(if (numberp id)
	  (number-to-string id)
	  id)))

;;;
;;; Search
;;;
(defun subsonic-search-parse (data)
  "Retrieve a list of search results from some parsed json DATA."
  (let*
    (
      (search-results (subsonic-recursive-assoc data '("subsonic-response" "searchResult3")))
      (result
        (append
          (mapcar
           (lambda (artist)
             (list
                `(,(subsonic-get-id-as-string artist) . "artist")
                (vector "Artist" (assoc-default "name" artist))))
            (assoc-default "artist" search-results))
          (mapcar
            (lambda (album)
              (list
                `(,(subsonic-get-id-as-string album) . "album")
                (vector "Album" (assoc-default "name" album))))
            (assoc-default "album" search-results))
          (mapcar
            (lambda (song)
              (list
                `(,(subsonic-get-id-as-string song) . "song")
                (vector "Song" (assoc-default "title" song))))
            (assoc-default "song" search-results)))))
    result))

(aio-defun subsonic-search-refresh (query buff)
  "Refresh the list of search results from QUERY into BUFF."
  (let ((data (aio-await (subsonic-get-json (subsonic-build-url "/search3.view" `(("query" . ,query)))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (subsonic-search-parse data))
        (tabulated-list-print t)))))

(defun subsonic-open-search-appropriate-result (result)
  "Opens the RESULT from a search in the appropriate buffer."
  (let ((type (cdr result)))
    (cond
      ((string-equal type "artist")
        (subsonic-albums (car result)))
      ((string-equal type "album")
        (subsonic-tracks (car result)))
      ((string-equal type "song")
       (subsonic-mpv-start (list (car result)))))))

(defun subsonic-open-search-result ()
  "Open a view of the result from the result at point."
  (interactive)
  (subsonic-open-search-appropriate-result (tabulated-list-get-id)))

(defvar subsonic-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'subsonic-open-search-result)
    map))

;;;###autoload
(defun subsonic-search ()
  "List subsonic search results."
  (interactive)
  (let ((new-buff (get-buffer-create "*subsonic-search*")))
    (set-buffer new-buff)
    (setq buffer-read-only t)
    (subsonic-search-mode)
    (subsonic-search-refresh (url-hexify-string (read-string "Query: ")) (current-buffer))
    (pop-to-buffer (current-buffer))))

(define-derived-mode
  subsonic-search-mode
  tabulated-list-mode
  "Subsonic search mode"
  ;;  type: artist|album|track
  (setq tabulated-list-format [("Type" 10 t) ("Name" 30 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))


;;;
;;; Tracks
;;;

(defun subsonic-get-tracklist-id (id)
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

(defvar subsonic--tracks-extract
  (if subsonic-browse-by-tags
	  '("subsonic-response" "album" "song")
	'("subsonic-response" "directory" "child")))

(defun subsonic-tracks-parse (data)
  "Parse tracks from json DATA."
  (let*
    (
      (tracks (subsonic-recursive-assoc data subsonic--tracks-extract))
      (result
        (mapcar
          (lambda (track)
            (let* ((duration (assoc-default "duration" track)))
              (list
                (subsonic-get-id-as-string track)
                (vector
                  (assoc-default "title" track)
                  (format-seconds "%m:%.2s" duration)
                  (format "%d" (or (assoc-default "track" track) 0))))))
          tracks)))
    result))

(aio-defun subsonic-tracks-json (id)
  "Fetch the raw getAlbum/getMusicDirectory json response for ID."
  (aio-await
    (subsonic-get-json (if subsonic-browse-by-tags
						   (subsonic-build-url "/getAlbum.view" `(("id" . ,id)))
						 (subsonic-build-url "/getMusicDirectory.view" `(("id" . ,id)))))))

(aio-defun subsonic-get-album-track-ids (id)
  "Return the list of track ids for album/directory ID."
  (mapcar #'car (subsonic-tracks-parse (aio-await (subsonic-tracks-json id)))))

(aio-defun subsonic-tracks-refresh (id buff)
  "Refresh the list of subsonic tracks from ID into BUFF."
  (let ((data (aio-await (subsonic-tracks-json id))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (subsonic-tracks-parse data))
        (tabulated-list-print t)))))

(defun subsonic-play-tracks ()
  "Play all the tracks after the point in the list."
  (interactive)
  (subsonic-mpv-start (subsonic-get-tracklist-id (tabulated-list-get-id))))

(defun subsonic-enqueue-tracks ()
  "Add all the tracks after the point in the list to the play queue."
  (interactive)
  (let ((ids (subsonic-get-tracklist-id (tabulated-list-get-id))))
    (subsonic-mpv-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids))))

(defvar subsonic-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'subsonic-play-tracks)
    (define-key map (kbd "a") #'subsonic-enqueue-tracks)
    map))

(defun subsonic-tracks (id)
  "Create a buffer with a list of tracks from ID."
  (let ((new-buff (get-buffer-create "*subsonic-tracks*")))
    (set-buffer new-buff)
    (subsonic-tracks-mode)
    (subsonic-tracks-refresh id new-buff)
    (pop-to-buffer-same-window (current-buffer))))

(define-derived-mode
  subsonic-tracks-mode
  tabulated-list-mode
  "Subsonic Tracks"
  (setq tabulated-list-format [("Title" 30 t) ("Duration" 10 t) ("Track" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Albums
;;;

(defun subsonic-albums-parse (data)
  "Retrieve a list of albums from some parsed json DATA."
  (let*
    (
      (albums (subsonic-recursive-assoc data '("subsonic-response" "artist" "album")))
      (result
        (mapcar
          (lambda (album)
            (list
              (subsonic-get-id-as-string album)
              (vector
                (format "%d" (or (assoc-default "year" album) 0))
                (assoc-default "name" album)
                "")))
          albums)))
    result))

(defun subsonic-albums-type-parse (data)
  "Retrieve a list of albums from some parsed json DATA."
  (let*
    (
      (albums (subsonic-recursive-assoc data '("subsonic-response" "albumList2" "album")))
      (result
        (mapcar
          (lambda (album)
            (list
              (subsonic-get-id-as-string album)
              (vector (assoc-default "name" album) (assoc-default "artist" album) "")))
          albums)))
    result))

(aio-defun subsonic-albums-refresh (id buff)
  "Refresh the albums list for a given artist ID into BUFF."
  (let ((data (aio-await (subsonic-get-json (subsonic-build-url "/getArtist.view" `(("id" . ,id)))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (subsonic-albums-parse data))
        (tabulated-list-print t)
        (subsonic-get-images tabulated-list-entries 2 buff)))))


(aio-defun subsonic-albums-refresh-type (type buff)
  "Refresh the albums list for a given albumlist TYPE into BUFF."
  (let ((data
          (aio-await
            (subsonic-get-json
              (subsonic-build-url
                "/getAlbumList2.view"
                `(("type" . ,type) ("size" . ,(number-to-string subsonic-album-list-count))))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (subsonic-albums-type-parse data))
        (tabulated-list-print t)
        (subsonic-get-images tabulated-list-entries 2 buff)))))

(defun subsonic-open-tracks ()
  "Open a list of tracks at point."
  (interactive)
  (subsonic-tracks (tabulated-list-get-id)))

(aio-defun subsonic-enqueue-album ()
  "Add all the tracks of the album at point to the play queue."
  (interactive)
  (let* ((track-id (tabulated-list-get-id))
         (ids (aio-await (subsonic-get-album-track-ids track-id))))
    (subsonic-mpv-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids))))

(defvar subsonic-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'subsonic-open-tracks)
    (define-key map (kbd "a") #'subsonic-enqueue-album)
    map))

(defvar subsonic-album-type-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'subsonic-open-tracks)
    (define-key map (kbd "a") #'subsonic-enqueue-album)
    map))

(defun subsonic-recent-albums ()
  "Show a list of recently played albums."
  (interactive)
  (subsonic-albums nil "recent"))

(defun subsonic-random-albums ()
  "Show a list of random albums."
  (interactive)
  (subsonic-albums nil "random"))

(defun subsonic-newest-albums ()
  "Show a list of recently added albums."
  (interactive)
  (subsonic-albums nil "newest"))

(defun subsonic-albums (&optional id type)
  "Open a buffer of albums for artist ID or list TYPE."
  (cond
   (id
	(let ((new-buff (get-buffer-create "*subsonic-artist-albums*")))
	  (set-buffer new-buff)
	  (subsonic-album-mode)
	  (subsonic-albums-refresh id new-buff)))
   (type
    (let ((new-buff (get-buffer-create "*subsonic-albums*")))
	  (set-buffer new-buff)
	  (subsonic-album-type-mode)
	  (subsonic-albums-refresh-type type new-buff))))
  (pop-to-buffer-same-window (current-buffer)))

(define-derived-mode
  subsonic-album-type-mode
  tabulated-list-mode
  "Subsonic Album List"
  (setq tabulated-list-format [("Albums" 30 t) ("Artists" 30 t) ("Art" 30 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(define-derived-mode
  subsonic-album-mode
  tabulated-list-mode
  "Subsonic Albums"
  (setq tabulated-list-format [("Year" 5 t) ("Albums" 40 t) ("Art" 30 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Artists
;;;
(defun subsonic-open-album ()
  "Open the albums for the artist at point."
  (interactive)
  (subsonic-albums (tabulated-list-get-id)))

(defun subsonic-artists-parse (data)
  "Retrieve a list of artists from some parsed json DATA."
  (let*
    (
      (artists (subsonic-recursive-assoc data '("subsonic-response" "artists" "index")))
      (result
        (seq-reduce
          (lambda (accu artist-index)
            (append
              accu
              (mapcar
                (lambda (artist)
                  (list (subsonic-get-id-as-string artist) (vector (assoc-default "name" artist))))
                (assoc-default "artist" artist-index))))
          artists '())))
    result))

(aio-defun subsonic-artists-refresh (buff)
  "Refresh the list of artists into BUFF."
  (let ((data (aio-await (subsonic-get-json (subsonic-build-url "/getArtists.view" '())))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (subsonic-artists-parse data))
        (tabulated-list-print t)))))

(defun subsonic-artists-revert ()
  "Refresh the artists buffer from the Subsonic server."
  (interactive)
  (subsonic-artists-refresh (current-buffer)))

(defvar subsonic-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'subsonic-open-album)
    (define-key map (kbd "g") #'subsonic-artists-revert)
    map))

;;;###autoload
(defun subsonic-artists ()
  "List artists."
  (interactive)
  (let ((new-buff (get-buffer-create "*subsonic-artists*")))
    (set-buffer new-buff)
    (setq buffer-read-only t)
    (subsonic-artist-mode)
    (subsonic-artists-refresh new-buff)
    (pop-to-buffer (current-buffer))))

(define-derived-mode
  subsonic-artist-mode
  tabulated-list-mode
  "Subsonic Artists"
  (setq tabulated-list-format [("Artist" 30 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Podcasts
;;;
(defun subsonic-podcasts-parse (data)
  "Retrieve a list of podcasts from some parsed json DATA."
  (let*
    (
      (podcasts (subsonic-recursive-assoc data '("subsonic-response" "podcasts" "channel")))
      (result
        (mapcar
          (lambda (channel)
            (list (subsonic-get-id-as-string channel) (vector (assoc-default "title" channel) "")))
          podcasts)))
    result))

(aio-defun subsonic-podcasts-refresh (buff)
  "Refresh the list of podcasts into BUFF."
  (let ((data
          (aio-await
            (subsonic-get-json
              (subsonic-build-url "/getPodcasts.view" '(("includeEpisodes" . "false")))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (subsonic-podcasts-parse data))
        (tabulated-list-print t)
        (subsonic-get-images tabulated-list-entries 1 buff)))))


(defun subsonic-open-podcast-episodes ()
  "Open a view of podcasts episodes from the podcast at point."
  (interactive)
  (subsonic-podcast-episodes (tabulated-list-get-id)))

(aio-defun subsonic-add-podcast ()
  "Add a new podcast."
  (interactive)
  (aio-await
    (subsonic-get-json
      (subsonic-build-url
        "/createPodcastChannel.view"
        `(("url" . ,(url-hexify-string (read-string "feed url: "))))))))

(transient-define-prefix
  subsonic-podcast-help () "Help transient for podcasts."
  ["Subsonic podcast help"
    ("a" "Add a podcast" subsonic-add-podcast)
    ("RET" "Open a podcast" subsonic-open-podcast-episodes)])

(defvar subsonic-podcast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'subsonic-open-podcast-episodes)
    (define-key map (kbd "?") #'subsonic-podcast-help)
    (define-key map (kbd "a") #'subsonic-add-podcast)
    map))

;;;###autoload
(defun subsonic-podcasts ()
  "List podcasts."
  (interactive)
  (let ((new-buff (get-buffer-create "*subsonic-podcasts*")))
    (set-buffer new-buff)
    (setq buffer-read-only t)
    (subsonic-podcast-mode)
    (subsonic-podcasts-refresh new-buff)
    (pop-to-buffer (current-buffer))))

(define-derived-mode
  subsonic-podcast-mode
  tabulated-list-mode
  "Subsonic Podcasts"
  (setq tabulated-list-format [("Podcasts" 30 t) ("Art" 20 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;
;;; Podcast episodes
;;;

(defun subsonic-podcast-episodes-parse (data)
  "Retrieve a list of podcast episodes from some parsed json DATA."
  (let*
    (
      (episodes
        (assoc-default
          "episode"
          (car (subsonic-recursive-assoc data '("subsonic-response" "podcasts" "channel")))))
      (result
        (mapcar
          (lambda (episode)
            (list
              (subsonic-get-id-as-string episode)
              (vector
                (assoc-default "title" episode)
                (format-seconds "%h:%.2m:%.2s" (assoc-default "duration" episode))
                (assoc-default "status" episode))))
          episodes)))
    result))

(defun subsonic-play-podcast ()
  "Play a podcast episode at point."
  (interactive)
  (subsonic-mpv-start (list (tabulated-list-get-id))))

(aio-defun subsonic-podcasts-episode-refresh (id buff)
  "Refresh the list of podcast episodes for a podcast ID into BUFF."
  (let ((data
          (aio-await
            (subsonic-get-json
              (subsonic-build-url "/getPodcasts.view" `(("id" . ,id) ("includeEpisodes" . "true")))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (subsonic-podcast-episodes-parse data))
        (tabulated-list-print t)))))

(aio-defun subsonic-download-podcast-episode ()
  "Tell the subsonic server to download an episode at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (aio-await
      (subsonic-get-json
        (subsonic-build-url "/downloadPodcastEpisode.view" `(("id" . ,id)))))))

(transient-define-prefix
  subsonic-podcast-episode-help
  ()
  "Help transient for podcast episodes."
  ["Subsonic podcast episode help"
    ("d" "Download" subsonic-download-podcast-episode)
    ("RET" "Start playing" subsonic-play-podcast)])


(defvar subsonic-podcast-episodes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'subsonic-podcast-episode-help)
    (define-key map (kbd "RET") #'subsonic-play-podcast)
    (define-key map (kbd "d") #'subsonic-download-podcast-episode)
    map))

(defun subsonic-podcast-episodes (id)
  "Open a buffer with a list of podcast episodes from podcast ID."
  (let ((new-buff (get-buffer-create "*subsonic-podcast-episodes*")))
    (set-buffer new-buff)
    (subsonic-podcast-episodes-mode)
    (subsonic-podcasts-episode-refresh id new-buff)
    (pop-to-buffer-same-window (current-buffer))))

(define-derived-mode
  subsonic-podcast-episodes-mode
  tabulated-list-mode
  "Subsonic Podcast Episodes"
  (setq tabulated-list-format [("Title" 50 t) ("Duration" 10 t) ("Status" 24 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload (autoload 'subsonic "subsonic" nil t)
(transient-define-prefix
  subsonic () "Help transient for subsonic."
  ["Subsonic"
   ("a" "Artists" subsonic-artists)
   ("r" "Random Albums" subsonic-random-albums)
   ("n" "Newest Albums" subsonic-newest-albums)   
   ("s" "Search subsonic" subsonic-search)
   ("p" "Podcasts" subsonic-podcasts)]
  ["Controls"
   ("Q" "Show queue" subsonic-show-queue)
   ("t" "Toggle playing" subsonic-toggle-playing)
   ("f" "Skip track" subsonic-skip-track)
   ("b" "Previous track" subsonic-prev-track)
   ("F" "Seek forward" subsonic-seek-forward)
   ("B" "Seek back" subsonic-seek-back)])

(provide 'subsonic)

;;; subsonic.el ends here
