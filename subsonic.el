;;; subsonic.el --- Browse and play music from subsonic servers with mpv  -*- lexical-binding: t; -*-

;; Author: Alex McGrath <amk@amk.ie>
;; URL: https://git.sr.ht/~amk/subsonic.el
;; Version: 0.2.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "27.1") (transient "0.2"))

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
  "Set size for the subsonic album art download query."
  :type 'integer
  :group 'subsonic)

(defcustom subsonic-art-cache-path (expand-file-name "subsonic-cache" user-emacs-directory)
  "Path to store cached subsonic art."
  :type 'string
  :group 'subsonic)

(defcustom subsonic-ssl t
  "Choose either a https or http connection to subsonic."
  :type 'boolean
  :group 'subsonic)

(defcustom subsonic-curl-image-download nil
  "Use curl for downloading album images, can be a performance improvement."
  :type 'boolean
  :group 'subsonic)

(defcustom subsonic-curl-image-parallel t
  "Use --parallel when calling curl."
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
  (clrhash subsonic-mpv--pending-requests))

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
          "mpv-player" nil subsonic-mpv
          ;; "--no-terminal" leave this out, breaks on debian?
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
    (subsonic--mpv-load-track id "append")))

;;;###autoload
(defun subsonic-mpv-enqueue (ids)
  "Append IDS to the end of the current mpv queue.
Starts playback if mpv is currently idle; otherwise leaves whatever is
already playing undisturbed and simply queues IDS after it."
  (subsonic-mpv-ensure-running)
  (dolist (id ids)
    (subsonic--mpv-load-track id "append-play")))

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
	   (subsonic-scrobble-plays
		(let ((event (alist-get 'event parsed-response)))
		  (cond
		   ((string-equal event "end-file")
			(subsonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response)
										subsonic--playlist)))
		   ((string-equal event "start-file")
			(subsonic-scrobble (gethash (alist-get 'playlist_entry_id parsed-response)
										subsonic--playlist)
							   t)))))))))

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

(defun subsonic-get-json (url)
  "Return a parsed json response from URL."
  (condition-case nil
    (let*
      (
        (json-array-type 'list)
        (json-key-type 'string))
      (json-read-from-string
        (with-temp-buffer
          (url-insert-file-contents url)
          (prog1 (buffer-string)
            (kill-buffer)))))
    (json-readtable-error (error "Failed to read json"))))

;; fix byte-compiler complaints
(defvar url-http-end-of-headers)

(defun subsonic-image-propertize (id)
  "Generate a property for a subsonic ID."
  (propertize
    " "
    'display
    (create-image (expand-file-name id subsonic-art-cache-path) nil nil :height 100)))

(defun subsonic-get-image (id vec n buff)
  "Update a tablist VEC entry with an image from ID.
BUFF is used to specify the buffer that will be
reverted upon image load and N specifies the index"
  (if (file-exists-p (expand-file-name id subsonic-art-cache-path))
    (aset vec n (subsonic-image-propertize id))
    (url-retrieve
     (subsonic-build-url
        "/getCoverArt.view"
        `(("id" . ,id) ("size" . ,(int-to-string subsonic-art-size))))
      (lambda (_status)
        (write-region
          (+ url-http-end-of-headers 1)
          (point-max)
          (expand-file-name id subsonic-art-cache-path)
          nil
          'no-message)
        (aset vec n (subsonic-image-propertize id))
        (set-buffer buff)
        (when (derived-mode-p 'tabulated-list-mode)
          (tabulated-list-revert))))))

(defun subsonic-curl-images (entries n buff)
  "Use curl to download images for each of the ENTRIES.
N specifies the tablist index and BUFF is the buffer to be
reverted."
  (let ((curl-args (list "curl")))
    (when subsonic-curl-image-parallel
      (setq curl-args (append curl-args '("-Z"))))
    (dolist (entry entries)
      (let
        (
          (id (car entry))
          (vec (nth 1 entry)))
        (if (file-exists-p (expand-file-name id subsonic-art-cache-path))
          (aset vec n (subsonic-image-propertize id))
          (setq curl-args
            (append
              curl-args
              `
              ("-o"
                ,(expand-file-name id subsonic-art-cache-path)
                ,
                (subsonic-build-url
                  "/getCoverArt.view"
                  `(("id" . ,id) ("size" . ,(int-to-string subsonic-art-size))))))))))
    (let ((curl-process (apply #'start-process (append (list "subsonic-curl" nil) curl-args))))
      (set-process-sentinel
        curl-process
        (lambda (process _signal)
          (when (memq (process-status process) '(exit signal))
            (dolist (entry entries)
              (let
                (
                  (id (car entry))
                  (vec (nth 1 entry)))
                (when (file-exists-p (expand-file-name id subsonic-art-cache-path))
                  (aset vec n (subsonic-image-propertize id))))
              (set-buffer buff)
              (when (derived-mode-p 'tabulated-list-mode)
                (tabulated-list-revert)))))))))

(defun subsonic-get-images (entries n buff)
  (if (or (not subsonic-enable-art) (not (display-graphic-p)))
    (dolist (entry tabulated-list-entries)
      (aset (nth 1 entry) n ""))
    (progn
      (when (not (file-exists-p subsonic-art-cache-path))
        (mkdir subsonic-art-cache-path))
      (if subsonic-curl-image-download
        (subsonic-curl-images entries n (current-buffer))
        (dolist (entry tabulated-list-entries)
          (subsonic-get-image (car entry) (nth 1 entry) n buff))))))


(defun subsonic-recursive-assoc (data keys)
  "Recursively assoc DATA from a list of KEYS."
  (if keys
    (subsonic-recursive-assoc (assoc-default (car keys) data) (cdr keys))
    data))

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
          `
          (("u" . ,(plist-get subsonic-auth :user))
            ("p" . ,(funcall (plist-get subsonic-auth :secret)))
            ("c" . "ElSonic")
            ("v" . "1.16.0")
            ("f" . "json"))
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

(defun subsonic-queue-parse (playlist)
  "Turn mpv's PLAYLIST (from a \"get_property playlist\" reply) into
tabulated-list entries."
  (mapcar
    (lambda (entry)
      (let* ((mpv-id (alist-get 'id entry))
             (track-id (gethash mpv-id subsonic--playlist))
             (song
               (and track-id
                 (subsonic-recursive-assoc
                   (subsonic-get-json (subsonic-build-url "/getSong.view" `(("id" . ,track-id))))
                   '("subsonic-response" "song")))))
        (list
          (or track-id (format "%s" mpv-id))
          (vector
            (if (alist-get 'current entry) "▶" "")
            (if song (assoc-default "title" song) "?")
            (if song (or (assoc-default "artist" song) "") "")
            (if song (or (assoc-default "album" song) "") "")))))
    playlist))

(defun subsonic-queue-fetch-and-render (buff)
  "Query mpv for its current playlist and render it into BUFF."
  (if (subsonic-mpv-live-p)
    (subsonic-mpv-command-with-callback
      (lambda (response)
        (when (buffer-live-p buff)
          (with-current-buffer buff
            (setq tabulated-list-entries
              (subsonic-queue-parse (alist-get 'data response)))
            (tabulated-list-print t))))
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
  (let ((buff (get-buffer-create "*subsonic-queue*")))
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

(defun subsonic-search-refresh (query)
  "Refresh the list of search results from QUERY."
  (setq tabulated-list-entries
    (subsonic-search-parse
      (subsonic-get-json (subsonic-build-url "/search3.view" `(("query" . ,query)))))))

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
    (pop-to-buffer (current-buffer))))

(define-derived-mode
  subsonic-search-mode
  tabulated-list-mode
  "Subsonic search mode"
  ;;  type: artist|album|track
  (setq tabulated-list-format [("Type" 10 t) ("Name" 30 t)])
  (setq tabulated-list-padding 2)
  (subsonic-search-refresh (url-hexify-string (read-string "Query: ")))
  (tabulated-list-revert)
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
                  (format "%d" (assoc-default "track" track))))))
          tracks)))
    result))

(defun subsonic-tracks-json (id)
  "Fetch the raw getAlbum/getMusicDirectory json response for ID."
  (subsonic-get-json (if subsonic-browse-by-tags
						 (subsonic-build-url "/getAlbum.view" `(("id" . ,id)))
					   (subsonic-build-url "/getMusicDirectory.view" `(("id" . ,id))))))

(defun subsonic-get-album-track-ids (id)
  "Return the list of track ids for album/directory ID."
  (mapcar #'car (subsonic-tracks-parse (subsonic-tracks-json id))))

(defun subsonic-tracks-refresh (id)
  "Refresh the list of subsonic tracks from ID."
  (setq tabulated-list-entries (subsonic-tracks-parse (subsonic-tracks-json id))))

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
    (subsonic-tracks-refresh id)
    (tabulated-list-revert)
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

(defun subsonic-albums-refresh (id)
  "Refresh the albums list for a given artist ID."
  (setq tabulated-list-entries
    (subsonic-albums-parse
      (subsonic-get-json (subsonic-build-url "/getArtist.view" `(("id" . ,id))))))
  (subsonic-get-images tabulated-list-entries 2 (current-buffer)))


(defun subsonic-albums-refresh-type (type)
  "Refresh the albums list for a given albumlist TYPE."
  (setq tabulated-list-entries
    (subsonic-albums-type-parse
      (subsonic-get-json
        (subsonic-build-url
          "/getAlbumList2.view"
          `(("type" . ,type) ("size" . ,(number-to-string subsonic-album-list-count)))))))
  (subsonic-get-images tabulated-list-entries 2 (current-buffer)))

(defun subsonic-open-tracks ()
  "Open a list of tracks at point."
  (interactive)
  (subsonic-tracks (tabulated-list-get-id)))

(defun subsonic-enqueue-album ()
  "Add all the tracks of the album at point to the play queue."
  (interactive)
  (let ((ids (subsonic-get-album-track-ids (tabulated-list-get-id))))
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
  "Show a list of recently played subsonic albums."
  (interactive)
  (subsonic-albums nil "recent"))

(defun subsonic-random-albums ()
  "Show a list of random subsonic albums."
  (interactive)
  (subsonic-albums nil "random"))

(defun subsonic-newest-albums ()
  "Show a list of recently added subsonic albums."
  (interactive)
  (subsonic-albums nil "newest"))

(defun subsonic-albums (&optional id type)
  "Open a buffer of albums for artist ID or list TYPE."
  (cond
   (id
	(let ((new-buff (get-buffer-create "*subsonic-artist-albums*")))
	  (set-buffer new-buff)
	  (subsonic-album-mode)
	  (subsonic-albums-refresh id)))
   (type
    (let ((new-buff (get-buffer-create "*subsonic-albums*")))
	  (set-buffer new-buff)
	  (subsonic-album-type-mode)
	  (subsonic-albums-refresh-type type))))
  (tabulated-list-revert)
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

(defun subsonic-artists-refresh ()
  "Refresh the list of subsonic artists."
  (setq tabulated-list-entries
    (subsonic-artists-parse (subsonic-get-json (subsonic-build-url "/getArtists.view" '())))))

(defvar subsonic-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'subsonic-open-album)
    map))

;;;###autoload
(defun subsonic-artists ()
  "List subsonic artists."
  (interactive)
  (let ((new-buff (get-buffer-create "*subsonic-artists*")))
    (set-buffer new-buff)
    (setq buffer-read-only t)
    (subsonic-artist-mode)
    (tabulated-list-revert)
    (pop-to-buffer (current-buffer))))

(define-derived-mode
  subsonic-artist-mode
  tabulated-list-mode
  "Subsonic Artists"
  (setq tabulated-list-format [("Artist" 30 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'subsonic-artists-refresh nil t)
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

(defun subsonic-podcasts-refresh ()
  "Refresh the list of podcasts."
  (setq tabulated-list-entries
    (subsonic-podcasts-parse
      (subsonic-get-json
        (subsonic-build-url "/getPodcasts.view" '(("includeEpisodes" . "false"))))))
  (subsonic-get-images tabulated-list-entries 1 (current-buffer)))


(defun subsonic-open-podcast-episodes ()
  "Open a view of podcasts episodes from the podcast at point."
  (interactive)
  (subsonic-podcast-episodes (tabulated-list-get-id)))

(defun subsonic-add-podcast ()
  "Add a new subsonic podcast."
  (interactive)
  (subsonic-get-json
    (subsonic-build-url
      "/createPodcastChannel.view"
      `(("url" . ,(url-hexify-string (read-string "feed url: ")))))))

(transient-define-prefix
  subsonic-podcast-help () "Help transient for subsonic podcasts."
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
  "List subsonic podcasts."
  (interactive)
  (let ((new-buff (get-buffer-create "*subsonic-podcasts*")))
    (set-buffer new-buff)
    (setq buffer-read-only t)
    (subsonic-podcast-mode)
    (subsonic-podcasts-refresh)
    (tabulated-list-revert)
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

(defun subsonic-podcasts-episode-refresh (id)
  "Refresh the list of podcast episodes for a podcast ID."
  (setq tabulated-list-entries
    (subsonic-podcast-episodes-parse
      (subsonic-get-json
        (subsonic-build-url "/getPodcasts.view" `(("id" . ,id) ("includeEpisodes" . "true")))))))

(defun subsonic-download-podcast-episode ()
  "Tell the subsonic server to download an episode at point."
  (interactive)
  (subsonic-get-json
    (subsonic-build-url "/downloadPodcastEpisode.view" `(("id" . ,(tabulated-list-get-id))))))

(transient-define-prefix
  subsonic-podcast-episode-help
  ()
  "Help transient for subsonic podcast episodes."
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
    (subsonic-podcasts-episode-refresh id)
    (tabulated-list-revert)
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
    ("p" "Podcasts" subsonic-podcasts)
    ("a" "Artists" subsonic-artists)
    ("r" "Random Albums" subsonic-random-albums)
    ("n" "Newest Albums" subsonic-newest-albums)
    ("s" "Search subsonic" subsonic-search)]
  ["Controls"
    ("t" "Toggle playing" subsonic-toggle-playing)
    ("f" "Skip track" subsonic-skip-track)
    ("b" "Previous track" subsonic-prev-track)
    ("F" "Seek forward" subsonic-seek-forward)
    ("B" "Seek back" subsonic-seek-back)
    ("Q" "Show queue" subsonic-show-queue)])

(provide 'subsonic)

;;; subsonic.el ends here
