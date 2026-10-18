;;; supersonic-music-assistant.el --- Music Assistant library provider for supersonic.el -*- lexical-binding: t; -*-

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

;; The Music Assistant library provider, registered as
;; `music-assistant'.  Enable it with:
;;
;;   (require 'supersonic-music-assistant)
;;   (setq supersonic-provider 'music-assistant)
;;
;; It uses the server's JSON-RPC endpoint `/api', authenticated with a
;; long-lived token from `supersonic-music-assistant-token-function'
;; (server schema 28 or later).  Ids are Music Assistant's item URIs,
;; such as library://album/8.  It lists library items only.
;;
;; Music Assistant plays on its own players, so there is no
;; `stream-url' and no `scrobble'; see
;; `supersonic-music-assistant-player.el' for the playback backend.
;; Adding podcasts and downloading episodes aren't supported.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'aio)

(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-provider)

(defcustom supersonic-music-assistant-token-function
  #'supersonic-music-assistant-auth-source-token
  "Function returning the token for the Music Assistant server.
It is called with `supersonic-music-assistant-url' and returns the
token, a function returning it, or nil.  The default looks the server
up in auth-source.

For a token stored elsewhere, e.g. in a pass entry:

  (setq supersonic-music-assistant-token-function
        (lambda (_server) (auth-source-pass-get \='secret \"my/entry\")))

It is called for every request, including every poll of the player,
so wrap a slow lookup like this one in `supersonic-cached':

  (setq supersonic-music-assistant-token-function
        (supersonic-cached
         (lambda (_server) (auth-source-pass-get \='secret \"my/entry\"))))"
  :type '(choice (function-item supersonic-music-assistant-auth-source-token)
                 (function :tag "Other function"))
  :group 'supersonic)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defconst supersonic-music-assistant--min-schema 28
  "The oldest server schema supported, the first with token authentication.")

(defconst supersonic-music-assistant--page-size 500
  "Number of items to request per page of a list.")

(defconst supersonic-music-assistant--proxy-sizes '(80 160 256 512 1024)
  "The sizes the image proxy scales to.  It rejects others, except 0.")

(defconst supersonic-music-assistant--album-list-args
  '((random ("order_by" . "random"))
    (newest ("order_by" . "timestamp_added_desc")))
  "Alist of album list type to the arguments of `music/albums/library_items'.
`recent' comes from the play history instead, see
`supersonic-music-assistant--recent-albums'.")

(defvar supersonic-music-assistant--recent-lookups 10
  "Number of history tracks to look up at the same time.")

(defvar supersonic-music-assistant--recent-history-start 4
  "Number of history tracks to request per recent album, at first.")

(defvar supersonic-music-assistant--recent-history-factor 16
  "Maximum number of history tracks to request per recent album.")

(defvar supersonic-music-assistant--clock-offset 0
  "Seconds the local clock is ahead of the server's.")

(defvar supersonic-music-assistant--checked-url nil
  "The last server found to be a recent enough Music Assistant.")

;;;
;;; Transport
;;;

(defun supersonic-music-assistant--server ()
  "Return `supersonic-music-assistant-url', or signal if it has no scheme."
  (let ((server supersonic-music-assistant-url))
    (unless (and (stringp server) (string-match-p "\\`https?://[^/]" server))
      (user-error "Set `supersonic-music-assistant-url' to your Music Assistant server, scheme included, e.g. http://host:8095"))
    server))

(defun supersonic-music-assistant--url (server path)
  "Return the URL of PATH on SERVER."
  (concat (string-remove-suffix "/" server) path))

(defun supersonic-music-assistant-auth-source-token (server)
  "Return the token auth-source has for SERVER, or nil.
This is the default `supersonic-music-assistant-token-function'."
  (plist-get (car (auth-source-search :host server :require '(:secret))) :secret))

(defun supersonic-music-assistant--token (server)
  "Return a copy of the token for SERVER, or nil."
  (let* ((token (funcall supersonic-music-assistant-token-function server))
         (token (if (functionp token) (funcall token) token)))
    (and token (copy-sequence token))))

(defun supersonic-music-assistant--connection ()
  "Return (SERVER . TOKEN) for the requests of an operation.
Call it before the first `aio-await', so that auth-source can prompt."
  (let ((server (supersonic-music-assistant--server)))
    (cons server (supersonic-music-assistant--token server))))

(defun supersonic-music-assistant--retrieve (url &optional body token)
  "Return a promise of (STATUS . BUFFER) for a request for URL.
POST BODY, a JSON string, if non-nil, else GET.  TOKEN, if non-nil, is
sent as a bearer token.  Redirects are not followed, since url.el
would drop the token."
  ;; A plain function, so the bindings are in effect when
  ;; `url-retrieve' reads them.
  (let ((url-max-redirections 0)
        (url-request-method (if body "POST" "GET"))
        (url-request-extra-headers
         (append (and body '(("Content-Type" . "application/json")))
                 (and token `(("Authorization" . ,(concat "Bearer " token))))))
        (url-request-data (and body (encode-coding-string body 'utf-8))))
    (supersonic-url-retrieve url)))

(defun supersonic-music-assistant--describe-failure (err)
  "Return a readable description of ERR, a request's `:error'."
  (pcase err
    (`(error connection-failed ,(and (pred stringp) detail) . ,_)
     (format "connection failed (%s)" (string-trim detail)))
    (_ (error-message-string err))))

(defun supersonic-music-assistant--redirect-target (status)
  "Return the URL the request with STATUS was redirected to, or nil."
  (or (plist-get status :redirect)
      (pcase (plist-get status :error)
        (`(error http-redirect-limit ,target) target))))

(defun supersonic-music-assistant--read-response (url status)
  "Return the parsed JSON response to URL in the current buffer.
STATUS is the status `url-retrieve' passed.  Signal an error unless
the request succeeded."
  (let ((target (supersonic-music-assistant--redirect-target status)))
    (when target
      (user-error "%s redirects to %s; set `supersonic-music-assistant-url' (and its authinfo entry) to the address it redirects to"
                  url target)))
  (unless url-http-end-of-headers
    (error "No answer from %s: %s" url (supersonic-music-assistant--describe-failure (plist-get status :error))))
  (let ((code url-http-response-status)
        (body (string-trim
               (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8))))
    (cond
     ((eql code 401)
      (user-error "Music Assistant at %s rejected the token from `supersonic-music-assistant-token-function'%s"
                  url (or (supersonic--forget-rejected-credentials supersonic-music-assistant-url) "")))
     ((not (and (integerp code) (< code 300)))
      (error "Music Assistant answered %s with %s: %s" url code body))
     (t
      (let ((json-object-type 'alist)
            (json-key-type 'string)
            (json-array-type 'list)
            (json-false nil))
        (condition-case nil
            (json-read-from-string body)
          (json-error
           (error "%s did not answer with JSON; is it a Music Assistant server?" url))))))))

(defun supersonic-music-assistant--note-server-clock ()
  "Update `supersonic-music-assistant--clock-offset' from the Date header.
Offsets up to two seconds are taken as 0."
  (let* ((header (save-excursion
                   (save-restriction
                     (narrow-to-region (point-min) url-http-end-of-headers)
                     (goto-char (point-min))
                     (let ((case-fold-search t))
                       (and (re-search-forward "^Date:[ \t]*\\([^\r\n]+\\)" nil t)
                            (match-string 1))))))
         (date (and header
                    (condition-case nil
                        (date-to-time header)
                      (error
                       (supersonic--log "Unreadable Date header: %s" header)
                       nil)))))
    (when date
      (let ((offset (- (float-time) (float-time date))))
        (setq supersonic-music-assistant--clock-offset (if (> (abs offset) 2) offset 0))))))

(aio-defun supersonic-music-assistant--fetch (url &optional body token)
  "Return a promise resolving to the parsed JSON response to URL.
BODY and TOKEN are as for `supersonic-music-assistant--retrieve'."
  (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-music-assistant--retrieve url body token))))
    (unwind-protect
        (with-current-buffer buffer
          (when url-http-end-of-headers
            (supersonic-music-assistant--note-server-clock))
          (supersonic-music-assistant--read-response url status))
      (kill-buffer buffer))))

(aio-defun supersonic-music-assistant--check-server (server)
  "Return a promise rejecting if SERVER is not a recent enough Music Assistant.
SERVER is checked once."
  (unless (equal server supersonic-music-assistant--checked-url)
    (let* ((info (aio-await (supersonic-music-assistant--fetch (supersonic-music-assistant--url server "/info"))))
           (schema (and (listp info) (assoc-default "schema_version" info))))
      (unless (numberp schema)
        (user-error "%s does not look like a Music Assistant server: its /info names no schema version" server))
      (when (< schema supersonic-music-assistant--min-schema)
        (user-error "The Music Assistant server at %s is too old (schema %s); token authentication needs schema %d or later"
                    server schema supersonic-music-assistant--min-schema))
      (setq supersonic-music-assistant--checked-url server))))

(aio-defun supersonic-music-assistant--send (connection command &optional args)
  "Return a promise resolving to the result of COMMAND with ARGS.
CONNECTION is from `supersonic-music-assistant--connection'.  ARGS is
an alist of string keys."
  (pcase-let ((`(,server . ,token) connection))
    (aio-await (supersonic-music-assistant--check-server server))
    (unless token
      (user-error "No Music Assistant token for %s: add an authinfo entry with it as the machine, or set `supersonic-music-assistant-token-function'" server))
    (aio-await
     (supersonic-music-assistant--fetch
      (supersonic-music-assistant--url server "/api")
      (json-encode `(("command" . ,command) ("args" . ,(or args (make-hash-table)))))
      token))))

(aio-defun supersonic-music-assistant--command (command &optional args)
  "Return a promise resolving to the result of COMMAND with ARGS."
  (aio-await (supersonic-music-assistant--send (supersonic-music-assistant--connection) command args)))

(aio-defun supersonic-music-assistant--all-items (command &optional args connection)
  "Return a promise resolving to all items COMMAND lists, page by page.
ARGS are further arguments.  CONNECTION defaults to a new one."
  (let ((connection (or connection (supersonic-music-assistant--connection)))
        (offset 0)
        (items nil)
        (done nil))
    (while (not done)
      (let ((page (aio-await
                   (supersonic-music-assistant--send
                    connection command
                    `(("limit" . ,supersonic-music-assistant--page-size)
                      ("offset" . ,offset)
                      ,@args)))))
        (setq items (nconc items page))
        (setq offset (+ offset (length page)))
        (setq done (< (length page) supersonic-music-assistant--page-size))))
    items))

(defun supersonic-music-assistant--split-uri (uri)
  "Return (PROVIDER . ITEM-ID) of the Music Assistant item URI."
  (let* ((separator (string-search "://" uri))
         (slash (and separator (string-search "/" uri (+ separator 3)))))
    (unless (and separator (> separator 0) slash (> slash (+ separator 3)) (< (1+ slash) (length uri)))
      (error "Not a Music Assistant item URI: %s" uri))
    (cons (substring uri 0 separator) (substring uri (1+ slash)))))

(defun supersonic-music-assistant--item-args (uri)
  "Return the arguments naming the item URI."
  (pcase-let ((`(,provider . ,item-id) (supersonic-music-assistant--split-uri uri)))
    `(("item_id" . ,item-id) ("provider_instance_id_or_domain" . ,provider))))

;;;
;;; MA JSON -> facade plists
;;;

(defun supersonic-music-assistant--plist (&rest keys-and-values)
  "Return the plist KEYS-AND-VALUES without the keys whose value is nil."
  (let (plist)
    (while keys-and-values
      (let ((key (pop keys-and-values))
            (value (pop keys-and-values)))
        (when value
          (setq plist (nconc plist (list key value))))))
    plist))

(defun supersonic-music-assistant--positive-integer (value)
  "Return VALUE rounded if it is a positive number, else nil."
  (and (numberp value) (> value 0) (round value)))

(defun supersonic-music-assistant--artist-names (data)
  "Return the artist names of item DATA, separated by commas, or nil."
  (let ((names (delq nil (mapcar (lambda (artist) (assoc-default "name" artist)) (assoc-default "artists" data)))))
    (and names (string-join names ", "))))

(defun supersonic-music-assistant--image (data type)
  "Return item DATA's image of TYPE, e.g. \"thumb\", or nil.
Fall back to the album's and then the artists' images, and for a
thumb to a landscape image."
  (and (consp data)
       (or (let ((image (assoc-default "image" data)))
             (and (consp image) (equal (assoc-default "type" image) type) image))
           (seq-find (lambda (image) (equal (assoc-default "type" image) type))
                     (assoc-default "images" (assoc-default "metadata" data)))
           (supersonic-music-assistant--image (assoc-default "album" data) type)
           (seq-some (lambda (artist) (supersonic-music-assistant--image artist type))
                     (assoc-default "artists" data))
           (and (equal type "thumb")
                (supersonic-music-assistant--image data "landscape")))))

(defun supersonic-music-assistant--art (data)
  "Return the `:art' reference of item DATA, or nil.
That is the image's path on the server's image proxy, or the URL of a
public image."
  (let* ((image (supersonic-music-assistant--image data "thumb"))
         (path (assoc-default "path" image))
         (proxy-id (assoc-default "proxy_id" image)))
    (cond
     ((and (stringp proxy-id) (not (string-empty-p proxy-id)))
      (concat "/imageproxy/" (url-hexify-string proxy-id)))
     ((and (assoc-default "remotely_accessible" image)
           (stringp path)
           (string-match-p "\\`https?://" path))
      path))))

(defun supersonic-music-assistant--artist (data)
  "Return the artist plist for the Music Assistant artist DATA."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :name (assoc-default "name" data)
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--album (data)
  "Return the album plist for the Music Assistant album DATA."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :name (assoc-default "name" data)
   :artist (supersonic-music-assistant--artist-names data)
   :year (supersonic-music-assistant--positive-integer (assoc-default "year" data))
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--track (data)
  "Return the track plist for the Music Assistant track DATA."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :title (assoc-default "name" data)
   :artist (supersonic-music-assistant--artist-names data)
   :album (assoc-default "name" (assoc-default "album" data))
   :duration (supersonic-music-assistant--positive-integer (assoc-default "duration" data))
   :track (supersonic-music-assistant--positive-integer (assoc-default "track_number" data))
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--podcast (data)
  "Return the podcast plist for the Music Assistant podcast DATA."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :title (assoc-default "name" data)
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--episode-status (data)
  "Return \"played\", \"started\" or nil for the podcast episode DATA."
  (let ((position (assoc-default "resume_position_ms" data)))
    (cond
     ((assoc-default "fully_played" data) "played")
     ((and (numberp position) (> position 0)) "started"))))

(defun supersonic-music-assistant--episode (data)
  "Return the episode plist for the Music Assistant episode DATA."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :title (assoc-default "name" data)
   :duration (supersonic-music-assistant--positive-integer (assoc-default "duration" data))
   :status (supersonic-music-assistant--episode-status data)))

;;;
;;; Operations
;;;

(aio-defun supersonic-music-assistant--artists ()
  "Return a promise resolving to the album artists in the library."
  (mapcar #'supersonic-music-assistant--artist
          (aio-await
           (supersonic-music-assistant--all-items
            "music/artists/library_items"
            '(("order_by" . "sort_name") ("album_artists_only" . t))))))

(aio-defun supersonic-music-assistant--artist-albums (uri)
  "Return a promise resolving to the library albums of the artist URI."
  (mapcar #'supersonic-music-assistant--album
          (aio-await
           (supersonic-music-assistant--command
            "music/artists/artist_albums"
            ;; Without it, older servers add albums from streaming providers.
            `(,@(supersonic-music-assistant--item-args uri) ("in_library_only" . t))))))

(aio-defun supersonic-music-assistant--album-list (type count)
  "Return a promise resolving to at most COUNT albums of list TYPE."
  (let ((args (alist-get type supersonic-music-assistant--album-list-args)))
    (mapcar #'supersonic-music-assistant--album
            (aio-await
             (cond
              ((eq type 'recent) (supersonic-music-assistant--recent-albums count))
              (args (supersonic-music-assistant--command
                     "music/albums/library_items" `(("limit" . ,count) ,@args)))
              (t (user-error "Music Assistant has no `%s' album list" type)))))))

(aio-defun supersonic-music-assistant--recent-albums (count)
  "Return a promise resolving to the albums of the tracks last played.
At most COUNT albums, most recent first.  The history has tracks only,
so each is looked up.  Signal the first error if all lookups fail."
  (let ((connection (supersonic-music-assistant--connection))
        (limit (* supersonic-music-assistant--recent-history-start count))
        (max-limit (* supersonic-music-assistant--recent-history-factor count))
        (skip 0)
        (albums nil)
        (any-resolved nil)
        (failure nil)
        (done (< count 1)))
    (while (not done)
      (let* ((history (aio-await
                       (supersonic-music-assistant--send
                        connection "music/recently_played_items"
                        `(("limit" . ,limit) ("media_types" . ["track"])))))
             (tracks (seq-filter
                      (lambda (item)
                        (and (equal (assoc-default "media_type" item) "track")
                             (stringp (assoc-default "uri" item))))
                      (nthcdr skip history))))
        (while (and tracks (< (length albums) count))
          (let ((lookups
                 (mapcar (lambda (track)
                           (aio-catch
                            (supersonic-music-assistant--send
                             connection "music/item_by_uri"
                             `(("uri" . ,(assoc-default "uri" track))))))
                         (seq-take tracks supersonic-music-assistant--recent-lookups))))
            (setq tracks (nthcdr supersonic-music-assistant--recent-lookups tracks))
            (dolist (lookup lookups)
              (pcase (aio-await lookup)
                (`(:error . ,err) (unless failure (setq failure err)))
                (`(:success . ,track)
                 (setq any-resolved t)
                 (let* ((album (and (consp track) (assoc-default "album" track)))
                        (uri (and (consp album) (assoc-default "uri" album))))
                   (when (and (stringp uri)
                              (< (length albums) count)
                              (not (seq-find (lambda (seen) (equal uri (assoc-default "uri" seen)))
                                             albums)))
                     (push album albums))))))))
        (setq skip (length history))
        (setq done (or (>= (length albums) count)
                       (< (length history) limit)
                       (>= limit max-limit)))
        (setq limit (min (* 2 limit) max-limit))))
    (when (and failure (not any-resolved))
      (signal (car failure) (cdr failure)))
    (nreverse albums)))

(aio-defun supersonic-music-assistant--album-tracks (uri)
  "Return a promise resolving to the tracks of the album URI."
  (mapcar #'supersonic-music-assistant--track
          (aio-await
           ;; Not `in_library_only': a library album from a streaming
           ;; provider usually has no library tracks.
           (supersonic-music-assistant--command
            "music/albums/album_tracks" (supersonic-music-assistant--item-args uri)))))

(aio-defun supersonic-music-assistant--track-by-uri (uri)
  "Return a promise resolving to the track URI."
  (supersonic-music-assistant--track
   (aio-await (supersonic-music-assistant--command "music/item_by_uri" `(("uri" . ,uri))))))

(aio-defun supersonic-music-assistant--search (query)
  "Return a promise resolving to the library items matching QUERY."
  (let ((results (aio-await
                  (supersonic-music-assistant--command
                   "music/search"
                   `(("search_query" . ,query)
                     ("media_types" . ["artist" "album" "track"])
                     ;; Not `providers', which schema 28 doesn't know.
                     ("library_only" . t))))))
    (list
     :artists (mapcar #'supersonic-music-assistant--artist (assoc-default "artists" results))
     :albums (mapcar #'supersonic-music-assistant--album (assoc-default "albums" results))
     :tracks (mapcar #'supersonic-music-assistant--track (assoc-default "tracks" results)))))

(aio-defun supersonic-music-assistant--podcasts ()
  "Return a promise resolving to the podcasts in the library."
  (mapcar #'supersonic-music-assistant--podcast
          (aio-await
           (supersonic-music-assistant--all-items
            "music/podcasts/library_items" '(("order_by" . "sort_name"))))))

(aio-defun supersonic-music-assistant--podcast-episodes (uri)
  "Return a promise resolving to the episodes of the podcast URI."
  (mapcar #'supersonic-music-assistant--episode
          (aio-await
           (supersonic-music-assistant--command
            "music/podcasts/podcast_episodes" (supersonic-music-assistant--item-args uri)))))

(defun supersonic-music-assistant--proxy-size (size)
  "Return the smallest image proxy size not below SIZE, or 0 for the original."
  (or (seq-find (lambda (proxy-size) (>= proxy-size size)) supersonic-music-assistant--proxy-sizes)
      0))

(defun supersonic-music-assistant--cover-art-request-url (art size)
  "Return the URL of the image ART at SIZE."
  (if (string-prefix-p "/" art)
      (format "%s?size=%d"
              (supersonic-music-assistant--url (supersonic-music-assistant--server) art)
              (supersonic-music-assistant--proxy-size size))
    art))

(defun supersonic-music-assistant--retrieve-image (url)
  "Return a promise of (STATUS . BUFFER) for the image at URL.
Never prompt for credentials."
  ;; Not prompting: images may come from any host.
  (let ((url-request-noninteractive t))
    (supersonic-url-retrieve url)))

(aio-defun supersonic-music-assistant--cover-art (art size)
  "Return a promise resolving to the bytes of the image ART at SIZE."
  ;; No token: the image proxy and public URLs need none.
  (let ((url (supersonic-music-assistant--cover-art-request-url art size)))
    (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-music-assistant--retrieve-image url))))
      (unwind-protect
          (let ((err (plist-get status :error)))
            (when err
              (error "Failed to fetch cover art from %s: %s" url (supersonic-music-assistant--describe-failure err)))
            (with-current-buffer buffer
              (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max))))
        (kill-buffer buffer)))))

(defun supersonic-music-assistant--cache-namespace ()
  "Return `supersonic-music-assistant-url', as ids are only unique per server."
  supersonic-music-assistant-url)

(defun supersonic-music-assistant--config-hints ()
  "Return what to check when a request to the Music Assistant server fails."
  (list "Check that supersonic-music-assistant-url points at your server, e.g. http://host:8095"
        (format "Verify .authinfo has an entry with host %s (exactly) and a token from Music Assistant's web UI as its password, or what supersonic-music-assistant-token-function returns"
                (if (string-empty-p supersonic-music-assistant-url)
                    "<supersonic-music-assistant-url>"
                  supersonic-music-assistant-url))
        (format "The server must have schema version %d or later (see its /info)"
                supersonic-music-assistant--min-schema)))

(supersonic-provider-register
 'music-assistant
 '((artists . supersonic-music-assistant--artists)
   (artist-albums . supersonic-music-assistant--artist-albums)
   (album-list . supersonic-music-assistant--album-list)
   (album-tracks . supersonic-music-assistant--album-tracks)
   (track . supersonic-music-assistant--track-by-uri)
   (search . supersonic-music-assistant--search)
   (podcasts . supersonic-music-assistant--podcasts)
   (podcast-episodes . supersonic-music-assistant--podcast-episodes)
   (cover-art . supersonic-music-assistant--cover-art)
   (cache-namespace . supersonic-music-assistant--cache-namespace)
   (config-hints . supersonic-music-assistant--config-hints)))

(provide 'supersonic-music-assistant)
;;; supersonic-music-assistant.el ends here
