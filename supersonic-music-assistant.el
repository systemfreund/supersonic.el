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

;; The Music Assistant library provider (#58): implements the browse
;; operations of the facade in `supersonic-provider.el' against a
;; Music Assistant (MA) server, and registers them under the name
;; `music-assistant'.  Like `supersonic-jukebox.el' it is opt-in:
;;
;;   (require 'supersonic-music-assistant)
;;   (setq supersonic-provider 'music-assistant)
;;
;; Transport.  MA offers a WebSocket API, which pushes events, and a
;; plain JSON-RPC endpoint at `/api': a POST of
;; {"command": ..., "args": {...}} answered with the command's result.
;; This file uses `/api' through `url.el', as the Subsonic code does,
;; so there is no dependency on a WebSocket client; the price is that
;; a playback backend has to poll for player state instead of being
;; told.  Failures come back as an HTTP status with a plain-text body:
;; 401 for a missing or rejected token, 400 for an unknown command,
;; 500 for anything else -- an id the server does not know included.
;;
;; Authentication is a long-lived token created in MA's web UI and
;; sent as `Authorization: Bearer'.  It is read from auth-source,
;; matched on `supersonic-music-assistant-url' exactly as Subsonic
;; credentials are matched on `supersonic-host', and looked up afresh
;; for every request, so it never sits in a variable, a URL or a
;; subprocess's argv.  Token authentication needs server schema 28 or
;; later, so the server's unauthenticated `/info' is asked once per
;; server address before anything else.
;;
;; Ids are MA's own item URIs -- `library://album/8',
;; `spotify://track/...' -- handed out verbatim.  Commands that want a
;; provider and an item id instead of a URI get them by splitting the
;; URI the way MA's own `parse_uri' does: this provider reading its
;; own ids, which the facade leaves to it.
;;
;; MA plays on its own players and keeps its own play history, so
;; `stream-url' and `scrobble' are deliberately left out: there is no
;; URL a local player could stream a whole track from, and nothing to
;; report.  That alone keeps mpv and the UPnP backend away from this
;; provider.

;;; Code:

(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'aio)

(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-provider)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defconst supersonic-music-assistant-min-schema 28
  "The oldest server schema version this provider can talk to.
Token authentication, the only kind this provider does, came with
schema 28.")

(defconst supersonic-music-assistant--page-size 500
  "How many items to ask for per request when fetching a whole list.")

(defconst supersonic-music-assistant--album-list-args
  '((recent ("order_by" . "last_played_desc") ("played_only" . t))
    (random ("order_by" . "random"))
    (newest ("order_by" . "timestamp_added_desc")))
  "Map of facade album list type to the args producing it.
The args are for `music/albums/library_items', on top of a limit.")

(defvar supersonic-music-assistant--checked-url nil
  "The server address whose schema version was last found recent enough.
Set by `supersonic-music-assistant--check-server', so that `/info' is
asked once per server rather than before every request.")

;;;
;;; Transport
;;;

(defun supersonic-music-assistant--url (path)
  "Return the URL of PATH on the server at `supersonic-music-assistant-url'."
  (when (string-empty-p (or supersonic-music-assistant-url ""))
    (user-error "Set `supersonic-music-assistant-url' to your Music Assistant server, e.g. http://host:8095"))
  (concat (string-remove-suffix "/" supersonic-music-assistant-url) path))

(defun supersonic-music-assistant--token ()
  "Return the token auth-source has for `supersonic-music-assistant-url'.
Looked up afresh on every call, as `supersonic-auth' does, so that a
corrected entry takes effect once auth-source's cache is forgotten."
  (let ((secret (plist-get (car (auth-source-search :host supersonic-music-assistant-url :require '(:secret)))
                           :secret)))
    (unless secret
      (user-error "No Music Assistant token in auth-source for host %s" supersonic-music-assistant-url))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun supersonic-music-assistant--retrieve (url &optional body token)
  "Start a request for URL, returning a promise of (STATUS . BUFFER).
A POST of BODY, a JSON string, if BODY is non-nil, a GET otherwise.
TOKEN, if non-nil, is sent as a bearer token.  A plain function rather
than an `aio-defun', so that the `url-request-*' bindings are certain
to be in effect while `url-retrieve' reads them."
  (let ((url-request-method (if body "POST" "GET"))
        (url-request-extra-headers
         (append (and body '(("Content-Type" . "application/json")))
                 (and token `(("Authorization" . ,(concat "Bearer " token))))))
        (url-request-data (and body (encode-coding-string body 'utf-8))))
    (supersonic-url-retrieve url)))

(defun supersonic-music-assistant--describe-failure (err)
  "Return a readable description of ERR, a request's `:error' status.
`url.el' reports a refused connection as (error connection-failed
DETAIL ...), which `error-message-string' renders as a \"peculiar
error\"."
  (pcase err
    (`(error connection-failed ,(and (pred stringp) detail) . ,_)
     (format "connection failed (%s)" (string-trim detail)))
    (_ (error-message-string err))))

(defun supersonic-music-assistant--read-response (url status)
  "Return the parsed JSON body of the response to URL in the current buffer.
STATUS is what `url-retrieve' handed its callback.  Signals an error
for a request that got no answer at all, or an answer other than
success -- a `user-error' for a rejected token, since that is the
user's to fix."
  (unless url-http-end-of-headers
    (error "No answer from %s: %s" url (supersonic-music-assistant--describe-failure (plist-get status :error))))
  (let ((code url-http-response-status)
        (body (string-trim
               (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8))))
    (cond
     ((eql code 401)
      (user-error "Music Assistant rejected the token from auth-source for host %s" supersonic-music-assistant-url))
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

(aio-defun
 supersonic-music-assistant--fetch (url &optional body token)
 "Return a promise resolving to the parsed JSON answer to a request for URL.
BODY and TOKEN are as for `supersonic-music-assistant--retrieve'."
 (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-music-assistant--retrieve url body token))))
   (unwind-protect
       (with-current-buffer buffer
         (supersonic-music-assistant--read-response url status))
     (kill-buffer buffer))))

(aio-defun
 supersonic-music-assistant--check-server ()
 "Return a promise resolving once the server is known to be recent enough.
Asks the server's `/info' unless `supersonic-music-assistant-url' was
already found good, and rejects with a `user-error' for a server older
than `supersonic-music-assistant-min-schema'."
 (let ((url supersonic-music-assistant-url))
   (unless (and url (equal url supersonic-music-assistant--checked-url))
     (let ((schema (assoc-default
                    "schema_version"
                    (aio-await (supersonic-music-assistant--fetch (supersonic-music-assistant--url "/info"))))))
       (unless (and (numberp schema) (>= schema supersonic-music-assistant-min-schema))
         (user-error "The Music Assistant server at %s is too old (schema %s); token authentication needs schema %d or later"
                     url (or schema "unknown") supersonic-music-assistant-min-schema))
       (setq supersonic-music-assistant--checked-url url)))))

(aio-defun
 supersonic-music-assistant--command (command &optional args)
 "Return a promise resolving to the result of running COMMAND with ARGS.
COMMAND is an MA API command such as \"music/search\"; ARGS is an alist
of its arguments, keyed by strings, with t and `:json-false' for the
booleans and vectors for the arrays."
 (aio-await (supersonic-music-assistant--check-server))
 (aio-await
  (supersonic-music-assistant--fetch
   (supersonic-music-assistant--url "/api")
   (json-encode `(("command" . ,command) ("args" . ,(or args (make-hash-table)))))
   (supersonic-music-assistant--token))))

(defun supersonic-music-assistant--split-uri (uri)
  "Return the provider and item id of MA item URI, as (PROVIDER . ITEM-ID).
Splits the way MA's own `parse_uri' does: the provider is what comes
before \"://\", the item id everything after the media type that
follows -- which may itself contain slashes, as a file path does."
  (let* ((separator (string-search "://" uri))
         (slash (and separator (string-search "/" uri (+ separator 3)))))
    (unless (and separator (> separator 0) slash (> slash (+ separator 3)) (< (1+ slash) (length uri)))
      (error "Not a Music Assistant item URI: %s" uri))
    (cons (substring uri 0 separator) (substring uri (1+ slash)))))

(defun supersonic-music-assistant--item-args (uri)
  "Return the args naming the item with URI, for commands that take no URI."
  (pcase-let ((`(,provider . ,item-id) (supersonic-music-assistant--split-uri uri)))
    `(("item_id" . ,item-id) ("provider_instance_id_or_domain" . ,provider))))

;;;
;;; MA JSON -> facade plists
;;;

(defun supersonic-music-assistant--plist (&rest keys-and-values)
  "Return KEYS-AND-VALUES as a plist, leaving out each key whose value is nil.
So that a value the server does not know reads the same as a key it
never heard of, as the facade asks."
  (let (plist)
    (while keys-and-values
      (let ((key (pop keys-and-values))
            (value (pop keys-and-values)))
        (when value
          (setq plist (nconc plist (list key value))))))
    plist))

(defun supersonic-music-assistant--whole (value)
  "Return VALUE rounded, if it is a positive number, and nil otherwise.
MA reports an unknown year, duration or track number as null or zero."
  (and (numberp value) (> value 0) (round value)))

(defun supersonic-music-assistant--artist-names (data)
  "Return the names of the artists of MA item DATA joined by commas, or nil."
  (let ((names (delq nil (mapcar (lambda (artist) (assoc-default "name" artist)) (assoc-default "artists" data)))))
    (and names (string-join names ", "))))

(defun supersonic-music-assistant--artist (data)
  "Turn an MA artist alist DATA into a facade artist plist."
  (supersonic-music-assistant--plist :id (assoc-default "uri" data) :name (assoc-default "name" data)))

(defun supersonic-music-assistant--album (data)
  "Turn an MA album alist DATA into a facade album plist."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :name (assoc-default "name" data)
   :artist (supersonic-music-assistant--artist-names data)
   :year (supersonic-music-assistant--whole (assoc-default "year" data))))

(defun supersonic-music-assistant--track (data)
  "Turn an MA track alist DATA into a facade track plist."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :title (assoc-default "name" data)
   :artist (supersonic-music-assistant--artist-names data)
   :album (assoc-default "name" (assoc-default "album" data))
   :duration (supersonic-music-assistant--whole (assoc-default "duration" data))
   :track (supersonic-music-assistant--whole (assoc-default "track_number" data))))

;;;
;;; Operations
;;;

(aio-defun
 supersonic-music-assistant--artists ()
 "Return a promise resolving to every album artist in the library.
Asks `music/artists/library_items' a page at a time until a page comes
back short.  Artists that only appear on tracks are left out, as
Subsonic's getArtists leaves them out: they have no albums to open."
 (let ((offset 0)
       (artists nil)
       (done nil))
   (while (not done)
     (let ((page (aio-await
                  (supersonic-music-assistant--command
                   "music/artists/library_items"
                   `(("limit" . ,supersonic-music-assistant--page-size)
                     ("offset" . ,offset)
                     ("order_by" . "sort_name")
                     ("album_artists_only" . t))))))
       (setq artists (nconc artists (mapcar #'supersonic-music-assistant--artist page)))
       (setq offset (+ offset (length page)))
       (setq done (< (length page) supersonic-music-assistant--page-size))))
   artists))

(aio-defun
 supersonic-music-assistant--artist-albums (uri)
 "Return a promise resolving to the albums of the artist with URI."
 (mapcar #'supersonic-music-assistant--album
         (aio-await
          (supersonic-music-assistant--command
           "music/artists/artist_albums" (supersonic-music-assistant--item-args uri)))))

(aio-defun
 supersonic-music-assistant--album-list (type count)
 "Return a promise resolving to at most COUNT library albums of list TYPE.
Asks `music/albums/library_items' in the order
`supersonic-music-assistant--album-list-args' maps TYPE to."
 (let ((args (alist-get type supersonic-music-assistant--album-list-args)))
   (unless args
     (user-error "Music Assistant has no `%s' album list" type))
   (mapcar #'supersonic-music-assistant--album
           (aio-await
            (supersonic-music-assistant--command
             "music/albums/library_items" `(("limit" . ,count) ,@args))))))

(aio-defun
 supersonic-music-assistant--album-tracks (uri)
 "Return a promise resolving to the tracks of the album with URI, in album order."
 (mapcar #'supersonic-music-assistant--track
         (aio-await
          (supersonic-music-assistant--command
           "music/albums/album_tracks" (supersonic-music-assistant--item-args uri)))))

(aio-defun
 supersonic-music-assistant--track-by-uri (uri)
 "Return a promise resolving to the track with URI, via `music/item_by_uri'."
 (supersonic-music-assistant--track
  (aio-await (supersonic-music-assistant--command "music/item_by_uri" `(("uri" . ,uri))))))

(aio-defun
 supersonic-music-assistant--search (query)
 "Return a promise resolving to the artists, albums and tracks matching QUERY."
 (let ((results (aio-await
                 (supersonic-music-assistant--command
                  "music/search" `(("search_query" . ,query) ("media_types" . ["artist" "album" "track"]))))))
   (list
    :artists (mapcar #'supersonic-music-assistant--artist (assoc-default "artists" results))
    :albums (mapcar #'supersonic-music-assistant--album (assoc-default "albums" results))
    :tracks (mapcar #'supersonic-music-assistant--track (assoc-default "tracks" results)))))

(defun supersonic-music-assistant--cache-namespace ()
  "Return the server's address, `supersonic-music-assistant-url'.
A `library://' URI is only unique per server, so this is what keeps
one server's cached art apart from another's."
  (or supersonic-music-assistant-url ""))

(defun supersonic-music-assistant--config-hints ()
  "Return what to check when a request to the Music Assistant server fails."
  (list "Check that supersonic-music-assistant-url points at your server, e.g. http://host:8095"
        (format "Verify .authinfo has an entry with host %s (exactly) and a token from Music Assistant's web UI as its password"
                (or supersonic-music-assistant-url "<supersonic-music-assistant-url>"))
        (format "The server must have schema version %d or later (see its /info)"
                supersonic-music-assistant-min-schema)))

(supersonic-provider-register
 'music-assistant
 '((artists . supersonic-music-assistant--artists)
   (artist-albums . supersonic-music-assistant--artist-albums)
   (album-list . supersonic-music-assistant--album-list)
   (album-tracks . supersonic-music-assistant--album-tracks)
   (track . supersonic-music-assistant--track-by-uri)
   (search . supersonic-music-assistant--search)
   (cache-namespace . supersonic-music-assistant--cache-namespace)
   (config-hints . supersonic-music-assistant--config-hints)))

(provide 'supersonic-music-assistant)
;;; supersonic-music-assistant.el ends here
