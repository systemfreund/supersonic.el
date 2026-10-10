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
;; Images (#59).  An item's `:art' is the image's path under the
;; server's `/imageproxy', even for an image anyone could fetch, since
;; the proxy scales it on the server -- if only to a few fixed sizes.
;; Only a server too old to hand out proxy ids gets a public image's
;; URL fetched as it is.  The proxy asks for no token, so none is
;; sent: cover art never involves auth-source.
;;
;; Podcasts come from whatever podcast providers the server has; their
;; episodes are not in MA's library, so the server fetches them anew
;; for every listing.  MA subscribes to a feed by setting up a
;; provider for it, and has no notion of downloading an episode, so
;; `add-podcast' and `download-podcast-episode' are left out.
;;
;; MA plays on its own players and keeps its own play history, so
;; `stream-url' and `scrobble' are deliberately left out: there is no
;; URL a local player could stream a whole track from, and nothing to
;; report.  That alone keeps mpv and the UPnP backend away from this
;; provider.

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

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defconst supersonic-music-assistant--min-schema 28
  "The oldest server schema version this provider can talk to.
Token authentication, the only kind this provider does, came with
schema 28.")

(defconst supersonic-music-assistant--page-size 500
  "How many items to ask for per request when fetching a whole list.")

(defconst supersonic-music-assistant--proxy-sizes '(80 160 256 512 1024)
  "The sizes MA's image proxy scales to, smallest first.
It answers any other size with 400; 0, the image as it is, it also
takes.")

(defconst supersonic-music-assistant--album-list-args
  '((recent ("order_by" . "last_played_desc") ("played_only" . t))
    (random ("order_by" . "random"))
    (newest ("order_by" . "timestamp_added_desc")))
  "Map of facade album list type to the args producing it.
The args are for `music/albums/library_items', on top of a limit.
`recent' asks it too, rather than `music/recently_played_items': that
answers with slim item mappings, without the artists the album list
shows, where `library_items' gives the same full albums as every other
list.  A server too old to know `played_only' ignores it, and lists
the albums never played after the ones played.")

(defvar supersonic-music-assistant--checked-url nil
  "The server address whose schema version was last found recent enough.
Set by `supersonic-music-assistant--check-server', so that `/info' is
asked once per server rather than before every request.")

;;;
;;; Transport
;;;

(defun supersonic-music-assistant--server ()
  "Return `supersonic-music-assistant-url', signalling unless it is usable.
It must name its scheme: it is matched verbatim against the authinfo
host, and `url.el' cannot reach an address without one.  Read once
per operation, by `supersonic-music-assistant--connection', so that
the server checked, the token looked up and the server asked agree."
  (let ((server supersonic-music-assistant-url))
    (unless (and (stringp server) (string-match-p "\\`https?://[^/]" server))
      (user-error "Set `supersonic-music-assistant-url' to your Music Assistant server, scheme included, e.g. http://host:8095"))
    server))

(defun supersonic-music-assistant--url (server path)
  "Return the URL of PATH on SERVER.
SERVER is as returned by `supersonic-music-assistant--server'."
  (concat (string-remove-suffix "/" server) path))

(defun supersonic-music-assistant--token (server)
  "Return the token auth-source has for SERVER, or nil if it has none.
SERVER is as returned by `supersonic-music-assistant--server', which
also guarantees it is never nil or empty: a lookup for no host would
match any entry, a Subsonic password included.  Looked up afresh for
every operation, as `supersonic-auth' is for every request, so that a
corrected entry takes effect once auth-source's cache is forgotten."
  (let ((secret (plist-get (car (auth-source-search :host server :require '(:secret))) :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun supersonic-music-assistant--connection ()
  "Return what an operation sends its commands with, as (SERVER . TOKEN).
TOKEN is nil when auth-source has none for SERVER; that is reported
only once SERVER has passed `supersonic-music-assistant--check-server',
so that a server too old to take a token at all says so first.

For an operation to call before its first `aio-await', while the
command that asked is still running, as Subsonic does: anything
auth-source asks of the user -- a GnuPG passphrase, unlocking a
Secret Service collection -- is then asked by that command rather
than later from a timer, and an operation sending several commands
looks the token up only once."
  (let ((server (supersonic-music-assistant--server)))
    (cons server (supersonic-music-assistant--token server))))

(defun supersonic-music-assistant--retrieve (url &optional body token)
  "Start a request for URL, returning a promise of (STATUS . BUFFER).
A POST of BODY, a JSON string, if BODY is non-nil, a GET otherwise.
TOKEN, if non-nil, is sent as a bearer token.  A plain function rather
than an `aio-defun', so that the `url-request-*' bindings are certain
to be in effect while `url-retrieve' reads them.

Redirects are not followed where `url.el' takes `url-max-redirections'
per request: it drops the token from a redirected request, and turns
a redirected POST into a GET, so following one could only fail, and
be reported as a rejected token.  Emacs 28 reads the variable only
once the answer is in, and follows anyway; either way,
`supersonic-music-assistant--redirect-target' tells."
  (let ((url-max-redirections 0)
        (url-request-method (if body "POST" "GET"))
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

(defun supersonic-music-assistant--redirect-target (status)
  "Return where the request behind STATUS was redirected to, or nil.
`url.el' records a redirect it followed as `:redirect' in STATUS, and
one it would not follow as an `http-redirect-limit' error."
  (or (plist-get status :redirect)
      (pcase (plist-get status :error)
        (`(error http-redirect-limit ,target) target))))

(defun supersonic-music-assistant--read-response (url status)
  "Return the parsed JSON body of the response to URL in the current buffer.
STATUS is what `url-retrieve' handed its callback.  Signals an error
for a request that got no answer at all, or an answer other than
success -- a `user-error' for a redirect or a rejected token, since
those are the user's to fix."
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
      (user-error "Music Assistant at %s rejected the token from auth-source" url))
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
 supersonic-music-assistant--check-server (server)
 "Return a promise resolving once SERVER is known to be recent enough.
Asks SERVER's `/info' unless it was already found good, and rejects
with a `user-error' for a server older than
`supersonic-music-assistant--min-schema', or for one whose `/info'
names no schema version at all, which is not Music Assistant."
 (unless (equal server supersonic-music-assistant--checked-url)
   (let* ((info (aio-await (supersonic-music-assistant--fetch (supersonic-music-assistant--url server "/info"))))
          (schema (and (listp info) (assoc-default "schema_version" info))))
     (unless (numberp schema)
       (user-error "%s does not look like a Music Assistant server: its /info names no schema version" server))
     (when (< schema supersonic-music-assistant--min-schema)
       (user-error "The Music Assistant server at %s is too old (schema %s); token authentication needs schema %d or later"
                   server schema supersonic-music-assistant--min-schema))
     (setq supersonic-music-assistant--checked-url server))))

(aio-defun
 supersonic-music-assistant--send (connection command &optional args)
 "Return a promise resolving to the result of running COMMAND with ARGS.
CONNECTION is as returned by `supersonic-music-assistant--connection'.
COMMAND is an MA API command such as \"music/search\"; ARGS is an alist
of its arguments, keyed by strings, with t and `:json-false' for the
booleans and vectors for the arrays."
 (pcase-let ((`(,server . ,token) connection))
   (aio-await (supersonic-music-assistant--check-server server))
   (unless token
     (user-error "No Music Assistant token in auth-source for host %s" server))
   (aio-await
    (supersonic-music-assistant--fetch
     (supersonic-music-assistant--url server "/api")
     (json-encode `(("command" . ,command) ("args" . ,(or args (make-hash-table)))))
     token))))

(aio-defun
 supersonic-music-assistant--command (command &optional args)
 "Return a promise resolving to the result of running COMMAND with ARGS.
As `supersonic-music-assistant--send', for an operation that sends
only the one command, over a connection settled as it is called."
 (aio-await (supersonic-music-assistant--send (supersonic-music-assistant--connection) command args)))

(aio-defun
 supersonic-music-assistant--all-items (command &optional args)
 "Return a promise resolving to every item a `library_items' COMMAND lists.
Asks with ARGS a page at a time, over a connection settled as it is
called, until a page comes back short."
 (let ((connection (supersonic-music-assistant--connection))
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
  "Return the provider and item id of MA item URI, as (PROVIDER . ITEM-ID).
Splits the way MA's own `parse_uri' does: the provider is what comes
before \"://\", the item id everything after the media type that
follows -- which may itself contain slashes, as a file path does."
  (let* ((separator (string-search "://" uri))
         (slash (and separator (string-search "/" uri (+ separator 3)))))
    (unless (and separator (> separator 0) slash (> slash (+ separator 3)) (< (1+ slash) (length uri)))
      (error "Not a Music Assistant item URI: %s" uri))
    (cons (substring uri 0 separator) (substring uri (1+ slash)))))

(defun supersonic-music-assistant--library-item-args (uri)
  "Return the args naming the item with URI, for commands that take no URI.
They also ask for what is in the library only, as everything else this
provider lists is: without `in_library_only', MA adds what every
linked streaming provider has for a library album, and on older
servers -- schema 28, for one -- for a library artist too.  A server
that does not know the argument ignores it, as MA does any argument
it does not know."
  (pcase-let ((`(,provider . ,item-id) (supersonic-music-assistant--split-uri uri)))
    `(("item_id" . ,item-id) ("provider_instance_id_or_domain" . ,provider) ("in_library_only" . t))))

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

(defun supersonic-music-assistant--positive-integer (value)
  "Return VALUE rounded, if it is a positive number, and nil otherwise.
MA reports an unknown year, duration or track number as null or zero."
  (and (numberp value) (> value 0) (round value)))

(defun supersonic-music-assistant--artist-names (data)
  "Return the names of the artists of MA item DATA joined by commas, or nil."
  (let ((names (delq nil (mapcar (lambda (artist) (assoc-default "name" artist)) (assoc-default "artists" data)))))
    (and names (string-join names ", "))))

(defun supersonic-music-assistant--image (data type)
  "Return the image of TYPE, e.g. \"thumb\", that MA item DATA shows, or nil.
Looks at the item's own image first -- the single one an item mapping
carries, then those in its metadata -- and only for an item without
one at its album's image, then at its artists' images.  So a track
with art of its own shows that, where MA's own client, in its
`get_media_item_image', always prefers the album's.  A thumb not found
there is made up for by a landscape image, looked for the same way --
at every level, as the client does, so that an album with only a
landscape image still wins over its artist's thumb."
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
  "Return the `:art' reference of MA item DATA, or nil if it shows no image.
The reference is the image's path on the server, under its image
proxy, which knows an image by the `proxy_id' the server hands out
with it from schema 31 on.  That holds for an image anyone could fetch
too -- a podcast's cover from its feed, say -- since the proxy scales
it, where the feed's own may be thousands of pixels wide.  Only an
image without a proxy id, from an older server, is referred to by its
URL, if it has one anyone can fetch, which
`supersonic-music-assistant--cover-art' then fetches as it is; any
other is left without art."
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
  "Turn an MA artist alist DATA into a facade artist plist."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :name (assoc-default "name" data)
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--album (data)
  "Turn an MA album alist DATA into a facade album plist."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :name (assoc-default "name" data)
   :artist (supersonic-music-assistant--artist-names data)
   :year (supersonic-music-assistant--positive-integer (assoc-default "year" data))
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--track (data)
  "Turn an MA track alist DATA into a facade track plist."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :title (assoc-default "name" data)
   :artist (supersonic-music-assistant--artist-names data)
   :album (assoc-default "name" (assoc-default "album" data))
   :duration (supersonic-music-assistant--positive-integer (assoc-default "duration" data))
   :track (supersonic-music-assistant--positive-integer (assoc-default "track_number" data))
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--podcast (data)
  "Turn an MA podcast alist DATA into a facade podcast plist."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :title (assoc-default "name" data)
   :art (supersonic-music-assistant--art data)))

(defun supersonic-music-assistant--episode-status (data)
  "Return how far MA podcast episode DATA has been listened to, or nil.
\"played\" for an episode played to the end, \"started\" for one with
a resume point, nil for one never started -- or whose provider does
not say."
  (let ((position (assoc-default "resume_position_ms" data)))
    (cond
     ((assoc-default "fully_played" data) "played")
     ((and (numberp position) (> position 0)) "started"))))

(defun supersonic-music-assistant--episode (data)
  "Turn an MA podcast episode alist DATA into a facade episode plist.
Its `:id' is the episode's URI, which MA's player queues play as they
do a track's."
  (supersonic-music-assistant--plist
   :id (assoc-default "uri" data)
   :title (assoc-default "name" data)
   :duration (supersonic-music-assistant--positive-integer (assoc-default "duration" data))
   :status (supersonic-music-assistant--episode-status data)))

;;;
;;; Operations
;;;

(aio-defun
 supersonic-music-assistant--artists ()
 "Return a promise resolving to every album artist in the library.
Asks `music/artists/library_items' a page at a time until a page comes
back short.  Artists that only appear on tracks are left out, as
Subsonic's getArtists leaves them out: they have no albums to open."
 (mapcar #'supersonic-music-assistant--artist
         (aio-await
          (supersonic-music-assistant--all-items
           "music/artists/library_items"
           '(("order_by" . "sort_name") ("album_artists_only" . t))))))

(aio-defun
 supersonic-music-assistant--artist-albums (uri)
 "Return a promise resolving to the albums of the artist with URI."
 (mapcar #'supersonic-music-assistant--album
         (aio-await
          (supersonic-music-assistant--command
           "music/artists/artist_albums" (supersonic-music-assistant--library-item-args uri)))))

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
           "music/albums/album_tracks" (supersonic-music-assistant--library-item-args uri)))))

(aio-defun
 supersonic-music-assistant--track-by-uri (uri)
 "Return a promise resolving to the track with URI, via `music/item_by_uri'."
 (supersonic-music-assistant--track
  (aio-await (supersonic-music-assistant--command "music/item_by_uri" `(("uri" . ,uri))))))

(aio-defun
 supersonic-music-assistant--search (query)
 "Return a promise resolving to the artists, albums and tracks matching QUERY.
Searches the library only, as everything else this provider lists
comes from the library.  Asks with `library_only', which newer servers
still honour, rather than `providers', which schema 28 does not know."
 (let ((results (aio-await
                 (supersonic-music-assistant--command
                  "music/search"
                  `(("search_query" . ,query)
                    ("media_types" . ["artist" "album" "track"])
                    ("library_only" . t))))))
   (list
    :artists (mapcar #'supersonic-music-assistant--artist (assoc-default "artists" results))
    :albums (mapcar #'supersonic-music-assistant--album (assoc-default "albums" results))
    :tracks (mapcar #'supersonic-music-assistant--track (assoc-default "tracks" results)))))

(aio-defun
 supersonic-music-assistant--podcasts ()
 "Return a promise resolving to every podcast in the library.
Asks `music/podcasts/library_items' a page at a time."
 (mapcar #'supersonic-music-assistant--podcast
         (aio-await
          (supersonic-music-assistant--all-items
           "music/podcasts/library_items" '(("order_by" . "sort_name"))))))

(aio-defun
 supersonic-music-assistant--podcast-episodes (uri)
 "Return a promise resolving to the episodes of the podcast with URI.
MA keeps no episodes in its library, but fetches them from the
podcast's provider every time, so this asks without `in_library_only'."
 (pcase-let ((`(,provider . ,item-id) (supersonic-music-assistant--split-uri uri)))
   (mapcar #'supersonic-music-assistant--episode
           (aio-await
            (supersonic-music-assistant--command
             "music/podcasts/podcast_episodes"
             `(("item_id" . ,item-id) ("provider_instance_id_or_domain" . ,provider)))))))

(defun supersonic-music-assistant--proxy-size (size)
  "Return the size to ask MA's image proxy for to show an image at SIZE.
The smallest size it scales to that is not smaller than SIZE, so that
Emacs only ever scales down; 0, the image as it is, past the largest."
  (or (seq-find (lambda (proxy-size) (>= proxy-size size)) supersonic-music-assistant--proxy-sizes)
      0))

(defun supersonic-music-assistant--cover-art-request-url (art size)
  "Return the URL to fetch the image with `:art' reference ART from, at SIZE.
A path under the image proxy gets the server's address and a size the
proxy scales to; a URL anyone can fetch is fetched as it is, unscaled,
since only the proxy scales."
  (if (string-prefix-p "/" art)
      (format "%s?size=%d"
              (supersonic-music-assistant--url (supersonic-music-assistant--server) art)
              (supersonic-music-assistant--proxy-size size))
    art))

(defun supersonic-music-assistant--retrieve-image (url)
  "Start a GET of the image at URL, returning a promise of (STATUS . BUFFER).
Marked noninteractive, so that a server answering 401 -- a public
image's host is whatever a feed points at -- makes `url.el' fail the
request rather than prompt for a user name and password, from a timer
in the middle of painting a list.  `url.el' still asks auth-source for
that host first, as it does for any 401; only the prompts are gone.
A plain function, as `supersonic-music-assistant--retrieve' is, so
that the binding is certain to be in effect while `url-retrieve'
reads it."
  (let ((url-request-noninteractive t))
    (supersonic-url-retrieve url)))

(aio-defun
 supersonic-music-assistant--cover-art (art size)
 "Return a promise resolving to the bytes of the image ART refers to, at SIZE.
ART is an `:art' reference, as made by `supersonic-music-assistant--art'.
No token goes with the request: MA's image proxy serves anyone, as it
must for the players it points at images, and a public URL needs none."
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
  "Return the server's address, `supersonic-music-assistant-url'.
A `library://' URI is only unique per server, so this is what keeps
one server's cached art apart from another's."
  supersonic-music-assistant-url)

(defun supersonic-music-assistant--config-hints ()
  "Return what to check when a request to the Music Assistant server fails."
  (list "Check that supersonic-music-assistant-url points at your server, e.g. http://host:8095"
        (format "Verify .authinfo has an entry with host %s (exactly) and a token from Music Assistant's web UI as its password"
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
