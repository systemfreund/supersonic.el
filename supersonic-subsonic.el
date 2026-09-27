;;; supersonic-subsonic.el --- Subsonic library provider for supersonic.el -*- lexical-binding: t; -*-

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

;; The Subsonic library provider: implements the operations of the
;; facade in `supersonic-provider.el' against any server speaking the
;; Subsonic REST API, and registers them under the name `subsonic'.
;;
;; Everything Subsonic-shaped about browsing -- and about streaming,
;; scrobbling and cover art, which the backends and caches ask for
;; through the facade too -- stops here.  Requests go
;; out through `supersonic-api.el'; what comes back is turned from
;; Subsonic's JSON into the facade's plist vocabulary before anyone
;; else sees it, so no list buffer ever has to know which endpoint an
;; artist came from or what key a server keeps an album's year under.
;; Subsonic ids are handed out verbatim, as the opaque ids the facade
;; asks for, and `coverArt' becomes `:art'.
;;
;; Which endpoint an album's tracks come from is Subsonic's own
;; business too: `supersonic-browse-by-tags' picks between the ID3 tag
;; view (getAlbum) and the folder view (getMusicDirectory) in here,
;; not in the buffer that shows the result.

;;; Code:

(require 'aio)
(require 'url)

(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-provider)

(defvar url-http-end-of-headers)

;;;
;;; Subsonic JSON -> facade plists
;;;

(defun supersonic-subsonic--plist (data mapping)
  "Build a facade plist from Subsonic alist DATA according to MAPPING.
MAPPING is a list of (KEY . FIELD) pairs: each KEY of the result is
set to DATA's FIELD.  Fields DATA does not have are left out entirely
instead of being set to nil, so a missing value reads the same as a
key the provider never heard of.  `:id' always comes first, taken from
DATA's \"id\" as a string."
  (let ((plist (list :id (supersonic-get-id-as-string data))))
    (dolist (pair mapping)
      (let ((value (assoc-default (cdr pair) data)))
        (when value
          (setq plist (append plist (list (car pair) value))))))
    plist))

(defun supersonic-subsonic--artist (data)
  "Turn a Subsonic artist alist DATA into a facade artist plist."
  (supersonic-subsonic--plist data '((:name . "name") (:art . "coverArt"))))

(defun supersonic-subsonic--album (data)
  "Turn a Subsonic album alist DATA into a facade album plist."
  (supersonic-subsonic--plist
   data '((:name . "name") (:artist . "artist") (:year . "year") (:art . "coverArt"))))

(defun supersonic-subsonic--track (data)
  "Turn a Subsonic song/child alist DATA into a facade track plist.
On top of the facade's own track keys, carries Subsonic's \"genre\"
as `:genre', for a now-playing row of the user's own to read."
  (supersonic-subsonic--plist
   data
   '((:title . "title")
     (:artist . "artist")
     (:album . "album")
     (:duration . "duration")
     (:track . "track")
     (:art . "coverArt")
     (:suffix . "suffix")
     (:content-type . "contentType")
     (:size . "size")
     (:genre . "genre"))))

(defun supersonic-subsonic--podcast (data)
  "Turn a Subsonic podcast channel alist DATA into a facade podcast plist."
  (supersonic-subsonic--plist data '((:title . "title") (:art . "coverArt"))))

(defun supersonic-subsonic--episode (data)
  "Turn a Subsonic podcast episode alist DATA into a facade episode plist."
  (supersonic-subsonic--plist data '((:title . "title") (:duration . "duration") (:status . "status"))))

(defun supersonic-subsonic--artists-from (data)
  "Return the facade artists in a parsed getArtists response DATA.
Flattens every letter bucket of the \"index\" array, in bucket order."
  (mapcan
   (lambda (bucket) (mapcar #'supersonic-subsonic--artist (assoc-default "artist" bucket)))
   (supersonic-recursive-assoc data '("subsonic-response" "artists" "index"))))

(defun supersonic-subsonic--artist-albums-from (data)
  "Return the facade albums in a parsed getArtist response DATA."
  (mapcar #'supersonic-subsonic--album (supersonic-recursive-assoc data '("subsonic-response" "artist" "album"))))

(defun supersonic-subsonic--album-list-from (data)
  "Return the facade albums in a parsed getAlbumList2 response DATA."
  (mapcar #'supersonic-subsonic--album (supersonic-recursive-assoc data '("subsonic-response" "albumList2" "album"))))

(defun supersonic-subsonic--tracks-path ()
  "Return the json path to a track list, per current `supersonic-browse-by-tags'.
Computed fresh on every call rather than cached, so that toggling
`supersonic-browse-by-tags' at runtime stays consistent with
`supersonic-subsonic--album-tracks', which also reads it live to pick
the endpoint -- a cached path here would otherwise go stale and parse
the response at the wrong key."
  (if supersonic-browse-by-tags
      '("subsonic-response" "album" "song")
    '("subsonic-response" "directory" "child")))

(defun supersonic-subsonic--album-tracks-from (data)
  "Return the facade tracks in a parsed getAlbum/getMusicDirectory response DATA."
  (mapcar #'supersonic-subsonic--track (supersonic-recursive-assoc data (supersonic-subsonic--tracks-path))))

(defun supersonic-subsonic--song-from (data)
  "Return the facade track in a parsed getSong response DATA.
Signals an error when DATA has no song in it, so a caller that looks a
track up never mistakes an empty answer for a track with no metadata."
  (let ((song (supersonic-recursive-assoc data '("subsonic-response" "song"))))
    (unless song
      (error "No song in getSong response"))
    (supersonic-subsonic--track song)))

(defun supersonic-subsonic--search-from (data)
  "Return the facade search result in a parsed search3 response DATA."
  (let ((results (supersonic-recursive-assoc data '("subsonic-response" "searchResult3"))))
    (list
     :artists (mapcar #'supersonic-subsonic--artist (assoc-default "artist" results))
     :albums (mapcar #'supersonic-subsonic--album (assoc-default "album" results))
     :tracks (mapcar #'supersonic-subsonic--track (assoc-default "song" results)))))

(defun supersonic-subsonic--podcasts-from (data)
  "Return the facade podcast channels in a parsed getPodcasts response DATA."
  (mapcar #'supersonic-subsonic--podcast (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel"))))

(defun supersonic-subsonic--podcast-episodes-from (data)
  "Return the facade episodes in a parsed getPodcasts response DATA.
DATA is the answer to a request for one channel with its episodes, so
only the first channel in it is read."
  (mapcar
   #'supersonic-subsonic--episode
   (assoc-default "episode" (car (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel"))))))

;;;
;;; Operations
;;;

(aio-defun
 supersonic-subsonic--artists () "Return a promise resolving to every artist, via getArtists."
 (supersonic-subsonic--artists-from (aio-await (supersonic-get-json (supersonic-build-url "/getArtists.view" '())))))

(aio-defun
 supersonic-subsonic--artist-albums
 (id)
 "Return a promise resolving to the albums of artist ID, via getArtist."
 (supersonic-subsonic--artist-albums-from
  (aio-await (supersonic-get-json (supersonic-build-url "/getArtist.view" `(("id" . ,id)))))))

(aio-defun
 supersonic-subsonic--album-list
 (type count)
 "Return a promise resolving to at most COUNT albums of list TYPE.
Asks getAlbumList2.  TYPE is one of
`supersonic-provider-album-list-types', each of which is also the
name getAlbumList2 knows that list under."
 (supersonic-subsonic--album-list-from
  (aio-await
   (supersonic-get-json
    (supersonic-build-url
     "/getAlbumList2.view" `(("type" . ,(symbol-name type)) ("size" . ,(number-to-string count))))))))

(aio-defun
 supersonic-subsonic--album-tracks
 (id)
 "Return a promise resolving to the tracks of album ID.
Asks getAlbum when `supersonic-browse-by-tags' is non-nil, and
getMusicDirectory -- ID then being a directory -- otherwise."
 (supersonic-subsonic--album-tracks-from
  (aio-await
   (supersonic-get-json
    (if supersonic-browse-by-tags
        (supersonic-build-url "/getAlbum.view" `(("id" . ,id)))
      (supersonic-build-url "/getMusicDirectory.view" `(("id" . ,id))))))))

(aio-defun
 supersonic-subsonic--song (id) "Return a promise resolving to track ID, via getSong."
 (supersonic-subsonic--song-from (aio-await (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,id)))))))

(aio-defun
 supersonic-subsonic--search (query) "Return a promise resolving to the search3 results for QUERY."
 (supersonic-subsonic--search-from
  (aio-await (supersonic-get-json (supersonic-build-url "/search3.view" `(("query" . ,query)))))))

(aio-defun
 supersonic-subsonic--podcasts () "Return a promise resolving to every channel, via getPodcasts."
 (supersonic-subsonic--podcasts-from
  (aio-await (supersonic-get-json (supersonic-build-url "/getPodcasts.view" '(("includeEpisodes" . "false")))))))

(aio-defun
 supersonic-subsonic--podcast-episodes
 (id)
 "Return a promise resolving to the episodes of podcast channel ID.
Asks getPodcasts for that one channel, episodes included."
 (supersonic-subsonic--podcast-episodes-from
  (aio-await
   (supersonic-get-json (supersonic-build-url "/getPodcasts.view" `(("id" . ,id) ("includeEpisodes" . "true")))))))

(aio-defun
 supersonic-subsonic--add-podcast (url) "Subscribe to the podcast feed at URL, via createPodcastChannel."
 (aio-await (supersonic-get-json (supersonic-build-url "/createPodcastChannel.view" `(("url" . ,url)))))
 nil)

(aio-defun
 supersonic-subsonic--download-podcast-episode
 (id)
 "Have the server download podcast episode ID, via downloadPodcastEpisode."
 (aio-await (supersonic-get-json (supersonic-build-url "/downloadPodcastEpisode.view" `(("id" . ,id)))))
 nil)

(aio-defun
 supersonic-subsonic--stream-url (id &optional format)
 "Return a promise resolving to the stream.view URL for track ID.
Built locally, without asking the server anything.  It carries the
\"u\"/\"t\"/\"s\" token-auth triple, which never expires -- hence the
facade's warning to keep it off command lines.

FORMAT, if given, is the plist the facade documents for `stream-url' --
`(:format EXT :max-bit-rate KBPS)'.  `:format' becomes stream.view's
own `format' parameter (transcode to EXT, or \"raw\" to skip Subsonic's
usual on-the-fly transcoding), `:max-bit-rate' becomes `maxBitRate' in
kbps.  Either key left out of FORMAT is left out of the request, the
same as when FORMAT is nil altogether, so the server keeps deciding
for itself exactly as it always has."
 (supersonic-build-url
  "/stream.view"
  (append
   `(("id" . ,id))
   (and (plist-get format :format) `(("format" . ,(plist-get format :format))))
   (and (plist-get format :max-bit-rate)
        `(("maxBitRate" . ,(number-to-string (plist-get format :max-bit-rate))))))))

(defun supersonic-subsonic--scrobble (id now-playing)
  "Scrobble track ID via scrobble.view, as now playing if NOW-PLAYING."
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
   (lambda (_status) (kill-buffer (current-buffer)))))

(aio-defun
 supersonic-subsonic--cover-art (art size)
 "Return a promise resolving to the bytes of cover art ART at SIZE.
Asks getCoverArt, which scales the image to SIZE on the server."
 (pcase-let ((`(,status . ,buffer)
              (aio-await
               (supersonic-url-retrieve
                (supersonic-build-url "/getCoverArt.view" `(("id" . ,art) ("size" . ,(int-to-string size))))))))
   (unwind-protect
       (let ((err (plist-get status :error)))
         (when err
           (error "Failed to fetch cover art %s: %s" art (error-message-string err)))
         (with-current-buffer buffer
           (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max))))
     (kill-buffer buffer))))

(defun supersonic-subsonic--cache-namespace ()
  "Return the Subsonic server's address, `supersonic-host'.
Ids are only unique per server, so this is what keeps one server's
cached art and waveforms apart from another's."
  (or supersonic-host ""))

(defun supersonic-subsonic--config-hints ()
  "Return what to check when a request to the Subsonic server fails."
  '("Check that supersonic-host is configured correctly"
    "Ensure the scheme (http:// or https://) matches your server"
    "Verify .authinfo has the correct host (must match supersonic-host exactly)"))

(supersonic-provider-register
 'subsonic
 `((artists . supersonic-subsonic--artists)
   (artist-albums . supersonic-subsonic--artist-albums)
   (album-list . supersonic-subsonic--album-list)
   (album-tracks . supersonic-subsonic--album-tracks)
   (track . supersonic-subsonic--song)
   (search . supersonic-subsonic--search)
   (podcasts . supersonic-subsonic--podcasts)
   (podcast-episodes . supersonic-subsonic--podcast-episodes)
   (add-podcast . supersonic-subsonic--add-podcast)
   (download-podcast-episode . supersonic-subsonic--download-podcast-episode)
   (stream-url . supersonic-subsonic--stream-url)
   (scrobble . supersonic-subsonic--scrobble)
   (cover-art . supersonic-subsonic--cover-art)
   (cache-namespace . supersonic-subsonic--cache-namespace)
   (config-hints . supersonic-subsonic--config-hints)))

(provide 'supersonic-subsonic)
;;; supersonic-subsonic.el ends here
