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

;; The Subsonic library provider, registered as `subsonic'.  It
;; implements `supersonic-provider-operations' for any server speaking
;; the Subsonic API, turning its JSON into item plists.

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
  "Return an item plist from the Subsonic alist DATA.
MAPPING is a list of (KEY . FIELD): KEY is set to DATA's FIELD.
Missing fields are left out.  `:id' always comes first."
  (let ((plist (list :id (supersonic-get-id-as-string data))))
    (dolist (pair mapping)
      (let ((value (assoc-default (cdr pair) data)))
        (when value
          (setq plist (append plist (list (car pair) value))))))
    plist))

(defun supersonic-subsonic--artist (data)
  "Return the artist plist for the Subsonic artist DATA."
  (supersonic-subsonic--plist data '((:name . "name") (:art . "coverArt"))))

(defun supersonic-subsonic--album (data)
  "Return the album plist for the Subsonic album DATA."
  (supersonic-subsonic--plist
   data '((:name . "name") (:artist . "artist") (:year . "year") (:art . "coverArt"))))

(defun supersonic-subsonic--track (data)
  "Return the track plist for the Subsonic song DATA.
It also has the Subsonic-specific key `:genre'."
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
  "Return the podcast plist for the Subsonic podcast channel DATA."
  (supersonic-subsonic--plist data '((:title . "title") (:art . "coverArt"))))

(defun supersonic-subsonic--episode (data)
  "Return the episode plist for the Subsonic podcast episode DATA."
  (supersonic-subsonic--plist data '((:title . "title") (:duration . "duration") (:status . "status"))))

(defun supersonic-subsonic--artists-from (data)
  "Return the artists in the getArtists response DATA."
  (mapcan
   (lambda (bucket) (mapcar #'supersonic-subsonic--artist (assoc-default "artist" bucket)))
   (supersonic-recursive-assoc data '("subsonic-response" "artists" "index"))))

(defun supersonic-subsonic--artist-albums-from (data)
  "Return the albums in the getArtist response DATA."
  (mapcar #'supersonic-subsonic--album (supersonic-recursive-assoc data '("subsonic-response" "artist" "album"))))

(defun supersonic-subsonic--album-list-from (data)
  "Return the albums in the getAlbumList2 response DATA."
  (mapcar #'supersonic-subsonic--album (supersonic-recursive-assoc data '("subsonic-response" "albumList2" "album"))))

(defun supersonic-subsonic--tracks-path ()
  "Return the JSON path to the tracks of an album or directory.
It depends on `supersonic-browse-by-tags'."
  (if supersonic-browse-by-tags
      '("subsonic-response" "album" "song")
    '("subsonic-response" "directory" "child")))

(defun supersonic-subsonic--album-tracks-from (data)
  "Return the tracks in the getAlbum or getMusicDirectory response DATA."
  (mapcar #'supersonic-subsonic--track (supersonic-recursive-assoc data (supersonic-subsonic--tracks-path))))

(defun supersonic-subsonic--song-from (data)
  "Return the track in the getSong response DATA.
Signal an error if there is none."
  (let ((song (supersonic-recursive-assoc data '("subsonic-response" "song"))))
    (unless song
      (error "No song in getSong response"))
    (supersonic-subsonic--track song)))

(defun supersonic-subsonic--search-from (data)
  "Return the search results in the search3 response DATA."
  (let ((results (supersonic-recursive-assoc data '("subsonic-response" "searchResult3"))))
    (list
     :artists (mapcar #'supersonic-subsonic--artist (assoc-default "artist" results))
     :albums (mapcar #'supersonic-subsonic--album (assoc-default "album" results))
     :tracks (mapcar #'supersonic-subsonic--track (assoc-default "song" results)))))

(defun supersonic-subsonic--podcasts-from (data)
  "Return the podcast channels in the getPodcasts response DATA."
  (mapcar #'supersonic-subsonic--podcast (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel"))))

(defun supersonic-subsonic--podcast-episodes-from (data)
  "Return the episodes of the first channel in the getPodcasts response DATA."
  (mapcar
   #'supersonic-subsonic--episode
   (assoc-default "episode" (car (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel"))))))

;;;
;;; Operations
;;;

(aio-defun supersonic-subsonic--artists ()
  "Return a promise resolving to all artists."
  (supersonic-subsonic--artists-from (aio-await (supersonic-get-json (supersonic-build-url "/getArtists.view" '())))))

(aio-defun supersonic-subsonic--artist-albums (id)
  "Return a promise resolving to the albums of artist ID."
  (supersonic-subsonic--artist-albums-from
   (aio-await (supersonic-get-json (supersonic-build-url "/getArtist.view" `(("id" . ,id)))))))

(aio-defun supersonic-subsonic--album-list (type count)
  "Return a promise resolving to at most COUNT albums of list TYPE.
TYPE is one of `supersonic-provider-album-list-types'."
  (supersonic-subsonic--album-list-from
   (aio-await
    (supersonic-get-json
     (supersonic-build-url
      "/getAlbumList2.view" `(("type" . ,(symbol-name type)) ("size" . ,(number-to-string count))))))))

(aio-defun supersonic-subsonic--album-tracks (id)
  "Return a promise resolving to the tracks of album ID.
ID is a directory if `supersonic-browse-by-tags' is nil."
  (supersonic-subsonic--album-tracks-from
   (aio-await
    (supersonic-get-json
     (if supersonic-browse-by-tags
         (supersonic-build-url "/getAlbum.view" `(("id" . ,id)))
       (supersonic-build-url "/getMusicDirectory.view" `(("id" . ,id))))))))

(aio-defun supersonic-subsonic--song (id)
  "Return a promise resolving to the track ID."
  (supersonic-subsonic--song-from (aio-await (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,id)))))))

(aio-defun supersonic-subsonic--search (query)
  "Return a promise resolving to the search results for QUERY."
  (supersonic-subsonic--search-from
   (aio-await (supersonic-get-json (supersonic-build-url "/search3.view" `(("query" . ,query)))))))

(aio-defun supersonic-subsonic--podcasts ()
  "Return a promise resolving to the podcast channels."
  (supersonic-subsonic--podcasts-from
   (aio-await (supersonic-get-json (supersonic-build-url "/getPodcasts.view" '(("includeEpisodes" . "false")))))))

(aio-defun supersonic-subsonic--podcast-episodes (id)
  "Return a promise resolving to the episodes of podcast channel ID."
  (supersonic-subsonic--podcast-episodes-from
   (aio-await
    (supersonic-get-json (supersonic-build-url "/getPodcasts.view" `(("id" . ,id) ("includeEpisodes" . "true")))))))

(aio-defun supersonic-subsonic--add-podcast (url)
  "Return a promise resolving once the podcast feed at URL is subscribed."
  (aio-await (supersonic-get-json (supersonic-build-url "/createPodcastChannel.view" `(("url" . ,url)))))
  nil)

(aio-defun supersonic-subsonic--download-podcast-episode (id)
  "Return a promise resolving once the server downloads episode ID."
  (aio-await (supersonic-get-json (supersonic-build-url "/downloadPodcastEpisode.view" `(("id" . ,id)))))
  nil)

(aio-defun supersonic-subsonic--stream-url (id &optional format)
  "Return a promise resolving to the stream URL of track ID.
FORMAT's `:format' and `:max-bit-rate' are passed on to the server."
  (supersonic-build-url
   "/stream.view"
   (append
    `(("id" . ,id))
    (and (plist-get format :format) `(("format" . ,(plist-get format :format))))
    (and (plist-get format :max-bit-rate)
         `(("maxBitRate" . ,(number-to-string (plist-get format :max-bit-rate))))))))

(defun supersonic-subsonic--scrobble (id now-playing)
  "Scrobble track ID, as now playing if NOW-PLAYING is non-nil."
  (url-retrieve
   (supersonic-build-url
    "/scrobble.view"
    `(("id" . ,id)
      ("submission" .
       ,(if now-playing
            "false"
          "true"))))
   ;; The response isn't needed.
   (lambda (_status) (kill-buffer (current-buffer)))))

(defun supersonic-subsonic--cover-art-request-url (art size)
  "Return the getCoverArt URL for cover art ART, scaled to SIZE."
  (supersonic-build-url "/getCoverArt.view" `(("id" . ,art) ("size" . ,(int-to-string size)))))

(aio-defun supersonic-subsonic--cover-art (art size)
  "Return a promise resolving to the bytes of cover art ART at SIZE pixels."
  (pcase-let ((`(,status . ,buffer)
               (aio-await (supersonic-url-retrieve (supersonic-subsonic--cover-art-request-url art size)))))
    (unwind-protect
        (let ((err (plist-get status :error)))
          (when err
            (error "Failed to fetch cover art %s: %s" art (error-message-string err)))
          (with-current-buffer buffer
            (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max))))
      (kill-buffer buffer))))

(aio-defun supersonic-subsonic--cover-art-url (art size)
  "Return a promise resolving to the URL of cover art ART at SIZE pixels."
  (supersonic-subsonic--cover-art-request-url art size))

(defun supersonic-subsonic--cache-namespace ()
  "Return `supersonic-host', as ids are only unique per server."
  (or supersonic-host ""))

(defun supersonic-subsonic--config-hints ()
  "Return what to check when a request to the Subsonic server fails."
  '("Check that supersonic-host is configured correctly"
    "Ensure the scheme (http:// or https://) matches your server"
    "Verify .authinfo has the correct host (must match supersonic-host exactly), or what supersonic-auth-function returns"))

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
   (cover-art-url . supersonic-subsonic--cover-art-url)
   (cache-namespace . supersonic-subsonic--cache-namespace)
   (config-hints . supersonic-subsonic--config-hints)))

(provide 'supersonic-subsonic)
;;; supersonic-subsonic.el ends here
