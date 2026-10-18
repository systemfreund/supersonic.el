;;; supersonic-provider.el --- Library provider facade for supersonic.el -*- lexical-binding: t; -*-

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

;; The library side of supersonic.el: where music comes from, as
;; `supersonic-playback.el' is how it gets played.  A provider --
;; Subsonic in `supersonic-subsonic.el', Music Assistant in
;; `supersonic-music-assistant.el' -- registers functions for the
;; operations in `supersonic-provider-operations', and the
;; `supersonic-provider-*' functions call those of the provider
;; `supersonic-provider' selects.  Operations are optional; see
;; `supersonic-provider-supports-p'.
;;
;; Ids are opaque strings: pass them back as they are and compare them
;; with `equal'.  Items are plists:
;;
;;   artist  :id :name :art
;;   album   :id :name :artist :year :art
;;   track   :id :title :artist :album :duration :track :art
;;           :suffix :content-type :size
;;   podcast :id :title :art
;;   episode :id :title :duration :status
;;
;; Only `:id' is always present.  `:duration' is in seconds, `:size' in
;; bytes; `:art' is a cover art reference, not necessarily the item's
;; id; an episode's `:status' is display text.  Providers may add keys.
;;
;; Also here: the error reporting the list buffers use.

;;; Code:

(require 'aio)

(require 'supersonic-custom)

(defconst supersonic-provider-operations
  '(artists artist-albums album-list album-tracks track search
            podcasts podcast-episodes add-podcast download-podcast-episode
            stream-url scrobble cover-art cover-art-url cache-namespace config-hints)
  "The library operations a provider can implement.
All but `scrobble', `cache-namespace' and `config-hints' return a
promise.  Items are plists as described in the Commentary of
`supersonic-provider.el'.

- `artists' (): all artists.
- `artist-albums' (ARTIST-ID): the artist's albums.
- `album-list' (TYPE COUNT): at most COUNT albums of TYPE, one of
  `recent', `random' or `newest'.  Signals a `user-error' for a TYPE
  the provider can't produce.
- `album-tracks' (ALBUM-ID): the album's tracks, in order.
- `track' (TRACK-ID): one track.
- `search' (QUERY): (:artists ARTISTS :albums ALBUMS :tracks TRACKS).
- `podcasts' (): the subscribed podcast channels.
- `podcast-episodes' (CHANNEL-ID): the channel's episodes.  An
  episode's `:id' is a track id.
- `add-podcast' (URL): subscribe to a feed.
- `download-podcast-episode' (EPISODE-ID): start downloading it on
  the server.
- `stream-url' (TRACK-ID &optional FORMAT): a URL to stream the track
  from.  FORMAT, (:format EXT :max-bit-rate KBPS), is a hint.
- `scrobble' (TRACK-ID NOW-PLAYING): report the track as now playing,
  or as played.  The return value is ignored.
- `cover-art' (ART SIZE): the image bytes, as a unibyte string.  SIZE
  in pixels is a hint.
- `cover-art-url' (ART SIZE): a URL to fetch the image from.
- `cache-namespace' (): a string naming the connected library, such
  as the server address.
- `config-hints' (): strings saying what to check in the
  configuration when a request fails.

URLs may carry credentials, so keep them off command lines.")

(defconst supersonic-provider-album-list-types '(recent random newest)
  "The album list types `supersonic-provider-album-list' may be asked for.")

(defvar supersonic-provider--providers (make-hash-table :test #'eq)
  "Map of provider name (a symbol) to that provider's operation alist.")

(defconst supersonic-provider-libraries
  '((subsonic . supersonic-subsonic)
    (music-assistant . supersonic-music-assistant))
  "Alist of each provider shipped with supersonic.el to its feature.
Requiring the feature registers the provider, so a provider named here
is loaded the first time it is looked up -- see
`supersonic-provider--load' -- rather than having to be required up
front.  A provider not named here still has to be loaded by whoever
wants it.")

(defun supersonic-provider--load (provider)
  "Load the file registering PROVIDER, unless it is registered already.
Does nothing for a PROVIDER not in `supersonic-provider-libraries'."
  (unless (gethash provider supersonic-provider--providers)
    (let ((feature (alist-get provider supersonic-provider-libraries)))
      (when feature
        (require feature)))))

(defun supersonic-provider-register (name operations)
  "Register NAME as a library provider implementing OPERATIONS.
NAME is the symbol `supersonic-provider' selects it by.  OPERATIONS is
an alist of operations from `supersonic-provider-operations' to their
functions.  Registering NAME again replaces it."
  (dolist (operation operations)
    (unless (memq (car operation) supersonic-provider-operations)
      (error "Unknown provider operation `%s' for provider `%s'" (car operation) name))
    (unless (functionp (cdr operation))
      (error "Implementation of `%s' for provider `%s' is not a function" (car operation) name)))
  (puthash name operations supersonic-provider--providers))

(defun supersonic-provider-names ()
  "Return the names of the registered providers.
The providers shipped with supersonic.el are loaded first."
  (dolist (library supersonic-provider-libraries)
    (supersonic-provider--load (car library)))
  (let (names)
    (maphash (lambda (name _operations) (push name names)) supersonic-provider--providers)
    (nreverse names)))

(defun supersonic-provider-registered-p (provider)
  "Return non-nil if PROVIDER is registered.
Loads PROVIDER first if it is in `supersonic-provider-libraries', but
no other provider -- unlike `supersonic-provider-names', which loads
them all to list them."
  (supersonic-provider--load provider)
  ;; Not the operations themselves: a provider may register none.
  (not (eq (gethash provider supersonic-provider--providers 'unregistered) 'unregistered)))

(defun supersonic-provider-supports-p (operation &optional provider)
  "Return non-nil if PROVIDER implements OPERATION.
PROVIDER defaults to `supersonic-provider'.  A shipped provider is
loaded first.  Never signals."
  (let ((provider (or provider supersonic-provider)))
    (ignore-errors (supersonic-provider--load provider))
    (and (alist-get operation (gethash provider supersonic-provider--providers)) t)))

(defun supersonic-provider--implementation (operation)
  "Return the active provider's function for OPERATION.
Load the provider first if it is shipped with supersonic.el.  Signal
a `user-error' if the provider isn't registered or doesn't implement
OPERATION."
  (supersonic-provider--load supersonic-provider)
  (let ((operations (gethash supersonic-provider supersonic-provider--providers)))
    (unless operations
      (user-error "No library provider named `%s' is registered" supersonic-provider))
    (or (alist-get operation operations)
        (user-error "The `%s' provider does not support `%s'" supersonic-provider operation))))

(defun supersonic-provider-require (operation)
  "Signal a `user-error' unless the active provider implements OPERATION."
  (supersonic-provider--implementation operation)
  nil)

(defun supersonic-provider--call (operation &rest args)
  "Call the active provider's implementation of OPERATION with ARGS."
  (apply (supersonic-provider--implementation operation) args))

;; Async so that a missing operation rejects the promise rather than
;; signaling at the call site.

(aio-defun supersonic-provider-artists ()
  "Return a promise resolving to all artists."
  (aio-await (supersonic-provider--call 'artists)))

(aio-defun supersonic-provider-artist-albums (artist-id)
  "Return a promise resolving to the albums of the artist ARTIST-ID."
  (aio-await (supersonic-provider--call 'artist-albums artist-id)))

(aio-defun supersonic-provider-album-list (type count)
  "Return a promise resolving to at most COUNT albums of list TYPE.
TYPE is one of `supersonic-provider-album-list-types'."
  (unless (memq type supersonic-provider-album-list-types)
    (error "Unknown album list type `%s'" type))
  (aio-await (supersonic-provider--call 'album-list type count)))

(aio-defun supersonic-provider-album-tracks (album-id)
  "Return a promise resolving to the tracks of the album ALBUM-ID."
  (aio-await (supersonic-provider--call 'album-tracks album-id)))

(aio-defun supersonic-provider-track (track-id)
  "Return a promise resolving to the track TRACK-ID."
  (aio-await (supersonic-provider--call 'track track-id)))

(aio-defun supersonic-provider-search (query)
  "Return a promise resolving to the search results for QUERY.
The result is a plist (:artists ARTISTS :albums ALBUMS :tracks TRACKS)."
  (aio-await (supersonic-provider--call 'search query)))

(aio-defun supersonic-provider-podcasts ()
  "Return a promise resolving to the subscribed podcast channels."
  (aio-await (supersonic-provider--call 'podcasts)))

(aio-defun supersonic-provider-podcast-episodes (channel-id)
  "Return a promise resolving to the episodes of podcast channel CHANNEL-ID."
  (aio-await (supersonic-provider--call 'podcast-episodes channel-id)))

(aio-defun supersonic-provider-add-podcast (url)
  "Return a promise resolving once the podcast feed at URL is subscribed."
  (aio-await (supersonic-provider--call 'add-podcast url)))

(aio-defun supersonic-provider-download-podcast-episode (episode-id)
  "Return a promise resolving once the download of EPISODE-ID has started."
  (aio-await (supersonic-provider--call 'download-podcast-episode episode-id)))

(aio-defun supersonic-provider-stream-url (track-id &optional format)
  "Return a promise resolving to a URL to stream the track TRACK-ID from.
FORMAT is a hint, see `supersonic-provider-operations'.  The URL may
carry credentials, so keep it off command lines."
  (aio-await
   (if format
       (supersonic-provider--call 'stream-url track-id format)
     (supersonic-provider--call 'stream-url track-id))))

(defun supersonic-provider-scrobble (track-id &optional now-playing)
  "Scrobble TRACK-ID, as now playing if NOW-PLAYING is non-nil.
Do nothing unless `supersonic-enable-scrobbling' is non-nil and the
provider can scrobble.  Errors are reported, not signaled."
  (when (and supersonic-enable-scrobbling track-id (supersonic-provider-supports-p 'scrobble))
    (condition-case err
        (supersonic-provider--call 'scrobble track-id now-playing)
      (error
       (message "[Supersonic] Failed to scrobble: %s" (error-message-string err))))
    nil))

(aio-defun supersonic-provider-cover-art (art size)
  "Return a promise resolving to the bytes of cover art ART at about SIZE pixels.
ART is an item's `:art' reference."
  (aio-await (supersonic-provider--call 'cover-art art size)))

(aio-defun supersonic-provider-cover-art-url (art size)
  "Return a promise resolving to a URL of cover art ART at about SIZE pixels.
ART is an item's `:art' reference.  The URL may carry credentials."
  (aio-await (supersonic-provider--call 'cover-art-url art size)))

(defun supersonic-provider-cache-name (id)
  "Return a cache file name for ID, unique to the connected library.
ID is an id or `:art' reference.  It is hashed with the provider's
`cache-namespace'.  Never signals."
  (let ((namespace (or (and (supersonic-provider-supports-p 'cache-namespace)
                            (condition-case err
                                (supersonic-provider--call 'cache-namespace)
                              (error
                               (supersonic--log "cache-namespace failed: %s" (error-message-string err))
                               nil)))
                       "")))
    (format "%s-%s"
            (replace-regexp-in-string "[^[:alnum:]_-]" "_" (symbol-name supersonic-provider))
            (secure-hash 'sha256 (encode-coding-string (concat namespace "\0" id) 'utf-8)))))

(defun supersonic-provider-config-hints ()
  "Return the active provider's configuration hints, a list of strings.
Never signals."
  (and (supersonic-provider-supports-p 'config-hints)
       (condition-case err
           (supersonic-provider--call 'config-hints)
         (error
          (supersonic--log "config-hints failed: %s" (error-message-string err))
          nil))))

;;;
;;; Reporting failed refreshes
;;;

(defun supersonic--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS to *Messages* without showing it."
  (let ((inhibit-message t))
    (apply #'message (concat "[Supersonic] " format-string) args)))

(defun supersonic--report-async-error (description err)
  "Report in the echo area that DESCRIPTION failed with ERR.
DESCRIPTION is a phrase like \"fetch tracks\"."
  (message "[Supersonic] Failed to %s: %s" description (error-message-string err)))

(defun supersonic--handle-async-error (buffer description err)
  "Report that DESCRIPTION failed with ERR, in BUFFER and the echo area.
BUFFER's contents are replaced by the error and the provider's
configuration hints."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (hints (supersonic-provider-config-hints)))
        (erase-buffer)
        (insert (format "Error: Failed to %s: %s\n" description (error-message-string err)))
        (when hints
          (insert "\nConfiguration hint:\n")
          (dolist (hint hints)
            (insert "  - " hint "\n"))))))
  (supersonic--report-async-error description err))

(defmacro supersonic--with-async-error-handling (buff description &rest body)
  "Run BODY, reporting an error instead of signaling it.
DESCRIPTION is a phrase like \"fetch tracks\".  If BUFF is non-nil,
the error is also shown in it, see `supersonic--handle-async-error'.
Usable inside `aio-defun'."
  (declare (indent 2))
  `(condition-case err
       (progn
         ,@body)
     (error
      (if ,buff
          (supersonic--handle-async-error ,buff ,description err)
        (supersonic--report-async-error ,description err)))))

(defun supersonic--init-list-buffer (buff mode-fn placeholder)
  "Prepare BUFF for an async refresh: turn on MODE-FN and show PLACEHOLDER."
  (with-current-buffer buff
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert placeholder "\n")
    (setq buffer-read-only t)
    (funcall mode-fn)))

(provide 'supersonic-provider)
;;; supersonic-provider.el ends here
