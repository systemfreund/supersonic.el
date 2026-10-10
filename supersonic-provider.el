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

;; The single door through which every list buffer supersonic.el
;; renders asks for library data -- and so do the play queue, the
;; now-playing buffer and MPRIS for the metadata of what is playing,
;; the playback backends for what to stream and where to scrobble,
;; and the cover art and waveform caches for art and a name to file
;; things under.
;; It is the counterpart, for *where music comes from*, of what
;; `supersonic-playback.el' is for *how it gets played*.  A provider -- Subsonic, via
;; `supersonic-subsonic.el', or Music Assistant, via the opt-in
;; `supersonic-music-assistant.el' -- registers the functions implementing a
;; fixed set of operations under a name, and the generic
;; `supersonic-provider-*' functions here dispatch to whichever
;; provider `supersonic-provider' currently selects.
;;
;; What crosses this door is the provider's business to produce and
;; nobody else's to pick apart.  Every id -- of an artist, an album, a
;; track -- is an opaque string only the provider that handed it out
;; knows how to read: a Subsonic id, a path relative to an mpd music
;; directory, a Music Assistant URI.  Callers pass it back in as-is,
;; compare it with `equal', and never build or parse one.  Every item
;; is a plist in this file's own vocabulary rather than whatever shape
;; the provider's wire format has:
;;
;;   artist  :id :name :art
;;   album   :id :name :artist :year :art
;;   track   :id :title :artist :album :duration :track :art
;;           :suffix :content-type :size
;;   podcast :id :title :art
;;   episode :id :title :duration :status
;;
;; Only `:id' is guaranteed; any other key may be missing, meaning the
;; provider does not know it.  `:duration' is in seconds, `:year' and
;; `:track' are numbers, `:size' is in bytes, and `:art' is an opaque
;; cover-art reference that need not be the item's own id.  An
;; episode's `:status' is free text in the provider's own words --
;; Subsonic's "completed", Music Assistant's "played" -- for display
;; only, never to be matched on.  A provider may add keys of its own on
;; top.
;;
;; As with playback backends, not every provider can do everything, so
;; every operation is optional: one a provider leaves out is reported
;; as such when it is called, and `supersonic-provider-supports-p' lets
;; a caller ask first when a missing capability should simply go
;; unmentioned instead.
;;
;; This file therefore knows nothing about Subsonic, HTTP, or any
;; buffer: it only knows the operation names.  The dependency runs the
;; other way round, each provider requiring this file and registering
;; itself on load.  It also hosts the helpers the list buffers wrap
;; their refreshes in, since reporting a failed refresh is where the
;; provider gets to say how to fix its own configuration.

;;; Code:

(require 'aio)

(require 'supersonic-custom)

(defconst supersonic-provider-operations
  '(artists artist-albums album-list album-tracks track search
    podcasts podcast-episodes add-podcast download-podcast-episode
    stream-url scrobble cover-art cover-art-url cache-namespace config-hints)
  "The library operations a provider can implement.
All but `scrobble', `cache-namespace' and `config-hints' return a
promise.

- `artists' takes no arguments and resolves to a list of artists.
- `artist-albums' takes an artist id and resolves to that artist's
  albums.
- `album-list' takes a TYPE, one of `recent' (recently played),
  `random' or `newest' (recently added), and a COUNT, and resolves to
  at most COUNT albums across all artists.  A provider that cannot
  produce a given TYPE signals a `user-error' saying so.
- `album-tracks' takes an album id and resolves to its tracks, in
  album order.
- `track' takes a track id and resolves to that one track -- how the
  play queue, the now-playing buffer and MPRIS turn the bare ids a
  playback backend reports into something to show.
- `search' takes a query string and resolves to a plist of three
  lists, (:artists ARTISTS :albums ALBUMS :tracks TRACKS).
- `podcasts' takes no arguments and resolves to every podcast
  channel subscribed to.
- `podcast-episodes' takes a channel id and resolves to that
  channel's episodes.  An episode's `:id' is a track id: it is what
  gets handed to `supersonic-playback-start' to play it.
- `add-podcast' takes a feed URL, subscribes to it, and resolves
  once that is done.
- `download-podcast-episode' takes an episode id, has the provider
  fetch the episode so it can be played, and resolves once that has
  been set in motion.
- `stream-url' takes a track id and an optional FORMAT plist and
  resolves to a URL a player can stream that track from.  A promise
  even where the URL is built locally, as for Subsonic, so that a
  provider which has to ask its server first -- for a signed or
  short-lived URL, say -- fits the same contract.  The URL may carry
  credentials, so whoever hands it to a subprocess does so over a
  pipe or socket and never on a command line, where any local user
  could read it.  FORMAT, when given, is `(:format EXT :max-bit-rate
  KBPS)', a hint that a caller which cares what it gets back --
  transcoding for a picky UPnP renderer, say -- can ask for; a
  provider is free to ignore either key, or FORMAT altogether, and
  hand back whatever it would have without it.  A caller that has no
  opinion, such as mpv or the waveform transcoder, simply omits
  FORMAT, and every existing provider implementation keeps working
  unchanged.
- `scrobble' takes a track id and a NOW-PLAYING flag and reports the
  track to the provider: as having just started when NOW-PLAYING is
  non-nil, as having been played otherwise.  Fire and forget: it sets
  the report in motion and returns, and its return value means
  nothing -- nobody waits for a scrobble to land.
- `cover-art' takes an `:art' reference and a SIZE in pixels and
  resolves to the image's bytes, as a unibyte string.  SIZE is a hint
  only: a provider that cannot scale on its side returns the image as
  it is, and Emacs scales it for display.
- `cover-art-url' takes the same ART and SIZE and resolves to a URL
  the image can be fetched from instead of its bytes, for a device
  that fetches it itself -- a UPnP renderer showing it on its own
  display.  Like `stream-url''s, the URL may carry credentials.
- `cache-namespace' takes no arguments and returns a string naming
  the library this provider is currently connected to -- a server
  address, a music directory -- so that what is cached from one never
  stands in for another's.  See `supersonic-provider-cache-name'.
- `config-hints' takes no arguments and returns a list of strings,
  each a hint on what to check in this provider's configuration when
  a request fails -- shown underneath the error in the list buffer
  whose refresh failed.

Items are plists in the vocabulary described in the Commentary of
`supersonic-provider.el'.  A provider need not implement all of these
-- an operation it has no equivalent for is simply left out of the
alist passed to `supersonic-provider-register', and calling the
corresponding `supersonic-provider-*' function then reports that
rather than failing silently.")

(defconst supersonic-provider-album-list-types '(recent random newest)
  "The album list types `supersonic-provider-album-list' may be asked for.")

(defvar supersonic-provider--providers (make-hash-table :test #'eq)
  "Map of provider name (a symbol) to that provider's operation alist.
Populated by `supersonic-provider-register', which every provider
calls as it is loaded, and read by `supersonic-provider--implementation'
when dispatching.")

(defun supersonic-provider-register (name operations)
  "Register NAME as a library provider implementing OPERATIONS.
NAME is the symbol users select with `supersonic-provider'.
OPERATIONS is an alist mapping operation symbols from
`supersonic-provider-operations' to the functions implementing them.
Registering a name that is already registered replaces it, so
re-loading a provider file is harmless."
  (dolist (operation operations)
    (unless (memq (car operation) supersonic-provider-operations)
      (error "Unknown provider operation `%s' for provider `%s'" (car operation) name))
    (unless (functionp (cdr operation))
      (error "Implementation of `%s' for provider `%s' is not a function" (car operation) name)))
  (puthash name operations supersonic-provider--providers))

(defun supersonic-provider-names ()
  "Return the names of every provider currently registered."
  (let (names)
    (maphash (lambda (name _operations) (push name names)) supersonic-provider--providers)
    (nreverse names)))

(defun supersonic-provider-supports-p (operation &optional provider)
  "Return non-nil if PROVIDER implements OPERATION.
PROVIDER defaults to the active one, `supersonic-provider'.  Never
signals, not even for a provider that was never registered -- the
answer is then simply nil -- so it is safe to ask from inside an
error handler."
  (and (alist-get operation (gethash (or provider supersonic-provider) supersonic-provider--providers)) t))

(defun supersonic-provider--implementation (operation)
  "Return the active provider's implementation of OPERATION.
Signals a `user-error' if `supersonic-provider' names a provider that
was never registered -- typically because the file providing it has
not been loaded -- or one that does not implement OPERATION.  Both are
configuration problems the user can act on, hence `user-error' rather
than a backtrace."
  (let ((operations (gethash supersonic-provider supersonic-provider--providers)))
    (unless operations
      (user-error "No library provider named `%s' is registered" supersonic-provider))
    (or (alist-get operation operations)
        (user-error "The `%s' provider does not support `%s'" supersonic-provider operation))))

(defun supersonic-provider-require (operation)
  "Signal a `user-error' unless the active provider implements OPERATION.
For a command to call before it does anything else -- prompting for
input, opening a buffer -- so that a provider without OPERATION is
reported right away rather than only once the work is under way."
  (supersonic-provider--implementation operation)
  nil)

(defun supersonic-provider--call (operation &rest args)
  "Call the active provider's implementation of OPERATION with ARGS."
  (apply (supersonic-provider--implementation operation) args))

;; Each of these is an `aio-defun' rather than a plain function
;; returning whatever the implementation returns, so that a provider
;; that is missing or cannot do OPERATION rejects the promise the
;; caller awaits instead of signalling synchronously at the call site.

(aio-defun
 supersonic-provider-artists ()
 "Return a promise resolving to every artist in the active provider's library."
 (aio-await (supersonic-provider--call 'artists)))

(aio-defun
 supersonic-provider-artist-albums (artist-id)
 "Return a promise resolving to the albums of the artist with ARTIST-ID."
 (aio-await (supersonic-provider--call 'artist-albums artist-id)))

(aio-defun
 supersonic-provider-album-list (type count)
 "Return a promise resolving to at most COUNT albums of list TYPE.
TYPE is one of `supersonic-provider-album-list-types'."
 (unless (memq type supersonic-provider-album-list-types)
   (error "Unknown album list type `%s'" type))
 (aio-await (supersonic-provider--call 'album-list type count)))

(aio-defun
 supersonic-provider-album-tracks (album-id)
 "Return a promise resolving to the tracks of the album with ALBUM-ID."
 (aio-await (supersonic-provider--call 'album-tracks album-id)))

(aio-defun
 supersonic-provider-track (track-id)
 "Return a promise resolving to the track with TRACK-ID."
 (aio-await (supersonic-provider--call 'track track-id)))

(aio-defun
 supersonic-provider-search (query)
 "Return a promise resolving to the active provider's results for QUERY.
The result is a plist (:artists ARTISTS :albums ALBUMS :tracks TRACKS)."
 (aio-await (supersonic-provider--call 'search query)))

(aio-defun
 supersonic-provider-podcasts ()
 "Return a promise resolving to every podcast channel subscribed to."
 (aio-await (supersonic-provider--call 'podcasts)))

(aio-defun
 supersonic-provider-podcast-episodes (channel-id)
 "Return a promise resolving to the episodes of podcast channel CHANNEL-ID."
 (aio-await (supersonic-provider--call 'podcast-episodes channel-id)))

(aio-defun
 supersonic-provider-add-podcast (url)
 "Return a promise resolving once the podcast feed at URL is subscribed to."
 (aio-await (supersonic-provider--call 'add-podcast url)))

(aio-defun
 supersonic-provider-download-podcast-episode (episode-id)
 "Return a promise resolving once EPISODE-ID's download has been started."
 (aio-await (supersonic-provider--call 'download-podcast-episode episode-id)))

(aio-defun
 supersonic-provider-stream-url (track-id &optional format)
 "Return a promise resolving to a URL to stream the track with TRACK-ID from.
The URL may carry credentials -- keep it off any subprocess's command
line.

FORMAT, if given, is the plist `supersonic-provider-operations'
documents for `stream-url' -- `(:format EXT :max-bit-rate KBPS)'.  It
is passed on to the active provider only when non-nil, so a provider
implementation written before this hint existed, expecting just
TRACK-ID, is never called with more arguments than it knows about."
 (aio-await
  (if format
      (supersonic-provider--call 'stream-url track-id format)
    (supersonic-provider--call 'stream-url track-id))))

(defun supersonic-provider-scrobble (track-id &optional now-playing)
  "Scrobble TRACK-ID, as now playing if NOW-PLAYING is non-nil.
Does nothing unless `supersonic-enable-scrobbling' is set, for a nil
TRACK-ID -- a backend reporting an entry it never resolved -- or when
the active provider cannot scrobble: a playback backend calls this at
every track change, and a provider without scrobbling is no reason
to interrupt playback, let alone to say so each time.

Never signals either.  A backend calls this from wherever it notices
a track change -- mpv's socket filter, the jukebox's poll timer -- and
a scrobble that fails, say for want of credentials, must not break
that; it is reported in the echo area instead."
  (when (and supersonic-enable-scrobbling track-id (supersonic-provider-supports-p 'scrobble))
    (condition-case err
        (supersonic-provider--call 'scrobble track-id now-playing)
      (error
       (message "[Supersonic] Failed to scrobble: %s" (error-message-string err))))
    nil))

(aio-defun
 supersonic-provider-cover-art (art size)
 "Return a promise resolving to the bytes of cover art ART at about SIZE pixels.
ART is an item's `:art' reference, never the item's own id."
 (aio-await (supersonic-provider--call 'cover-art art size)))

(aio-defun
 supersonic-provider-cover-art-url (art size)
 "Return a promise resolving to a URL for cover art ART at about SIZE pixels.
ART is an item's `:art' reference, never the item's own id.  The URL
may carry credentials, as `supersonic-provider-stream-url''s may."
 (aio-await (supersonic-provider--call 'cover-art-url art size)))

(defun supersonic-provider-cache-name (id)
  "Return a file name for ID that is safe, bounded and unique to the library.
ID is any id or `:art' reference the active provider handed out.
Provider ids are opaque and may be paths or URIs -- slashes, colons,
spaces, non-ASCII -- so ID is hashed rather than used as is, together
with the provider's `cache-namespace', so that the same ID from two
servers still names two files.  The provider's name leads, readable,
for whoever looks at the cache directory.  Never signals, even for a
provider that is not registered, since cache lookups happen during
rendering."
  (let ((namespace (or (and (supersonic-provider-supports-p 'cache-namespace)
                            (ignore-errors
                              (supersonic-provider--call 'cache-namespace)))
                       "")))
    (format "%s-%s"
            (replace-regexp-in-string "[^[:alnum:]_-]" "_" (symbol-name supersonic-provider))
            (secure-hash 'sha256 (encode-coding-string (concat namespace "\0" id) 'utf-8)))))

(defun supersonic-provider-config-hints ()
  "Return the active provider's configuration hints, a list of strings.
nil for a provider that has none to give -- or when there is no
active provider at all, since this is asked while reporting some
other failure and must not raise one of its own."
  (and (supersonic-provider-supports-p 'config-hints)
       (ignore-errors
         (supersonic-provider--call 'config-hints))))

;;;
;;; Reporting failed refreshes
;;;

(defun supersonic--report-async-error (description err)
  "Tell the user that DESCRIPTION failed with ERR via the echo area.
DESCRIPTION is a short present-tense phrase, e.g. \"fetch tracks\"."
  (message "[Supersonic] Failed to %s: %s" description (error-message-string err)))

(defun supersonic--handle-async-error (buffer description err)
  "Report that DESCRIPTION failed with ERR, both in BUFFER and the echo area.
BUFFER is the tabulated-list buffer whose refresh failed; its contents
are replaced with the error, followed by whatever
`supersonic-provider-config-hints' the active provider has to offer.
The same failure is also echoed via `supersonic--report-async-error'."
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
       (progn
         ,@body)
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

(provide 'supersonic-provider)
;;; supersonic-provider.el ends here
