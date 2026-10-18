;;; supersonic-custom.el --- User options for supersonic.el -*- lexical-binding: t; -*-

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

;; The user options of supersonic.el.  Every other file requires this
;; one, so it requires none of them.  Options whose default is a
;; function of the package, like `supersonic-auth-function', live next
;; to that function instead.

;;; Code:

(defgroup supersonic nil
  "Browse and play music from a Subsonic server."
  :prefix "supersonic-"
  :group 'external)

(defcustom supersonic-host ""
  "URL of the Subsonic server, e.g. \"https://host:4533\".
\"https://\" is assumed if no scheme is given.  The default
`supersonic-auth-function' looks this exact string up as the
\"machine\" in authinfo."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-credentials-cache-expiry 3600
  "Seconds a function made by `supersonic-cached' caches a result.
`supersonic-forget-credentials' clears the cache earlier."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-mpv (executable-find "mpv")
  "Path to the mpv executable, or nil if there is none on variable `exec-path'."
  :type '(choice (const :tag "Not available" nil) (file :tag "Path to mpv"))
  :group 'supersonic)

(defcustom supersonic-default-volume 100
  "Default volume for mpv to use."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-seek-step 10
  "Seconds `supersonic-seek-forward' and `supersonic-seek-back' jump by.
A numeric prefix argument overrides it for one seek."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-cache-path (expand-file-name "supersonic-cache" user-emacs-directory)
  "Path to store cached cover art and waveform peak/RMS envelopes."
  :type 'directory
  :group 'supersonic)

(defcustom supersonic-enable-art t
  "Enable displaying album art in graphic frames."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-list-art-size 250
  "Height in pixels of the cover art in the album and podcast lists.
Art cached at another size is downloaded again."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-artist-art-size 64
  "Height in pixels of the artist images in the artist list, or nil for none.
Art cached at another size is downloaded again."
  :type '(choice (const :tag "No artist images" nil) integer)
  :group 'supersonic)

(defcustom supersonic-list-use-header-line nil
  "Non-nil to show list column headers in the header line.
The header line stays in place as the list scrolls.  With nil the
headers are the first line of the buffer.  Applies to lists opened
afterwards."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-now-playing-art-size 500
  "Height in pixels of the cover art in the now-playing buffer.
Art cached at another size is downloaded again."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-art-fetch-concurrency 6
  "Maximum number of cover art downloads running at the same time."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-enable-waveform t
  "Non-nil to show a clickable waveform seekbar in the now-playing buffer.
Needs a graphic frame and mpv.  A track's waveform is computed the
first time it is shown, which takes a moment, and cached."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-waveform-buckets 300
  "Number of peak/RMS samples computed across a track.
This is also the horizontal resolution of the seekbar.  Changing it
recomputes cached waveforms."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-waveform-width 500
  "Width in pixels of the rendered waveform seekbar."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-waveform-height 48
  "Height in pixels of the rendered waveform seekbar."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-now-playing-interval 1
  "Seconds between position updates in the now-playing buffer.
Updates only run while the buffer is visible."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-cycle-fields '(title artist)
  "Fields the now-playing label cycles through, in order.
A list of `title', `artist' and `album'; nil shows the title only.
See `supersonic-now-playing-cycle-interval'."
  :type '(repeat (choice (const title) (const artist) (const album)))
  :group 'supersonic)

(defcustom supersonic-now-playing-cycle-interval 10
  "Seconds between switches of the field the now-playing label shows.
See `supersonic-now-playing-cycle-fields'."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-animation-frame-interval 0.2
  "Seconds between frames of the now-playing animations.
Lower values make the scrolling label smoother, not faster."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-force-redisplay nil
  "Non-nil to redisplay after every now-playing update.
Without it, a visible but unselected frame may only update when Emacs
is idle."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-now-playing-scroll-step 30
  "Speed in pixels per second of the scrolling label on the cover art.
See `supersonic-now-playing-animate-art-overlay-scroll'."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-scroll-pause 2
  "Seconds the scrolling label on the cover art rests at either end."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-album-list-count 50
  "Number of albums shown in the random, newest and recent album lists."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-browse-by-tags t
  "Non-nil to browse by tags, nil to browse by folder."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-enable-scrobbling nil
  "Request that the supersonic server scrobble played tracks."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-provider 'subsonic
  "The library provider supersonic.el browses.
A symbol registered with `supersonic-provider-register'.  The
providers shipped with supersonic.el, `subsonic' and
`music-assistant', are loaded on first use."
  :type '(choice (const :tag "Subsonic-compatible server" subsonic)
                 (const :tag "Music Assistant server" music-assistant)
                 (symbol :tag "Other registered provider"))
  :group 'supersonic)

(defcustom supersonic-music-assistant-url ""
  "URL of the Music Assistant server, e.g. \"http://192.168.1.10:8095\".
The default `supersonic-music-assistant-token-function' looks the
token up in auth-source with this exact string as the host:

  machine http://192.168.1.10:8095 password TOKEN

Create a long-lived token under your profile in Music Assistant's web UI."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-music-assistant-player nil
  "The Music Assistant player the `music-assistant' backend plays on.
A plist (:id ID :name NAME), or nil for none.  Set it with
`supersonic-music-assistant-select-player'."
  :type '(choice (const :tag "None selected" nil)
                 (plist :options ((:id string) (:name string))))
  :group 'supersonic)

(defcustom supersonic-music-assistant-poll-interval 2
  "Seconds between polls of the Music Assistant player.
Changes made elsewhere show up within this interval."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-playback-backend 'mpv
  "The playback backend supersonic.el plays with.
A symbol registered with `supersonic-playback-register-backend'.  The
backends shipped with supersonic.el, `mpv', `jukebox', `upnp' and
`music-assistant', are loaded on first use.  Not every backend can
play for every `supersonic-provider'; `supersonic-playback-switch-backend'
offers the ones that can."
  :type '(choice (const :tag "Local mpv process" mpv)
                 (const :tag "Subsonic server's jukebox" jukebox)
                 (const :tag "UPnP/DLNA renderer" upnp)
                 (const :tag "Music Assistant player" music-assistant)
                 (symbol :tag "Other registered backend"))
  :group 'supersonic)

(defcustom supersonic-mpv-timeout 0.5
  "Seconds to wait when starting or killing the mpv process."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-request-timeout 10
  "Seconds to wait for the server to answer a request before giving up."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-jukebox-poll-interval 3
  "Seconds between polls of the jukebox while `jukebox' is the backend.
Changes made elsewhere show up within this interval."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-upnp-stream-format nil
  "Format to stream to a UPnP renderer, or nil for the provider's default.
A plist (:format EXT :max-bit-rate KBPS); either key may be left out.
Many cheap renderers cannot decode FLAC or Opus; for those, use
\(:format \"mp3\" :max-bit-rate 320)."
  :type '(choice (const :tag "Whatever the provider serves" nil)
                 (plist :options ((:format string) (:max-bit-rate integer))))
  :group 'supersonic)

(defcustom supersonic-upnp-renderer nil
  "The UPnP renderer the `upnp' backend plays to.
A plist (:location URL :udn UDN :name NAME), or nil for none.  URL is
the renderer's device description, UDN its unique device name.  Set
it with `supersonic-upnp-select-renderer'."
  :type '(choice (const :tag "None selected" nil)
                 (plist :options ((:location string) (:udn string) (:name string))))
  :group 'supersonic)

(defcustom supersonic-upnp-poll-interval 2
  "Seconds between polls of the renderer while `upnp' is the backend.
Changes, and the move to the next track, happen within this interval."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-upnp-discovery-timeout 5
  "Seconds to wait for UPnP renderers to answer a discovery search.
The same limit applies to fetching each renderer's description."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-upnp-art-size 600
  "Size in pixels of the cover art shown on a UPnP renderer."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-waveform-samplerate 3000
  "Sample rate in Hz at which tracks are analysed for the waveform.
Time and memory grow with it.  Changing it recomputes cached
waveforms."
  :type 'integer
  :group 'supersonic)

(defface supersonic-now-playing-label
  '((t :inherit bold))
  "Face for the track/artist/album label below the cover art."
  :group 'supersonic)

(defface supersonic-now-playing-art-overlay-label
  '((t :weight bold :foreground "white"))
  "Face for the label drawn onto the cover art.
Only `:weight', `:family' and `:foreground' apply."
  :group 'supersonic)

(defface supersonic-now-playing-button
  '((t :box t :height 0.85))
  "Face for the transport buttons in the now-playing buffer."
  :group 'supersonic)

(defface supersonic-albums-name
  '((t :inherit bold))
  "Face for album names in the album lists."
  :group 'supersonic)

(defface supersonic-albums-artist
  '((t :inherit supersonic-albums-name))
  "Face for artist names in the album lists."
  :group 'supersonic)

(provide 'supersonic-custom)
;;; supersonic-custom.el ends here
