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

;; The user options of supersonic.el and their `supersonic' group.
;; They live here, in a file with no dependencies that every other file
;; requires, because several are read by more than one file, and so
;; that requiring any one file defines the options it reads.
;;
;; The exception is an option whose default is a function of the
;; package, such as `supersonic-auth-function': it lives next to that
;; function, so that this file does not refer back to the files
;; requiring it.  `supersonic-credentials-cache-expiry' lives with
;; `supersonic-cached', see `supersonic-credentials.el'.

;;; Code:

(defgroup supersonic nil
  "Browse and play music from a Subsonic server."
  :prefix "supersonic-"
  :group 'external)

(defcustom supersonic-host ""
  "URL of the supersonic service, e.g. \"http://host:4533\".
Must match the \"machine\" field of the corresponding authinfo
entry verbatim, unless `supersonic-auth-function' finds the
credentials some other way.  May be given without a scheme
\(\"http://\" or \"https://\"), in which case \"https://\" is
assumed."
  :type 'string
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
Either command also takes a numeric prefix argument, which overrides
this for that one seek."
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
The art is also downloaded at this size, so raising it costs a
re-download of anything already cached at the old size."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-artist-art-size 64
  "Height in pixels of the artist images in the artist list, or nil for none.
Smaller than `supersonic-list-art-size' because every row of the
artist list is this high.  A new size means fetching the images again,
and takes effect for the next artist list opened."
  :type '(choice (const :tag "No artist images" nil) integer)
  :group 'supersonic)

(defcustom supersonic-list-use-header-line nil
  "Where the column headers go in supersonic's artist, album and podcast lists.
Non-nil puts them in the window's header line, pinned as the list
scrolls; nil puts them in the buffer above the first row, scrolling
with it.  Takes effect for lists opened afterwards."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-now-playing-art-size 500
  "Height in pixels of the cover art in the now-playing buffer.
The art is also downloaded at this size, so raising it costs a
re-download of anything already cached at the old size."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-art-fetch-concurrency 6
  "How many cover art downloads may be in flight at the same time.
A list buffer asks for every row's art at once, which would otherwise
open one connection per row.  Six is what browsers and
`url-queue-parallel-processes' settle on per host."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-enable-waveform t
  "Enable a clickable waveform seekbar in the now-playing buffer.
Requires a graphic frame and the `pbm' image type.  The waveform is
computed by transcoding the whole track with mpv, so the first time a
track is shown its seekbar appears a moment later; it is cached
afterwards."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-waveform-buckets 300
  "Number of peak/RMS samples computed across a track.
Also the horizontal resolution of the rendered seekbar.  Changing it
means analysing cached tracks again."
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
  "Seconds between playback position updates in the now-playing buffer.
Each update is a single query to the active backend, and only runs
while that buffer is on display.  Independent of the polling backends'
own intervals: between polls, they count the position on."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-cycle-fields '(title artist)
  "Fields `supersonic-now-playing-animation-functions' rotates through.
A list drawn from `title', `artist' and `album', in the order they
should cycle; nil pins the display on the track's title.  How often it
advances is `supersonic-now-playing-cycle-interval'."
  :type '(repeat (choice (const title) (const artist) (const album)))
  :group 'supersonic)

(defcustom supersonic-now-playing-cycle-interval 10
  "Seconds between advances of `supersonic-now-playing-cycle-fields'.
Only takes effect while that list is non-nil.  Independent of
`supersonic-now-playing-animation-frame-interval'."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-animation-frame-interval 0.2
  "Seconds between animation timer ticks.
This is how often `supersonic-now-playing-animation-functions' get a
chance to redraw, not how often fields switch (that is
`supersonic-now-playing-cycle-interval').  Lowering it makes the
scrolling art overlay smoother, not faster."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-force-redisplay nil
  "Force a redisplay after every now-playing tick, even off the selected frame.
Emacs redisplays when it next goes idle, which a frame that is visible
but not selected can wait long for while Emacs is kept busy.  Turning
this on forces a `redisplay' on every tick, so updates show up right
away, at the cost of that overhead."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-now-playing-scroll-step 30
  "Pixels per second `supersonic-now-playing-animate-art-overlay-scroll' advances.
A speed, so independent of
`supersonic-now-playing-animation-frame-interval'."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-scroll-pause 2
  "Seconds `supersonic-now-playing-animate-art-overlay-scroll' rests at each end.
Text wider than the cover art scrolls to its end, rests this long,
scrolls back, rests again, and repeats.  Text that fits doesn't
scroll."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-album-list-count 50
  "Number of albums to display in random/newest albums etc."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-browse-by-tags t
  "Browse by folder or by idv3 tags."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-enable-scrobbling nil
  "Request that the supersonic server scrobble played tracks."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-provider 'subsonic
  "Which library provider supersonic.el browses.
The symbol a provider registered itself under via
`supersonic-provider-register': where the artist, album and track
lists and search results come from, and what the ids in them mean.
Only `subsonic' is built in; `music-assistant' needs
`supersonic-music-assistant' loaded first.  Selecting a provider whose
file has not been loaded is reported when a list buffer is next opened."
  :type '(choice (const :tag "Subsonic-compatible server" subsonic)
                 (const :tag "Music Assistant server (load supersonic-music-assistant first)" music-assistant)
                 (symbol :tag "Other registered provider"))
  :group 'supersonic)

(defcustom supersonic-music-assistant-url ""
  "Address of the server the `music-assistant' provider browses.
Its scheme, host and port, e.g. \"http://192.168.1.10:8095\".  The
token is looked up in auth-source with exactly this string as the
host, so the authinfo line for it reads

  machine http://192.168.1.10:8095 password TOKEN

where TOKEN is a long-lived token created in Music Assistant's web UI,
under your profile.  `supersonic-music-assistant-token-function' can
look the token up some other way."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-music-assistant-player nil
  "The Music Assistant player the `music-assistant' backend plays on.
A plist `(:id ID :name NAME)': ID is the player's id, which is also
the id of its queue; NAME is its display name, only ever shown to the
user.

`supersonic-music-assistant-select-player' sets this from the players
the server has, and saves it if asked to; nil means none is selected."
  :type '(choice (const :tag "None selected" nil)
                 (plist :options ((:id string) (:name string))))
  :group 'supersonic)

(defcustom supersonic-music-assistant-poll-interval 2
  "Seconds between polls of the Music Assistant player.
Only while `music-assistant' is the active backend.  Over HTTP, Music
Assistant reports nothing on its own, so changes, including those from
other clients, show up within one interval.  The position shown in
between is counted on from the last poll."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-playback-backend 'mpv
  "Which playback backend plays what supersonic.el is asked to play.
The symbol a backend registered itself under via
`supersonic-playback-register-backend'; every play, enqueue and
transport command dispatches to it.  Only `mpv' is built in, and
selecting a backend whose file has not been loaded is reported when a
playback command is next used.  So is one that cannot play for the
active `supersonic-provider': mpv plays for any provider that can
name a stream URL, the jukebox only for `subsonic'.  Switching with
`supersonic-playback-switch-backend' offers only backends that fit."
  :type '(choice (const :tag "Local mpv process" mpv) (symbol :tag "Other registered backend"))
  :group 'supersonic)

(defcustom supersonic-mpv-timeout 0.5
  "Seconds to wait when starting or killing the mpv process."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-request-timeout 10
  "Seconds to wait for the server to answer a request before giving up.
Asynchronous `url-retrieve' has no timeout of its own, so without one
a request the server never answers holds its connection, and with it
a file descriptor, open for the rest of the session."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-jukebox-poll-interval 3
  "Seconds between jukeboxControl polls while `jukebox' is the active backend.
The Subsonic API cannot push the jukebox's state, so it is polled.
Lower values notice changes from other clients sooner, at the cost of
one request to the server per interval."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-upnp-stream-format nil
  "What to ask the provider to stream to a UPnP renderer.
A plist `(:format EXT :max-bit-rate KBPS)', passed as the FORMAT hint
of `supersonic-provider-stream-url'; either key may be left out, and
nil, the default, streams whatever the provider would hand mpv.  Many
cheap renderers cannot decode FLAC or Opus; for those, ask for
`(:format \"mp3\" :max-bit-rate 320)'.  The
`protocolInfo' a renderer is told always follows this, so it is never
promised one format while receiving another."
  :type '(choice (const :tag "Whatever the provider serves" nil)
                 (plist :options ((:format string) (:max-bit-rate integer))))
  :group 'supersonic)

(defcustom supersonic-upnp-renderer nil
  "The UPnP renderer the `upnp' backend plays to.
A plist `(:location URL :udn UDN :name NAME)': LOCATION is the URL of
the renderer's device description, which is enough to reach it again
without running discovery; UDN is its unique device name, so that a
renderer turning up under a different address can still be recognized;
NAME is its friendly name, only ever shown to the user.

`supersonic-upnp-select-renderer' sets this, by discovery or from a
description URL entered by hand, and saves it if asked to; nil means
none is selected."
  :type '(choice (const :tag "None selected" nil)
                 (plist :options ((:location string) (:udn string) (:name string))))
  :group 'supersonic)

(defcustom supersonic-upnp-poll-interval 2
  "Seconds between polls of the renderer while `upnp' is the active backend.
A UPnP renderer reports its state only when asked, so changes, and the
move to the next queued track once one ends, happen within one
interval.  The position shown in between is counted on from the last
poll."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-upnp-discovery-timeout 5
  "Seconds to wait for UPnP renderers to answer a discovery search.
Renderers are told to answer within one second less than this, so
that a slow one still makes it in time.  The same limit applies to
fetching each renderer's device description afterwards, so that one
device that answered but then never serves its description does not
hold up the rest."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-upnp-art-size 600
  "Size in pixels of the cover art a UPnP renderer is pointed at.
A hint the provider may scale to; a renderer shows it on its own
display, if it has one."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-waveform-samplerate 3000
  "Sample rate in Hz mpv transcodes a track to for waveform analysis.
Analysis time and memory grow linearly with it: an hour-long podcast
is about 22 MB at 3000 Hz.  A few hundred samples per bucket already pin a
peak down, so raising it is unlikely to be visible; lowering it far
enough flattens short transients.  Changing it means analysing cached
tracks again."
  :type 'integer
  :group 'supersonic)

(defface supersonic-now-playing-label
  '((t :inherit bold))
  "Face for the track/artist/album label below the cover art."
  :group 'supersonic)

(defface supersonic-now-playing-art-overlay-label
  '((t :weight bold :foreground "white"))
  "Face for the label drawn onto the cover art itself.
The text is drawn into an SVG image, so only `:weight', `:family' and
`:foreground' apply; its size follows `supersonic-now-playing-art-size'."
  :group 'supersonic)

(defface supersonic-now-playing-button
  '((t :box t :height 0.85))
  "Face for the transport buttons in the now-playing buffer.
Doesn't inherit `button', which looks like a hyperlink rather than a
control."
  :group 'supersonic)

(defface supersonic-albums-name
  '((t :inherit bold))
  "Face for album names in *supersonic-albums* and *supersonic-artist-albums*."
  :group 'supersonic)

(defface supersonic-albums-artist
  '((t :inherit supersonic-albums-name))
  "Face for artist names in *supersonic-albums* and *supersonic-artist-albums*.
Inherits from `supersonic-albums-name' but can be customized separately
to distinguish artists from album names."
  :group 'supersonic)

(provide 'supersonic-custom)
;;; supersonic-custom.el ends here
