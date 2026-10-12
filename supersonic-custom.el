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

;; The user options of supersonic.el, plus the `supersonic'
;; customization group they all belong to.
;;
;; They live here rather than each in the file that reads them because
;; several are read by more than one: `supersonic-cache-path' by both
;; the cover art and the waveform cache, `supersonic-mpv' by both the
;; playback backend and the waveform transcoder (which is otherwise
;; deliberately independent of that backend -- see
;; `supersonic-waveform.el').  Splitting them up would mean either
;; picking an arbitrary owner for each shared one or duplicating
;; declarations.
;;
;; This is also the file with no dependencies of its own, which every
;; other file in the package requires first.  That is the point: before
;; this file existed, the options were defined in `supersonic.el' ahead
;; of its own `require's, and the files that read them carried `defvar'
;; stubs to keep the byte-compiler quiet -- so requiring any one of them
;; on its own (a test, or a user who only wants the seekbar) left those
;; options genuinely void, with the stubs hiding it.
;;
;; The one exception is an option whose default is a function of the
;; package -- `supersonic-auth-function', `supersonic-art-overlay-layers'
;; and the like.  It lives next to that function instead, in the file
;; that is also the one reading it, so that this file does not refer
;; back to the files requiring it.  Requiring that file defines the
;; option, so it cannot be void there either.
;; `supersonic-credentials-cache-expiry' lives with
;; `supersonic-cached' for another reason, given in
;; `supersonic-credentials.el'.

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
  "Default  volume for mpv to use."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-seek-step 10
  "Seconds `supersonic-seek-forward' and `supersonic-seek-back' jump by.
Either command also takes a numeric prefix argument, which overrides
this for that one seek."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-cache-path (expand-file-name "supersonic-cache" user-emacs-directory)
  "Path to store cached cover art and waveform peak/RMS envelopes.
Shared by both `supersonic-art-cache-file' and
`supersonic-waveform-cache-file', which prefix their file names
distinctly enough (\"art-\"/\"waveform-\") that the two never collide,
even though both are keyed on provider ids that could otherwise
coincide (e.g. a track and its own cover art id).  The rest of each
name is `supersonic-provider-cache-name' of the id, so files cached
from different providers or servers never stand in for each other."
  :type 'directory
  :group 'supersonic)

(defcustom supersonic-enable-art t
  "Enable displaying album art in supported frames.
Also a statement about the frame itself, since supersonic only draws
art it can draw -- see `supersonic-art-available-p'."
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
Smaller than `supersonic-list-art-size' because the artist list holds
the whole library and is browsed rather than looked at: every row is
this high.  Only the rows in view are fetched, see
`supersonic-art-fill-visible'.  Like that setting, a new size means
fetching the images again.  Read when the artist list is opened, so it
takes effect for the next one."
  :type '(choice (const :tag "No artist images" nil) integer)
  :group 'supersonic)

(defcustom supersonic-list-use-header-line nil
  "Where the column headers go in supersonic's artist, album and podcast lists.
Seeded into `tabulated-list-use-header-line' by `supersonic-artist-mode',
`supersonic-album-mode', `supersonic-album-type-mode',
`supersonic-podcast-mode' and `supersonic-podcast-episodes-mode', which
is the whole of its reach -- the other list buffers (tracks, search
results, the play queue) leave that variable at its own default and are
unaffected.

This chooses between the two ways tabulated-list can present the column
names, not between showing and hiding them: non-nil puts them in the
window's header line, where they stay pinned as the list scrolls; nil
\(the default) puts them in the buffer itself, as a
`tabulated-list-fake-header' line above the first row, which scrolls
away with everything else.  There is no third setting that leaves them
out altogether.

Read once, when the major mode is turned on.  Changing it does not
reach list buffers that are already open -- reopen them (`RET' from the
artist list, `M-x supersonic-albums', ...) for a new value to take
effect, since only that runs the mode function again."
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
A list buffer asks for every row's art at once; without a cap that
opens one connection per row, so a 50 album list hits the server with
50 simultaneous requests and leaves 50 idle keep-alive connections in
`url-http-open-connections' afterwards.  Six is what browsers and
`url-queue-parallel-processes' settle on per host."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-enable-waveform t
  "Enable a clickable waveform seekbar in the now-playing buffer.
Requires a graphic frame and the `pbm' image type (see
`supersonic-waveform-available-p'), the same restriction
`supersonic-enable-art' has.  Generating a waveform means transcoding
the whole track through a disposable mpv process, so the first time a
track is shown its seekbar appears a beat after the rest of the
buffer; it is cached on disk afterwards (see `supersonic-cache-path')."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-waveform-buckets 300
  "Number of peak/RMS samples computed across a track.
Also the horizontal resolution of the rendered seekbar.  The cache is
keyed on this value (see `supersonic-waveform-cache-file'), so raising
it costs a re-analysis of anything already cached, not just a redraw."
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
Each update is a single query to the active backend via
`supersonic-playback-status', and only runs while that buffer is both
open and on display.

Left at its own pace even under the `jukebox' backend, whose own poll
timer (`supersonic-jukebox-poll-interval') only refreshes its cached
position every few seconds: the two intervals are independent, and
that backend interpolates the position it answers with forward from
the wall clock time elapsed since its last poll (see
`supersonic-poller-position'), so it still has
something new to show this often even between polls."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-cycle-fields '(title artist)
  "Fields `supersonic-now-playing-animation-functions' rotates through.
A list drawn from `title', `artist' and `album', in the order they
should cycle; nil turns cycling off and pins the display on the
track's title, same as `supersonic-now-playing-cycle-label' being nil
used to.  How often it advances is
`supersonic-now-playing-cycle-interval'; what advancing actually
draws is `supersonic-now-playing-animation-functions'."
  :type '(repeat (choice (const title) (const artist) (const album)))
  :group 'supersonic)

(defcustom supersonic-now-playing-cycle-interval 10
  "Seconds between advances of `supersonic-now-playing-cycle-fields'.
Only takes effect while that list is non-nil.  Measured against real
elapsed time (see `supersonic-now-playing--advance-field'), not against
how often the animation timer happens to fire -- that is
`supersonic-now-playing-animation-frame-interval', a separate setting,
so turning that one down for a smooth
`supersonic-now-playing-animate-art-overlay-scroll' crawl does not also
speed up how often the field switches here."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-animation-frame-interval 0.2
  "Seconds between animation timer ticks.
Independent of `supersonic-now-playing-cycle-interval': this is how
often `supersonic-now-playing-animation-functions' merely get a chance
to redraw, not how often anything actually changes.
`supersonic-now-playing-animate-label' and
`supersonic-now-playing-animate-art-overlay' still only switch fields
once `supersonic-now-playing-cycle-interval' seconds have really
passed, however many ticks that took;
`supersonic-now-playing-animate-art-overlay-scroll' instead redraws on
every tick, moving forward in proportion to real elapsed time (see
`supersonic-now-playing-scroll-step'), so lowering this only makes its
crawl look smoother, not faster."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-force-redisplay nil
  "Force a redisplay after every now-playing tick, even off the selected frame.
Emacs's own redisplay normally happens whenever it next goes idle,
which shows an animation/position update within a fraction of a second
of the tick that caused it -- but a frame that is visible without being
selected (its own frame, while a different one has input focus, say)
can end up waiting far longer than that for a truly idle moment,
especially if something else -- another frame doing its own frequent
updates, for instance -- keeps Emacs busy in the meantime.  The buffer
still ticks along correctly the whole time either way (see
`supersonic-now-playing--field-index' advancing regardless); this is
purely about how promptly that becomes visible.

Turning this on adds a forced, uninterruptible `redisplay' call -- see
`redisplay's FORCE argument -- to every
`supersonic-now-playing-animation-frame-interval'/
`supersonic-now-playing-interval' tick, so updates show up right away
regardless of which frame is selected.  Off by default: most setups
never need it, and forcing a redisplay this often is real, avoidable
overhead for the ones that don't."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-now-playing-scroll-step 30
  "Pixels per second `supersonic-now-playing-animate-art-overlay-scroll' advances.
Only meaningful with that function listed in
`supersonic-now-playing-animation-functions' -- the other two built-ins
ignore it.  A speed, not a per-tick amount, so it stays the same crawl
however often (or unevenly) `supersonic-now-playing-animation-frame-interval'
actually fires.  Does not apply at all when
`supersonic-now-playing-cycle-fields''s joined-up text already fits
without scrolling -- see `supersonic-now-playing-scroll-pause'."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-now-playing-scroll-pause 2
  "Seconds `supersonic-now-playing-animate-art-overlay-scroll' rests at each end.
That function does not loop continuously: when
`supersonic-now-playing-cycle-fields''s joined-up text is wider than
the cover art, it scrolls left just far enough to bring the far end
into view, rests here for this long, scrolls back to the start, rests
there for this long too, and repeats.  When the text fits without
scrolling at all, it is shown once, motionless, and this is not
consulted."
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
Only while `music-assistant' is the active backend.  This package
talks to Music Assistant over plain HTTP, which reports nothing on its
own, so `supersonic-music-assistant-player.el' asks the player's
queue for its state on this interval.  The now-playing
buffer, the queue buffer and MPRIS follow the player within one
interval, changes made from other Music Assistant clients included.
The position shown in between is counted on from the last poll."
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
The Subsonic API has no push mechanism for jukebox state, so
`supersonic-jukebox.el' polls `action=get' on this interval instead,
caching the result and firing the generalized playback hooks whenever
the cached snapshot changes; the jukebox backend's status accessor and
queue listing both answer from that cache rather than issuing a fresh
request of their own.  Lower values notice a track change or a pause
toggled from another client sooner, at the cost of one request to the
server per interval."
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
A UPnP renderer reports its state only when asked -- this package does
not subscribe to its events -- so `supersonic-upnp.el' asks it for its
transport state and position on this interval.  The now-playing
buffer, the queue buffer and MPRIS follow the renderer within one
interval; so does moving on to the next queued track once one ends.
The position shown in between is counted on from the last poll."
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
Every sample is walked individually in Lisp and every byte of the
transcode passes through a temp file and an Emacs buffer, so both
analysis time and peak memory scale linearly with this: an hour-long
podcast is 11 MB and 5.4 M iterations at 3000 Hz, four times that at
12000.  Only `supersonic-waveform-buckets' peak/RMS pairs come out the
far end either way, so the precision this buys is precision inside a
bucket -- a few hundred samples per bucket already pins a peak down
well.  Raising it is unlikely to be visible in the rendered seekbar;
lowering it far enough eventually flattens short transients.

The cache is keyed on this value (see `supersonic-waveform-cache-file'),
so changing it costs a re-analysis of anything already cached rather
than mixing envelopes measured at different rates."
  :type 'integer
  :group 'supersonic)

(defface supersonic-now-playing-label
  '((t :inherit bold))
  "Face for the track/artist/album label below the cover art.
Applied by `supersonic-now-playing-animate-label' to whichever field
`supersonic-now-playing-cycle-fields' currently has up; customize this
to change that label's font, e.g. `:height' for its size or `:family'
for a different typeface, without touching the rest of the buffer."
  :group 'supersonic)

(defface supersonic-now-playing-art-overlay-label
  '((t :weight bold :foreground "white"))
  "Face for the label drawn onto the cover art itself.
Used by `supersonic-now-playing-animate-art-overlay'/`-scroll', as
opposed to `supersonic-now-playing-label'
for the plain-text side label -- this text is painted into the SVG
overlay image rather than shown as propertized buffer text, so only
`:weight', `:family' and `:foreground' carry over, onto the SVG text's
`font-weight'/`font-family'/`fill' respectively (see
`supersonic-art-overlay-font-weight'/`-font-family'/`-fill').  `:height'
does not: the pixel size is decided by `supersonic-art-overlay-font-size',
proportional to `supersonic-now-playing-art-size' rather than a fixed
point size, so the label keeps the same size relative to the art
whatever that is set to."
  :group 'supersonic)

(defface supersonic-now-playing-button
  '((t :box t :height 0.85))
  "Face for the transport buttons in the now-playing buffer.
Deliberately does not inherit the default `button' face: that one in
turn inherits `link', whose underline and link color read as a
hyperlink rather than a pushable control.  A plain box around the
default face is the ready-player look this is going for instead; hover
feedback still comes for free from `button-type-get's `mouse-face',
which is unaffected by this face.  `:height' is relative (a float),
scaling off whatever face this is displayed alongside, e.g.
`supersonic-now-playing-label' below the cover art, rather than a
fixed point size that would stop tracking it."
  :group 'supersonic)

(defface supersonic-albums-name
  '((t :inherit bold))
  "Face for album names in *supersonic-albums* and *supersonic-artist-albums*.
Customize this to change the font size, weight, or typeface of album
listings, e.g. via `M-x customize-face'."
  :group 'supersonic)

(defface supersonic-albums-artist
  '((t :inherit supersonic-albums-name))
  "Face for artist names in *supersonic-albums* and *supersonic-artist-albums*.
Inherits from `supersonic-albums-name' but can be customized separately
to distinguish artists from album names."
  :group 'supersonic)

(provide 'supersonic-custom)
;;; supersonic-custom.el ends here
