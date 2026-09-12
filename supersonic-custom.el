;;; supersonic-custom.el --- User options for supersonic.el -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
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

;; Every user option supersonic.el has, in one place, plus the
;; `supersonic' customization group they all belong to.
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

;;; Code:

(defgroup supersonic nil
  "Browse and play music from a Subsonic server."
  :prefix "supersonic-"
  :group 'external)

(defcustom supersonic-host ""
  "URL of the supersonic service, e.g. \"http://host:4533\".
Must match the \"machine\" field of the corresponding authinfo
entry verbatim; that entry's host is what is actually used to
build request URLs.  May be given without a scheme (\"http://\" or
\"https://\"), in which case \"https://\" is assumed."
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
even though both are keyed on a Subsonic id that could otherwise
coincide (e.g. a track and its own cover art id)."
  :type 'directory
  :group 'supersonic)

(defcustom supersonic-enable-art nil
  "Enable displaying album art in supported frames.
Also a statement about the frame itself, since supersonic only draws
art it can draw -- see `supersonic-art-available-p'."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-list-art-size 100
  "Height in pixels of the cover art in the album and podcast lists.
The art is also downloaded at this size, so raising it costs a
re-download of anything already cached at the old size."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-now-playing-art-size 300
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

(defcustom supersonic-enable-waveform nil
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
`supersonic-jukebox--interpolated-position'), so it still has
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

;; Defined in supersonic.el, which requires this file rather than the
;; other way around (see the Commentary above) -- declared here purely
;; to keep the byte-compiler quiet about the forward references below,
;; not to actually load them early.
(declare-function supersonic-now-playing-animate-label "supersonic")
(declare-function supersonic-now-playing-update-duration-field "supersonic")
(declare-function supersonic-now-playing-recolor-waveform "supersonic")
(declare-function supersonic-now-playing-maybe-fetch-waveform "supersonic")

(defcustom supersonic-now-playing-animation-functions (list #'supersonic-now-playing-animate-label)
  "Functions run in order on every animation tick to render the buffer.
Each is called as (FUNCTION BUFF DELTA), DELTA being the real seconds
elapsed since the last tick (0 for an unconditional first paint -- see
`supersonic-now-playing--render').  A function that signals an error is
reported rather than left to take the rest of the list -- and the timer
driving all of them -- down with it; see
`supersonic-now-playing--run-field-functions'.

Three are built in:

- `supersonic-now-playing-animate-label' (the default): updates the
  text label next to the cover art -- the only place a rotated field
  ever showed before this existed.  Only actually switches fields (and
  redraws) once `supersonic-now-playing-cycle-interval' seconds of
  DELTA have really accumulated, however many ticks that took.
- `supersonic-now-playing-animate-art-overlay': layers the field onto
  the cover art itself instead, in place of the side label, switching
  between fields (and gating its redraws) the same way the label does.
- `supersonic-now-playing-animate-art-overlay-scroll': also layers onto
  the art, but joins every field in `supersonic-now-playing-cycle-fields'
  into one line instead of switching between them one at a time.  Shown
  once, motionless, if that already fits; otherwise bounced left just
  far enough to read the far end and back, resting at each end -- see
  `supersonic-now-playing-scroll-step' and
  `supersonic-now-playing-scroll-pause'.

To show it in both places at once, list both rather than looking for a
fourth built-in for it -- there is no in-between behaviour left to name
that a plain combination of two of the above doesn't already cover.

See `supersonic-now-playing-position-functions' for this list's
counterpart: the same idea, but driven by the playback position the
active backend reports rather than by elapsed time."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-now-playing-position-functions
  (list #'supersonic-now-playing-update-duration-field
        #'supersonic-now-playing-recolor-waveform
        #'supersonic-now-playing-maybe-fetch-waveform)
  "Functions run in order whenever the now-playing buffer's position updates.
The position-driven counterpart to
`supersonic-now-playing-animation-functions': each is called as
(FUNCTION BUFF POSITION), POSITION being what the active backend just
reported rather than a DELTA -- a seek or a pause between two updates
must never show up as drift, which ruled out counting elapsed seconds
locally the way the animation side does.  Runs on
`supersonic-now-playing-interval''s regular timer and also right away
on a seek, via `supersonic-now-playing-maybe-update-position'.  Same
per-function error isolation as `supersonic-now-playing-animation-functions'.

Three are built in, and make up the default:

- `supersonic-now-playing-update-duration-field': redraws the
  \"Duration:\" line's elapsed/total split.
- `supersonic-now-playing-recolor-waveform': redraws the waveform
  seekbar's played/unplayed split, once a waveform is actually showing
  for the current track.
- `supersonic-now-playing-maybe-fetch-waveform': kicks off waveform
  generation for the current track, unless that has already been
  requested (also requires `supersonic-enable-waveform').

Dropping one of these from the list turns that behaviour off; adding a
function of your own -- synced lyrics keyed on position, say -- runs it
alongside the rest."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-now-playing-waveform-in-overlay nil
  "Layer the waveform seekbar onto the cover art instead of below it.
Requires both `supersonic-enable-art' and `supersonic-enable-waveform'
to actually show anything -- see
`supersonic-now-playing-animate-art-overlay'/`-scroll', which draw the
waveform into a lane above their text once this is on, the same way
they already draw that text onto the art instead of beside it.  The
combined image stays clickable to seek, exactly like the standalone
seekbar this replaces.

Only takes effect together with `supersonic-now-playing-animate-art-overlay'
or `-scroll' in `supersonic-now-playing-animation-functions': with
`supersonic-now-playing-animate-label', or for a track with no cover
art cached, nothing draws a waveform onto anything, and this option
just told `supersonic-now-playing--render' not to reserve a standalone
line for one either -- so the waveform would not appear anywhere at
all.  `supersonic-waveform-width'/`supersonic-waveform-height' do not
apply to the composited lane either; it is sized off
`supersonic-now-playing-art-size' instead, the same as the text scrim
is."
  :type 'boolean
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

(defcustom supersonic-playback-backend 'mpv
  "Which playback backend plays what supersonic.el is asked to play.
The symbol a backend registered itself under via
`supersonic-playback-register-backend'; every play, enqueue and
transport command dispatches to it.  Only `mpv' is built in, and
selecting a backend whose file has not been loaded is reported when a
playback command is next used."
  :type '(choice (const :tag "Local mpv process" mpv) (symbol :tag "Other registered backend"))
  :group 'supersonic)

(defcustom supersonic-mpv-timeout 0.5
  "Seconds to wait when starting or killing the mpv process."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-jukebox-poll-interval 3
  "Seconds between jukeboxControl polls while `jukebox' is the active backend.
The Subsonic API has no push mechanism for jukebox state, so
`supersonic-jukebox.el' polls `action=get' on this interval instead,
caching the result and firing the generalized playback hooks whenever
the cached snapshot changes; the jukebox backend's status accessor and
queue listing both answer from that cache rather than issuing a fresh
request of their own. Lower values notice a track change or a pause
toggled from another client sooner, at the cost of one request to the
server per interval."
  :type 'number
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

(defface supersonic-now-playing-button
  '((t :box t :height 0.85))
  "Face for the transport buttons in the now-playing buffer.
Deliberately does not inherit the default `button' face: that one in
turn inherits `link', whose underline and link color read as a
hyperlink rather than a pushable control.  A plain box around the
default face is the ready-player look this is going for instead; hover
feedback still comes for free from `button-type-get's `mouse-face',
which is unaffected by this face.  `:height' is relative (a float),
scaling off whatever face this is displayed alongside, e.g. the label's
`bold' next to the cover art, rather than a fixed point size that would
stop tracking it."
  :group 'supersonic)

(provide 'supersonic-custom)
;;; supersonic-custom.el ends here
