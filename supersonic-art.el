;;; supersonic-art.el --- Cover art cache layer for supersonic.el -*- lexical-binding: t; -*-

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

;; Cover art caching for supersonic.el: naming/locating the on-disk
;; cache, fetching art through the active provider's `cover-art' with
;; bounded concurrency, and painting it into a tabulated-list buffer
;; column.  Art is always looked up by an item's `:art' reference,
;; which the provider hands out and which need not be the item's id.

;;; Code:
(require 'cl-lib)
(require 'aio)
(require 'svg)
(require 'url-util)
(require 'tabulated-list)
(require 'text-property-search)
(require 'supersonic-custom)
(require 'supersonic-provider)
;; Only for `supersonic-waveform-bars'/`supersonic-waveform-svg-bars': the
;; bucket geometry and SVG drawing a waveform layered onto the cover art
;; needs (see `supersonic-art-overlay-propertize') are owned by that file,
;; the same way the (PEAKS . RMS) envelope shape they operate on is --
;; this file otherwise knows nothing about waveforms, the same way that
;; one otherwise knows nothing about the now-playing buffer.
(require 'supersonic-waveform)

(defcustom supersonic-art-overlay-layers
  (list #'supersonic-art-overlay-layer-art
        #'supersonic-art-overlay-layer-scrim
        #'supersonic-art-overlay-layer-waveform
        #'supersonic-art-overlay-layer-text)
  "Layers composited into `supersonic-art-overlay-propertize's SVG, in order.
Each is called as (FUNCTION CTX), CTX being the shared layout context
`supersonic-art-overlay--context' builds once per call -- geometry
\(scrim height, text baseline, whether a waveform lane is reserved at
all) is decided there from what is present in this list and passed in,
not from where in the list a layer happens to sit, so reordering this
list changes stacking without changing layout.  First in the list
draws first, so ends up on the bottom; the default order -- art, then
the scrim, then the waveform lane, then text -- is what always drew,
before this existed to be configurable.

Reordering this list moves a layer's position in the final image, e.g.
listing `supersonic-art-overlay-layer-text' ahead of
`supersonic-art-overlay-layer-waveform' to draw the waveform lane over
the label rather than below it.  Dropping a layer removes it from the
image entirely, including the room `supersonic-art-overlay-layer-scrim'
would otherwise reserve for it -- see `supersonic-art-overlay--context'.
Adding a function of your own -- a border, a gradient -- draws it
alongside the rest, reading CTX's `:svg' and whichever other keys it
needs.

See `supersonic-art-overlay-scroll-layers' for the scrolling variant's
counterpart, used instead whenever
`supersonic-now-playing-animate-art-overlay-scroll' is the one drawing."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-art-overlay-scroll-layers
  (list #'supersonic-art-overlay-layer-art
        #'supersonic-art-overlay-layer-scrim
        #'supersonic-art-overlay-layer-waveform
        #'supersonic-art-overlay-layer-text-scroll)
  "Layers composited into `supersonic-art-overlay-scroll-propertize's SVG.
The scrolling variant of `supersonic-art-overlay-layers', used whenever
`supersonic-now-playing-animate-art-overlay-scroll' is the one drawing
rather than `supersonic-now-playing-animate-art-overlay' -- everything
that variable's docstring says about reordering and dropping layers
applies here identically, the one difference being the last entry:
`supersonic-art-overlay-layer-text-scroll' instead of
`-layer-text', which is the one layer function this list draws with
that the other one has no equivalent of."
  :type '(repeat function)
  :group 'supersonic)

(defun supersonic-art-available-p ()
  "Return non-nil if cover art can actually be shown right now.
Mirrors `supersonic-waveform-available-p'.  Callers that only download
art should test this too: a frame that cannot draw the image has no
use for the file either.  Nor is there art for a provider without a
`cover-art' operation -- lists and now-playing then simply go without."
  (and supersonic-enable-art (display-graphic-p) (supersonic-provider-supports-p 'cover-art)))

(defun supersonic-art-cache-file (id size)
  "Return the path cover art ID is cached under when fetched at SIZE.
ID is an `:art' reference.  It is named via
`supersonic-provider-cache-name', which makes any ID a single valid
file name and keeps different providers' and servers' art apart.
The size is part of the file name because the same art is shown at
different sizes in different buffers (see `supersonic-list-art-size'
and `supersonic-now-playing-art-size'): sharing one file per art id
would hand whichever buffer asked second the other one's resolution,
and would silently keep serving the old resolution after either
setting is changed.  Prefixed with \"art-\": `supersonic-cache-path' is
shared with `supersonic-waveform-cache-file', whose own ID could
otherwise coincide with this one (e.g. a track and its own cover art
id) and collide on the same file name."
  (expand-file-name (format "art-%s-%d" (supersonic-provider-cache-name id) size) supersonic-cache-path))

(defun supersonic-image-propertize (id size &optional max-width)
  "Generate a property displaying cover art ID at SIZE pixels high.
With MAX-WIDTH, an image wider than that many pixels at that height is
scaled down further to fit, keeping its proportions."
  (propertize " " 'display (apply #'create-image (supersonic-art-cache-file id size) nil nil :height size
                                  (and max-width (list :max-width max-width)))))

(defun supersonic-art-overlay-font-size (size)
  "Return the pixel font size the art overlay scrim's text is drawn at for SIZE.
Shared by `supersonic-art-overlay-propertize',
`supersonic-art-overlay-scroll-propertize' and
`supersonic-art-scroll-max-offset', so the figure a decision (does TEXT
fit without scrolling?) is made against always matches the figure
actually drawn with."
  (max 10 (round (* size 0.07))))

(defun supersonic-art-overlay-font-weight ()
  "Return the SVG `font-weight' for the art overlay label face.
Derived from `supersonic-now-playing-art-overlay-label's
`:weight'.  SVG text only understands \"bold\"/\"normal\", so any of Emacs's
finer-grained weight symbols (`semi-bold', `extra-bold', ...) collapses
to whichever of the two it reads closer to."
  (if (memq (face-attribute 'supersonic-now-playing-art-overlay-label :weight)
            '(bold semi-bold extra-bold ultra-bold))
      "bold"
    "normal"))

(defun supersonic-art-overlay-font-family ()
  "Return `supersonic-now-playing-art-overlay-label's `:family', or nil if unset.
Nil leaves the SVG text without a `font-family' attribute altogether,
so the renderer falls back to its own default, rather than passing on
whatever `face-attribute' would otherwise resolve an unset `:family' to
\(typically the selected frame's default font\), which is unlikely to be
installed wherever the SVG image is actually rendered."
  (let ((family (face-attribute 'supersonic-now-playing-art-overlay-label :family)))
    (unless (eq family 'unspecified) family)))

(defun supersonic-art-overlay-fill ()
  "Return the SVG `fill' color for the art overlay label face.
Taken from `supersonic-now-playing-art-overlay-label's `:foreground'."
  (let ((foreground (face-attribute 'supersonic-now-playing-art-overlay-label :foreground)))
    (if (eq foreground 'unspecified) "white" foreground)))

(defun supersonic-art-overlay-waveform-lane-height (size)
  "Return how tall the waveform lane below the text row is, at SIZE.
Only added to the scrim at all when there is going to be a lane in the
first place -- see `supersonic-art-overlay-waveform-lane-p' for the two
halves of that -- and independent of
`supersonic-art-overlay-font-size', so a track's text sits at the same
size whether or not a waveform lane is drawn below it, just higher up
in the (now taller) scrim to make room."
  (round (* size 0.16)))

(defun supersonic-art-overlay-waveform-lane-p (waveform layers)
  "Return non-nil if LAYERS will actually draw WAVEFORM as a lane in the scrim.
Both halves have to hold: WAVEFORM itself must be non-nil (there is an
envelope to draw at all) and `supersonic-art-overlay-layer-waveform'
must be a member of LAYERS (the caller's resolved
`supersonic-art-overlay-layers'/`-scroll-layers', whichever list is
about to be run) -- the layer draws nothing without the data, and the
data draws nothing without the layer.

The single answer everything that has to agree about the lane asks:
`supersonic-art-overlay--context' for whether to reserve room for it,
and `supersonic-now-playing--maybe-seekable' for whether the finished
image is worth making clickable to seek.  Those two used to test the
condition separately, which is exactly how they came apart -- a
customization that dropped the layer kept handing back cover art that
still seeked on click, with no lane under the pointer to seek in."
  (and waveform (memq #'supersonic-art-overlay-layer-waveform layers) t))

(defun supersonic-art-overlay-text-row-p (layers)
  "Return non-nil if LAYERS will actually draw a text row in the scrim.
True for either of the two built-in text layers --
`supersonic-art-overlay-layer-text' (in `supersonic-art-overlay-layers')
and `supersonic-art-overlay-layer-text-scroll' (in
`-scroll-layers') -- since LAYERS is whichever of the two lists is
about to be run, and each carries its own variant of the same row.
The text counterpart to `supersonic-art-overlay-waveform-lane-p', and
read for the same reason: `supersonic-art-overlay--context' reserves
the text row's height only when something is going to draw in it."
  (and (or (memq #'supersonic-art-overlay-layer-text layers)
           (memq #'supersonic-art-overlay-layer-text-scroll layers))
       t))

(defun supersonic-art-scroll-text-width (text font-size)
  "Measure TEXT's rendered width in pixels at FONT-SIZE.
`svg.el' has no way to ask the image back how wide TEXT came out once
drawn -- the scrolling overlay draws into an image that never becomes
one -- but the selected frame's own font metrics
\(`string-pixel-width', added in Emacs 29.1) are a real answer rather
than a guess: a flat per-character multiplier used to be all this had,
and a title's actual mix of narrow (\"i\", \"l\", a space) and wide
\(\"m\", \"w\") characters swung that guess wide enough to call text
that visually fit worth scrolling anyway.  Measured against
`supersonic-now-playing-art-overlay-label's `:weight' and `:family'
rather than a fixed `bold', so a customization that widens or narrows
the actual glyphs (a different typeface, a lighter weight) moves the
scrolling threshold along with it instead of leaving it measured
against a font nothing is drawn in any more.  Falls back to 0.6 of
FONT-SIZE per character on Emacs versions before `string-pixel-width'
existed -- this package still supports 28.1, see the
\"Package-Requires\" header in supersonic.el -- where that flat guess
is the best available without it."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width
       (propertize text 'face
                    (list :weight (face-attribute 'supersonic-now-playing-art-overlay-label :weight)
                          :font (font-spec :size font-size :family (supersonic-art-overlay-font-family)))))
    (* (length text) font-size 0.6)))

(defun supersonic-art-scroll-pad (size)
  "Return the pixel margin the scrolling text keeps clear of the edges.
Scales with SIZE, the cover art's edge length in pixels.
Shared by `supersonic-art-overlay-scroll-propertize' and
`supersonic-art-scroll-max-offset', for the same reason
`supersonic-art-overlay-font-size' is."
  (round (* size 0.04)))

(defun supersonic-art-scroll-max-offset (size text)
  "Return how many pixels TEXT overflows the scrim's padded width at SIZE.
0 if TEXT fits already -- `supersonic-now-playing-animate-art-overlay-scroll'
draws it once, statically, rather than scrolling at all in that case.
Otherwise, the pixel distance between showing TEXT's left edge (OFFSET
0 in `supersonic-art-overlay-scroll-propertize') and showing its right
edge, which is the far end `supersonic-now-playing--advance-scroll'
bounces `supersonic-now-playing--scroll-offset' out to before turning
back."
  (let* ((font-size (supersonic-art-overlay-font-size size))
         (available (- size (* 2 (supersonic-art-scroll-pad size)))))
    (max 0 (round (- (supersonic-art-scroll-text-width text font-size) available)))))

(defun supersonic-art-overlay--context (id size text waveform layers &optional offset)
  "Build the layout CTX `supersonic-art-overlay-layers'/`-scroll-layers' draw from.
Every geometry figure a layer might need -- the scrim's height and top
edge, the text row's baseline, the waveform lane's height, the
scrolling variant's left padding -- is computed once here, up front,
rather than by whichever layer happens to run first: layers only ever
read these, never derive them from what some other layer already drew,
so the same CTX produces the same picture whatever order
`supersonic-art-overlay-layers' lists them in.

The scrim's height is presence-driven rather than order-driven: it is
the text row plus the waveform lane, counting only the ones LAYERS is
actually going to draw (see `supersonic-art-overlay-text-row-p' and
`supersonic-art-overlay-waveform-lane-p' for what each of those two
takes).  Dropping either layer reclaims its share of the scrim instead
of leaving a permanently empty band behind, the same way dropping any
other layer leaves no trace of it; dropping both leaves no scrim at
all.  A custom replacement layer under a different name is not
recognized for this and gets no room reserved for it; write one that
reserves its own space via a :before-ish layer earlier in LAYERS if
that matters.

ID is the cover art id the background image is looked up under, at SIZE.
TEXT is the label the text row draws, WAVEFORM the envelope the
waveform lane draws; either may be nil when that layer has nothing to
show.

OFFSET is only meaningful to `supersonic-art-overlay-layer-text-scroll'
and defaults to 0 for the static variant, which never reads it."
  (let* ((file (supersonic-art-cache-file id size))
         (text-height
          (if (supersonic-art-overlay-text-row-p layers)
              (round (* size 0.22))
            0))
         (lane-height
          (if (supersonic-art-overlay-waveform-lane-p waveform layers)
              (supersonic-art-overlay-waveform-lane-height size)
            0))
         (scrim-height (+ text-height lane-height))
         (scrim-y (- size scrim-height)))
    (list :svg (svg-create size size)
          :size size :text text :waveform waveform :offset (or offset 0)
          :file file :mime (format "image/%s" (image-type-from-file-header file))
          :text-height text-height :lane-height lane-height
          :scrim-height scrim-height :scrim-y scrim-y
          :font-size (supersonic-art-overlay-font-size size)
          :baseline-y (+ scrim-y (/ text-height 2))
          :pad (supersonic-art-scroll-pad size))))

(defun supersonic-art-overlay-layer-art (ctx)
  "Draw CTX's cached cover art, filling the whole canvas.
The one layer every other built-in layer implicitly draws on top of --
first in both `supersonic-art-overlay-layers' and `-scroll-layers' by
default -- but a layer like any other, so a customization is free to
list something ahead of it (a background color showing through where
`supersonic-art-overlay-layer-scrim' isn't opaque, say) or drop it
altogether.

Referenced by file name rather than embedded with `svg-embed': the
overlay is redrawn on every waveform bucket change, and an embedded
cover was base64-encoded, printed into the SVG and decoded again by
librsvg each time -- most of the redraw's cost, for an image that never
changes.  The name resolves against the `:base-uri' both
`supersonic-art-overlay-propertize' and `-scroll-propertize' set to
the same file, which also keeps it within the directory librsvg allows
an SVG to load files from."
  (dom-append-child
   (plist-get ctx :svg)
   (dom-node
    'image
    `((xlink:href . ,(url-hexify-string (file-name-nondirectory (plist-get ctx :file))))
      (width . ,(plist-get ctx :size))
      (height . ,(plist-get ctx :size))))))

(defun supersonic-art-overlay-layer-scrim (ctx)
  "Draw the semi-opaque scrim CTX's text and waveform lane sit on.
Sized to CTX's `:scrim-height', which already accounts for which of
those two are actually going to be drawn -- see
`supersonic-art-overlay--context'.  Left with nothing to cover, and so
drawn at zero height, when neither is."
  (svg-rectangle (plist-get ctx :svg) 0 (plist-get ctx :scrim-y)
                 (plist-get ctx :size) (plist-get ctx :scrim-height)
                 :fill "black" :fill-opacity 0.55))

(defun supersonic-art-overlay-layer-waveform (ctx)
  "Draw CTX's waveform bars into the lane reserved for them, if CTX has any.
No-op when CTX's `:waveform' is nil -- see `supersonic-art-overlay--context'
for what reserves the lane in the first place.  `supersonic-waveform-width'/
`-height' do not apply to this lane; it is sized off
`supersonic-now-playing-art-size' instead, the same as the scrim is."
  (let ((waveform (plist-get ctx :waveform))
        (size (plist-get ctx :size))
        (lane-height (plist-get ctx :lane-height)))
    (when waveform
      (supersonic-waveform-svg-bars
       (plist-get ctx :svg) (supersonic-waveform-bars (car waveform) (cdr waveform) size lane-height)
       (- size lane-height) lane-height))))

(defun supersonic-art-overlay-layer-text (ctx)
  "Draw CTX's text centered in its text row.
The static counterpart to `supersonic-art-overlay-layer-text-scroll',
used by `supersonic-art-overlay-layers'."
  (let ((family (supersonic-art-overlay-font-family)))
    (apply #'svg-text (plist-get ctx :svg) (plist-get ctx :text)
           :x (/ (plist-get ctx :size) 2) :y (plist-get ctx :baseline-y)
           :fill (supersonic-art-overlay-fill) :font-size (plist-get ctx :font-size)
           :font-weight (supersonic-art-overlay-font-weight)
           :text-anchor "middle" :dominant-baseline "middle"
           (and family (list :font-family family)))))

(defun supersonic-art-overlay-layer-text-scroll (ctx)
  "Draw CTX's text left-aligned, shifted by CTX's `:offset', clipped to its row.
The scrolling counterpart to `supersonic-art-overlay-layer-text', used
by `supersonic-art-overlay-scroll-layers' -- see
`supersonic-art-overlay-scroll-propertize' for what feeds `:offset'.
Clipped to its own text row so text never draws outside of it,
whichever edge is currently cut off; a waveform lane below that row, if
CTX has one, is left unclipped, since nothing ever scrolls there."
  (let* ((svg (plist-get ctx :svg))
         (size (plist-get ctx :size))
         (clip (svg-clip-path svg :id "supersonic-art-scroll-clip"))
         (family (supersonic-art-overlay-font-family)))
    (svg-rectangle clip 0 (plist-get ctx :scrim-y) size (plist-get ctx :text-height))
    (apply #'svg-text svg (plist-get ctx :text)
           :x (- (plist-get ctx :pad) (plist-get ctx :offset)) :y (plist-get ctx :baseline-y)
           :fill (supersonic-art-overlay-fill) :font-size (plist-get ctx :font-size)
           :font-weight (supersonic-art-overlay-font-weight)
           :text-anchor "start" :dominant-baseline "middle"
           :clip-path "url(#supersonic-art-scroll-clip)"
           (and family (list :font-family family)))))

(defun supersonic-art-overlay-propertize (id size text &optional waveform)
  "Generate a property displaying cover art ID at SIZE with TEXT layered over it.
TEXT sits in a semi-opaque scrim across the bottom, composited in with
`svg.el' rather than shown as a string alongside the art the way
`supersonic-image-propertize' is used for -- the point of this one is
text that reads as part of the cover itself, the way a lock-screen
\"now playing\" widget overlays a track name on the art instead of
setting it beside it.  Like `supersonic-image-propertize', expects the
art to be cached at SIZE already.

The actual drawing is delegated to `supersonic-art-overlay-layers', run
in order over a shared context built by `supersonic-art-overlay--context'
-- this function only builds that context and unwraps the finished SVG
into a display property afterwards.

WAVEFORM, if given, is (ENVELOPE . PROGRESS) as `supersonic-waveform-propertize'
takes them; when given, `supersonic-art-overlay-layer-waveform' (in the
default `supersonic-art-overlay-layers') draws a lane of waveform bars
\(`supersonic-waveform-svg-bars') below TEXT, right at the bottom edge
of the art, extending the scrim by `supersonic-art-overlay-waveform-lane-height'
to fit it."
  (let ((ctx (supersonic-art-overlay--context id size text waveform supersonic-art-overlay-layers)))
    (dolist (layer supersonic-art-overlay-layers)
      (funcall layer ctx))
    (propertize " " 'display (svg-image (plist-get ctx :svg) :base-uri (plist-get ctx :file)))))

(defun supersonic-art-overlay-scroll-propertize (id size text offset &optional waveform)
  "Generate a property showing TEXT over cover art ID at SIZE, OFFSET pixels in.
Like `supersonic-art-overlay-propertize', but left-aligned and
left by OFFSET pixels instead of centered and fixed in place --
`supersonic-now-playing-animate-art-overlay-scroll' bounces OFFSET
between 0 and `supersonic-art-scroll-max-offset' (see that function) to
reveal TEXT a little at a time when it does not fit in one line; this
function only ever draws the single frame it is given for whatever
OFFSET that is.

Delegates to `supersonic-art-overlay-scroll-layers' the same way
`supersonic-art-overlay-propertize' delegates to `supersonic-art-overlay-layers'
-- see that function's docstring.

WAVEFORM is as in `supersonic-art-overlay-propertize'."
  (let ((ctx (supersonic-art-overlay--context
              id size text waveform supersonic-art-overlay-scroll-layers offset)))
    (dolist (layer supersonic-art-overlay-scroll-layers)
      (funcall layer ctx))
    (propertize " " 'display (svg-image (plist-get ctx :svg) :base-uri (plist-get ctx :file)))))

(aio-defun
 supersonic--fetch-art (id size)
 "Ensure cover art ID is cached on disk at SIZE, fetching it if necessary.
ID is an `:art' reference, fetched through `supersonic-provider-cover-art'.
Returns a promise that resolves once the art is cached, and rejects if
it could not be fetched."
 (let ((file (supersonic-art-cache-file id size)))
   (unless (file-exists-p file)
     (let ((data (aio-await (supersonic-provider-cover-art id size))))
       (unless (file-exists-p supersonic-cache-path)
         (mkdir supersonic-cache-path t))
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert data)
         ;; Cover art is arbitrary binary image data, not text -- write the bytes as-is instead of letting Emacs
         ;; guess (and possibly prompt for) a coding system.
         (let ((coding-system-for-write 'no-conversion))
           (write-region nil nil file nil 'no-message)))))))

(aio-defun
 supersonic--fetch-art-throttled (sem id size)
 "Fetch cover art ID at SIZE once SEM hands out a slot.
Callers create every fetch up front, so the semaphore -- not the number
of entries -- is what decides how many requests are on the wire at once
\(see `supersonic-art-fetch-concurrency').  Failures are swallowed here
rather than left to reject, so that a slot is always given back and one
unreachable cover doesn't hold up the rest."
 (aio-await (aio-sem-wait sem)) (aio-await (aio-catch (supersonic--fetch-art id size))) (aio-sem-post sem))

(aio-defun
 supersonic-get-images (entries arts n buff)
 "Fetch/cache cover art for ENTRIES and paint it into column N of BUFF.
ARTS holds each entry's `:art' reference, in the same order as
ENTRIES; an entry whose reference is nil is left without art.  Taken
separately because an entry's id is the item's, and a provider's art
reference need not be the same thing.
Fetches are fired up front, before anything is awaited, but only
`supersonic-art-fetch-concurrency' of them run at a time; individual
failures are tolerated, leaving those entries without art rather than
aborting the rest.  BUFF is (re)printed once every fetch has settled,
so callers don't need to print again themselves."
 (if (not (supersonic-art-available-p))
     (dolist (entry entries)
       (aset (nth 1 entry) n ""))
   (let* ((sem (aio-sem supersonic-art-fetch-concurrency))
          (pending
           (cl-mapcar
            (lambda (entry art)
              (list entry art (and art (supersonic--fetch-art-throttled sem art supersonic-list-art-size))))
            entries arts)))
     (pcase-dolist (`(,entry ,art ,promise) pending)
       (when promise
         (aio-await promise)
         (when (file-exists-p (supersonic-art-cache-file art supersonic-list-art-size))
           (aset (nth 1 entry) n (supersonic-image-propertize art supersonic-list-art-size)))))))
 (when (buffer-live-p buff)
   (with-current-buffer buff
     (when (derived-mode-p 'tabulated-list-mode)
       (tabulated-list-print t)))))

;;; Art filled in as rows come into view

(defvar-local supersonic-art--lazy nil
  "What `supersonic-art-fill-visible' fills into this buffer, or nil.
A plist: `:column' and `:size' the art goes into and is shown at,
`:arts' a table from each item's id to its `:art' reference, and
`:requested' the ids whose art was asked for and not given up on.  A
fresh plist for every fill, so a fetch can tell by `eq' whether the
list it was started for is still the one shown.")

(defvar-local supersonic-art--lazy-sem nil
  "The semaphore this buffer's `supersonic-art-fill-visible' fetches share.
Permanently local, so that neither a refresh nor the list being opened
anew, which turns its mode on again, gives the fetches of the new list
a second set of `supersonic-art-fetch-concurrency' slots next to the
ones the old list's fetches still hold.  It is made with the value
that setting has the first time; a new value takes a new buffer.")
(put 'supersonic-art--lazy-sem 'permanent-local t)

(defvar-local supersonic-art--lazy-timer nil
  "The timer a scroll of this buffer set to ask for the art it brought in view.")

(defun supersonic-art-fill-visible (items n size buff)
  "Fill column N of BUFF's rows with ITEMS' art at SIZE as they come into view.
ITEMS are the facade plists the rows were made from; each row is
matched to its item by `:id', so the order of the rows, which sorting
the list changes, does not matter.  Unlike `supersonic-get-images',
which fetches the art of every entry up front, only the rows in view
are fetched for, at most `supersonic-art-fetch-concurrency' at a time,
and the rest follow as they are scrolled to -- for a list such as the
artists, which holds the whole library.  A row scrolled away before
its fetch got a slot is skipped, to be asked for again when it comes
back.  Each row is redrawn on its own as its art arrives, at most SIZE
pixels wide.  BUFF must already be printed."
  (when (and (buffer-live-p buff) (supersonic-art-available-p))
    (with-current-buffer buff
      (let ((table (make-hash-table :test #'equal)))
        (dolist (item items)
          (when (plist-get item :art)
            (puthash (plist-get item :id) (plist-get item :art) table)))
        (setq supersonic-art--lazy
              (list :column n :size size :arts table :requested (make-hash-table :test #'equal))))
      (unless supersonic-art--lazy-sem
        (setq supersonic-art--lazy-sem (aio-sem supersonic-art-fetch-concurrency)))
      (add-hook 'window-scroll-functions #'supersonic-art--on-scroll nil t)
      (add-hook 'post-command-hook #'supersonic-art--fill-windows nil t)
      (supersonic-art--fill-windows))))

(defun supersonic-art--on-scroll (_window _start)
  "Ask for the art a scroll brought in view, once redisplay is done.
On `window-scroll-functions', which runs in the middle of redisplay:
art already cached would be drawn right away, and changing the height
of the rows there would pull the ground from under the redisplay that
just placed them."
  (unless supersonic-art--lazy-timer
    (let ((buff (current-buffer)))
      (setq supersonic-art--lazy-timer
            (run-at-time 0 nil (lambda ()
                                 (when (buffer-live-p buff)
                                   (with-current-buffer buff
                                     (setq supersonic-art--lazy-timer nil)
                                     (supersonic-art--fill-windows)))))))))

(defun supersonic-art--in-view (window)
  "Return the ids of the rows in view in WINDOW, top to bottom.
Taken as many rows from `window-start' on as WINDOW has text lines,
since `window-end' is not up to date before redisplay: too many when
rows are art-high, never too few."
  (with-current-buffer (window-buffer window)
    (let ((ids nil))
      (save-excursion
        (goto-char (window-start window))
        (dotimes (_ (window-body-height window))
          (let ((id (tabulated-list-get-id)))
            (when id (push id ids)))
          (forward-line 1)))
      (nreverse ids))))

(defun supersonic-art--in-view-p (id)
  "Return non-nil if the row of ID is in view in a window on the current buffer.
In view as `supersonic-art--in-view' has it."
  (seq-some (lambda (window) (member id (supersonic-art--in-view window)))
            (get-buffer-window-list (current-buffer) nil t)))

(defun supersonic-art--fill-windows ()
  "Ask for the art of the rows in view in each window on the current buffer."
  (when supersonic-art--lazy
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (mapc #'supersonic-art--request (supersonic-art--in-view window)))))

(defun supersonic-art--request (id)
  "Start filling in the art of the row of ID, unless asked for already."
  (let* ((lazy supersonic-art--lazy)
         (art (gethash id (plist-get lazy :arts)))
         (requested (plist-get lazy :requested)))
    (when (and art (not (gethash id requested)))
      (puthash id t requested)
      (ignore (supersonic-art--fill-row (current-buffer) lazy id art)))))

(defun supersonic-art--current-p (buff lazy)
  "Return non-nil if BUFF is live and still filled under LAZY."
  (and (buffer-live-p buff) (eq lazy (buffer-local-value 'supersonic-art--lazy buff))))

(aio-defun
 supersonic-art--fill-row (buff lazy id art)
 "Fetch ART for the row of ID in BUFF, then redraw that row with it.
LAZY is the `supersonic-art--lazy' the request was made under.  Art
already cached is shown without waiting for a slot.  A request that
gets its slot only after BUFF was refilled is dropped unfetched: it
belongs to a list no longer shown, and the new one asks again for
whatever is in view.  One whose row was scrolled away meanwhile is
dropped too, and forgotten, so that it is asked for again when the
row is back in view -- rather than holding up the rows in view now.
Failures leave the row without art."
 (let ((size (plist-get lazy :size)))
   (unless (file-exists-p (supersonic-art-cache-file art size))
     (let ((sem (buffer-local-value 'supersonic-art--lazy-sem buff)))
       (aio-await (aio-sem-wait sem))
       (when (supersonic-art--current-p buff lazy)
         (if (with-current-buffer buff (supersonic-art--in-view-p id))
             (aio-await (aio-catch (supersonic--fetch-art art size)))
           (remhash id (plist-get lazy :requested))))
       (aio-sem-post sem)))
   (when (and (supersonic-art--current-p buff lazy)
              (file-exists-p (supersonic-art-cache-file art size)))
     (with-current-buffer buff
       (supersonic-art--redraw-row id (plist-get lazy :column) (supersonic-image-propertize art size size))))))

(defun supersonic-art--redraw-row (id n cell)
  "Set column N of the entry ID to CELL and redraw just its row.
Reprinting the whole list for every image that arrives would cost as
much as the list is long, and `tabulated-list-set-col' cannot fill a
cell printed empty, which leaves no text to replace.  Point, if on
that row, keeps its column."
  (let ((entry (assoc id tabulated-list-entries)))
    (when entry
      (aset (cadr entry) n cell)
      (let ((column (and (equal id (tabulated-list-get-id)) (current-column))))
        (save-excursion
          (goto-char (point-min))
          (let ((match (text-property-search-forward 'tabulated-list-id id t)))
            (when match
              (goto-char (prop-match-beginning match))
              (let ((inhibit-read-only t))
                (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
                (funcall tabulated-list-printer id (cadr entry))))))
        (when column
          (move-to-column column))))))

(provide 'supersonic-art)
;;; supersonic-art.el ends here
