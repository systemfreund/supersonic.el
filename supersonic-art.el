;;; supersonic-art.el --- Cover art cache layer for supersonic.el -*- lexical-binding: t; -*-

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

;; Cover art caching for supersonic.el: naming/locating the on-disk
;; cache, fetching art from the Subsonic server with bounded
;; concurrency, and painting it into a tabulated-list buffer column.

;;; Code:
(require 'url)
(require 'aio)
(require 'svg)
(require 'supersonic-custom)
(require 'supersonic-api)
;; Only for `supersonic-waveform-bars'/`supersonic-waveform-svg-bars': the
;; bucket geometry and SVG drawing a waveform layered onto the cover art
;; needs (see `supersonic-art-overlay-propertize') are owned by that file,
;; the same way the (PEAKS . RMS) envelope shape they operate on is --
;; this file otherwise knows nothing about waveforms, the same way that
;; one otherwise knows nothing about the now-playing buffer.
(require 'supersonic-waveform)
(defvar url-http-end-of-headers)

(defun supersonic-art-available-p ()
  "Return non-nil if cover art can actually be shown right now.
Mirrors `supersonic-waveform-available-p'.  Callers that only download
art should test this too: a frame that cannot draw the image has no
use for the file either."
  (and supersonic-enable-art (display-graphic-p)))

(defun supersonic-art-cache-file (id size)
  "Return the path cover art ID is cached under when fetched at SIZE.
The size is part of the file name because the same art is shown at
different sizes in different buffers (see `supersonic-list-art-size'
and `supersonic-now-playing-art-size'): sharing one file per art id
would hand whichever buffer asked second the other one's resolution,
and would silently keep serving the old resolution after either
setting is changed.  Prefixed with \"art-\": `supersonic-cache-path' is
shared with `supersonic-waveform-cache-file', whose own ID could
otherwise coincide with this one (e.g. a track and its own cover art
id) and collide on the same file name."
  (expand-file-name (format "art-%s-%d" id size) supersonic-cache-path))

(defun supersonic-image-propertize (id size)
  "Generate a property displaying cover art ID at SIZE pixels high."
  (propertize " " 'display (create-image (supersonic-art-cache-file id size) nil nil :height size)))

(defun supersonic-art-overlay-font-size (size)
  "Return the pixel font size the art overlay scrim's text is drawn at for SIZE.
Shared by `supersonic-art-overlay-propertize',
`supersonic-art-overlay-scroll-propertize' and
`supersonic-art-scroll-max-offset', so the figure a decision (does TEXT
fit without scrolling?) is made against always matches the figure
actually drawn with."
  (max 10 (round (* size 0.07))))

(defun supersonic-art-overlay-waveform-lane-height (size)
  "Return how tall the waveform lane above the text row is, at SIZE.
Only added to the scrim at all when the overlay propertize functions
are actually given a WAVEFORM argument -- independent of
`supersonic-art-overlay-font-size', so a track's text sits at the same
size and the same distance from the bottom edge whether or not a
waveform lane is drawn above it."
  (round (* size 0.16)))

(defun supersonic-art-overlay-propertize (id size text &optional waveform)
  "Generate a property displaying cover art ID at SIZE with TEXT layered over it.
TEXT sits in a semi-opaque scrim across the bottom, composited in with
`svg.el' rather than shown as a string alongside the art the way
`supersonic-image-propertize' is used for -- the point of this one is
text that reads as part of the cover itself, the way a lock-screen
\"now playing\" widget overlays a track name on the art instead of
setting it beside it.  Like `supersonic-image-propertize', expects the
art to be cached at SIZE already.

WAVEFORM, if given, is (ENVELOPE . PROGRESS) as `supersonic-waveform-propertize'
takes them; when given, a lane of waveform bars
\(`supersonic-waveform-svg-bars') is drawn between the scrim and TEXT,
extending the scrim by `supersonic-art-overlay-waveform-lane-height' to
fit it, so the result reads as art, then waveform, then text, stacked
in that order, instead of TEXT sitting directly on the scrim the way it
does without one."
  (let* ((file (supersonic-art-cache-file id size))
         (mime (format "image/%s" (image-type-from-file-header file)))
         (svg (svg-create size size))
         (text-height (round (* size 0.22)))
         (lane-height (if waveform (supersonic-art-overlay-waveform-lane-height size) 0))
         (scrim-height (+ text-height lane-height))
         (font-size (supersonic-art-overlay-font-size size)))
    (svg-embed svg file mime nil :width size :height size)
    (svg-rectangle svg 0 (- size scrim-height) size scrim-height :fill "black" :fill-opacity 0.55)
    (when waveform
      (supersonic-waveform-svg-bars
       svg (supersonic-waveform-bars (car waveform) (cdr waveform) size lane-height) (- size scrim-height)
       lane-height))
    (svg-text svg text
              :x (/ size 2) :y (- size (/ text-height 2))
              :fill "white" :font-size font-size :font-weight "bold"
              :text-anchor "middle" :dominant-baseline "middle")
    (propertize " " 'display (svg-image svg))))

(defun supersonic-art-scroll-text-width (text font-size)
  "Measure TEXT's rendered width in pixels at FONT-SIZE.
`svg.el' has no way to ask the image back how wide TEXT came out once
drawn -- the scrolling overlay draws into an image that never becomes
one -- but the selected frame's own font metrics
(`string-pixel-width', added in Emacs 29.1) are a real answer rather
than a guess: a flat per-character multiplier used to be all this had,
and a title's actual mix of narrow (\"i\", \"l\", a space) and wide
(\"m\", \"w\") characters swung that guess wide enough to call text
that visually fit worth scrolling anyway.  Falls back to 0.6 of
FONT-SIZE per character on Emacs versions before `string-pixel-width'
existed -- this package still supports 28.1, see the
\"Package-Requires\" header in supersonic.el -- where that flat guess
is the best available without it."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width (propertize text 'face (list :weight 'bold :font (font-spec :size font-size))))
    (* (length text) font-size 0.6)))

(defun supersonic-art-scroll-pad (size)
  "Return the pixel margin the scrolling overlay's text keeps clear of the edges.
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

(defun supersonic-art-overlay-scroll-propertize (id size text offset &optional waveform)
  "Generate a property showing TEXT across cover art ID at SIZE, OFFSET pixels in.
Like `supersonic-art-overlay-propertize', but left-aligned and shifted
left by OFFSET pixels instead of centered and fixed in place --
`supersonic-now-playing-animate-art-overlay-scroll' bounces OFFSET
between 0 and `supersonic-art-scroll-max-offset' (see that function) to
reveal TEXT a little at a time when it does not fit in one line; this
function only ever draws the single frame it is given for whatever
OFFSET that is.  Clipped to its own text row so TEXT never draws
outside of it, whichever edge is currently cut off -- WAVEFORM's lane
above that row, if there is one, is left unclipped, since nothing ever
scrolls there.

WAVEFORM is as in `supersonic-art-overlay-propertize'."
  (let* ((file (supersonic-art-cache-file id size))
         (mime (format "image/%s" (image-type-from-file-header file)))
         (svg (svg-create size size))
         (text-height (round (* size 0.22)))
         (lane-height (if waveform (supersonic-art-overlay-waveform-lane-height size) 0))
         (scrim-height (+ text-height lane-height))
         (scrim-y (- size scrim-height))
         (font-size (supersonic-art-overlay-font-size size))
         (baseline-y (- size (/ text-height 2)))
         (pad (supersonic-art-scroll-pad size))
         (clip (svg-clip-path svg :id "supersonic-art-scroll-clip")))
    (svg-embed svg file mime nil :width size :height size)
    (svg-rectangle svg 0 scrim-y size scrim-height :fill "black" :fill-opacity 0.55)
    (when waveform
      (supersonic-waveform-svg-bars svg (supersonic-waveform-bars (car waveform) (cdr waveform) size lane-height)
                                     scrim-y lane-height))
    (svg-rectangle clip 0 (- size text-height) size text-height)
    (svg-text svg text
              :x (- pad offset) :y baseline-y
              :fill "white" :font-size font-size :font-weight "bold"
              :text-anchor "start" :dominant-baseline "middle"
              :clip-path "url(#supersonic-art-scroll-clip)")
    (propertize " " 'display (svg-image svg))))

(aio-defun
 supersonic--fetch-art (id size)
 "Ensure cover art ID is cached on disk at SIZE, fetching it if necessary.
Returns a promise that resolves once the fetch has settled; callers
should re-check `file-exists-p' afterwards rather than assume success,
since a failed fetch resolves without signalling here."
 (unless (file-exists-p (supersonic-art-cache-file id size))
   (unless (file-exists-p supersonic-cache-path)
     (mkdir supersonic-cache-path))
   (pcase-let ((`(,status . ,buffer)
                (aio-await
                 (aio-url-retrieve
                  (supersonic-build-url "/getCoverArt.view" `(("id" . ,id) ("size" . ,(int-to-string size))))))))
     (unwind-protect
         (unless (plist-get status :error)
           (with-current-buffer buffer
             ;; Cover art is arbitrary binary image data, not text -- write the bytes as-is instead of letting Emacs
             ;; guess (and possibly prompt for) a coding system.
             (let ((coding-system-for-write 'no-conversion))
               (write-region (1+ url-http-end-of-headers) (point-max) (supersonic-art-cache-file id size)
                             nil
                             'no-message))))
       (kill-buffer buffer)))))

(aio-defun
 supersonic--fetch-art-throttled (sem id size)
 "Fetch cover art ID at SIZE once SEM hands out a slot.
Callers create every fetch up front, so the semaphore -- not the number
of entries -- is what decides how many requests are on the wire at once
(see `supersonic-art-fetch-concurrency').  Failures are swallowed here
rather than left to reject, so that a slot is always given back and one
unreachable cover doesn't hold up the rest."
 (aio-await (aio-sem-wait sem)) (aio-await (aio-catch (supersonic--fetch-art id size))) (aio-sem-post sem))

(aio-defun
 supersonic-get-images (entries n buff)
 "Fetch/cache cover art for ENTRIES and paint it into column N of BUFF.
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
           (mapcar
            (lambda (entry)
              (cons entry (supersonic--fetch-art-throttled sem (car entry) supersonic-list-art-size)))
            entries)))
     (dolist (item pending)
       (aio-await (cdr item))
       (let ((entry (car item)))
         (when (file-exists-p (supersonic-art-cache-file (car entry) supersonic-list-art-size))
           (aset (nth 1 entry) n (supersonic-image-propertize (car entry) supersonic-list-art-size)))))))
 (when (buffer-live-p buff)
   (with-current-buffer buff
     (when (derived-mode-p 'tabulated-list-mode)
       (tabulated-list-print t)))))

(provide 'supersonic-art)
;;; supersonic-art.el ends here
