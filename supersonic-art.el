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

(defun supersonic-art-overlay-propertize (id size text)
  "Generate a property displaying cover art ID at SIZE with TEXT layered over it.
TEXT sits in a semi-opaque scrim across the bottom, composited in with
`svg.el' rather than shown as a string alongside the art the way
`supersonic-image-propertize' is used for -- the point of this one is
text that reads as part of the cover itself, the way a lock-screen
\"now playing\" widget overlays a track name on the art instead of
setting it beside it.  Like `supersonic-image-propertize', expects the
art to be cached at SIZE already."
  (let* ((file (supersonic-art-cache-file id size))
         (mime (format "image/%s" (image-type-from-file-header file)))
         (svg (svg-create size size))
         (scrim-height (round (* size 0.22)))
         (font-size (max 10 (round (* size 0.07)))))
    (svg-embed svg file mime nil :width size :height size)
    (svg-rectangle svg 0 (- size scrim-height) size scrim-height :fill "black" :fill-opacity 0.55)
    (svg-text svg text
              :x (/ size 2) :y (- size (/ scrim-height 2))
              :fill "white" :font-size font-size :font-weight "bold"
              :text-anchor "middle" :dominant-baseline "middle")
    (propertize " " 'display (svg-image svg))))

(defun supersonic-art-scroll-text-width (text font-size)
  "Estimate TEXT's rendered width in pixels at FONT-SIZE.
`svg.el' has no way to ask back how wide a string came out once drawn --
only a live frame's font metrics could answer that exactly, and the
scrolling overlay draws into an image that never becomes one.  0.6 of
FONT-SIZE per character is close enough for the bold sans-serif
`supersonic-art-overlay-scroll-propertize' draws with to time its
wraparound by; being a little off just makes the gap between the two
passes a little wider or narrower than intended, not that either one
draws in the wrong place."
  (* (length text) font-size 0.6))

(defun supersonic-art-overlay-scroll-propertize (id size text offset)
  "Generate a property scrolling TEXT across cover art ID at SIZE.
Like `supersonic-art-overlay-propertize', but instead of a single line
pinned centered in the scrim, TEXT crawls across it right to left, the
way a now-playing widget's ticker does when there is more to show than
fits in one line -- the point being several metadata fields shown
together rather than `supersonic-now-playing-animate-art-overlay's one
field at a time.  OFFSET is how far the crawl has moved so far, in
pixels; advancing it and calling this again on every animation tick is
`supersonic-now-playing-animate-art-overlay-scroll''s job; this
function just draws one frame of it for whatever OFFSET it is given.
Two copies of TEXT are drawn one period apart (see
`supersonic-art-scroll-text-width') so the crawl loops seamlessly
instead of visibly resetting; both are clipped to the scrim so neither
ever draws outside of it."
  (let* ((file (supersonic-art-cache-file id size))
         (mime (format "image/%s" (image-type-from-file-header file)))
         (svg (svg-create size size))
         (scrim-height (round (* size 0.22)))
         (scrim-y (- size scrim-height))
         (font-size (max 10 (round (* size 0.07))))
         (baseline-y (- size (/ scrim-height 2)))
         (text-width (supersonic-art-scroll-text-width text font-size))
         ;; A gap a few characters wide between one pass and the next --
         ;; otherwise the second copy's leading edge would butt straight
         ;; up against the first copy's trailing edge and read as one
         ;; run-on line instead of a loop.
         (gap (* font-size 3))
         (period (+ text-width gap))
         (start-x (- size (mod offset period)))
         (clip (svg-clip-path svg :id "supersonic-art-scroll-clip")))
    (svg-embed svg file mime nil :width size :height size)
    (svg-rectangle svg 0 scrim-y size scrim-height :fill "black" :fill-opacity 0.55)
    (svg-rectangle clip 0 scrim-y size scrim-height)
    (dolist (x (list start-x (+ start-x period)))
      (svg-text svg text
                :x x :y baseline-y
                :fill "white" :font-size font-size :font-weight "bold"
                :text-anchor "start" :dominant-baseline "middle"
                :clip-path "url(#supersonic-art-scroll-clip)"))
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
