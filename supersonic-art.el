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
;; Only for the bars the overlay's waveform lane draws.
(require 'supersonic-waveform)

(defcustom supersonic-art-overlay-layers
  (list #'supersonic-art-overlay-layer-art
        #'supersonic-art-overlay-layer-scrim
        #'supersonic-art-overlay-layer-waveform
        #'supersonic-art-overlay-layer-text)
  "Layers composited into `supersonic-art-overlay-propertize's SVG, in order.
Each is called as (FUNCTION CTX), CTX being the layout context
`supersonic-art-overlay--context' builds.  The first layer draws first,
so it ends up at the bottom.  Layout depends only on which layers are
present, not on their order: reordering changes stacking, and dropping
a layer also drops the room the scrim reserves for it.  A function of
your own reads CTX's `:svg' and whichever other keys it needs.

See `supersonic-art-overlay-scroll-layers' for the scrolling variant."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-art-overlay-scroll-layers
  (list #'supersonic-art-overlay-layer-art
        #'supersonic-art-overlay-layer-scrim
        #'supersonic-art-overlay-layer-waveform
        #'supersonic-art-overlay-layer-text-scroll)
  "Layers composited into `supersonic-art-overlay-scroll-propertize's SVG.
Like `supersonic-art-overlay-layers', but with
`supersonic-art-overlay-layer-text-scroll' in place of
`supersonic-art-overlay-layer-text'."
  :type '(repeat function)
  :group 'supersonic)

(defun supersonic-art-available-p ()
  "Return non-nil if cover art can actually be shown right now.
Mirrors `supersonic-waveform-available-p'.  Callers that only download
art should test this too.  A provider without a `cover-art' operation
has no art."
  (and supersonic-enable-art (display-graphic-p) (supersonic-provider-supports-p 'cover-art)))

(defun supersonic-art-cache-file (id size)
  "Return the path cover art ID is cached under when fetched at SIZE.
ID is an `:art' reference, named via `supersonic-provider-cache-name'.
SIZE is part of the name since lists and now-playing show art at
different sizes; the \"art-\" prefix keeps it apart from
`supersonic-waveform-cache-file' in the shared `supersonic-cache-path'."
  (expand-file-name (format "art-%s-%d" (supersonic-provider-cache-name id) size) supersonic-cache-path))

(defun supersonic-image-propertize (id size &optional max-width)
  "Generate a property displaying cover art ID at SIZE pixels high.
With MAX-WIDTH, an image wider than that many pixels at that height is
scaled down further to fit, keeping its proportions."
  (propertize " " 'display (apply #'create-image (supersonic-art-cache-file id size) nil nil :height size
                                  (and max-width (list :max-width max-width)))))

(defun supersonic-art-overlay-font-size (size)
  "Return the pixel font size the art overlay scrim's text is drawn at for SIZE.
Shared with `supersonic-art-scroll-max-offset', so whether text needs
scrolling is decided at the size it is drawn at."
  (max 10 (round (* size 0.07))))

(defun supersonic-art-overlay-font-weight ()
  "Return the SVG `font-weight' for the art overlay label face.
Derived from `supersonic-now-playing-art-overlay-label's `:weight',
rounded to \"bold\" or \"normal\", the only two SVG text knows."
  (if (memq (face-attribute 'supersonic-now-playing-art-overlay-label :weight)
            '(bold semi-bold extra-bold ultra-bold))
      "bold"
    "normal"))

(defun supersonic-art-overlay-font-family ()
  "Return `supersonic-now-playing-art-overlay-label's `:family', or nil if unset.
Nil leaves the SVG text without a `font-family', so the renderer uses
its own default."
  (let ((family (face-attribute 'supersonic-now-playing-art-overlay-label :family)))
    (unless (eq family 'unspecified) family)))

(defun supersonic-art-overlay-fill ()
  "Return the SVG `fill' color for the art overlay label face.
Taken from `supersonic-now-playing-art-overlay-label's `:foreground'."
  (let ((foreground (face-attribute 'supersonic-now-playing-art-overlay-label :foreground)))
    (if (eq foreground 'unspecified) "white" foreground)))

(defun supersonic-art-overlay-waveform-lane-height (size)
  "Return how tall the waveform lane below the text row is, at SIZE.
Independent of `supersonic-art-overlay-font-size', so the text keeps
its size whether or not a lane is drawn below it."
  (round (* size 0.16)))

(defun supersonic-art-overlay-waveform-lane-p (waveform layers)
  "Return non-nil if LAYERS will draw WAVEFORM as a lane in the scrim.
That takes both a non-nil WAVEFORM and
`supersonic-art-overlay-layer-waveform' in LAYERS.  Asked by both
`supersonic-art-overlay--context' and
`supersonic-now-playing--maybe-seekable', so the art is only seekable
by click when there is a lane to click in."
  (and waveform (memq #'supersonic-art-overlay-layer-waveform layers) t))

(defun supersonic-art-overlay-text-row-p (layers)
  "Return non-nil if LAYERS will draw a text row in the scrim.
That is, if LAYERS has either built-in text layer.  The text
counterpart to `supersonic-art-overlay-waveform-lane-p'."
  (and (or (memq #'supersonic-art-overlay-layer-text layers)
           (memq #'supersonic-art-overlay-layer-text-scroll layers))
       t))

(defun supersonic-art-scroll-text-width (text font-size)
  "Measure TEXT's rendered width in pixels at FONT-SIZE.
Uses `string-pixel-width' with the `:weight' and `:family' of
`supersonic-now-playing-art-overlay-label'.  Emacs 28, which lacks it,
guesses 0.6 of FONT-SIZE per character."
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
0 if TEXT fits, which then isn't scrolled; otherwise the offset at
which the scroll turns back."
  (let* ((font-size (supersonic-art-overlay-font-size size))
         (available (- size (* 2 (supersonic-art-scroll-pad size)))))
    (max 0 (round (- (supersonic-art-scroll-text-width text font-size) available)))))

(defun supersonic-art-overlay--context (id size text waveform layers &optional offset)
  "Build the layout CTX `supersonic-art-overlay-layers'/`-scroll-layers' draw from.
All geometry is computed here, so no layer depends on another and
their order only changes stacking.  The scrim holds the text row and
the waveform lane, each only if LAYERS draws it; a custom layer gets
no room reserved.

ID is the cover art drawn at SIZE.  TEXT is the label and WAVEFORM the
envelope; either may be nil.  OFFSET is for
`supersonic-art-overlay-layer-text-scroll' and defaults to 0."
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
Referenced by file name rather than embedded with `svg-embed': the
overlay is redrawn on every waveform bucket change, and re-encoding
the cover each time was most of the cost.  The name resolves against
the `:base-uri' the overlay sets to the same file, whose directory
librsvg allows loading from."
  (dom-append-child
   (plist-get ctx :svg)
   (dom-node
    'image
    `((xlink:href . ,(url-hexify-string (file-name-nondirectory (plist-get ctx :file))))
      (width . ,(plist-get ctx :size))
      (height . ,(plist-get ctx :size))))))

(defun supersonic-art-overlay-layer-scrim (ctx)
  "Draw the semi-opaque scrim CTX's text and waveform lane sit on.
Its height, CTX's `:scrim-height', is zero when neither is drawn."
  (svg-rectangle (plist-get ctx :svg) 0 (plist-get ctx :scrim-y)
                 (plist-get ctx :size) (plist-get ctx :scrim-height)
                 :fill "black" :fill-opacity 0.55))

(defun supersonic-art-overlay-layer-waveform (ctx)
  "Draw CTX's waveform bars into the lane reserved for them, if CTX has any.
The lane is sized off the art, not `supersonic-waveform-width' and
`supersonic-waveform-height'."
  (let ((waveform (plist-get ctx :waveform))
        (size (plist-get ctx :size))
        (lane-height (plist-get ctx :lane-height)))
    (when waveform
      (supersonic-waveform-svg-bars
       (plist-get ctx :svg) (car waveform) (cdr waveform) size (- size lane-height) lane-height))))

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
The scrolling counterpart to `supersonic-art-overlay-layer-text'."
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
TEXT sits in a semi-opaque scrim across the bottom.  Expects the art
to be cached at SIZE already.  The drawing is done by
`supersonic-art-overlay-layers'.

WAVEFORM, if given, is (ENVELOPE . PROGRESS) as
`supersonic-waveform-propertize' takes them, drawn as a lane of bars
below TEXT."
  (let ((ctx (supersonic-art-overlay--context id size text waveform supersonic-art-overlay-layers)))
    (dolist (layer supersonic-art-overlay-layers)
      (funcall layer ctx))
    (propertize " " 'display (svg-image (plist-get ctx :svg) :base-uri (plist-get ctx :file)))))

(defun supersonic-art-overlay-scroll-propertize (id size text offset &optional waveform)
  "Generate a property showing TEXT over cover art ID at SIZE, OFFSET pixels in.
Like `supersonic-art-overlay-propertize', but TEXT is left-aligned and
shifted left by OFFSET: one frame of the scroll
`supersonic-now-playing-animate-art-overlay-scroll' animates.  Drawn by
`supersonic-art-overlay-scroll-layers'.  WAVEFORM is as in
`supersonic-art-overlay-propertize'."
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
         ;; Binary data: write the bytes as-is.
         (let ((coding-system-for-write 'no-conversion))
           (write-region nil nil file nil 'no-message)))))))

(aio-defun
 supersonic--fetch-art-throttled (sem id size)
 "Fetch cover art ID at SIZE once SEM hands out a slot.
Failures are swallowed, so the slot is always given back and one
unreachable cover doesn't hold up the rest."
 (aio-await (aio-sem-wait sem)) (aio-await (aio-catch (supersonic--fetch-art id size))) (aio-sem-post sem))

(aio-defun
 supersonic-get-images (entries arts n buff)
 "Fetch/cache cover art for ENTRIES and paint it into column N of BUFF.
ARTS holds each entry's `:art' reference, in the same order as
ENTRIES; an entry whose reference is nil is left without art.  At most
`supersonic-art-fetch-concurrency' fetches run at a time, and a failed
one leaves its entry without art.  BUFF is reprinted once every fetch
has settled."
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
Permanently local, so a refreshed or reopened list shares the slots
the old list's fetches still hold instead of getting a second set.
A new `supersonic-art-fetch-concurrency' takes a new buffer.")
(put 'supersonic-art--lazy-sem 'permanent-local t)

(defvar-local supersonic-art--lazy-timer nil
  "The timer a scroll of this buffer set to ask for the art it brought in view.")

(defun supersonic-art-fill-visible (items n size buff)
  "Fill column N of BUFF's rows with ITEMS' art at SIZE as they come into view.
ITEMS are the facade plists the rows were made from, matched to the
rows by `:id'.  Unlike `supersonic-get-images', only the rows in view
are fetched for, at most `supersonic-art-fetch-concurrency' at a time;
the rest follow as they are scrolled to.  Each row is redrawn as its
art arrives, at most SIZE pixels wide.  BUFF must already be printed."
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
already cached is shown without waiting for a slot.  Once it has a
slot, the request is dropped if BUFF was refilled meanwhile, and
dropped and forgotten if its row was scrolled away, so it is asked for
again when the row is back.  Failures leave the row without art."
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
