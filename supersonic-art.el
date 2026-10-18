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

;; Cover art: the disk cache, downloads through the provider's
;; `cover-art', list columns, and the overlay drawn onto the art in the
;; now-playing buffer.

;;; Code:
(require 'cl-lib)
(require 'aio)
(require 'svg)
(require 'url-util)
(require 'tabulated-list)
(require 'text-property-search)
(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-waveform)

(defcustom supersonic-art-overlay-layers
  (list #'supersonic-art-overlay-layer-art
        #'supersonic-art-overlay-layer-scrim
        #'supersonic-art-overlay-layer-waveform
        #'supersonic-art-overlay-layer-text)
  "Functions drawing the layers of the cover art overlay, bottom first.
Each is called with CTX, the plist `supersonic-art-overlay--context'
returns, and draws into its `:svg'.  Removing a layer also removes the
space reserved for it.  See also `supersonic-art-overlay-scroll-layers'."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-art-overlay-scroll-layers
  (list #'supersonic-art-overlay-layer-art
        #'supersonic-art-overlay-layer-scrim
        #'supersonic-art-overlay-layer-waveform
        #'supersonic-art-overlay-layer-text-scroll)
  "Functions drawing the layers of the cover art overlay with scrolling text.
Like `supersonic-art-overlay-layers', but with
`supersonic-art-overlay-layer-text-scroll' for the text."
  :type '(repeat function)
  :group 'supersonic)

(defun supersonic-art-available-p ()
  "Return non-nil if cover art can be shown."
  (and supersonic-enable-art (display-graphic-p) (supersonic-provider-supports-p 'cover-art)))

(defun supersonic-art-cache-file (id size)
  "Return the cache file of cover art ID at SIZE pixels."
  (expand-file-name (format "art-%s-%d" (supersonic-provider-cache-name id) size) supersonic-cache-path))

(defun supersonic-image-propertize (id size &optional max-width)
  "Return a string displaying cover art ID, SIZE pixels high.
If MAX-WIDTH is non-nil, scale the image down to at most that width."
  (propertize " " 'display (apply #'create-image (supersonic-art-cache-file id size) nil nil :height size
                                  (and max-width (list :max-width max-width)))))

(defun supersonic-art-overlay-font-size (size)
  "Return the font size of the overlay text on cover art of SIZE pixels."
  (max 10 (round (* size 0.07))))

(defun supersonic-art-overlay-font-weight ()
  "Return the SVG font weight of `supersonic-now-playing-art-overlay-label'."
  (if (memq (face-attribute 'supersonic-now-playing-art-overlay-label :weight)
            '(bold semi-bold extra-bold ultra-bold))
      "bold"
    "normal"))

(defun supersonic-art-overlay-font-family ()
  "Return the font family of `supersonic-now-playing-art-overlay-label', or nil."
  (let ((family (face-attribute 'supersonic-now-playing-art-overlay-label :family)))
    (unless (eq family 'unspecified) family)))

(defun supersonic-art-overlay-fill ()
  "Return the text color of `supersonic-now-playing-art-overlay-label'."
  (let ((foreground (face-attribute 'supersonic-now-playing-art-overlay-label :foreground)))
    (if (eq foreground 'unspecified) "white" foreground)))

(defun supersonic-art-overlay-waveform-lane-height (size)
  "Return the height of the overlay's waveform lane on art of SIZE pixels."
  (round (* size 0.16)))

(defun supersonic-art-overlay-waveform-lane-p (waveform layers)
  "Return non-nil if LAYERS draw WAVEFORM in the overlay."
  (and waveform (memq #'supersonic-art-overlay-layer-waveform layers) t))

(defun supersonic-art-overlay-text-row-p (layers)
  "Return non-nil if LAYERS draw text in the overlay."
  (and (or (memq #'supersonic-art-overlay-layer-text layers)
           (memq #'supersonic-art-overlay-layer-text-scroll layers))
       t))

(defun supersonic-art-scroll-text-width (text font-size)
  "Return the width in pixels of TEXT in the overlay font at FONT-SIZE.
Emacs 28 lacks `string-pixel-width', so estimate it there."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width
       (propertize text 'face
                   (list :weight (face-attribute 'supersonic-now-playing-art-overlay-label :weight)
                         :font (font-spec :size font-size :family (supersonic-art-overlay-font-family)))))
    (* (length text) font-size 0.6)))

(defun supersonic-art-scroll-pad (size)
  "Return the margin of the overlay text on cover art of SIZE pixels."
  (round (* size 0.04)))

(defun supersonic-art-scroll-max-offset (size text)
  "Return how many pixels TEXT is wider than the overlay at SIZE, or 0."
  (let* ((font-size (supersonic-art-overlay-font-size size))
         (available (- size (* 2 (supersonic-art-scroll-pad size)))))
    (max 0 (round (- (supersonic-art-scroll-text-width text font-size) available)))))

(defun supersonic-art-overlay--context (id size text waveform layers &optional offset)
  "Return the plist the overlay layers draw from.
ID is the cover art, SIZE its size in pixels, TEXT the label,
WAVEFORM the (ENVELOPE . PROGRESS) or nil, LAYERS the layer functions
and OFFSET the scroll offset of the text."
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
  "Draw CTX's cover art."
  ;; By file name, relative to the image's `:base-uri': embedding the
  ;; art anew on every redraw is slow.
  (dom-append-child
   (plist-get ctx :svg)
   (dom-node
    'image
    `((xlink:href . ,(url-hexify-string (file-name-nondirectory (plist-get ctx :file))))
      (width . ,(plist-get ctx :size))
      (height . ,(plist-get ctx :size))))))

(defun supersonic-art-overlay-layer-scrim (ctx)
  "Draw the dark band behind CTX's text and waveform."
  (svg-rectangle (plist-get ctx :svg) 0 (plist-get ctx :scrim-y)
                 (plist-get ctx :size) (plist-get ctx :scrim-height)
                 :fill "black" :fill-opacity 0.55))

(defun supersonic-art-overlay-layer-waveform (ctx)
  "Draw CTX's waveform, if any."
  (let ((waveform (plist-get ctx :waveform))
        (size (plist-get ctx :size))
        (lane-height (plist-get ctx :lane-height)))
    (when waveform
      (supersonic-waveform-svg-bars
       (plist-get ctx :svg) (car waveform) (cdr waveform) size (- size lane-height) lane-height))))

(defun supersonic-art-overlay-layer-text (ctx)
  "Draw CTX's text, centered."
  (let ((family (supersonic-art-overlay-font-family)))
    (apply #'svg-text (plist-get ctx :svg) (plist-get ctx :text)
           :x (/ (plist-get ctx :size) 2) :y (plist-get ctx :baseline-y)
           :fill (supersonic-art-overlay-fill) :font-size (plist-get ctx :font-size)
           :font-weight (supersonic-art-overlay-font-weight)
           :text-anchor "middle" :dominant-baseline "middle"
           (and family (list :font-family family)))))

(defun supersonic-art-overlay-layer-text-scroll (ctx)
  "Draw CTX's text, scrolled left by CTX's `:offset'."
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
  "Return a string displaying cover art ID at SIZE with TEXT over it.
WAVEFORM, (ENVELOPE . PROGRESS), is drawn below TEXT if non-nil.  The
art must be cached.  See `supersonic-art-overlay-layers'."
  (let ((ctx (supersonic-art-overlay--context id size text waveform supersonic-art-overlay-layers)))
    (dolist (layer supersonic-art-overlay-layers)
      (funcall layer ctx))
    (propertize " " 'display (svg-image (plist-get ctx :svg) :base-uri (plist-get ctx :file)))))

(defun supersonic-art-overlay-scroll-propertize (id size text offset &optional waveform)
  "Return a string displaying cover art ID at SIZE with scrolling TEXT.
TEXT is shifted left by OFFSET pixels.  WAVEFORM is as in
`supersonic-art-overlay-propertize'.  See
`supersonic-art-overlay-scroll-layers'."
  (let ((ctx (supersonic-art-overlay--context
              id size text waveform supersonic-art-overlay-scroll-layers offset)))
    (dolist (layer supersonic-art-overlay-scroll-layers)
      (funcall layer ctx))
    (propertize " " 'display (svg-image (plist-get ctx :svg) :base-uri (plist-get ctx :file)))))

(aio-defun supersonic--fetch-art (id size)
  "Return a promise resolving once cover art ID is cached at SIZE.
The promise rejects if the art can't be fetched."
  (let ((file (supersonic-art-cache-file id size)))
    (unless (file-exists-p file)
      (let ((data (aio-await (supersonic-provider-cover-art id size))))
        (unless (file-exists-p supersonic-cache-path)
          (mkdir supersonic-cache-path t))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert data)
          (let ((coding-system-for-write 'no-conversion))
            (write-region nil nil file nil 'no-message)))))))

(aio-defun supersonic--fetch-art-throttled (sem id size)
  "Fetch cover art ID at SIZE once semaphore SEM allows, ignoring errors."
  (aio-await (aio-sem-wait sem)) (aio-await (aio-catch (supersonic--fetch-art id size))) (aio-sem-post sem))

(aio-defun supersonic-get-images (entries arts n buff)
  "Fetch the cover art ARTS of ENTRIES and show it in column N of BUFF.
ARTS are the `:art' references of ENTRIES, in the same order; nil
means no art.  BUFF is redrawn once all are fetched."
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
  "Plist describing the art `supersonic-art-fill-visible' fills in, or nil.
Its keys are `:column', `:size', `:arts', a table of item id to `:art',
and `:requested', the ids whose art was requested.")

(defvar-local supersonic-art--lazy-sem nil
  "Semaphore limiting the art downloads of this buffer.
It survives a refresh of the buffer, so downloads still running count.")
(put 'supersonic-art--lazy-sem 'permanent-local t)

(defvar-local supersonic-art--lazy-timer nil
  "Timer filling in the art of rows scrolled into view.")

(defun supersonic-art-fill-visible (items n size buff)
  "Show ITEMS' art in column N of BUFF as their rows become visible.
ITEMS are the item plists of the rows.  Art is shown at SIZE pixels."
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
  "Fill in the art of rows scrolled into view, after redisplay."
  (unless supersonic-art--lazy-timer
    (let ((buff (current-buffer)))
      (setq supersonic-art--lazy-timer
            (run-at-time 0 nil (lambda ()
                                 (when (buffer-live-p buff)
                                   (with-current-buffer buff
                                     (setq supersonic-art--lazy-timer nil)
                                     (supersonic-art--fill-windows)))))))))

(defun supersonic-art--in-view (window)
  "Return the ids of the rows visible in WINDOW, maybe a few more."
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
  "Return non-nil if the row of ID is visible in a window."
  (seq-some (lambda (window) (member id (supersonic-art--in-view window)))
            (get-buffer-window-list (current-buffer) nil t)))

(defun supersonic-art--fill-windows ()
  "Request the art of the rows visible in the current buffer's windows."
  (when supersonic-art--lazy
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (mapc #'supersonic-art--request (supersonic-art--in-view window)))))

(defun supersonic-art--request (id)
  "Start fetching the art of the row of ID, unless already requested."
  (let* ((lazy supersonic-art--lazy)
         (art (gethash id (plist-get lazy :arts)))
         (requested (plist-get lazy :requested)))
    (when (and art (not (gethash id requested)))
      (puthash id t requested)
      (ignore (supersonic-art--fill-row (current-buffer) lazy id art)))))

(defun supersonic-art--current-p (buff lazy)
  "Return non-nil if BUFF is live and its art is still described by LAZY."
  (and (buffer-live-p buff) (eq lazy (buffer-local-value 'supersonic-art--lazy buff))))

(aio-defun supersonic-art--fill-row (buff lazy id art)
  "Fetch ART and show it in the row of ID in BUFF.
LAZY is the `supersonic-art--lazy' of the request.  If the row is no
longer visible when the download could start, forget the request."
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
  "Set column N of entry ID to CELL and redraw its row."
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
