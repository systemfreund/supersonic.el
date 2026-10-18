;;; supersonic.el --- Music client for Subsonic servers and Music Assistant -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
;; Assisted-by: Claude:claude-opus-5
;; URL: https://github.com/systemfreund/supersonic.el
;; Version: 0.7.3
;; Keywords: multimedia
;; Package-Requires: ((emacs "28.1") (transient "0.2") (aio "1.2"))

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

;; A music client for Subsonic-compatible servers (Navidrome, Gonic,
;; ...) and Music Assistant: browse artists, albums, album lists and
;; podcasts, search, and manage a play queue from Emacs.
;;
;; Playback happens locally through mpv, or remotely on the Subsonic
;; server's jukebox, a UPnP/DLNA renderer or a Music Assistant player.
;; Start with `M-x supersonic'; see the README for configuration.
;;
;; mpv is driven over a Unix-domain socket, so local playback does not
;; work on native Windows.

;;; Code:
(require 'json)
(require 'cl-lib)
(require 'subr-x)
;; `truncate-string-ellipsis' is not autoloaded.
(require 'mule-util)
(require 'url)
(require 'seq)
(require 'aio)
;; `prop-match-beginning'/`prop-match-end' are not autoloaded.
(require 'text-property-search)

(require 'transient)

(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-subsonic)
(require 'supersonic-art)
(require 'supersonic-playback)
(require 'supersonic-mpv)
(require 'supersonic-waveform)

(defcustom supersonic-now-playing-layout-functions
  (list #'supersonic-now-playing-layout-art
        #'supersonic-now-playing-layout-label
        #'supersonic-now-playing-layout-buttons)
  "Functions inserting the top rows of the now-playing buffer, in order.
Each is called with BUFF and SONG, like
`supersonic-now-playing-render-functions', which run after them.
Errors are reported and the function skipped.  Built in:

- `supersonic-now-playing-layout-art': the cover art.
- `supersonic-now-playing-layout-label': the label row, see
  `supersonic-now-playing-animation-functions'.
- `supersonic-now-playing-layout-buttons': the transport buttons.
- `supersonic-now-playing-layout-waveform': a separate waveform
  seekbar; not in the default."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-now-playing-animation-functions (list #'supersonic-now-playing-animate-art-overlay)
  "Functions run on every animation frame of the now-playing buffer.
Each is called with BUFF and DELTA, the seconds since the last frame,
or 0 for the first.  Errors are reported and the function skipped.
Built in:

- `supersonic-now-playing-animate-label': show the current field of
  `supersonic-now-playing-cycle-fields' below the cover art.
- `supersonic-now-playing-animate-art-overlay': show it on the cover
  art.  The default.
- `supersonic-now-playing-animate-art-overlay-scroll': show all the
  fields in one line on the cover art, scrolling if it doesn't fit."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-now-playing-position-functions
  (list #'supersonic-now-playing-update-duration-field
        #'supersonic-now-playing-recolor-waveform
        #'supersonic-now-playing-maybe-fetch-waveform)
  "Functions run when the playback position in the now-playing buffer updates.
Each is called with BUFF and POSITION in seconds, every
`supersonic-now-playing-interval' seconds and after a seek.  Errors
are reported and the function skipped.  Built in:

- `supersonic-now-playing-update-duration-field': update the
  \"Duration:\" row.
- `supersonic-now-playing-recolor-waveform': update the played part
  of the waveform.
- `supersonic-now-playing-maybe-fetch-waveform': compute the
  waveform of the current track."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-now-playing-render-functions
  (list #'supersonic-now-playing-render-title
        #'supersonic-now-playing-render-artist
        #'supersonic-now-playing-render-album
        #'supersonic-now-playing-render-duration
        #'supersonic-now-playing-render-queue-position
        #'supersonic-now-playing-render-format
        #'supersonic-now-playing-render-size)
  "Functions inserting the text rows of the now-playing buffer, in order.
Each is called with BUFF and SONG, the track plist, after
`supersonic-now-playing-layout-functions'.  SONG may have any keys
described in `supersonic-provider.el', and keys of the provider's
own, such as Subsonic's `:genre'.  The built-ins omit their row when
the value is unknown.  Errors are reported and the function skipped."
  :type '(repeat function)
  :group 'supersonic)

;;;
;;; List buffers
;;;

(defun supersonic--cell-label (cell)
  "Return the text of the tabulated-list CELL, or nil for an image."
  (cond
   ((stringp cell) cell)
   ;; Drawn over a single space; its real width is unknown.
   ((eq (car cell) 'image) nil)
   (t (car cell))))

(defun supersonic--cell-width (cell)
  "Return the width of CELL in characters."
  (let ((label (supersonic--cell-label cell)))
    (if label (string-width label) 1)))

(defun supersonic--pixelwise-p ()
  "Return non-nil if cells can be measured in pixels (Emacs 29 and later)."
  (fboundp 'string-pixel-width))

(defun supersonic--string-pixel-width (string)
  "Return the width of STRING in pixels in the selected window.
Count face remappings where possible.  Before Emacs 29, estimate."
  (if (fboundp 'string-pixel-width)
      (if (and face-remapping-alist (>= (cdr (func-arity 'string-pixel-width)) 2))
          (funcall #'string-pixel-width string (current-buffer))
        (string-pixel-width string))
    (* (string-width string) (frame-char-width))))

(defvar-local supersonic--column-pixels nil
  "Width of a column in pixels, as of the last fit.")

(defvar-local supersonic--cell-pixels nil
  "Hash table of each cell's width in pixels, as of the last fit.")

(defvar-local supersonic--cut-cells nil
  "Hash table of the cells cut to fit, to (PIXELS . SHOWN).")

(defun supersonic--cell-columns (cell)
  "Return the number of columns CELL takes up in the selected window.
Never less than its width in characters."
  (let ((label (supersonic--cell-label cell)))
    (if (not label)
        1
      (max (string-width label)
           (ceiling (or (gethash cell supersonic--cell-pixels)
                        (puthash cell (supersonic--string-pixel-width label) supersonic--cell-pixels))
                    supersonic--column-pixels)))))

(defun supersonic--widest-shrinkable-column (widths minimums)
  "Return the index of the widest of WIDTHS above its MINIMUMS, or nil."
  (cl-loop with widest = nil
           with widest-width = 0
           for width in widths
           for minimum in minimums
           for n from 0
           when (and (> width minimum) (> width widest-width))
           do (setq widest n widest-width width)
           finally return widest))

(defun supersonic--fit-column-widths (format entries available &optional measure)
  "Return a copy of FORMAT with column widths fitted to ENTRIES.
FORMAT and ENTRIES are as in `tabulated-list-format' and
`tabulated-list-entries'.  Each column but the last gets as wide as
its widest cell or header.  If the row is wider than AVAILABLE
columns, the widest columns shrink first.  MEASURE returns a cell's
width and defaults to `supersonic--cell-width'."
  (let* ((count (length format))
         (fitted (max 0 (1- count)))
         (minimums
          (mapcar
           (lambda (n)
             (let ((col (aref format n)))
               (max 1 (+ (string-width (nth 0 col)) (if (nth 2 col) 2 0)))))
           (number-sequence 0 (1- fitted))))
         (widths
          (cl-mapcar
           (lambda (n minimum)
             (seq-reduce
              (lambda (widest entry) (max widest (funcall (or measure #'supersonic--cell-width) (aref (cadr entry) n))))
              entries minimum))
           (number-sequence 0 (1- fitted)) minimums)))
    (when (and available widths)
      (let ((budget (- available
                       tabulated-list-padding
                       (seq-reduce
                        (lambda (sum n) (+ sum (or (plist-get (nthcdr 3 (aref format n)) :pad-right) 1)))
                        (number-sequence 0 (1- fitted)) 0)
                       (nth 1 (aref format fitted))))
            widest)
        (let ((excess (- (apply #'+ widths) budget)))
          (while (and (> excess 0)
                      (setq widest (supersonic--widest-shrinkable-column widths minimums)))
            (cl-decf (nth widest widths))
            (cl-decf excess)))))
    (vconcat
     (seq-map-indexed
      (lambda (col n) (if (< n fitted) (cons (car col) (cons (nth n widths) (cddr col))) col))
      format))))

(defun supersonic--truncate-to-pixels (string pixels)
  "Return STRING shortened, with an ellipsis, to fit PIXELS."
  (let ((ellipsis (apply #'propertize (truncate-string-ellipsis) (text-properties-at 0 string))))
    (if (fboundp 'truncate-string-pixelwise)
        ;; From Emacs 31; keeps composed characters whole and counts face remappings.
        (truncate-string-pixelwise string pixels (and face-remapping-alist (current-buffer)) ellipsis)
      (let ((low 0) (high (1- (length string))) (fitting ellipsis))
        ;; Binary search for the longest prefix that still fits with its ellipsis.
        (while (<= low high)
          (let* ((mid (/ (+ low high 1) 2))
                 (candidate (concat (substring string 0 mid) ellipsis)))
            (if (<= (supersonic--string-pixel-width candidate) pixels)
                (setq fitting candidate low (1+ mid))
              (setq high (1- mid)))))
        fitting))))

(defun supersonic--shown-cells (cols)
  "Return COLS with each cell cut down to its column's width in pixels.
COLS itself is not modified.  The last column is left as is."
  (let ((shown cols))
    (dotimes (n (1- (length tabulated-list-format)))
      (let* ((cell (aref cols n))
             (pixels (* supersonic--column-pixels (nth 1 (aref tabulated-list-format n))))
             (drawn (and (stringp cell) (gethash cell supersonic--cell-pixels))))
        (when (and drawn (> drawn pixels))
          (when (eq shown cols)
            (setq shown (copy-sequence cols)))
          (aset shown n (let ((cut (gethash cell supersonic--cut-cells)))
                          (if (eql (car cut) pixels)
                              (cdr cut)
                            (cdr (puthash cell (cons pixels (supersonic--truncate-to-pixels cell pixels))
                                          supersonic--cut-cells))))))))
    shown))

(defun supersonic--print-entry (id cols)
  "Insert the entry ID with COLS at point, cells cut down to fit.
Used as `tabulated-list-printer'.  The entry keeps the whole cells,
for sorting."
  (let ((beg (point)))
    (tabulated-list-print-entry id (supersonic--shown-cells cols))
    (put-text-property beg (point) 'tabulated-list-entry cols)))

(defun supersonic--window-columns ()
  "Return the number of columns of the selected window's text area.
In columns of the frame's default font, excluding line numbers."
  (1- (/ (- (window-body-width nil t) (line-number-display-width t)) (frame-char-width))))

(defun supersonic--fit-columns ()
  "Fit the column widths of the current tabulated-list buffer to its entries.
Fit them to the window showing the buffer, or the selected one.  From
Emacs 29, cells are measured in pixels and cut down where needed."
  (when (> (length tabulated-list-format) 0)
    (let ((buff (current-buffer)))
      (with-selected-window (or (get-buffer-window buff t) (selected-window))
        (with-current-buffer buff
          (let ((pixelwise (supersonic--pixelwise-p)))
            (setq supersonic--column-pixels (frame-char-width)
                  supersonic--cell-pixels (make-hash-table :test 'eq)
                  supersonic--cut-cells (make-hash-table :test 'eq))
            (setq tabulated-list-format
                  (supersonic--fit-column-widths
                   tabulated-list-format tabulated-list-entries (supersonic--window-columns)
                   (and pixelwise #'supersonic--cell-columns)))
            (when pixelwise
              (setq tabulated-list-printer #'supersonic--print-entry)
              ;; Now, while the window fitted to is selected.
              (dolist (entry tabulated-list-entries)
                (supersonic--shown-cells (cadr entry))))
            (tabulated-list-init-header)))))))

(defun supersonic--print-list (entries)
  "Show ENTRIES in the current tabulated-list buffer, with fitted columns."
  (setq tabulated-list-entries entries)
  (supersonic--fit-columns)
  (tabulated-list-print t))

;;;
;;; Queue
;;;

(defconst supersonic-queue-buffer-name "*supersonic-queue*"
  "Name of the buffer used by `supersonic-show-queue'.")

(defun supersonic-queue-buffer ()
  "Return the play queue buffer if it is currently live, else nil."
  (let ((buff (get-buffer supersonic-queue-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-queue-maybe-refresh ()
  "Refresh the play queue buffer, if it is open."
  (let ((buff (supersonic-queue-buffer)))
    (when buff
      (supersonic-queue-fetch-and-render buff))))

(aio-defun supersonic-queue-parse (entries)
  "Return a promise resolving to tabulated-list entries for queue ENTRIES.
ENTRIES are from `supersonic-playback-queue'.  Tracks are looked up
unless the entry has them; a failed lookup shows as \"?\"."
  (let* ((pending
          (seq-map-indexed
           (lambda (entry index)
             (let ((track-id (plist-get entry :track-id)))
               (list
                index track-id (plist-get entry :current) (plist-get entry :track)
                (and track-id
                     (not (plist-get entry :track))
                     (aio-catch (supersonic-provider-track track-id))))))
           entries)))
    ;; Not `mapcar': `aio-await' must not be inside a lambda.
    (let (rows)
      (dolist (item pending)
        (pcase-let ((`(,index ,track-id ,current ,brought ,promise) item))
          (let* ((outcome (and promise (aio-await promise)))
                 (track (or brought (and outcome (eq (car outcome) :success) (cdr outcome)))))
            (push (list
                   ;; Without a track id, the position serves as the row's id.
                   (or track-id (number-to-string index))
                   (vector
                    (if current
                        "▶"
                      " ")
                    (if track
                        (or (plist-get track :title) "")
                      "?")
                    (or (plist-get track :artist) "")
                    (or (plist-get track :album) "")))
                  rows))))
      (nreverse rows))))

(aio-defun supersonic-queue-fetch-and-render (buff)
  "Show the active backend's play queue in BUFF."
  (when (buffer-live-p buff)
    (let ((entries (aio-await (supersonic-queue-parse (aio-await (supersonic-playback-queue))))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list entries))))))

(defun supersonic-queue-refresh ()
  "Refresh the play queue buffer."
  (interactive)
  (supersonic-queue-fetch-and-render (current-buffer)))

(defvar supersonic-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-queue-refresh)
    map)
  "Keymap for `supersonic-queue-mode'.")

(define-derived-mode supersonic-queue-mode tabulated-list-mode "Supersonic Queue"
  "Major mode for the play queue buffer, see `supersonic-show-queue'."
  (setq tabulated-list-format [("" 2 nil) ("Title" 40 t) ("Artist" 25 t) ("Album" 25 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (supersonic--add-playback-hooks)
  (tabulated-list-init-header))

;;;###autoload
(defun supersonic-show-queue ()
  "Show the play queue of the active backend."
  (interactive)
  (let ((buff (get-buffer-create supersonic-queue-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-queue-mode)
        (supersonic-queue-mode)))
    (supersonic-queue-fetch-and-render buff)
    (pop-to-buffer-same-window buff)))

;;;
;;; Now playing
;;;

(defconst supersonic-now-playing-buffer-name "*supersonic-now-playing*"
  "Name of the buffer used by `supersonic-show-now-playing'.")

(defvar supersonic-now-playing--timer nil
  "Timer updating the playback position in the now-playing buffer.")

(defvar supersonic-now-playing--animation-timer nil
  "Timer running `supersonic-now-playing-animation-functions'.")

(defvar supersonic-now-playing--animation-timer-interval nil
  "Interval the animation timer was started with, or nil.")

(defvar-local supersonic-now-playing--title nil
  "Title of the track shown in the now-playing buffer, or nil.")

(defvar-local supersonic-now-playing--artist nil
  "Artist of the track shown in the now-playing buffer, or nil.")

(defvar-local supersonic-now-playing--album nil
  "Album of the track shown in the now-playing buffer, or nil.")

(defvar-local supersonic-now-playing--art-id nil
  "Cover art reference of the track shown in the now-playing buffer, or nil.")

(defvar-local supersonic-now-playing--field-index 0
  "Index in `supersonic-now-playing-cycle-fields' of the field shown.")

(defvar-local supersonic-now-playing--cycle-elapsed 0
  "Seconds elapsed toward the next switch of the field shown.")

(defvar-local supersonic-now-playing--cycle-tick nil
  "The animation frame whose time was last counted toward the field switch.
Several animation functions may count the same frame.")

(defvar-local supersonic-now-playing--cycle-changed nil
  "Whether the field shown switched in the last counted animation frame.")

(defvar-local supersonic-now-playing--animation-last-time nil
  "Time of the last animation frame, or nil.")

(defvar-local supersonic-now-playing--scroll-offset 0
  "Pixels the scrolling label on the cover art is scrolled by.")

(defvar-local supersonic-now-playing--scroll-phase 'pause-start
  "Phase of the scrolling label.
One of `pause-start', `forward', `pause-end' and `backward'.")

(defvar-local supersonic-now-playing--scroll-pause-elapsed 0
  "Seconds elapsed in the current pause of the scrolling label.")

(defvar-local supersonic-now-playing--duration nil
  "Duration in seconds of the track shown in the now-playing buffer.")

(defvar-local supersonic-now-playing--queue-place nil
  "(POSITION . TOTAL) of the track shown in the play queue, or nil.")

(defvar-local supersonic-now-playing--track-id nil
  "Id of the track shown in the now-playing buffer, or nil.")

(defvar-local supersonic-now-playing--paused nil
  "Non-nil if the now-playing buffer shows playback as paused.")

(defvar-local supersonic-now-playing--position nil
  "Return POSITION and DURATION in seconds, either may be nil, as text.")

(defvar-local supersonic-now-playing--waveform nil
  "(TRACK-ID . ENVELOPE) of the waveform shown, or nil.")

(defvar-local supersonic-now-playing--waveform-bucket nil
  "Index of the first unplayed bucket of the waveform as drawn, or nil.")

(defvar-local supersonic-now-playing--waveform-requested nil
  "Non-nil once the waveform of the current track was requested.")

(defvar-local supersonic-now-playing--stale-images nil
  "Alist of FIELD to the image last replaced in FIELD.
`supersonic-now-playing--update-field' flushes each on FIELD's next
update, not when replacing it.")

(defun supersonic-now-playing-buffer ()
  "Return the now-playing buffer if it is currently live, else nil."
  (let ((buff (get-buffer supersonic-now-playing-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-now-playing--start-timer ()
  "Start updating the playback position, unless already started."
  (unless supersonic-now-playing--timer
    (setq supersonic-now-playing--timer
          (run-at-time
           supersonic-now-playing-interval supersonic-now-playing-interval #'supersonic-now-playing--tick))))

(defun supersonic-now-playing--stop-timer ()
  "Stop updating the playback position."
  (when supersonic-now-playing--timer
    (cancel-timer supersonic-now-playing--timer)
    (setq supersonic-now-playing--timer nil)))

(defun supersonic-now-playing--field-value (field)
  "Return the value of FIELD, `title', `artist' or `album'."
  (pcase field
    ('title supersonic-now-playing--title)
    ('artist supersonic-now-playing--artist)
    ('album supersonic-now-playing--album)))

(defun supersonic-now-playing--current-field ()
  "Return (FIELD . VALUE) of the field to show.
VALUE falls back to the title, or \"?\"."
  (let* ((fields supersonic-now-playing-cycle-fields)
         (field (and fields (nth (mod supersonic-now-playing--field-index (length fields)) fields))))
    (cons (or field 'title) (or (and field (supersonic-now-playing--field-value field)) supersonic-now-playing--title "?"))))

(defun supersonic-now-playing--advance-field (delta)
  "Count DELTA seconds toward the next field and switch when due.
Return non-nil if the field switched in this frame."
  (if (equal supersonic-now-playing--cycle-tick supersonic-now-playing--animation-last-time)
      supersonic-now-playing--cycle-changed
    (setq supersonic-now-playing--cycle-tick supersonic-now-playing--animation-last-time)
    (setq supersonic-now-playing--cycle-elapsed (+ supersonic-now-playing--cycle-elapsed delta))
    (let ((steps (floor (/ supersonic-now-playing--cycle-elapsed supersonic-now-playing-cycle-interval))))
      (setq supersonic-now-playing--cycle-changed (> steps 0))
      (when supersonic-now-playing--cycle-changed
        (setq supersonic-now-playing--field-index (+ supersonic-now-playing--field-index steps))
        (setq supersonic-now-playing--cycle-elapsed
              (- supersonic-now-playing--cycle-elapsed (* steps supersonic-now-playing-cycle-interval))))
      supersonic-now-playing--cycle-changed)))

(defun supersonic-now-playing-animate-label (buff delta)
  "Show the current field of BUFF as a label below the cover art.
DELTA is the seconds since the last frame; 0 always redraws."
  (when (or (= delta 0) (supersonic-now-playing--advance-field delta))
    (supersonic-now-playing--update-field
     buff 'label (propertize (cdr (supersonic-now-playing--current-field)) 'face 'supersonic-now-playing-label))))

(defun supersonic-now-playing--overlay-waveform ()
  "Return (ENVELOPE . PROGRESS) of the current waveform, or nil."
  (when supersonic-now-playing--waveform
    (cons
     (cdr supersonic-now-playing--waveform)
     (supersonic-now-playing--progress-ratio supersonic-now-playing--position supersonic-now-playing--duration))))

(defun supersonic-now-playing--maybe-seekable (propertized waveform layers)
  "Return PROPERTIZED, made seekable by click if LAYERS draw WAVEFORM."
  (if (supersonic-art-overlay-waveform-lane-p waveform layers)
      (supersonic-waveform-seekable (get-text-property 0 'display propertized))
    propertized))

(defun supersonic-now-playing-animate-art-overlay (buff delta)
  "Show the current field of BUFF on the cover art, with the waveform.
DELTA is as in `supersonic-now-playing-animate-label', which is used
instead while there is no cover art."
  (with-current-buffer buff
    (if (and supersonic-now-playing--art-id
             (supersonic-art-available-p)
             (file-exists-p (supersonic-art-cache-file supersonic-now-playing--art-id supersonic-now-playing-art-size)))
        (when (or (= delta 0) (supersonic-now-playing--advance-field delta))
          (let ((waveform (supersonic-now-playing--overlay-waveform)))
            (supersonic-now-playing--update-field
             buff 'art
             (supersonic-now-playing--maybe-seekable
              (supersonic-art-overlay-propertize
               supersonic-now-playing--art-id supersonic-now-playing-art-size
               (cdr (supersonic-now-playing--current-field)) waveform)
              waveform supersonic-art-overlay-layers))))
      (supersonic-now-playing-animate-label buff delta))))

(defun supersonic-now-playing--scroll-text ()
  "Return the values of `supersonic-now-playing-cycle-fields' joined in one line."
  (let ((values (delq nil (mapcar #'supersonic-now-playing--field-value supersonic-now-playing-cycle-fields))))
    (if values
        (mapconcat #'identity values "   •   ")
      (or supersonic-now-playing--title "?"))))

(defun supersonic-now-playing--advance-scroll (delta max-offset)
  "Advance the scrolling label by DELTA seconds, between 0 and MAX-OFFSET.
It rests at either end for `supersonic-now-playing-scroll-pause'
seconds.  MAX-OFFSET must be positive."
  (pcase supersonic-now-playing--scroll-phase
    ('pause-start
     (setq supersonic-now-playing--scroll-pause-elapsed (+ supersonic-now-playing--scroll-pause-elapsed delta))
     (when (>= supersonic-now-playing--scroll-pause-elapsed supersonic-now-playing-scroll-pause)
       (setq supersonic-now-playing--scroll-phase 'forward)
       (setq supersonic-now-playing--scroll-pause-elapsed 0)))
    ('forward
     (setq supersonic-now-playing--scroll-offset
           (+ supersonic-now-playing--scroll-offset (* delta supersonic-now-playing-scroll-step)))
     (when (>= supersonic-now-playing--scroll-offset max-offset)
       (setq supersonic-now-playing--scroll-offset max-offset)
       (setq supersonic-now-playing--scroll-phase 'pause-end)))
    ('pause-end
     (setq supersonic-now-playing--scroll-pause-elapsed (+ supersonic-now-playing--scroll-pause-elapsed delta))
     (when (>= supersonic-now-playing--scroll-pause-elapsed supersonic-now-playing-scroll-pause)
       (setq supersonic-now-playing--scroll-phase 'backward)
       (setq supersonic-now-playing--scroll-pause-elapsed 0)))
    ('backward
     (setq supersonic-now-playing--scroll-offset
           (- supersonic-now-playing--scroll-offset (* delta supersonic-now-playing-scroll-step)))
     (when (<= supersonic-now-playing--scroll-offset 0)
       (setq supersonic-now-playing--scroll-offset 0)
       (setq supersonic-now-playing--scroll-phase 'pause-start)))))

(defun supersonic-now-playing-animate-art-overlay-scroll (buff delta)
  "Show all fields of BUFF in one line on the cover art, scrolling if needed.
DELTA is as in `supersonic-now-playing-animate-label', which is used
instead while there is no cover art."
  (with-current-buffer buff
    (if (and supersonic-now-playing--art-id
             (supersonic-art-available-p)
             (file-exists-p (supersonic-art-cache-file supersonic-now-playing--art-id supersonic-now-playing-art-size)))
        (let* ((text (supersonic-now-playing--scroll-text))
               (max-offset (supersonic-art-scroll-max-offset supersonic-now-playing-art-size text))
               (waveform (supersonic-now-playing--overlay-waveform)))
          (cond
           ;; Fits: drawn once, and left alone until the track changes.
           ((<= max-offset 0)
            (when (= delta 0)
              (setq supersonic-now-playing--scroll-offset 0)
              (supersonic-now-playing--update-field
               buff 'art
               (supersonic-now-playing--maybe-seekable
                (supersonic-art-overlay-scroll-propertize
                 supersonic-now-playing--art-id supersonic-now-playing-art-size text 0 waveform)
                waveform supersonic-art-overlay-scroll-layers))))
           (t
            (unless (= delta 0)
              (supersonic-now-playing--advance-scroll delta max-offset))
            (supersonic-now-playing--update-field
             buff 'art
             (supersonic-now-playing--maybe-seekable
              (supersonic-art-overlay-scroll-propertize
               supersonic-now-playing--art-id supersonic-now-playing-art-size
               text supersonic-now-playing--scroll-offset waveform)
              waveform supersonic-art-overlay-scroll-layers)))))
      (supersonic-now-playing-animate-label buff delta))))

(defun supersonic-now-playing--start-animation-timer ()
  "Start the animation timer, unless it is running at the current interval."
  (when (and supersonic-now-playing--animation-timer
             (not (equal supersonic-now-playing--animation-timer-interval
                         supersonic-now-playing-animation-frame-interval)))
    (supersonic-now-playing--stop-animation-timer))
  (when (and supersonic-now-playing-cycle-fields (not supersonic-now-playing--animation-timer))
    (setq supersonic-now-playing--animation-timer-interval supersonic-now-playing-animation-frame-interval)
    (setq supersonic-now-playing--animation-timer
          (run-at-time
           supersonic-now-playing-animation-frame-interval supersonic-now-playing-animation-frame-interval
           #'supersonic-now-playing--animation-tick))))

(defun supersonic-now-playing--stop-animation-timer ()
  "Stop the animation timer."
  (when supersonic-now-playing--animation-timer
    (cancel-timer supersonic-now-playing--animation-timer)
    (setq supersonic-now-playing--animation-timer nil)
    (setq supersonic-now-playing--animation-timer-interval nil)))

(defun supersonic-now-playing--stopped-buffer (stop-fn)
  "Return the now-playing buffer, or call STOP-FN and return nil.
Return nil if nothing is playing, too."
  (let ((buff (supersonic-now-playing-buffer)))
    (if (or (not buff) (not (supersonic-playback-live-p)))
        (progn
          (funcall stop-fn)
          nil)
      buff)))

(defun supersonic-now-playing--run-field-functions (functions buff arg)
  "Call each of FUNCTIONS with BUFF and ARG, reporting errors.
Then redisplay if `supersonic-now-playing-force-redisplay' is non-nil."
  (dolist (function functions)
    (condition-case err
        (funcall function buff arg)
      (error (message "supersonic: %s signalled %s" function err))))
  (when supersonic-now-playing-force-redisplay
    (redisplay t)))

(defun supersonic-now-playing--animation-tick ()
  "Run `supersonic-now-playing-animation-functions' for one frame.
Only while the buffer is visible.  Stop once nothing is playing."
  (let ((buff (supersonic-now-playing--stopped-buffer #'supersonic-now-playing--stop-animation-timer)))
    (when buff
      (with-current-buffer buff
        (let* ((now (float-time))
               (delta (if supersonic-now-playing--animation-last-time
                          (- now supersonic-now-playing--animation-last-time)
                        0)))
          (setq supersonic-now-playing--animation-last-time now)
          (when (get-buffer-window buff t)
            (supersonic-now-playing--run-field-functions supersonic-now-playing-animation-functions buff delta)))))))

(defun supersonic-now-playing--tick ()
  "Update the playback position in the now-playing buffer, if visible.
Stop once nothing is playing."
  (let ((buff (supersonic-now-playing--stopped-buffer #'supersonic-now-playing--stop-timer)))
    (when (and buff (get-buffer-window buff t))
      (supersonic-now-playing--show-position buff))))

(aio-defun supersonic-now-playing--show-position (buff)
  "Run `supersonic-now-playing-position-functions' in BUFF.
They get the current position."
  (let ((position (aio-await (supersonic-playback-status 'position))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq supersonic-now-playing--position position))
      (supersonic-now-playing--run-field-functions supersonic-now-playing-position-functions buff position))))

(defun supersonic-now-playing-maybe-update-position ()
  "Update the position in the now-playing buffer, if visible."
  (let ((buff (supersonic-now-playing-buffer)))
    (when (and buff (get-buffer-window buff t) (supersonic-playback-live-p))
      (supersonic-now-playing--show-position buff))))

(defun supersonic-now-playing-maybe-refresh ()
  "Refresh the now-playing buffer, if it is open."
  (let ((buff (supersonic-now-playing-buffer)))
    (when buff
      (supersonic-now-playing-fetch-and-render buff))))

(defconst supersonic--playback-hooks
  '((supersonic-playback-track-change-hook . supersonic-queue-maybe-refresh)
    (supersonic-playback-track-change-hook . supersonic-now-playing-maybe-refresh)
    (supersonic-playback-queue-change-hook . supersonic-queue-maybe-refresh)
    (supersonic-playback-queue-change-hook . supersonic-now-playing-maybe-refresh)
    (supersonic-playback-state-change-hook . supersonic-now-playing-maybe-refresh)
    (supersonic-playback-position-change-hook . supersonic-now-playing-maybe-update-position))
  "Alist of playback hooks to the functions keeping the buffers current.
They are added while a queue or now-playing buffer exists.")

(defun supersonic--add-playback-hooks ()
  "Add `supersonic--playback-hooks' until no buffer needs them."
  (pcase-dolist (`(,hook . ,fn) supersonic--playback-hooks)
    (add-hook hook fn))
  (add-hook 'kill-buffer-hook #'supersonic--maybe-remove-playback-hooks nil t))

(defun supersonic--remove-playback-hooks ()
  "Remove `supersonic--playback-hooks'."
  (pcase-dolist (`(,hook . ,fn) supersonic--playback-hooks)
    (remove-hook hook fn)))

(defun supersonic--maybe-remove-playback-hooks ()
  "Remove `supersonic--playback-hooks' unless another buffer needs them."
  (unless (seq-some
           (lambda (buff)
             (and (not (eq buff (current-buffer)))
                  (with-current-buffer buff
                    (derived-mode-p 'supersonic-queue-mode 'supersonic-now-playing-mode))))
           (buffer-list))
    (supersonic--remove-playback-hooks)))

(defun supersonic-now-playing--insert-field (label value &optional field)
  "Insert a \"LABEL: VALUE\" row, unless VALUE is nil.
FIELD, if non-nil, tags VALUE for `supersonic-now-playing--update-field'."
  (when value
    (insert
     (propertize (format "%-10s" (concat label ":")) 'face 'shadow)
     (if field
         (propertize value 'supersonic-now-playing-field field)
       value)
     "\n")))

(defun supersonic-now-playing--update-field (buff field value)
  "Replace the text tagged FIELD in BUFF with VALUE."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (save-excursion
        (goto-char (point-min))
        (let ((match (text-property-search-forward 'supersonic-now-playing-field field t)))
          (when match
            (let ((inhibit-read-only t)
                  (stale (get-text-property (prop-match-beginning match) 'display)))
              (delete-region (prop-match-beginning match) (prop-match-end match))
              ;; Flush replaced images, or they pile up in the image
              ;; cache.  One update late: flushing now would let the new
              ;; image reuse the old one's id, and redisplay skip it.
              (let ((previous (alist-get field supersonic-now-playing--stale-images)))
                (when (and previous (fboundp 'image-flush))
                  (image-flush previous t)))
              (setf (alist-get field supersonic-now-playing--stale-images)
                    (and (eq (car-safe stale) 'image) stale))
              (goto-char (prop-match-beginning match))
              (insert (propertize value 'supersonic-now-playing-field field)))))))))

(defun supersonic-now-playing--field-present-p (buff field)
  "Return non-nil if BUFF has text tagged FIELD."
  (with-current-buffer buff
    (and (text-property-any (point-min) (point-max) 'supersonic-now-playing-field field) t)))

(defun supersonic-now-playing--update-waveform-field (buff envelope progress)
  "Draw ENVELOPE at PROGRESS into BUFF's waveform row, if it has one."
  (when (supersonic-now-playing--field-present-p buff 'waveform)
    (supersonic-now-playing--update-field buff 'waveform (supersonic-waveform-propertize envelope progress))))

(defun supersonic-now-playing--progress-ratio (position duration)
  "Return POSITION divided by DURATION, between 0 and 1, or 0 if unknown."
  (if (and position duration (> duration 0))
      (max 0.0 (min 1.0 (/ (float position) duration)))
    0.0))

(defun supersonic-now-playing--progress-bucket (envelope progress)
  "Return the index of ENVELOPE's first unplayed bucket at PROGRESS."
  (floor (* progress (length (car envelope)))))

(defun supersonic-now-playing-update-duration-field (buff position)
  "Update BUFF's \"Duration:\" row with POSITION."
  (supersonic-now-playing--update-field
   buff 'duration
   (supersonic-now-playing--position position (buffer-local-value 'supersonic-now-playing--duration buff))))

(defun supersonic-now-playing-recolor-waveform (buff position)
  "Redraw BUFF's waveform at POSITION if the played part changed."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (when supersonic-now-playing--waveform
        (let* ((envelope (cdr supersonic-now-playing--waveform))
               (progress (supersonic-now-playing--progress-ratio position supersonic-now-playing--duration))
               (bucket (supersonic-now-playing--progress-bucket envelope progress)))
          (unless (eql bucket supersonic-now-playing--waveform-bucket)
            (setq supersonic-now-playing--waveform-bucket bucket)
            (supersonic-now-playing--update-waveform-field buff envelope progress)
            (supersonic-now-playing--run-field-functions supersonic-now-playing-animation-functions buff 0)))))))

(defun supersonic-now-playing--insert-button (label command)
  "Insert a button labeled LABEL running COMMAND."
  (insert-text-button
   (format " %s " label)
   'face 'supersonic-now-playing-button
   'action (lambda (_button) (call-interactively command))
   'follow-link t))

(defun supersonic-now-playing--format-time (seconds longest)
  "Return SECONDS as M:SS, or H:MM:SS if LONGEST is an hour or more."
  (let ((seconds (max 0 (truncate (or seconds 0)))))
    (if (>= (or longest 0) 3600)
        (format-seconds "%h:%.2m:%.2s" seconds)
      (format-seconds "%.2m:%.2s" seconds))))

(defun supersonic-now-playing--position (position duration)
  "Format POSITION and DURATION (seconds, either may be nil) for display."
  (cond
   ((and position duration)
    (format "%s / %s"
            (supersonic-now-playing--format-time position duration)
            (supersonic-now-playing--format-time duration duration)))
   (duration
    (supersonic-now-playing--format-time duration duration))
   (position
    (supersonic-now-playing--format-time position position))))

(defun supersonic-now-playing--format (song)
  "Return SONG's file format as \"MP3 (audio/mpeg)\", or nil if unknown."
  (let ((suffix (plist-get song :suffix))
        (type (plist-get song :content-type)))
    (cond
     ((and suffix type)
      (format "%s (%s)" (upcase suffix) type))
     (suffix
      (upcase suffix))
     (type
      type))))

(defun supersonic-now-playing--art (song)
  "Return a string displaying SONG's cover art, or nil.
The art must be cached."
  (let ((art-id (plist-get song :art)))
    (when (and (supersonic-art-available-p)
               art-id
               (file-exists-p (supersonic-art-cache-file art-id supersonic-now-playing-art-size)))
      (supersonic-image-propertize art-id supersonic-now-playing-art-size))))

(defun supersonic-now-playing-render-title (_buff song)
  "Insert SONG's \"Title:\" row, if known."
  (supersonic-now-playing--insert-field "Title" (plist-get song :title)))

(defun supersonic-now-playing-render-artist (_buff song)
  "Insert SONG's \"Artist:\" row, if known."
  (supersonic-now-playing--insert-field "Artist" (plist-get song :artist)))

(defun supersonic-now-playing-render-album (_buff song)
  "Insert SONG's \"Album:\" row, if known."
  (supersonic-now-playing--insert-field "Album" (plist-get song :album)))

(defun supersonic-now-playing-render-duration (buff _song)
  "Insert BUFF's \"Duration:\" row."
  (supersonic-now-playing--insert-field
   "Duration"
   (supersonic-now-playing--position
    (buffer-local-value 'supersonic-now-playing--position buff)
    (buffer-local-value 'supersonic-now-playing--duration buff))
   'duration))

(defun supersonic-now-playing-render-format (_buff song)
  "Insert SONG's \"Format:\" row, if known."
  (supersonic-now-playing--insert-field "Format" (supersonic-now-playing--format song)))

(defun supersonic-now-playing-render-size (_buff song)
  "Insert SONG's \"Size:\" row, if known."
  (let ((size (plist-get song :size)))
    (supersonic-now-playing--insert-field "Size" (and size (format "%.2f MB" (/ size 1048576.0))))))

(defun supersonic-now-playing-render-queue-position (buff _song)
  "Insert BUFF's \"Queue:\" row, e.g. \"Queue: 4/19\", if known."
  (let ((place (buffer-local-value 'supersonic-now-playing--queue-place buff)))
    (supersonic-now-playing--insert-field
     "Queue" (and place (format "%d/%d" (car place) (cdr place))))))

(defun supersonic-now-playing--current-queue-place (queue)
  "Return (POSITION . TOTAL) of the current entry in QUEUE, or nil.
POSITION counts from 1."
  (let ((position 0) found)
    (dolist (entry queue)
      (setq position (1+ position))
      (when (plist-get entry :current)
        (setq found position)))
    (and found (cons found (length queue)))))

(defun supersonic-now-playing-layout-art (_buff song)
  "Insert SONG's cover art row, if it has cover art.
The art overlay animations draw on this row."
  (let ((art (supersonic-now-playing--art song)))
    (when art
      (insert (propertize art 'supersonic-now-playing-field 'art) "\n"))))

(defun supersonic-now-playing-layout-waveform (_buff _song)
  "Insert the row of the separate waveform seekbar, if available."
  (when (supersonic-waveform-available-p)
    (insert (propertize " " 'supersonic-now-playing-field 'waveform) "\n")))

(defun supersonic-now-playing-layout-label (_buff _song)
  "Insert the label row and a blank line."
  (insert (propertize " " 'supersonic-now-playing-field 'label) "\n\n"))

(defun supersonic-now-playing-layout-buttons (buff _song)
  "Insert the transport buttons, as BUFF's playback state says, and a blank line."
  (supersonic-now-playing--insert-button "|◀◀" #'supersonic-prev-track)
  (insert "  ")
  (supersonic-now-playing--insert-button
   (if (buffer-local-value 'supersonic-now-playing--paused buff)
       "▶"
     "⏸")
   #'supersonic-toggle-playing)
  (insert "  ")
  (supersonic-now-playing--insert-button "▶▶|" #'supersonic-skip-track)
  (insert "  ")
  (supersonic-now-playing--insert-button "◀◀" #'supersonic-seek-back)
  (insert "  ")
  (supersonic-now-playing--insert-button "▶▶" #'supersonic-seek-forward)
  (insert "\n\n"))

(defun supersonic-now-playing--render (buff song paused position track-id &optional queue-place)
  "Show SONG in BUFF, or that nothing is playing if SONG is nil.
PAUSED, POSITION in seconds, TRACK-ID and QUEUE-PLACE, (POSITION
. TOTAL) or nil, describe the playback state."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (let* ((inhibit-read-only t)
             (duration (plist-get song :duration))
             (same-track (and track-id (equal track-id supersonic-now-playing--track-id))))
        (setq supersonic-now-playing--paused paused)
        (setq supersonic-now-playing--duration duration)
        (setq supersonic-now-playing--position position)
        (setq supersonic-now-playing--track-id track-id)
        (setq supersonic-now-playing--queue-place queue-place)
        (setq supersonic-now-playing--title (plist-get song :title))
        (setq supersonic-now-playing--artist (plist-get song :artist))
        (setq supersonic-now-playing--album (plist-get song :album))
        (setq supersonic-now-playing--art-id (plist-get song :art))
        (unless same-track
          ;; A new track starts the animation afresh.
          (setq supersonic-now-playing--field-index 0)
          (setq supersonic-now-playing--cycle-elapsed 0)
          (setq supersonic-now-playing--cycle-tick nil)
          (setq supersonic-now-playing--cycle-changed nil)
          (setq supersonic-now-playing--animation-last-time nil)
          (setq supersonic-now-playing--scroll-offset 0)
          (setq supersonic-now-playing--scroll-phase 'pause-start)
          (setq supersonic-now-playing--scroll-pause-elapsed 0)
          (setq supersonic-now-playing--waveform nil)
          (setq supersonic-now-playing--waveform-bucket nil)
          (setq supersonic-now-playing--waveform-requested nil))
        (if song
            (supersonic-now-playing--start-timer)
          (supersonic-now-playing--stop-timer))
        (if (and song supersonic-now-playing-cycle-fields)
            (supersonic-now-playing--start-animation-timer)
          (supersonic-now-playing--stop-animation-timer))
        (erase-buffer)
        (if (not song)
            (insert "Nothing is playing.\n")
          (progn
            (supersonic-now-playing--run-field-functions supersonic-now-playing-layout-functions buff song)
            (supersonic-now-playing--run-field-functions supersonic-now-playing-render-functions buff song)
            (supersonic-now-playing--run-field-functions supersonic-now-playing-animation-functions buff 0)))
        (goto-char (point-min))
        ;; Put back the seekbar `erase-buffer' took.
        (when supersonic-now-playing--waveform
          (supersonic-now-playing--show-waveform buff track-id (cdr supersonic-now-playing--waveform)))))))

(aio-defun supersonic-now-playing-fetch-and-render (buff)
  "Show the current track of the active backend in BUFF.
If the track can't be looked up, show its id."
  (supersonic--with-async-error-handling buff "refresh the now-playing buffer"
    (if (supersonic-playback-live-p)
        (let ((track-id (aio-await (supersonic-playback-status 'track-id))))
          (if track-id
              (let* ((outcome (aio-await (aio-catch (supersonic-provider-track track-id))))
                     (song
                      (if (eq (car outcome) :success)
                          (cdr outcome)
                        (list :id track-id :title track-id))))
                (when (and (supersonic-art-available-p) (plist-get song :art))
                  (aio-await (aio-catch (supersonic--fetch-art (plist-get song :art) supersonic-now-playing-art-size))))
                ;; After the art fetch, which may be slow, so they are current.
                (let* ((position-promise (supersonic-playback-status 'position))
                       (paused-promise (supersonic-playback-status 'paused))
                       (queue-promise (supersonic-playback-queue))
                       (position (aio-await position-promise))
                       (paused (aio-await paused-promise))
                       (queue-place (supersonic-now-playing--current-queue-place (aio-await queue-promise))))
                  (supersonic-now-playing--render
                   buff song paused position track-id queue-place)
                  (supersonic-now-playing-maybe-fetch-waveform buff position)))
            (supersonic-now-playing--render buff nil nil nil nil)))
      (supersonic-now-playing--render buff nil nil nil nil))))

(defun supersonic-now-playing--show-waveform (buff track-id envelope)
  "Show ENVELOPE in BUFF, if TRACK-ID is still its current track."
  (when (and envelope (buffer-live-p buff))
    (with-current-buffer buff
      ;; The buffer may have moved on to another track meanwhile.
      (when (equal track-id supersonic-now-playing--track-id)
        (setq supersonic-now-playing--waveform (cons track-id envelope))
        (let ((progress
               (supersonic-now-playing--progress-ratio
                supersonic-now-playing--position supersonic-now-playing--duration)))
          (setq supersonic-now-playing--waveform-bucket (supersonic-now-playing--progress-bucket envelope progress))
          (supersonic-now-playing--update-waveform-field buff envelope progress))))))

(defun supersonic-now-playing-maybe-fetch-waveform (buff position)
  "Request the waveform of BUFF's current track, unless already requested.
Only if waveforms are available, BUFF is visible and something shows
the waveform.  POSITION is ignored."
  (ignore position)
  (let ((track-id (buffer-local-value 'supersonic-now-playing--track-id buff)))
    (when (and (supersonic-waveform-available-p)
               track-id
               (buffer-live-p buff)
               (get-buffer-window buff t)
               (not (buffer-local-value 'supersonic-now-playing--waveform-requested buff))
               (or (supersonic-now-playing--field-present-p buff 'waveform) (supersonic-art-available-p)))
      (with-current-buffer buff
        (setq supersonic-now-playing--waveform-requested t))
      (supersonic-waveform-ensure
       track-id
       (lambda (envelope) (supersonic-now-playing--show-waveform buff track-id envelope))
       (lambda (envelope) (supersonic-now-playing--show-waveform buff track-id envelope))))))

(defun supersonic-now-playing-refresh ()
  "Refresh the now-playing buffer."
  (interactive)
  (supersonic-now-playing-fetch-and-render (current-buffer)))

(defvar supersonic-now-playing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-now-playing-refresh)
    (define-key map (kbd "SPC") #'supersonic-toggle-playing)
    (define-key map (kbd "n") #'supersonic-skip-track)
    (define-key map (kbd "p") #'supersonic-prev-track)
    (define-key map (kbd "f") #'supersonic-seek-forward)
    (define-key map (kbd "b") #'supersonic-seek-back)
    map)
  "Keymap for `supersonic-now-playing-mode'.")

(define-derived-mode supersonic-now-playing-mode special-mode "Supersonic Now Playing"
  "Major mode for the buffer opened by `supersonic-show-now-playing'."
  ;; Undo would keep every replaced image alive.
  (buffer-disable-undo)
  (supersonic--add-playback-hooks))

;;;###autoload
(defun supersonic-show-now-playing ()
  "Show the current track of the active backend.
The buffer updates itself as playback changes."
  (interactive)
  (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-now-playing-mode)
        (supersonic-now-playing-mode)))
    (ignore (supersonic-now-playing-fetch-and-render buff))
    (pop-to-buffer-same-window buff)))

(defun supersonic--format-duration (format seconds)
  "Return SECONDS formatted with `format-seconds' FORMAT, or \"\" if nil."
  (if seconds
      (format-seconds format seconds)
    ""))

;;;
;;; Search
;;;
(defun supersonic-search-rows (results)
  "Return tabulated-list entries for the search RESULTS.
Each entry's id is (ID . TYPE), TYPE being \"artist\", \"album\" or
\"song\"."
  (append
   (mapcar
    (lambda (artist)
      (list (cons (plist-get artist :id) "artist") (vector "Artist" (or (plist-get artist :name) ""))))
    (plist-get results :artists))
   (mapcar
    (lambda (album) (list (cons (plist-get album :id) "album") (vector "Album" (or (plist-get album :name) ""))))
    (plist-get results :albums))
   (mapcar
    (lambda (track) (list (cons (plist-get track :id) "song") (vector "Song" (or (plist-get track :title) ""))))
    (plist-get results :tracks))))

(aio-defun supersonic-search-refresh (query buff)
  "Show the search results for QUERY in BUFF."
  (supersonic--with-async-error-handling buff "search"
    (let ((results (aio-await (supersonic-provider-search query))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list (supersonic-search-rows results)))))))

(defun supersonic-open-search-appropriate-result (result)
  "Open the search RESULT, an (ID . TYPE)."
  (let ((type (cdr result)))
    (cond
     ((string-equal type "artist")
      (supersonic-albums (car result)))
     ((string-equal type "album")
      (supersonic-tracks (car result)))
     ((string-equal type "song")
      (supersonic-playback-start (list (car result)))))))

(defun supersonic-open-search-result ()
  "Open the search result at point."
  (interactive)
  (supersonic-open-search-appropriate-result (tabulated-list-get-id)))

(defvar supersonic-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-search-result)
    map)
  "Keymap for `supersonic-search-mode'.")

;;;###autoload
(defun supersonic-search ()
  "Search the library and list the results."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-search*")))
    (supersonic--init-list-buffer new-buff #'supersonic-search-mode "Searching...")
    (ignore (supersonic-search-refresh (read-string "Query: ") new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode supersonic-search-mode tabulated-list-mode "Supersonic search mode"
  "Major mode for search results, see `supersonic-search'."
  (setq tabulated-list-format [("Type" 10 t) ("Name" 30 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (tabulated-list-init-header))


;;;
;;; Tracks
;;;

(defun supersonic-get-tracklist-id (id)
  "Return the ids of the tracks from ID to the end of the list."
  (mapcar #'car (seq-drop-while (lambda (entry) (not (equal (car entry) id))) tabulated-list-entries)))

(defun supersonic-tracks-rows (tracks)
  "Return tabulated-list entries for TRACKS."
  (mapcar
   (lambda (track)
     (list
      (plist-get track :id)
      (vector
       (or (plist-get track :title) "")
       (supersonic--format-duration "%m:%.2s" (plist-get track :duration))
       (format "%d" (or (plist-get track :track) 0)))))
   tracks))

(aio-defun supersonic-get-album-track-ids (id)
  "Return a promise resolving to the track ids of album ID."
  (mapcar (lambda (track) (plist-get track :id)) (aio-await (supersonic-provider-album-tracks id))))

(aio-defun supersonic-tracks-refresh (id buff)
  "Show the tracks of album ID in BUFF."
  (supersonic--with-async-error-handling buff "fetch tracks"
    (let ((tracks (aio-await (supersonic-provider-album-tracks id))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list (supersonic-tracks-rows tracks)))))))

(defun supersonic-play-tracks ()
  "Play the track at point and the ones after it."
  (interactive)
  (supersonic-playback-start (supersonic-get-tracklist-id (tabulated-list-get-id))))

(defun supersonic-enqueue-tracks ()
  "Add the track at point and the ones after it to the play queue."
  (interactive)
  (let ((ids (supersonic-get-tracklist-id (tabulated-list-get-id))))
    (supersonic-playback-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids))))

(defvar supersonic-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-play-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-tracks)
    map)
  "Keymap for `supersonic-tracks-mode'.")

(defun supersonic-tracks (id)
  "Show the tracks of album ID."
  (let ((new-buff (get-buffer-create "*supersonic-tracks*")))
    (supersonic--init-list-buffer new-buff #'supersonic-tracks-mode "Loading tracks...")
    (ignore (supersonic-tracks-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode supersonic-tracks-mode tabulated-list-mode "Supersonic Tracks"
  "Major mode for the tracks of an album, see `supersonic-tracks'."
  (setq tabulated-list-format [("Title" 30 t) ("Duration" 10 t) ("Track" 10 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (tabulated-list-init-header))

;;;
;;; Albums
;;;

(defun supersonic--arts (items)
  "Return the `:art' references of ITEMS."
  (mapcar (lambda (item) (plist-get item :art)) items))

(defun supersonic-albums-rows (albums)
  "Return tabulated-list entries for an artist's ALBUMS."
  (mapcar
   (lambda (album)
     (list
      (plist-get album :id)
      (vector (format "%d" (or (plist-get album :year) 0))
              (propertize (or (plist-get album :name) "") 'face 'supersonic-albums-name)
              "")))
   albums))

(defun supersonic-albums-type-rows (albums)
  "Return tabulated-list entries for ALBUMS by various artists."
  (mapcar
   (lambda (album)
     (list
      (plist-get album :id)
      (vector (propertize (or (plist-get album :name) "") 'face 'supersonic-albums-name)
              (propertize (or (plist-get album :artist) "") 'face 'supersonic-albums-artist)
              "")))
   albums))

(aio-defun supersonic-albums-refresh (id buff)
  "Show the albums of artist ID in BUFF."
  (supersonic--with-async-error-handling buff "fetch albums"
    (let ((albums (aio-await (supersonic-provider-artist-albums id))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list (supersonic-albums-rows albums))
          (supersonic-get-images tabulated-list-entries (supersonic--arts albums) 2 buff))))))


(aio-defun supersonic-albums-refresh-type (type buff)
  "Show the albums of list TYPE in BUFF.
TYPE is one of `supersonic-provider-album-list-types'."
  (supersonic--with-async-error-handling buff "fetch albums"
    (let ((albums (aio-await (supersonic-provider-album-list type supersonic-album-list-count))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list (supersonic-albums-type-rows albums))
          (supersonic-get-images tabulated-list-entries (supersonic--arts albums) 2 buff))))))

(defun supersonic-open-tracks ()
  "Show the tracks of the album at point."
  (interactive)
  (supersonic-tracks (tabulated-list-get-id)))

(aio-defun supersonic-enqueue-album ()
  "Add all the tracks of the album at point to the play queue." (interactive)
  (supersonic--with-async-error-handling nil "enqueue album"
    (let* ((track-id (tabulated-list-get-id))
           (ids (aio-await (supersonic-get-album-track-ids track-id))))
      (supersonic-playback-enqueue ids)
      (message "Added %d track(s) to the queue" (length ids)))))

(defvar supersonic-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map)
  "Keymap for `supersonic-album-mode'.")

(defvar supersonic-album-type-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map)
  "Keymap for `supersonic-album-type-mode'.")

;;;###autoload
(defun supersonic-recent-albums ()
  "Show a list of recently played albums."
  (interactive)
  (supersonic-albums nil 'recent))

;;;###autoload
(defun supersonic-random-albums ()
  "Show a list of random albums."
  (interactive)
  (supersonic-albums nil 'random))

;;;###autoload
(defun supersonic-newest-albums ()
  "Show a list of recently added albums."
  (interactive)
  (supersonic-albums nil 'newest))

(defun supersonic-albums (&optional id type)
  "Show the albums of artist ID, or of list TYPE.
TYPE is one of `supersonic-provider-album-list-types', or its name."
  (when (stringp type)
    (setq type (intern type)))
  (cond
   (id
    (let ((new-buff (get-buffer-create "*supersonic-artist-albums*")))
      (supersonic--init-list-buffer new-buff #'supersonic-album-mode "Loading albums...")
      (ignore (supersonic-albums-refresh id new-buff))
      (pop-to-buffer-same-window new-buff)))
   (type
    (let ((new-buff (get-buffer-create "*supersonic-albums*")))
      (supersonic--init-list-buffer new-buff #'supersonic-album-type-mode "Loading albums...")
      (ignore (supersonic-albums-refresh-type type new-buff))
      (pop-to-buffer-same-window new-buff)))))

(define-derived-mode supersonic-album-type-mode tabulated-list-mode "Supersonic Album List"
  "Major mode for album lists such as the newest or random albums."
  (setq tabulated-list-format [("Albums" 30 t) ("Artists" 30 t) ("Art" 30 nil)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (setq tabulated-list-use-header-line supersonic-list-use-header-line)
  (tabulated-list-init-header))

(define-derived-mode supersonic-album-mode tabulated-list-mode "Supersonic Albums"
  "Major mode for the albums of an artist, see `supersonic-albums'."
  (setq tabulated-list-format [("Year" 5 t) ("Albums" 40 t) ("Art" 30 nil)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (setq tabulated-list-use-header-line supersonic-list-use-header-line)
  (tabulated-list-init-header))

;;;
;;; Artists
;;;
(defun supersonic-open-album ()
  "Show the albums of the artist at point."
  (interactive)
  (supersonic-albums (tabulated-list-get-id)))

(defun supersonic-artists-rows (artists)
  "Return tabulated-list entries for ARTISTS."
  (mapcar (lambda (artist) (list (plist-get artist :id) (vector (or (plist-get artist :name) "") ""))) artists))

(aio-defun supersonic-artists-refresh (buff)
  "Show the artists in BUFF."
  (supersonic--with-async-error-handling buff "fetch artists"
    (let ((artists (aio-await (supersonic-provider-artists))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list (supersonic-artists-rows artists))
          (when (> (length tabulated-list-format) 1)
            (supersonic-art-fill-visible artists 1 supersonic-artist-art-size buff)))))))

(defun supersonic-artists-revert ()
  "Refresh the artist list."
  (interactive)
  (supersonic-artists-refresh (current-buffer)))

(defvar supersonic-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-album)
    (define-key map (kbd "g") #'supersonic-artists-revert)
    map)
  "Keymap for `supersonic-artist-mode'.")

;;;###autoload
(defun supersonic-artists ()
  "Show the artists in the library."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-artists*")))
    (supersonic--init-list-buffer new-buff #'supersonic-artist-mode "Loading artists...")
    (ignore (supersonic-artists-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(defun supersonic-artists-format ()
  "Return the `tabulated-list-format' of the artist list.
It has an Art column only if artist images are shown."
  (if (and supersonic-artist-art-size (supersonic-art-available-p))
      `[("Artist" 30 t) ("Art" ,(max 3 (1+ (ceiling supersonic-artist-art-size (frame-char-width)))) nil)]
    [("Artist" 30 t)]))

(define-derived-mode supersonic-artist-mode tabulated-list-mode "Supersonic Artists"
  "Major mode for the artist list, see `supersonic-artists'."
  (setq tabulated-list-format (supersonic-artists-format))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-use-header-line supersonic-list-use-header-line)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (tabulated-list-init-header))

;;;
;;; Podcasts
;;;
(defun supersonic-podcasts-rows (podcasts)
  "Return tabulated-list entries for PODCASTS."
  (mapcar (lambda (channel) (list (plist-get channel :id) (vector (or (plist-get channel :title) "") ""))) podcasts))

(aio-defun supersonic-podcasts-refresh (buff)
  "Show the podcasts in BUFF."
  (supersonic--with-async-error-handling buff "fetch podcasts"
    (let ((podcasts (aio-await (supersonic-provider-podcasts))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list (supersonic-podcasts-rows podcasts))
          (supersonic-get-images tabulated-list-entries (supersonic--arts podcasts) 1 buff))))))


(defun supersonic-open-podcast-episodes ()
  "Show the episodes of the podcast at point."
  (interactive)
  (supersonic-podcast-episodes (tabulated-list-get-id)))

(defun supersonic-add-podcast (url)
  "Subscribe to the podcast feed at URL."
  (interactive
   (progn
     (supersonic-provider-require 'add-podcast)
     (list (read-string "feed url: "))))
  (supersonic--add-podcast url))

(aio-defun supersonic--add-podcast (url)
  "Subscribe to the podcast feed at URL, reporting the outcome."
  (supersonic--with-async-error-handling nil "add podcast"
    (aio-await (supersonic-provider-add-podcast url))
    (message "Podcast added")))

(transient-define-prefix supersonic-podcast-help ()
  "Help transient for podcasts."
  ["Supersonic podcast help"
   ("a" "Add a podcast" supersonic-add-podcast :if supersonic--offer-add-podcast-p)
   ("RET" "Open a podcast" supersonic-open-podcast-episodes :if supersonic--offer-podcast-episodes-p)])

(defvar supersonic-podcast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-podcast-episodes)
    (define-key map (kbd "?") #'supersonic-podcast-help)
    (define-key map (kbd "a") #'supersonic-add-podcast)
    map)
  "Keymap for `supersonic-podcast-mode'.")

;;;###autoload
(defun supersonic-podcasts ()
  "Show the subscribed podcasts."
  (interactive)
  (supersonic-provider-require 'podcasts)
  (let ((new-buff (get-buffer-create "*supersonic-podcasts*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-mode "Loading podcasts...")
    (ignore (supersonic-podcasts-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode supersonic-podcast-mode tabulated-list-mode "Supersonic Podcasts"
  "Major mode for the podcast list, see `supersonic-podcasts'."
  (setq tabulated-list-format [("Podcasts" 30 t) ("Art" 20 nil)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (setq tabulated-list-use-header-line supersonic-list-use-header-line)
  (tabulated-list-init-header))

;;;
;;; Podcast episodes
;;;

(defun supersonic-podcast-episodes-rows (episodes)
  "Return tabulated-list entries for podcast EPISODES."
  (mapcar
   (lambda (episode)
     (list
      (plist-get episode :id)
      (vector
       (or (plist-get episode :title) "")
       (supersonic--format-duration "%h:%.2m:%.2s" (plist-get episode :duration))
       (or (plist-get episode :status) ""))))
   episodes))

(defun supersonic-play-podcast ()
  "Play the podcast episode at point."
  (interactive)
  (supersonic-playback-start (list (tabulated-list-get-id))))

(aio-defun supersonic-podcasts-episode-refresh (id buff)
  "Show the episodes of podcast ID in BUFF."
  (supersonic--with-async-error-handling buff "fetch episodes"
    (let ((episodes (aio-await (supersonic-provider-podcast-episodes id))))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (supersonic--print-list (supersonic-podcast-episodes-rows episodes)))))))

(defun supersonic-download-podcast-episode ()
  "Have the server download the podcast episode at point."
  (interactive)
  (supersonic-provider-require 'download-podcast-episode)
  (supersonic--download-podcast-episode (tabulated-list-get-id)))

(aio-defun supersonic--download-podcast-episode (id)
  "Start the download of episode ID, reporting the outcome."
  (supersonic--with-async-error-handling nil "download episode"
    (aio-await (supersonic-provider-download-podcast-episode id))
    (message "Episode download started")))

(transient-define-prefix supersonic-podcast-episode-help ()
  "Help transient for podcast episodes."
  ["Supersonic podcast episode help"
   ("d" "Download" supersonic-download-podcast-episode :if supersonic--offer-download-podcast-episode-p)
   ("RET" "Start playing" supersonic-play-podcast)])


(defvar supersonic-podcast-episodes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'supersonic-podcast-episode-help)
    (define-key map (kbd "RET") #'supersonic-play-podcast)
    (define-key map (kbd "d") #'supersonic-download-podcast-episode)
    map)
  "Keymap for `supersonic-podcast-episodes-mode'.")

(defun supersonic-podcast-episodes (id)
  "Show the episodes of podcast ID."
  (supersonic-provider-require 'podcast-episodes)
  (let ((new-buff (get-buffer-create "*supersonic-podcast-episodes*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-episodes-mode "Loading episodes...")
    (ignore (supersonic-podcasts-episode-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode supersonic-podcast-episodes-mode tabulated-list-mode "Supersonic Podcast Episodes"
  "Major mode for the episodes of a podcast, see `supersonic-podcast-episodes'."
  (setq tabulated-list-format [("Title" 50 t) ("Duration" 10 t) ("Status" 24 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
  (setq tabulated-list-use-header-line supersonic-list-use-header-line)
  (tabulated-list-init-header))

;;;
;;; Switching providers
;;;

(defconst supersonic--provider-list-modes
  '(supersonic-artist-mode supersonic-album-mode supersonic-album-type-mode
                           supersonic-tracks-mode supersonic-search-mode
                           supersonic-podcast-mode supersonic-podcast-episodes-mode)
  "Major modes of the list buffers showing a provider's items.
`supersonic-provider-switch' kills these buffers.")

(defun supersonic--kill-provider-list-buffers ()
  "Kill the buffers in one of `supersonic--provider-list-modes'."
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (apply #'derived-mode-p supersonic--provider-list-modes))
      (kill-buffer buffer))))

;;;###autoload
(defun supersonic-provider-switch (provider &optional backend)
  "Make PROVIDER the library provider and BACKEND the playback backend.
Without BACKEND, choose one with `supersonic-playback-backend-for-provider'.
If the provider changes, stop playback and kill the list buffers of
the old provider."
  (interactive
   (let ((providers (mapcar #'symbol-name (remq supersonic-provider (supersonic-provider-names)))))
     (unless providers
       (user-error "No other library provider is registered"))
     (let ((answer (completing-read "Switch to library provider: " providers nil t nil nil (car providers))))
       (when (equal answer "")
         (user-error "Kept the `%s' provider" supersonic-provider))
       (list (intern answer)))))
  (unless (supersonic-provider-registered-p provider)
    (user-error "No library provider named `%s' is registered" provider))
  (let ((backend (or backend (supersonic-playback-backend-for-provider provider)))
        (provider-changes (not (eq provider supersonic-provider))))
    (unless (supersonic-playback-compatible-p backend provider)
      (user-error "The `%s' playback backend cannot play for the `%s' provider" backend provider))
    (supersonic-playback-select backend provider)
    (when provider-changes
      (supersonic--kill-provider-list-buffers))
    (message "[Supersonic] Provider `%s', backend `%s'" provider backend)))

;;;
;;; Transient
;;;

;; Hide the commands the active provider can't serve.

(defun supersonic--offers-p (operation)
  "Return non-nil if the transient should offer commands needing OPERATION.
That is, if the active provider implements OPERATION or isn't
registered; in the latter case the command reports the problem."
  (or (not (supersonic-provider-registered-p supersonic-provider))
      (supersonic-provider-supports-p operation)))

(defun supersonic--offer-artists-p ()
  "Return non-nil if the transient should offer listing artists."
  (supersonic--offers-p 'artists))

(defun supersonic--offer-album-list-p ()
  "Return non-nil if the transient should offer album lists."
  (supersonic--offers-p 'album-list))

(defun supersonic--offer-search-p ()
  "Return non-nil if the transient should offer searching."
  (supersonic--offers-p 'search))

(defun supersonic--offer-podcasts-p ()
  "Return non-nil if the transient should offer listing podcasts."
  (supersonic--offers-p 'podcasts))

(defun supersonic--offer-podcast-episodes-p ()
  "Return non-nil if the transient should offer podcast episodes."
  (supersonic--offers-p 'podcast-episodes))

(defun supersonic--offer-add-podcast-p ()
  "Return non-nil if the transient should offer adding a podcast."
  (supersonic--offers-p 'add-podcast))

(defun supersonic--offer-download-podcast-episode-p ()
  "Return non-nil if the transient should offer downloading an episode."
  (supersonic--offers-p 'download-podcast-episode))

;;;###autoload (autoload 'supersonic "supersonic" nil t)
(transient-define-prefix supersonic ()
  "Show the supersonic.el commands."
  [["Supersonic"
    ("a" "Artists" supersonic-artists :if supersonic--offer-artists-p)
    ("e" "Recent Albums" supersonic-recent-albums :if supersonic--offer-album-list-p)
    ("r" "Random Albums" supersonic-random-albums :if supersonic--offer-album-list-p)
    ("n" "Newest Albums" supersonic-newest-albums :if supersonic--offer-album-list-p)
    ("s" "Search" supersonic-search :if supersonic--offer-search-p)
    ("p" "Podcasts" supersonic-podcasts :if supersonic--offer-podcasts-p)]
   ["Controls"
    ("Q" "Show queue" supersonic-show-queue)
    ("N" "Now playing" supersonic-show-now-playing)
    ("SPC" "Toggle playing" supersonic-toggle-playing)
    ("f" "Skip track" supersonic-skip-track)
    ("b" "Previous track" supersonic-prev-track)
    ("F" "Seek forward" supersonic-seek-forward :transient t)
    ("B" "Seek back" supersonic-seek-back :transient t)
    ("k" "Switch backend" supersonic-playback-switch-backend)
    ("P" "Switch provider" supersonic-provider-switch)]])

(defun supersonic-unload-function ()
  "Remove the playback hooks before unloading."
  (supersonic--remove-playback-hooks)
  nil)

(provide 'supersonic)

;;; supersonic.el ends here
