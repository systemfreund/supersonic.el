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
  "Functions laying out the now-playing buffer's primary rows, in order.
Each is called as (FUNCTION BUFF SONG), as
`supersonic-now-playing-render-functions' are, before those.  Each
built-in inserts its row and any spacing after it, so reordering the
list reorders the rows, and dropping one drops its row.  A function
that signals an error is reported and skipped.

Four are built in, the first three making up the default:

- `supersonic-now-playing-layout-art': SONG's cover art, if any.
- `supersonic-now-playing-layout-label': a placeholder row for the
  field `supersonic-now-playing-animation-functions' cycles through.
- `supersonic-now-playing-layout-buttons': the transport buttons.
- `supersonic-now-playing-layout-waveform': a standalone waveform
  seekbar, independent of the one in the cover art overlay.

A function of your own can read BUFF's buffer-local state, such as
`supersonic-now-playing--paused', as the built-ins do."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-now-playing-animation-functions (list #'supersonic-now-playing-animate-art-overlay)
  "Functions run in order on every animation tick to render the buffer.
Each is called as (FUNCTION BUFF DELTA), DELTA being the real seconds
elapsed since the last tick, or 0 for an unconditional first paint.  A
function that signals an error is reported and skipped.

Three are built in:

- `supersonic-now-playing-animate-label': shows the current field of
  `supersonic-now-playing-cycle-fields' as a label below the art.
- `supersonic-now-playing-animate-art-overlay' (the default): shows
  it on the cover art itself.
- `supersonic-now-playing-animate-art-overlay-scroll': shows every
  field joined into one line on the art, scrolling it back and forth
  if it does not fit.

List two of them to show the field in both places.  See
`supersonic-now-playing-position-functions' for the counterpart driven
by the playback position."
  :type '(repeat function)
  :group 'supersonic)

(defcustom supersonic-now-playing-position-functions
  (list #'supersonic-now-playing-update-duration-field
        #'supersonic-now-playing-recolor-waveform
        #'supersonic-now-playing-maybe-fetch-waveform)
  "Functions run in order whenever the now-playing buffer's position updates.
Each is called as (FUNCTION BUFF POSITION), POSITION being what the
active backend reported, on `supersonic-now-playing-interval''s timer
and right after a seek.  A function that signals an error is reported
and skipped.

Three are built in, and make up the default:

- `supersonic-now-playing-update-duration-field': redraws the
  \"Duration:\" line.
- `supersonic-now-playing-recolor-waveform': redraws the waveform
  seekbar's played/unplayed split.
- `supersonic-now-playing-maybe-fetch-waveform': starts waveform
  generation for the current track, unless already requested.

A function of your own -- synced lyrics, say -- runs alongside them."
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
  "Functions laying out the now-playing buffer's informational text rows.
Each is called as (FUNCTION BUFF SONG), in order, after
`supersonic-now-playing-layout-functions'.  SONG is the track as
`supersonic-provider-track' resolved it: `:title', `:artist', `:album',
`:duration' (seconds), `:track', `:art', `:suffix', `:content-type' and
`:size' (bytes), each left out when unknown, plus keys of the
provider's own, such as Subsonic's `:genre'.  Should the lookup fail,
SONG is just (:id ID :title ID).  A function that signals an error is
reported and skipped.

The built-ins each leave their row out when its value is missing:
`supersonic-now-playing-render-title', `-artist', `-album',
`-duration', `-queue-position', `-format' and `-size'.  A function of
your own -- a \"Genre:\" row, say -- inserts its row alongside."
  :type '(repeat function)
  :group 'supersonic)

;;;
;;; List buffers
;;;

(defun supersonic--cell-label (cell)
  "Return the text CELL shows in a tabulated-list row, or nil for an image.
CELL is a column descriptor as in `tabulated-list-entries'."
  (cond
   ((stringp cell) cell)
   ;; `tabulated-list-print-col' draws an image over a single space, so the image's real width is unknown here.
   ;; Supersonic's own art is a `display'-property string in the last column, which is never measured.
   ((eq (car cell) 'image) nil)
   (t (car cell))))

(defun supersonic--cell-width (cell)
  "Return the width in characters CELL takes up in a tabulated-list row.
CELL is a column descriptor as in `tabulated-list-entries'.  Blind to
faces; `supersonic--cell-columns' sees them."
  (let ((label (supersonic--cell-label cell)))
    (if label (string-width label) 1)))

(defun supersonic--pixelwise-p ()
  "Return non-nil if cells can be measured as drawn, from Emacs 29."
  (fboundp 'string-pixel-width))

(defun supersonic--string-pixel-width (string)
  "Return the width of STRING in pixels as drawn in the selected window.
Counts the current buffer's face remappings (e.g. `text-scale-adjust')
where `string-pixel-width' can, from Emacs 31; only if there are any,
as that makes measuring several times slower.  Before Emacs 29, which
lacks it, makes do with STRING's width in characters."
  (if (fboundp 'string-pixel-width)
      (if (and face-remapping-alist (>= (cdr (func-arity 'string-pixel-width)) 2))
          (funcall #'string-pixel-width string (current-buffer))
        (string-pixel-width string))
    (* (string-width string) (frame-char-width))))

(defvar-local supersonic--column-pixels nil
  "Width of a column, in pixels, in the window the list was last fitted to.")

(defvar-local supersonic--cell-pixels nil
  "Each cell's width in pixels as drawn, by cell, from the last fit.
An `eq' hash table filled by `supersonic--cell-columns'.")

(defvar-local supersonic--cut-cells nil
  "The cells `supersonic--shown-cells' cut down, by cell.
An `eq' hash table of (PIXELS . SHOWN): SHOWN is the cell cut to fit
PIXELS.")

(defun supersonic--cell-columns (cell)
  "Return how many columns CELL takes up as drawn in the selected window.
Remembers its pixel width in `supersonic--cell-pixels'.  Never less
than its width in characters, by which `tabulated-list-print' truncates
it anyway, so a cell in a narrow face is not cut short."
  (let ((label (supersonic--cell-label cell)))
    (if (not label)
        1
      (max (string-width label)
           (ceiling (or (gethash cell supersonic--cell-pixels)
                        (puthash cell (supersonic--string-pixel-width label) supersonic--cell-pixels))
                    supersonic--column-pixels)))))

(defun supersonic--widest-shrinkable-column (widths minimums)
  "Return the index of the widest of WIDTHS still above its MINIMUMS entry.
Nil when every column is down to its minimum.  Ties go to the leftmost."
  (cl-loop with widest = nil
           with widest-width = 0
           for width in widths
           for minimum in minimums
           for n from 0
           when (and (> width minimum) (> width widest-width))
           do (setq widest n widest-width width)
           finally return widest))

(defun supersonic--fit-column-widths (format entries available &optional measure)
  "Return a copy of FORMAT with its widths fitted to ENTRIES.
FORMAT and ENTRIES are as in `tabulated-list-format' and
`tabulated-list-entries'.  Every column but the last becomes as wide as
its widest cell, though never narrower than its header plus room for
the sort indicator, nor than one column.  The last column, which
`tabulated-list-print' neither pads nor truncates, keeps its width, as
the room set aside for it (e.g. for cover art that arrives after the
first print), so art columns must come last.
When AVAILABLE, the columns a window line holds, is non-nil and the row
would not fit into it, the widest columns give way first, down to their
minimum; `supersonic--print-entry' then draws their cells cut down.
MEASURE, a function of a cell returning its width in columns, defaults
to `supersonic--cell-width'."
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
  "Return STRING shortened to fit PIXELS as drawn in the selected window.
What it leaves out becomes an ellipsis in STRING's face."
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
  "Return COLS as drawn: each cell too wide for its column cut down.
COLS is a vector of cells as in `tabulated-list-entries', left as it
is; a cut copy is returned only if a cell needs cutting.
`tabulated-list-print' truncates by characters alone, which leaves a
cell in a taller or wider face spilling into the next column; this cuts
by the pixel widths `supersonic--fit-columns' measured, against the
columns' current widths, so widening one with \\[tabulated-list-widen-current-column]
shows more again.  Like `tabulated-list-print', it leaves the last
column alone."
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
  "Insert the entry ID with COLS, cut down to fit, at point.
The `tabulated-list-printer' of a list fitted by `supersonic--fit-columns'.
Cutting only what is drawn keeps whole cells in `tabulated-list-entries'
and on the row, so sorting compares them whole."
  (let ((beg (point)))
    (tabulated-list-print-entry id (supersonic--shown-cells cols))
    (put-text-property beg (point) 'tabulated-list-entry cols)))

(defun supersonic--window-columns ()
  "Return how many columns the selected window's text area holds.
Columns of the frame's default font, the unit `:align-to' and so
`tabulated-list-print' place columns in, rather than the buffer's
possibly text-scaled one.  Leaves out line numbers and the column the
continuation glyph needs."
  (1- (/ (- (window-body-width nil t) (line-number-display-width t)) (frame-char-width))))

(defun supersonic--fit-columns ()
  "Fit the current tabulated-list buffer's columns to its entries.
The widths come from `supersonic--fit-column-widths', against the
window showing the buffer, or else the selected one, where a freshly
opened buffer is about to be shown.  From Emacs 29, cells are measured
as drawn there, so faces with their own height or typeface fit too,
and `supersonic--print-entry' draws those still too wide cut down.
Also on `tabulated-list-revert-hook', so \\[revert-buffer] refits, e.g.
after resizing the window."
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
              ;; Cut now, while the window fitted to is selected, rather than wherever the next print happens.
              (dolist (entry tabulated-list-entries)
                (supersonic--shown-cells (cadr entry))))
            (tabulated-list-init-header)))))))

(defun supersonic--print-list (entries)
  "Show ENTRIES in the current tabulated-list buffer, columns fitted to them.
ENTRIES is as in `tabulated-list-entries'; see `supersonic--fit-columns'."
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
  "Refresh the play queue buffer from the active backend, if it is open.
Called whenever the queue is likely to have changed: after
starting/enqueueing tracks and whenever the active backend reports a
track starting or ending."
  (let ((buff (supersonic-queue-buffer)))
    (when buff
      (supersonic-queue-fetch-and-render buff))))

(aio-defun
 supersonic-queue-parse (entries)
 "Turn ENTRIES into tabulated-list entries.
ENTRIES are as resolved by `supersonic-playback-queue'.  Each entry's
track is looked up through `supersonic-provider-track', all at once,
unless the entry brings its `:track' along; a failed lookup shows as
\"?\"."
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
   ;; `dolist' rather than `mapcar', so that `aio-await' stays in this
   ;; function's own body, where generator.el can transform it.
   (let (rows)
     (dolist (item pending)
       (pcase-let ((`(,index ,track-id ,current ,brought ,promise) item))
         (let* ((outcome (and promise (aio-await promise)))
                (track (or brought (and outcome (eq (car outcome) :success) (cdr outcome)))))
           (push (list
                  ;; Without a track id, the position serves as the row's id.
                  (or track-id (number-to-string index))
                  (vector
                   ;; A blank rather than "" when not current, so the fitted marker column keeps its width.
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

(aio-defun
 supersonic-queue-fetch-and-render
 (buff)
 "Query the active playback backend for its play queue and render it into BUFF."
 (when (buffer-live-p buff)
   (let ((entries (aio-await (supersonic-queue-parse (aio-await (supersonic-playback-queue))))))
     (when (buffer-live-p buff)
       (with-current-buffer buff
         (supersonic--print-list entries))))))

(defun supersonic-queue-refresh ()
  "Refresh the play queue buffer from the active backend's current play queue."
  (interactive)
  (supersonic-queue-fetch-and-render (current-buffer)))

(defvar supersonic-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-queue-refresh)
    map)
  "Keymap for `supersonic-queue-mode'.")

(define-derived-mode
 supersonic-queue-mode
 tabulated-list-mode
 "Supersonic Queue"
 "Major mode for the play queue opened by `supersonic-show-queue'.
Mirrors the active backend's play queue, refreshed whenever it may have changed."
 (setq tabulated-list-format [("" 2 nil) ("Title" 40 t) ("Artist" 25 t) ("Album" 25 t)])
 (setq tabulated-list-padding 2)
 (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
 (supersonic--add-playback-hooks)
 (tabulated-list-init-header))

;;;###autoload
(defun supersonic-show-queue ()
  "Open a buffer showing the active backend's current play queue."
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
  "Timer ticking the playback position shown in the now-playing buffer.")

(defvar supersonic-now-playing--animation-timer nil
  "Timer advancing `supersonic-now-playing-cycle-fields'.
Only runs while that list is non-nil and a track is playing; see
`supersonic-now-playing--animation-tick'.")

(defvar supersonic-now-playing--animation-timer-interval nil
  "Frame interval the animation timer was last started with, or nil.
A timer's period is fixed, so
`supersonic-now-playing--start-animation-timer' compares this with
`supersonic-now-playing-animation-frame-interval' to pick up a change
made while playing.")

(defvar-local supersonic-now-playing--title nil
  "Title of the track the now-playing buffer is currently showing, or nil.
Kept, like `supersonic-now-playing--artist' and
`supersonic-now-playing--album', for the animation to rotate through.")

(defvar-local supersonic-now-playing--artist nil
  "Artist of the track the now-playing buffer is currently showing, or nil.
See `supersonic-now-playing--title'.")

(defvar-local supersonic-now-playing--album nil
  "Album of the track the now-playing buffer is currently showing, or nil.
See `supersonic-now-playing--title'.")

(defvar-local supersonic-now-playing--art-id nil
  "Cover art id of the track the now-playing buffer is currently showing, or nil.
Kept for the art overlay to redraw the art with.")

(defvar-local supersonic-now-playing--field-index 0
  "Index into `supersonic-now-playing-cycle-fields' of the field currently shown.
Reset to 0, the title, whenever a new track is rendered.")

(defvar-local supersonic-now-playing--cycle-elapsed 0
  "Seconds accumulated toward the next `supersonic-now-playing--advance-field'.
Real elapsed time, so that fields switch on schedule however unevenly
the timer fires.  Reset to 0 whenever a new track is rendered.")

(defvar-local supersonic-now-playing--cycle-tick nil
  "Animation timestamp as of the last DELTA actually accumulated, or nil.
Both field-cycling built-ins in `supersonic-now-playing-animation-functions'
get the same DELTA in a tick; comparing this with
`supersonic-now-playing--animation-last-time' keeps it from being
accumulated twice.")

(defvar-local supersonic-now-playing--cycle-changed nil
  "Whether `supersonic-now-playing--advance-field' last switched the field.
Cached so a second call within the same tick (see
`supersonic-now-playing--cycle-tick') answers the same way the first
one did instead of re-deciding from DELTA already spent.")

(defvar-local supersonic-now-playing--animation-last-time nil
  "Value of `float-time' at this buffer's last animation tick, or nil.
For `supersonic-now-playing--animation-tick' to measure real elapsed
time.  Reset to nil whenever a new track is rendered.")

(defvar-local supersonic-now-playing--scroll-offset 0
  "Pixels `supersonic-now-playing-animate-art-overlay-scroll' has scrolled so far.
0 is the text's left edge, `supersonic-art-scroll-max-offset' its
right one.  Reset to 0 whenever a new track is rendered.")

(defvar-local supersonic-now-playing--scroll-phase 'pause-start
  "Where `supersonic-now-playing--advance-scroll' currently is in its bounce.
One of `pause-start', `forward', `pause-end' and `backward', cycled in
that order.  Reset to `pause-start' whenever a new track is rendered.")

(defvar-local supersonic-now-playing--scroll-pause-elapsed 0
  "Seconds accumulated in the current `pause-start' or `pause-end' phase.
Compared against `supersonic-now-playing-scroll-pause'.")

(defvar-local supersonic-now-playing--duration nil
  "Duration in seconds of the track the now-playing buffer is showing.")

(defvar-local supersonic-now-playing--queue-place nil
  "Where the now-playing buffer's track sits in the play queue, or nil.
A (POSITION . TOTAL) pair, POSITION being 1-based; nil when the
active backend has no queue to report on.")

(defvar-local supersonic-now-playing--track-id nil
  "Id of the track the now-playing buffer is currently showing, or nil.
Lets a waveform arriving after a track change be discarded.")

(defvar-local supersonic-now-playing--paused nil
  "Whether the now-playing buffer's last render showed playback as paused.
For `supersonic-now-playing-layout-buttons' to read.")

(defvar-local supersonic-now-playing--position nil
  "Playback position in seconds as of the now-playing buffer's last update.
Read by `supersonic-now-playing--show-waveform': a waveform arriving a
transcode after the render that asked for it must be drawn at the
current position, not the one from back then.")

(defvar-local supersonic-now-playing--waveform nil
  "(TRACK-ID . ENVELOPE) for the waveform seekbar currently shown, or nil.
ENVELOPE lets `supersonic-now-playing-recolor-waveform' recolor the
seekbar's played/unplayed split every second without asking
`supersonic-waveform-ensure' again.")

(defvar-local supersonic-now-playing--waveform-bucket nil
  "Index of the first unplayed bucket in the seekbar as last drawn, or nil.
The seekbar only changes once the position crosses into another
bucket, so `supersonic-now-playing-recolor-waveform' redraws only then.")

(defvar-local supersonic-now-playing--waveform-requested nil
  "Non-nil once a waveform fetch has been kicked off for the current track.
Set whether or not it succeeded, so that a failed one is not retried,
while one skipped for a hidden buffer is still made once it shows.")

(defvar-local supersonic-now-playing--stale-images nil
  "Alist of FIELD to the image `supersonic-now-playing--update-field' replaced last.
Each is flushed on FIELD's next update, not when replaced.")

(defun supersonic-now-playing-buffer ()
  "Return the now-playing buffer if it is currently live, else nil."
  (let ((buff (get-buffer supersonic-now-playing-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-now-playing--start-timer ()
  "Start ticking the playback position, unless that is already happening."
  (unless supersonic-now-playing--timer
    (setq supersonic-now-playing--timer
          (run-at-time
           supersonic-now-playing-interval supersonic-now-playing-interval #'supersonic-now-playing--tick))))

(defun supersonic-now-playing--stop-timer ()
  "Stop ticking the playback position."
  (when supersonic-now-playing--timer
    (cancel-timer supersonic-now-playing--timer)
    (setq supersonic-now-playing--timer nil)))

(defun supersonic-now-playing--field-value (field)
  "Return the current buffer's cached value for FIELD (title/artist/album)."
  (pcase field
    ('title supersonic-now-playing--title)
    ('artist supersonic-now-playing--artist)
    ('album supersonic-now-playing--album)))

(defun supersonic-now-playing--current-field ()
  "Return (FIELD . VALUE) for whichever field is currently up to be shown.
FIELD is what `supersonic-now-playing--field-index' points at in
`supersonic-now-playing-cycle-fields'; VALUE falls back to the title,
or \"?\", when the list is empty or the field has no value."
  (let* ((fields supersonic-now-playing-cycle-fields)
         (field (and fields (nth (mod supersonic-now-playing--field-index (length fields)) fields))))
    (cons (or field 'title) (or (and field (supersonic-now-playing--field-value field)) supersonic-now-playing--title "?"))))

(defun supersonic-now-playing--advance-field (delta)
  "Advance `supersonic-now-playing--field-index' if DELTA pushes it over.
DELTA, the real seconds since the last tick, is accumulated in
`supersonic-now-playing--cycle-elapsed' against
`supersonic-now-playing-cycle-interval', any remainder kept so that
switches stay on schedule.  A second call in the same tick does not
accumulate DELTA again; see `supersonic-now-playing--cycle-tick'.
Returns non-nil if the field changed this tick."
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
  "Show BUFF's currently due field as its text label below the cover art.
DELTA is as `supersonic-now-playing-animation-functions' get it; at 0,
this always redraws."
  (when (or (= delta 0) (supersonic-now-playing--advance-field delta))
    (supersonic-now-playing--update-field
     buff 'label (propertize (cdr (supersonic-now-playing--current-field)) 'face 'supersonic-now-playing-label))))

(defun supersonic-now-playing--overlay-waveform ()
  "Return (ENVELOPE . PROGRESS) for the cover art overlay to layer in, or nil.
Nil until a waveform is cached for the current track.  Whether it is
drawn is up to `supersonic-art-overlay-layers'/`-scroll-layers'."
  (when supersonic-now-playing--waveform
    (cons
     (cdr supersonic-now-playing--waveform)
     (supersonic-now-playing--progress-ratio supersonic-now-playing--position supersonic-now-playing--duration))))

(defun supersonic-now-playing--maybe-seekable (propertized waveform layers)
  "Return PROPERTIZED, made clickable-to-seek if LAYERS drew WAVEFORM into it.
Asks `supersonic-art-overlay-waveform-lane-p', as the overlay's layout
does, so that the two agree."
  (if (supersonic-art-overlay-waveform-lane-p waveform layers)
      (supersonic-waveform-seekable (get-text-property 0 'display propertized))
    propertized))

(defun supersonic-now-playing-animate-art-overlay (buff delta)
  "Layer BUFF's currently due field onto its cover art instead of the side label.
DELTA is as `supersonic-now-playing-animate-label' takes it.  Falls
back to that function while there is no cached art.  Also layers in
the waveform, clickable to seek where a lane was drawn."
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
  "Return `supersonic-now-playing-cycle-fields''s values joined into one line.
Fields without a value are left out; with none at all, the title, or
\"?\"."
  (let ((values (delq nil (mapcar #'supersonic-now-playing--field-value supersonic-now-playing-cycle-fields))))
    (if values
        (mapconcat #'identity values "   •   ")
      (or supersonic-now-playing--title "?"))))

(defun supersonic-now-playing--advance-scroll (delta max-offset)
  "Advance the bounce between 0 and MAX-OFFSET by DELTA real seconds.
Cycles `supersonic-now-playing--scroll-phase' through `pause-start',
resting at 0 for `supersonic-now-playing-scroll-pause' seconds,
`forward' at `supersonic-now-playing-scroll-step' pixels per second,
`pause-end', resting at MAX-OFFSET, and `backward'.  MAX-OFFSET must be
positive."
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
  "Bounce `supersonic-now-playing-cycle-fields' across BUFF's cover art.
DELTA is real seconds elapsed since the last animation tick.  Every
field is joined into one line, drawn once if it fits, and otherwise
scrolled back and forth; see `supersonic-now-playing--advance-scroll'.
Falls back to `supersonic-now-playing-animate-label' while there is no
cached art.  The waveform lane stays put below the scrolling text."
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
  "Start rotating `supersonic-now-playing-cycle-fields', unless already running.
Ticks every `supersonic-now-playing-animation-frame-interval' seconds,
restarting the timer if that changed since it was started."
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
  "Stop rotating `supersonic-now-playing-cycle-fields'."
  (when supersonic-now-playing--animation-timer
    (cancel-timer supersonic-now-playing--animation-timer)
    (setq supersonic-now-playing--animation-timer nil)
    (setq supersonic-now-playing--animation-timer-interval nil)))

(defun supersonic-now-playing--stopped-buffer (stop-fn)
  "Return the live now-playing buffer, or call STOP-FN and return nil.
Nil, too, when nothing is playing.  STOP-FN stops the calling tick's
timer."
  (let ((buff (supersonic-now-playing-buffer)))
    (if (or (not buff) (not (supersonic-playback-live-p)))
        (progn
          (funcall stop-fn)
          nil)
      buff)))

(defun supersonic-now-playing--run-field-functions (functions buff arg)
  "Call each of FUNCTIONS as (FUNCTION BUFF ARG), one broken entry at a time.
For the customizable function lists: an entry that signals an error is
reported and skipped rather than taking the rest, and the timer
driving them, down with it.  Then redisplays, if
`supersonic-now-playing-force-redisplay' says so."
  (dolist (function functions)
    (condition-case err
        (funcall function buff arg)
      (error (message "supersonic: %s signalled %s" function err))))
  (when supersonic-now-playing-force-redisplay
    (redisplay t)))

(defun supersonic-now-playing--animation-tick ()
  "Measure real elapsed time and pass it on to the animation functions.
Runs `supersonic-now-playing-animation-functions' with the real seconds
since the last tick, as a timer can run late.  Stops once nothing is
playing.  Skips the functions while the buffer is not on display -- a
scrolling overlay redraws a whole SVG every tick -- but still records
the time, so that a hidden stretch does not count as elapsed."
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
  "Update the playback position in the now-playing buffer.
Asks the active backend rather than counting locally, so that seeks
and pauses never cause drift.  Stops once there is nothing to update,
and keeps quiet while the buffer is not on display."
  (let ((buff (supersonic-now-playing--stopped-buffer #'supersonic-now-playing--stop-timer)))
    (when (and buff (get-buffer-window buff t))
      (supersonic-now-playing--show-position buff))))

(aio-defun
 supersonic-now-playing--show-position (buff)
 "Ask the active backend for position and update BUFF accordingly.
Runs `supersonic-now-playing-position-functions' with it."
 (let ((position (aio-await (supersonic-playback-status 'position))))
   (when (buffer-live-p buff)
     (with-current-buffer buff
       (setq supersonic-now-playing--position position))
     (supersonic-now-playing--run-field-functions supersonic-now-playing-position-functions buff position))))

(defun supersonic-now-playing-maybe-update-position ()
  "Update the position shown in the now-playing buffer, if it is on display.
On `supersonic-playback-position-change-hook', so that a seek shows
right away rather than at the next tick.  Looks up no metadata."
  (let ((buff (supersonic-now-playing-buffer)))
    (when (and buff (get-buffer-window buff t) (supersonic-playback-live-p))
      (supersonic-now-playing--show-position buff))))

(defun supersonic-now-playing-maybe-refresh ()
  "Refresh the now-playing buffer from the active backend's state, if it is open.
Called at the same points as `supersonic-queue-maybe-refresh', plus
whenever the active backend reports that playback was paused or resumed."
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
  "The `supersonic-playback.el' hook entries that keep this file's buffers current.
Each element is (HOOK . FUNCTION).  Added by
`supersonic--add-playback-hooks' once there is a play queue or
now-playing buffer, and removed when the last of them is killed,
rather than at load time.")

(defun supersonic--add-playback-hooks ()
  "Add `supersonic--playback-hooks', and remove them again with this buffer.
Called from the play queue and now-playing modes.  Adding is
idempotent, so every such buffer can call this; the buffer-local
`kill-buffer-hook' entry takes them off once no such buffer is left."
  (pcase-dolist (`(,hook . ,fn) supersonic--playback-hooks)
    (add-hook hook fn))
  (add-hook 'kill-buffer-hook #'supersonic--maybe-remove-playback-hooks nil t))

(defun supersonic--remove-playback-hooks ()
  "Remove every entry of `supersonic--playback-hooks'."
  (pcase-dolist (`(,hook . ,fn) supersonic--playback-hooks)
    (remove-hook hook fn)))

(defun supersonic--maybe-remove-playback-hooks ()
  "Remove `supersonic--playback-hooks' unless another buffer still needs them.
Run from `kill-buffer-hook' in a play queue or now-playing buffer, which
is still live at that point, so it is the other buffers that count."
  (unless (seq-some
           (lambda (buff)
             (and (not (eq buff (current-buffer)))
                  (with-current-buffer buff
                    (derived-mode-p 'supersonic-queue-mode 'supersonic-now-playing-mode))))
           (buffer-list))
    (supersonic--remove-playback-hooks)))

(defun supersonic-now-playing--insert-field (label value &optional field)
  "Insert one \"LABEL: VALUE\" metadata row, skipping it if VALUE is nil.
FIELD, if given, tags VALUE so `supersonic-now-playing--update-field'
can replace it later without re-rendering the whole buffer."
  (when value
    (insert
     (propertize (format "%-10s" (concat label ":")) 'face 'shadow)
     (if field
         (propertize value 'supersonic-now-playing-field field)
       value)
     "\n")))

(defun supersonic-now-playing--update-field (buff field value)
  "Replace the text tagged FIELD in BUFF with VALUE, leaving the rest alone.
The playback position ticks once a second; re-rendering everything that
often would rebuild the cover art image and yank point back to the top
of the buffer each time."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (save-excursion
        (goto-char (point-min))
        (let ((match (text-property-search-forward 'supersonic-now-playing-field field t)))
          (when match
            (let ((inhibit-read-only t)
                  (stale (get-text-property (prop-match-beginning match) 'display)))
              (delete-region (prop-match-beginning match) (prop-match-end match))
              ;; Every seekbar redraw is a new image, which stays cached
              ;; for `image-cache-eviction-delay' whether shown or not.
              ;; Flushing STALE right away would free its id for VALUE's
              ;; image to reuse, and redisplay skips a glyph whose id is
              ;; unchanged.  So it is flushed one update later instead.
              ;; FRAME t flushes from every window-system frame, where
              ;; the selected frame may be none.
              (let ((previous (alist-get field supersonic-now-playing--stale-images)))
                (when (and previous (fboundp 'image-flush))
                  (image-flush previous t)))
              (setf (alist-get field supersonic-now-playing--stale-images)
                    (and (eq (car-safe stale) 'image) stale))
              (goto-char (prop-match-beginning match))
              (insert (propertize value 'supersonic-now-playing-field field)))))))))

(defun supersonic-now-playing--field-present-p (buff field)
  "Return non-nil if BUFF has text tagged FIELD for updates to land in."
  (with-current-buffer buff
    (and (text-property-any (point-min) (point-max) 'supersonic-now-playing-field field) t)))

(defun supersonic-now-playing--update-waveform-field (buff envelope progress)
  "Draw ENVELOPE at PROGRESS into BUFF's standalone `waveform' row, if any.
Checks for the row first, so that the image, costly to build, is not
built for a layout without one, such as the default."
  (when (supersonic-now-playing--field-present-p buff 'waveform)
    (supersonic-now-playing--update-field buff 'waveform (supersonic-waveform-propertize envelope progress))))

(defun supersonic-now-playing--progress-ratio (position duration)
  "Return POSITION/DURATION clamped to 0..1, or 0 if either is unavailable.
POSITION is made a float first, as two integers would divide down to 0."
  (if (and position duration (> duration 0))
      (max 0.0 (min 1.0 (/ (float position) duration)))
    0.0))

(defun supersonic-now-playing--progress-bucket (envelope progress)
  "Return the index of ENVELOPE's first unplayed bucket at PROGRESS (0..1).
This is the only thing about the rendered seekbar that PROGRESS
changes, and what `supersonic-now-playing--waveform-bucket' records."
  (floor (* progress (length (car envelope)))))

(defun supersonic-now-playing-update-duration-field (buff position)
  "Redraw BUFF's \"Duration:\" line with POSITION as its elapsed side.
Built into `supersonic-now-playing-position-functions'; POSITION is
whatever the active backend most recently reported, read there off
`supersonic-now-playing--show-position'."
  (supersonic-now-playing--update-field
   buff 'duration
   (supersonic-now-playing--position position (buffer-local-value 'supersonic-now-playing--duration buff))))

(defun supersonic-now-playing-recolor-waveform (buff position)
  "Redraw BUFF's waveform seekbar(s) at POSITION.
No-op until a waveform is showing for the current track, and while the
played/unplayed split stays in the bucket last drawn.  Otherwise
patches the standalone `waveform' row, if any, and reruns
`supersonic-now-playing-animation-functions' with a DELTA of 0, so
that the art overlay repaints too."
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
  "Insert a boxed button reading LABEL that runs COMMAND when activated.
Padded and given `supersonic-now-playing-button', so that it reads as
a control rather than a link."
  (insert-text-button
   (format " %s " label)
   'face 'supersonic-now-playing-button
   'action (lambda (_button) (call-interactively command))
   'follow-link t))

(defun supersonic-now-playing--format-time (seconds longest)
  "Format SECONDS as a clock string no longer than it has to be.
LONGEST is the longest value the same line will show, so that a
position and the duration it counts towards stay the same shape: hours
only appear once LONGEST reaches an hour, e.g. \"05:01\" for a five
minute track but \"1:05:01\" once an hour is on the clock."
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
  "Return a display string for SONG's cover art, or nil if there is none.
Expects the art to be cached already, which
`supersonic-now-playing-fetch-and-render' takes care of before it
renders."
  (let ((art-id (plist-get song :art)))
    (when (and (supersonic-art-available-p)
               art-id
               (file-exists-p (supersonic-art-cache-file art-id supersonic-now-playing-art-size)))
      (supersonic-image-propertize art-id supersonic-now-playing-art-size))))

(defun supersonic-now-playing-render-title (_buff song)
  "Insert SONG's \"Title:\" row, if it has one.
Built into `supersonic-now-playing-render-functions'."
  (supersonic-now-playing--insert-field "Title" (plist-get song :title)))

(defun supersonic-now-playing-render-artist (_buff song)
  "Insert SONG's \"Artist:\" row, if it has one.
Built into `supersonic-now-playing-render-functions'."
  (supersonic-now-playing--insert-field "Artist" (plist-get song :artist)))

(defun supersonic-now-playing-render-album (_buff song)
  "Insert SONG's \"Album:\" row, if it has one.
Built into `supersonic-now-playing-render-functions'."
  (supersonic-now-playing--insert-field "Album" (plist-get song :album)))

(defun supersonic-now-playing-render-duration (buff _song)
  "Insert BUFF's \"Duration:\" row, tagged for later updates.
Reads position and duration off BUFF, as the position is not part of
SONG.  Tagged `duration', for
`supersonic-now-playing-update-duration-field' to patch."
  (supersonic-now-playing--insert-field
   "Duration"
   (supersonic-now-playing--position
    (buffer-local-value 'supersonic-now-playing--position buff)
    (buffer-local-value 'supersonic-now-playing--duration buff))
   'duration))

(defun supersonic-now-playing-render-format (_buff song)
  "Insert SONG's \"Format:\" row, if it is known.
Built into `supersonic-now-playing-render-functions'."
  (supersonic-now-playing--insert-field "Format" (supersonic-now-playing--format song)))

(defun supersonic-now-playing-render-size (_buff song)
  "Insert SONG's \"Size:\" row, if it is known.
Built into `supersonic-now-playing-render-functions'."
  (let ((size (plist-get song :size)))
    (supersonic-now-playing--insert-field "Size" (and size (format "%.2f MB" (/ size 1048576.0))))))

(defun supersonic-now-playing-render-queue-position (buff _song)
  "Insert BUFF's \"Queue:\" row, e.g. \"Queue: 4/19\".
Reads `supersonic-now-playing--queue-place' off BUFF; left out when
that is nil."
  (let ((place (buffer-local-value 'supersonic-now-playing--queue-place buff)))
    (supersonic-now-playing--insert-field
     "Queue" (and place (format "%d/%d" (car place) (cdr place))))))

(defun supersonic-now-playing--current-queue-place (queue)
  "Return where QUEUE's current track sits, as a (POSITION . TOTAL) pair.
QUEUE is as resolved by `supersonic-playback-queue'.  POSITION is the
1-based index of QUEUE's `:current' entry, TOTAL QUEUE's length; nil
if there is no current entry."
  (let ((position 0) found)
    (dolist (entry queue)
      (setq position (1+ position))
      (when (plist-get entry :current)
        (setq found position)))
    (and found (cons found (length queue)))))

(defun supersonic-now-playing-layout-art (_buff song)
  "Insert SONG's cover art as its own row, if it has any.
Tagged `art', for the art overlay animations to draw on.  Without this
row they draw nothing, so drop it only together with switching
`supersonic-now-playing-animation-functions' to
`supersonic-now-playing-animate-label'."
  (let ((art (supersonic-now-playing--art song)))
    (when art
      (insert (propertize art 'supersonic-now-playing-field 'art) "\n"))))

(defun supersonic-now-playing-layout-waveform (_buff _song)
  "Insert a placeholder row for the standalone waveform seekbar, if any.
Nothing when `supersonic-waveform-available-p' is nil.  Independent of
the waveform in the cover art overlay; filled in by
`supersonic-now-playing--show-waveform'."
  (when (supersonic-waveform-available-p)
    (insert (propertize " " 'supersonic-now-playing-field 'waveform) "\n")))

(defun supersonic-now-playing-layout-label (_buff _song)
  "Insert a placeholder row for the animated side label, then a blank line.
Unconditional: `supersonic-now-playing-animation-functions' fill it
in, or leave it empty."
  (insert (propertize " " 'supersonic-now-playing-field 'label) "\n\n"))

(defun supersonic-now-playing-layout-buttons (buff _song)
  "Insert the transport button row, then a blank line.
Reads BUFF's `supersonic-now-playing--paused'."
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
  "Render SONG into BUFF, marked as paused or playing according to PAUSED.
POSITION is how many seconds into SONG playback currently is.  SONG is
a track plist, as `supersonic-provider-track' resolves to; if it is
nil, BUFF shows that nothing is playing.  TRACK-ID is the id of the
track playing.  QUEUE-PLACE is its (POSITION . TOTAL) in the play
queue, or nil."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (let* ((inhibit-read-only t)
             (duration (plist-get song :duration))
             ;; Re-rendering the same track is common -- a pause, a
             ;; resume, `g' -- and must keep its waveform state.
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
            ;; Paint the animated field now rather than at the first tick,
            ;; even with cycling off.
            (supersonic-now-playing--run-field-functions supersonic-now-playing-animation-functions buff 0)))
        (goto-char (point-min))
        ;; Put back the seekbar `erase-buffer' took.
        (when supersonic-now-playing--waveform
          (supersonic-now-playing--show-waveform buff track-id (cdr supersonic-now-playing--waveform)))))))

(aio-defun
 supersonic-now-playing-fetch-and-render (buff)
 "Query the active backend for its current track and render it into BUFF.
A failed metadata lookup shows the bare track id instead.  Errors are
reported, since nobody awaits this."
 (supersonic--with-async-error-handling
  buff "refresh the now-playing buffer"
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
              ;; Fired concurrently and asked for last, so all three are as
              ;; fresh as possible: the art fetch above can take a while on a
              ;; cold cache.
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
  "Patch BUFF's waveform field to display ENVELOPE for TRACK-ID, if still current.
For both callbacks of `supersonic-waveform-ensure', and for
`supersonic-now-playing--render' putting the seekbar back.  The
played/unplayed split comes from `supersonic-now-playing--position',
which is current even when ENVELOPE arrives a transcode later."
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
  "Kick off waveform generation for BUFF's current track, if not already asked.
POSITION is unused.  Only when waveforms are available, BUFF is on
display, something would show the waveform -- a standalone row, or
cover art for the overlay -- and none was requested for this track
yet; `supersonic-now-playing--tick' tries again once these hold.  The
seekbar fills in as analysis goes."
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
  "Refresh the now-playing buffer from the active backend's current state."
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

(define-derived-mode
 supersonic-now-playing-mode
 special-mode
 "Supersonic Now Playing"
 "Major mode for the buffer opened by `supersonic-show-now-playing'."
 ;; Every field update replaces text whose display property is a whole
 ;; image spec; undo would keep each superseded cover art overlay and
 ;; seekbar alive, in a buffer nobody edits.
 (buffer-disable-undo)
 (supersonic--add-playback-hooks))

;;;###autoload
(defun supersonic-show-now-playing ()
  "Open a buffer showing the track the active backend is currently on.
The buffer follows the active backend on its own -- track changes,
pausing and resuming are all reflected without a manual refresh, the
same way the play queue buffer keeps itself current."
  (interactive)
  (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-now-playing-mode)
        (supersonic-now-playing-mode)))
    (ignore (supersonic-now-playing-fetch-and-render buff))
    (pop-to-buffer-same-window buff)))

(defun supersonic--format-duration (format seconds)
  "Format SECONDS with `format-seconds' FORMAT, or \"\" if there is no duration.
Subsonic leaves the duration out, e.g. for a podcast episode not yet
downloaded, and `format-seconds' signals on nil."
  (if seconds
      (format-seconds format seconds)
    ""))

;;;
;;; Search
;;;
(defun supersonic-search-rows (results)
  "Turn search RESULTS into tabulated-list entries.
RESULTS is as resolved by `supersonic-provider-search'.  Each entry's
id is (ID . TYPE), TYPE being \"artist\", \"album\" or \"song\", so
`supersonic-open-search-appropriate-result' knows what ID is an id of."
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

(aio-defun
 supersonic-search-refresh (query buff) "Refresh the list of search results from QUERY into BUFF."
 (supersonic--with-async-error-handling
  buff "search"
  (let ((results (aio-await (supersonic-provider-search query))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (supersonic--print-list (supersonic-search-rows results)))))))

(defun supersonic-open-search-appropriate-result (result)
  "Opens the RESULT from a search in the appropriate buffer."
  (let ((type (cdr result)))
    (cond
     ((string-equal type "artist")
      (supersonic-albums (car result)))
     ((string-equal type "album")
      (supersonic-tracks (car result)))
     ((string-equal type "song")
      (supersonic-playback-start (list (car result)))))))

(defun supersonic-open-search-result ()
  "Open a view of the result from the result at point."
  (interactive)
  (supersonic-open-search-appropriate-result (tabulated-list-get-id)))

(defvar supersonic-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-search-result)
    map)
  "Keymap for `supersonic-search-mode'.")

;;;###autoload
(defun supersonic-search ()
  "List supersonic search results."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-search*")))
    (supersonic--init-list-buffer new-buff #'supersonic-search-mode "Searching...")
    (ignore (supersonic-search-refresh (read-string "Query: ") new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-search-mode
 tabulated-list-mode
 "Supersonic search mode"
 "Major mode for the result list opened by `supersonic-search'.
Each row is an artist, album or song; opening one dispatches on which."
 (setq tabulated-list-format [("Type" 10 t) ("Name" 30 t)])
 (setq tabulated-list-padding 2)
 (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
 (tabulated-list-init-header))


;;;
;;; Tracks
;;;

(defun supersonic-get-tracklist-id (id)
  "Return the ids of the tracks from ID to the end of the current list.
Plays/enqueues \"this one and everything after it\", which is what both
`supersonic-play-tracks' and `supersonic-enqueue-tracks' mean by the
entry at point.  An ID that isn't in the list at all yields nil."
  (mapcar #'car (seq-drop-while (lambda (entry) (not (equal (car entry) id))) tabulated-list-entries)))

(defun supersonic-tracks-rows (tracks)
  "Turn TRACKS, as resolved by `supersonic-provider-album-tracks', into entries."
  (mapcar
   (lambda (track)
     (list
      (plist-get track :id)
      (vector
       (or (plist-get track :title) "")
       (supersonic--format-duration "%m:%.2s" (plist-get track :duration))
       (format "%d" (or (plist-get track :track) 0)))))
   tracks))

(aio-defun
 supersonic-get-album-track-ids
 (id)
 "Return the list of track ids for album ID."
 (mapcar (lambda (track) (plist-get track :id)) (aio-await (supersonic-provider-album-tracks id))))

(aio-defun
 supersonic-tracks-refresh (id buff) "Refresh the list of tracks from ID into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch tracks"
  (let ((tracks (aio-await (supersonic-provider-album-tracks id))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (supersonic--print-list (supersonic-tracks-rows tracks)))))))

(defun supersonic-play-tracks ()
  "Play all the tracks after the point in the list."
  (interactive)
  (supersonic-playback-start (supersonic-get-tracklist-id (tabulated-list-get-id))))

(defun supersonic-enqueue-tracks ()
  "Add all the tracks after the point in the list to the play queue."
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
  "Create a buffer with a list of tracks from ID."
  (let ((new-buff (get-buffer-create "*supersonic-tracks*")))
    (supersonic--init-list-buffer new-buff #'supersonic-tracks-mode "Loading tracks...")
    (ignore (supersonic-tracks-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-tracks-mode
 tabulated-list-mode
 "Supersonic Tracks"
 "Major mode for the track list of one album, opened by `supersonic-tracks'."
 (setq tabulated-list-format [("Title" 30 t) ("Duration" 10 t) ("Track" 10 t)])
 (setq tabulated-list-padding 2)
 (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
 (tabulated-list-init-header))

;;;
;;; Albums
;;;

(defun supersonic--arts (items)
  "Return the `:art' reference of each of ITEMS, in order.
What `supersonic-get-images' is handed next to the entries made from
ITEMS: art is looked up by the reference the provider gave, not by
the item's id."
  (mapcar (lambda (item) (plist-get item :art)) items))

(defun supersonic-albums-rows (albums)
  "Turn one artist's ALBUMS into year/name/art tabulated-list entries.
ALBUMS is as resolved by `supersonic-provider-artist-albums'."
  (mapcar
   (lambda (album)
     (list
      (plist-get album :id)
      (vector (format "%d" (or (plist-get album :year) 0))
              (propertize (or (plist-get album :name) "") 'face 'supersonic-albums-name)
              "")))
   albums))

(defun supersonic-albums-type-rows (albums)
  "Turn ALBUMS from across artists into name/artist/art tabulated-list entries.
ALBUMS is as resolved by `supersonic-provider-album-list'."
  (mapcar
   (lambda (album)
     (list
      (plist-get album :id)
      (vector (propertize (or (plist-get album :name) "") 'face 'supersonic-albums-name)
              (propertize (or (plist-get album :artist) "") 'face 'supersonic-albums-artist)
              "")))
   albums))

(aio-defun
 supersonic-albums-refresh (id buff) "Refresh the albums list for a given artist ID into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch albums"
  (let ((albums (aio-await (supersonic-provider-artist-albums id))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (supersonic--print-list (supersonic-albums-rows albums))
        (supersonic-get-images tabulated-list-entries (supersonic--arts albums) 2 buff))))))


(aio-defun
 supersonic-albums-refresh-type (type buff) "Refresh the albums list for a given album list TYPE into BUFF.
TYPE is one of `supersonic-provider-album-list-types'."
 (supersonic--with-async-error-handling
  buff "fetch albums"
  (let ((albums (aio-await (supersonic-provider-album-list type supersonic-album-list-count))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (supersonic--print-list (supersonic-albums-type-rows albums))
        (supersonic-get-images tabulated-list-entries (supersonic--arts albums) 2 buff))))))

(defun supersonic-open-tracks ()
  "Open a list of tracks at point."
  (interactive)
  (supersonic-tracks (tabulated-list-get-id)))

(aio-defun
 supersonic-enqueue-album () "Add all the tracks of the album at point to the play queue." (interactive)
 (supersonic--with-async-error-handling
  nil "enqueue album"
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
  "Open a buffer of albums for artist ID or list TYPE.
TYPE is one of `supersonic-provider-album-list-types'; the name of
one as a string is accepted too."
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

(define-derived-mode
 supersonic-album-type-mode
 tabulated-list-mode
 "Supersonic Album List"
 "Major mode for a list of albums across artists (newest, random, ...)."
 (setq tabulated-list-format [("Albums" 30 t) ("Artists" 30 t) ("Art" 30 nil)])
 (setq tabulated-list-padding 2)
 (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
 (setq tabulated-list-use-header-line supersonic-list-use-header-line)
 (tabulated-list-init-header))

(define-derived-mode
 supersonic-album-mode
 tabulated-list-mode
 "Supersonic Albums"
 "Major mode for the album list of one artist, opened by `supersonic-albums'."
 (setq tabulated-list-format [("Year" 5 t) ("Albums" 40 t) ("Art" 30 nil)])
 (setq tabulated-list-padding 2)
 (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
 (setq tabulated-list-use-header-line supersonic-list-use-header-line)
 (tabulated-list-init-header))

;;;
;;; Artists
;;;
(defun supersonic-open-album ()
  "Open the albums for the artist at point."
  (interactive)
  (supersonic-albums (tabulated-list-get-id)))

(defun supersonic-artists-rows (artists)
  "Turn ARTISTS, as resolved by `supersonic-provider-artists', into entries.
Each row has the artist's name and a cell for its art, which
`supersonic-art-fill-visible' fills in -- or which goes unprinted when
the list has no Art column, see `supersonic-artists-format'."
  (mapcar (lambda (artist) (list (plist-get artist :id) (vector (or (plist-get artist :name) "") ""))) artists))

(aio-defun
 supersonic-artists-refresh (buff) "Refresh the list of artists into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch artists"
  (let ((artists (aio-await (supersonic-provider-artists))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (supersonic--print-list (supersonic-artists-rows artists))
        (when (> (length tabulated-list-format) 1)
          (supersonic-art-fill-visible artists 1 supersonic-artist-art-size buff)))))))

(defun supersonic-artists-revert ()
  "Refresh the artists buffer from the active library provider."
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
  "List artists."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-artists*")))
    (supersonic--init-list-buffer new-buff #'supersonic-artist-mode "Loading artists...")
    (ignore (supersonic-artists-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(defun supersonic-artists-format ()
  "Return the `tabulated-list-format' of the artist list.
With an Art column only when `supersonic-artist-art-size' is set and
art can be shown at all (`supersonic-art-available-p'): an empty one
would still take the room `supersonic--fit-columns' keeps for the last
column from the names.  The column is as wide as an image of that size
in the selected frame, the images being fitted into a square of it,
whatever their proportions."
  (if (and supersonic-artist-art-size (supersonic-art-available-p))
      `[("Artist" 30 t) ("Art" ,(max 3 (1+ (ceiling supersonic-artist-art-size (frame-char-width)))) nil)]
    [("Artist" 30 t)]))

(define-derived-mode
 supersonic-artist-mode
 tabulated-list-mode
 "Supersonic Artists"
 "Major mode for the artist list opened by `supersonic-artists'."
 (setq tabulated-list-format (supersonic-artists-format))
 (setq tabulated-list-padding 2)
 (setq tabulated-list-use-header-line supersonic-list-use-header-line)
 (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
 (tabulated-list-init-header))

;;;
;;; Podcasts
;;;
(defun supersonic-podcasts-rows (podcasts)
  "Turn PODCASTS, as resolved by `supersonic-provider-podcasts', into entries."
  (mapcar (lambda (channel) (list (plist-get channel :id) (vector (or (plist-get channel :title) "") ""))) podcasts))

(aio-defun
 supersonic-podcasts-refresh (buff) "Refresh the list of podcasts into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch podcasts"
  (let ((podcasts (aio-await (supersonic-provider-podcasts))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (supersonic--print-list (supersonic-podcasts-rows podcasts))
        (supersonic-get-images tabulated-list-entries (supersonic--arts podcasts) 1 buff))))))


(defun supersonic-open-podcast-episodes ()
  "Open a view of podcasts episodes from the podcast at point."
  (interactive)
  (supersonic-podcast-episodes (tabulated-list-get-id)))

(defun supersonic-add-podcast (url)
  "Subscribe to the podcast feed at URL.
Interactively, prompts for URL -- unless the active library provider
has no podcasts to subscribe to, which is reported before asking."
  (interactive
   (progn
     (supersonic-provider-require 'add-podcast)
     (list (read-string "feed url: "))))
  (supersonic--add-podcast url))

(aio-defun
 supersonic--add-podcast (url) "Subscribe to URL, reporting how that went in the echo area."
 (supersonic--with-async-error-handling
  nil "add podcast"
  (aio-await (supersonic-provider-add-podcast url))
  (message "Podcast added")))

(transient-define-prefix
 supersonic-podcast-help () "Help transient for podcasts."
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
  "List podcasts."
  (interactive)
  (supersonic-provider-require 'podcasts)
  (let ((new-buff (get-buffer-create "*supersonic-podcasts*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-mode "Loading podcasts...")
    (ignore (supersonic-podcasts-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-podcast-mode
 tabulated-list-mode
 "Supersonic Podcasts"
 "Major mode for the podcast channel list opened by `supersonic-podcasts'."
 (setq tabulated-list-format [("Podcasts" 30 t) ("Art" 20 nil)])
 (setq tabulated-list-padding 2)
 (add-hook 'tabulated-list-revert-hook #'supersonic--fit-columns nil t)
 (setq tabulated-list-use-header-line supersonic-list-use-header-line)
 (tabulated-list-init-header))

;;;
;;; Podcast episodes
;;;

(defun supersonic-podcast-episodes-rows (episodes)
  "Turn podcast EPISODES into tabulated-list entries.
EPISODES is as resolved by `supersonic-provider-podcast-episodes'."
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
  "Play a podcast episode at point."
  (interactive)
  (supersonic-playback-start (list (tabulated-list-get-id))))

(aio-defun
 supersonic-podcasts-episode-refresh (id buff) "Refresh the list of podcast episodes for a podcast ID into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch episodes"
  (let ((episodes (aio-await (supersonic-provider-podcast-episodes id))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (supersonic--print-list (supersonic-podcast-episodes-rows episodes)))))))

(defun supersonic-download-podcast-episode ()
  "Have the library provider download the podcast episode at point."
  (interactive)
  (supersonic-provider-require 'download-podcast-episode)
  (supersonic--download-podcast-episode (tabulated-list-get-id)))

(aio-defun
 supersonic--download-podcast-episode (id) "Start downloading episode ID, reporting how that went."
 (supersonic--with-async-error-handling
  nil "download episode"
  (aio-await (supersonic-provider-download-podcast-episode id))
  (message "Episode download started")))

(transient-define-prefix
 supersonic-podcast-episode-help () "Help transient for podcast episodes."
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
  "Open a buffer with a list of podcast episodes from podcast ID."
  (supersonic-provider-require 'podcast-episodes)
  (let ((new-buff (get-buffer-create "*supersonic-podcast-episodes*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-episodes-mode "Loading episodes...")
    (ignore (supersonic-podcasts-episode-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-podcast-episodes-mode
 tabulated-list-mode
 "Supersonic Podcast Episodes"
 "Major mode for one podcast channel's episodes.
Opened by `supersonic-podcast-episodes'."
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
  "Major modes of the list buffers showing what a library provider handed out.
Their entries are that provider's ids, which mean nothing to any
other, so `supersonic-provider-switch' kills them.  The queue and
now-playing buffers are not among them: they show what the playback
backend reports, which the switch stops.")

(defun supersonic--kill-provider-list-buffers ()
  "Kill every buffer whose mode is or derives from a provider list mode.
The provider list modes are `supersonic--provider-list-modes'."
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (apply #'derived-mode-p supersonic--provider-list-modes))
      (kill-buffer buffer))))

;;;###autoload
(defun supersonic-provider-switch (provider &optional backend)
  "Make PROVIDER the active library provider, and BACKEND the playback backend.
Interactively, prompts among the other registered providers.  BACKEND
nil picks one as `supersonic-playback-backend-for-provider' does,
prompting only where it must; declining switches nothing.  A PROVIDER
that is not registered, or a backend that cannot play for it, is
refused before anything changes.

Whenever the provider changes, playback is stopped and the old
provider's list buffers are killed: what they hold are the old
provider's ids."
  (interactive
   (let ((providers (mapcar #'symbol-name (remq supersonic-provider (supersonic-provider-names)))))
     (unless providers
       (user-error "No other library provider is registered"))
     (let ((answer (completing-read "Switch to library provider: " providers nil t nil nil (car providers))))
       (when (equal answer "")
         (user-error "Kept the `%s' provider" supersonic-provider))
       (list (intern answer)))))
  (unless (memq provider (supersonic-provider-names))
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

;; `:if' predicates hiding the entries whose command needs an
;; operation the active provider lacks.  The commands themselves still
;; say so via `supersonic-provider-require' when called by other means.

(defun supersonic--offers-p (operation)
  "Return non-nil if the transient should offer an entry needing OPERATION.
That is, if the active provider implements it -- or is not registered
at all, typically because its file is not loaded: hiding every entry
would then leave no hint why, whereas using one reports that the
provider is missing."
  (or (not (memq supersonic-provider (supersonic-provider-names)))
      (supersonic-provider-supports-p operation)))

(defun supersonic--offer-artists-p ()
  "Return non-nil if the transient should offer listing artists."
  (supersonic--offers-p 'artists))

(defun supersonic--offer-album-list-p ()
  "Return non-nil if the transient should offer listing albums across artists."
  (supersonic--offers-p 'album-list))

(defun supersonic--offer-search-p ()
  "Return non-nil if the transient should offer searching."
  (supersonic--offers-p 'search))

(defun supersonic--offer-podcasts-p ()
  "Return non-nil if the transient should offer listing podcasts."
  (supersonic--offers-p 'podcasts))

(defun supersonic--offer-podcast-episodes-p ()
  "Return non-nil if the transient should offer opening a podcast's episodes."
  (supersonic--offers-p 'podcast-episodes))

(defun supersonic--offer-add-podcast-p ()
  "Return non-nil if the transient should offer subscribing to a podcast feed."
  (supersonic--offers-p 'add-podcast))

(defun supersonic--offer-download-podcast-episode-p ()
  "Return non-nil if the transient should offer downloading a podcast episode."
  (supersonic--offers-p 'download-podcast-episode))

;;;###autoload (autoload 'supersonic "supersonic" nil t)
(transient-define-prefix
 supersonic () "Help transient for supersonic."
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
  "Undo the `supersonic-playback.el' hook entries a queue/now-playing buffer added.
Called by `unload-feature', which would otherwise leave those hooks
holding references to functions that no longer exist.  Returns nil so
`unload-feature' still goes on to remove the definitions itself."
  (supersonic--remove-playback-hooks)
  nil)

(provide 'supersonic)

;;; supersonic.el ends here
