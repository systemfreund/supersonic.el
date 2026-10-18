;;; supersonic-waveform.el --- Waveform seekbar generation for supersonic.el -*- lexical-binding: t; -*-

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

;; The waveform seekbar of the now-playing buffer.  A separate mpv
;; transcodes the track to a small mono WAV file, whose samples are
;; reduced to a peak/RMS envelope, cached, and drawn as a PPM image.
;; Clicking it seeks.

;;; Code:
(require 'cl-lib)
(require 'svg)
(require 'aio)
(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)

(defvar supersonic-waveform--process nil
  "The mpv transcode process currently generating a waveform, if any.")

(defvar supersonic-waveform--outfile nil
  "The temporary WAV file `supersonic-waveform--process' writes to.")

(defvar supersonic-waveform--job nil
  "Plist of the waveform job in progress, or nil.
Its keys are :id, :callback and :progress-callback, see
`supersonic-waveform-ensure'.")

(defvar supersonic-waveform--generation 0
  "Counter incremented by `supersonic-waveform-cancel'.
An analysis in progress stops when it changes.")

(defun supersonic-waveform-available-p ()
  "Return non-nil if the waveform can be shown."
  (and supersonic-enable-waveform
       (display-graphic-p)
       (image-type-available-p 'pbm)
       (supersonic-provider-supports-p 'stream-url)))

(defun supersonic-waveform-cache-file (id buckets)
  "Return the cache file of track ID's envelope with BUCKETS buckets."
  (expand-file-name
   (format "waveform-%s-%d-%d" (supersonic-provider-cache-name id) buckets supersonic-waveform-samplerate)
   supersonic-cache-path))

(defun supersonic-waveform--delete-file (file)
  "Delete FILE, logging instead of signaling an error."
  (condition-case err
      (delete-file file)
    (file-error
     (supersonic--log "Could not delete %s: %s" file (error-message-string err)))))

(defun supersonic-waveform-cancel ()
  "Stop the waveform job in progress and delete its temporary file."
  (setq supersonic-waveform--generation (1+ supersonic-waveform--generation))
  (setq supersonic-waveform--job nil)
  (when (process-live-p supersonic-waveform--process)
    (process-put supersonic-waveform--process 'supersonic-waveform-cancelled t)
    (delete-process supersonic-waveform--process))
  (when (and supersonic-waveform--outfile (file-exists-p supersonic-waveform--outfile))
    (supersonic-waveform--delete-file supersonic-waveform--outfile))
  (setq
   supersonic-waveform--process nil
   supersonic-waveform--outfile nil)
  (remove-hook 'kill-emacs-hook #'supersonic-waveform-cancel))

;;;
;;; WAV parsing and peak/RMS analysis
;;;

(defun supersonic-waveform--le32 (buf pos)
  "Read a 4-byte little-endian unsigned integer from unibyte BUF at POS."
  (logior (aref buf pos) (ash (aref buf (1+ pos)) 8) (ash (aref buf (+ pos 2)) 16) (ash (aref buf (+ pos 3)) 24)))

(defun supersonic-waveform--find-data-chunk (buf)
  "Return (START . LENGTH) of the \"data\" chunk in the WAV data BUF, or nil."
  (let ((pos 12)
        (len (length buf))) ; skip "RIFF" <size> "WAVE"
    (catch 'found
      (while (< (+ pos 8) len)
        (let* ((id (substring buf pos (+ pos 4)))
               (size (supersonic-waveform--le32 buf (+ pos 4))))
          (if (string= id "data")
              (throw 'found (cons (+ pos 8) (min size (- len pos 8))))
            ;; Odd-sized chunks are followed by a padding byte.
            (setq pos
                  (+ pos 8
                     size
                     (if (cl-oddp size)
                         1
                       0))))))
      nil)))

(defun supersonic-waveform--normalize (magnitude)
  "Scale MAGNITUDE, between 0 and 32768, to a byte."
  (min 255 (round (* (/ magnitude 32768.0) 255))))

(defvar supersonic-waveform--analysis-tick-budget 0.02
  "Seconds the analysis may run before letting Emacs do other work.")

(defvar supersonic-waveform--progress-interval 0.1
  "Minimum seconds between progress reports of the analysis.")

(defun supersonic-waveform--analyze-bucket (buf data-start per-bucket total-samples b)
  "Return (PEAK . RMS) of bucket B of the samples, each 0 to 255.
BUF, DATA-START, PER-BUCKET and TOTAL-SAMPLES are as in
`supersonic-waveform--analyze-samples-async'."
  (let* ((start (* b per-bucket))
         (end (min total-samples (+ start per-bucket)))
         (offset (+ data-start (* start 2)))
         (limit (+ data-start (* end 2)))
         (n (- end start))
         (peak 0)
         (sum-squares 0.0))
    (while (< offset limit)
      ;; 16-bit little-endian two's complement.
      (let* ((unsigned (logior (aref buf offset) (ash (aref buf (1+ offset)) 8)))
             (sample
              (if (>= unsigned 32768)
                  (- unsigned 65536)
                unsigned)))
        (setq peak (max peak (abs sample)))
        (setq sum-squares (+ sum-squares (* (float sample) sample))))
      (setq offset (+ offset 2)))
    (cons
     (supersonic-waveform--normalize peak)
     (supersonic-waveform--normalize
      (if (> n 0)
          (sqrt (/ sum-squares n))
        0)))))

(defun supersonic-waveform--analyze-samples-async
    (buf data-start data-len buckets generation on-done &optional on-progress)
  "Compute the envelope of mono 16-bit samples in BUCKETS buckets.
BUF is the WAV data, the samples being DATA-LEN bytes at DATA-START.
Call ON-DONE with (PEAKS . RMS), two strings of BUCKETS bytes.  Call
ON-PROGRESS, if non-nil, with the partial envelope now and then.

The work is split into short timer slices.  It stops without calling
ON-DONE once `supersonic-waveform--generation' differs from
GENERATION."
  (let* ((total-samples (/ data-len 2))
         (per-bucket (max 1 (/ total-samples buckets)))
         (peaks (make-string buckets 0 nil))
         (rms (make-string buckets 0 nil))
         (b 0)
         (last-progress 0.0))
    (cl-labels
        ((step ()
           (when (= generation supersonic-waveform--generation)
             (let ((deadline (+ (float-time) supersonic-waveform--analysis-tick-budget))
                   (continue t))
               (while continue
                 (let ((pr (supersonic-waveform--analyze-bucket buf data-start per-bucket total-samples b)))
                   (aset peaks b (car pr))
                   (aset rms b (cdr pr)))
                 (setq b (1+ b))
                 (setq continue (and (< b buckets) (< (float-time) deadline)))))
             (if (< b buckets)
                 (progn
                   (when (and on-progress (>= (- (float-time) last-progress) supersonic-waveform--progress-interval))
                     (setq last-progress (float-time))
                     (funcall on-progress (cons (copy-sequence peaks) (copy-sequence rms))))
                   ;; Otherwise the zero-delay timer starves process output.
                   (accept-process-output nil 0)
                   (run-with-timer 0 nil #'step))
               (funcall on-done (cons peaks rms))))))
      (step))))

(defun supersonic-waveform--analyze-file-async (path buckets generation on-done &optional on-progress)
  "Compute the envelope of the WAV file PATH and delete it.
BUCKETS, GENERATION, ON-DONE and ON-PROGRESS are as in
`supersonic-waveform--analyze-samples-async'.  Signal an error if
PATH is not a WAV file."
  (let* ((file-name-handler-alist nil)
         (buf
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (buffer-string))))
    (supersonic-waveform--delete-file path)
    (unless (and (>= (length buf) 12) (string= (substring buf 0 4) "RIFF"))
      (error "Not a RIFF file: %s" path))
    (let ((chunk (supersonic-waveform--find-data-chunk buf)))
      (unless chunk
        (error "No \"data\" chunk found in %s" path))
      (supersonic-waveform--analyze-samples-async buf (car chunk) (cdr chunk) buckets generation on-done on-progress))))

;;;
;;; Disk cache
;;;

(defun supersonic-waveform--write-cache (file envelope)
  "Write ENVELOPE, (PEAKS . RMS), to FILE."
  (unless (file-exists-p supersonic-cache-path)
    (mkdir supersonic-cache-path t))
  (let ((coding-system-for-write 'no-conversion))
    (write-region (concat (car envelope) (cdr envelope)) nil file nil 'no-message)))

(defun supersonic-waveform--read-cache (file buckets)
  "Return the envelope in FILE, BUCKETS bytes each, or nil if invalid."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let ((buf (buffer-string)))
      (when (= (length buf) (* 2 buckets))
        (cons (substring buf 0 buckets) (substring buf buckets))))))

;;;
;;; Transcode pipeline
;;;

(defun supersonic-waveform--transcode-sentinel (outfile buckets cache-file job)
  "Return the sentinel of the transcode for JOB writing to OUTFILE.
When the process exits, compute the envelope of OUTFILE with BUCKETS
buckets, cache it in CACHE-FILE and call JOB's :callback with it, or
with nil on failure."
  (lambda (proc event)
    (unless (process-live-p proc)
      (when (eq proc supersonic-waveform--process)
        (setq
         supersonic-waveform--process nil
         supersonic-waveform--outfile nil)
        (remove-hook 'kill-emacs-hook #'supersonic-waveform-cancel))
      (cl-flet
          ((finish (envelope)
             (let ((file-name-handler-alist nil))
               (when envelope
                 (condition-case err
                     (supersonic-waveform--write-cache cache-file envelope)
                   (file-error
                    (supersonic--log "Could not cache waveform to %s: %s"
                                     cache-file (error-message-string err)))))
               (when (file-exists-p outfile)
                 (supersonic-waveform--delete-file outfile)))
             (when (eq job supersonic-waveform--job)
               (setq supersonic-waveform--job nil))
             (funcall (plist-get job :callback) envelope)))
        (cond
         ((process-get proc 'supersonic-waveform-cancelled)
          (finish nil))
         ((not (and (eq (process-status proc) 'exit) (= (process-exit-status proc) 0)))
          (message "[Supersonic] Failed to generate waveform: mpv exited abnormally (%s)" (string-trim event))
          (finish nil))
         ((not (file-exists-p outfile))
          (message "[Supersonic] Failed to generate waveform: mpv produced no output file")
          (finish nil))
         (t
          (condition-case err
              (supersonic-waveform--analyze-file-async outfile buckets supersonic-waveform--generation #'finish
                                                       (lambda (envelope)
                                                         (let ((on-progress (plist-get job :progress-callback)))
                                                           (when on-progress
                                                             (funcall on-progress envelope)))))
            (error
             (message "[Supersonic] Failed to generate waveform: %s" (error-message-string err))
             (finish nil)))))))))

(defun supersonic-waveform--start-transcode (url buckets cache-file job)
  "Start mpv transcoding the stream at URL to a WAV file for JOB.
BUCKETS and CACHE-FILE are passed to
`supersonic-waveform--transcode-sentinel'.  URL is passed on stdin to
keep its credentials out of the process list."
  (unless (and supersonic-mpv (executable-find supersonic-mpv))
    (error "No mpv executable found"))
  ;; Deletes the temporary file if Emacs exits during the transcode.
  (add-hook 'kill-emacs-hook #'supersonic-waveform-cancel)
  (let ((outfile (make-temp-file "supersonic-waveform-" nil ".tmp")))
    (setq supersonic-waveform--outfile outfile)
    (setq supersonic-waveform--process
          (make-process
           :name "supersonic-waveform"
           :command
           (list
            supersonic-mpv
            "--no-config"
            "--no-terminal"
            "--really-quiet"
            "--no-video"
            "--ao=pcm"
            (concat "--ao-pcm-file=" outfile)
            (format "--audio-samplerate=%d" supersonic-waveform-samplerate)
            "--audio-channels=mono"
            "--audio-format=s16"
            "--load-unsafe-playlists"
            "--playlist=fd://0")
           ;; So that `process-send-eof' closes stdin.
           :connection-type 'pipe
           :noquery t
           :sentinel (supersonic-waveform--transcode-sentinel outfile buckets cache-file job)))
    (process-send-string supersonic-waveform--process (concat url "\n"))
    (process-send-eof supersonic-waveform--process)))

(defun supersonic-waveform-ensure (id callback &optional progress-callback)
  "Call CALLBACK with the envelope (PEAKS . RMS) of track ID.
Take it from the cache, or compute and cache it, in which case call
PROGRESS-CALLBACK, if non-nil, with the partial envelope now and
then.  CALLBACK gets nil on failure.  Calling this again for the same
track while it is computed replaces the callbacks."
  (let* ((buckets supersonic-waveform-buckets)
         (cache-file (supersonic-waveform-cache-file id buckets))
         (cached
          (and (file-exists-p cache-file)
               (or (condition-case err
                       (supersonic-waveform--read-cache cache-file buckets)
                     (file-error
                      (supersonic--log "Could not read waveform cache %s: %s"
                                       cache-file (error-message-string err))
                      nil))
                   ;; Invalid: delete it, so that it is recomputed.
                   (progn
                     (supersonic-waveform--delete-file cache-file)
                     nil)))))
    (cond
     (cached
      (funcall callback cached))
     ;; Already being generated for this track: re-point the job.
     ((equal id (plist-get supersonic-waveform--job :id))
      (plist-put supersonic-waveform--job :callback callback)
      (plist-put supersonic-waveform--job :progress-callback progress-callback)
      nil)
     (t
      (supersonic-waveform-cancel)
      (let ((job (list :id id :callback callback :progress-callback progress-callback)))
        (setq supersonic-waveform--job job)
        (supersonic-waveform--transcode-when-streamable id buckets cache-file job)
        nil)))))

(aio-defun supersonic-waveform--transcode-when-streamable (id buckets cache-file job)
  "Start transcoding track ID for JOB once its stream URL is known.
BUCKETS and CACHE-FILE are passed on to
`supersonic-waveform--start-transcode'.  If JOB was cancelled
meanwhile, or on failure, call its callback with nil."
  (condition-case err
      (let ((url (aio-await (supersonic-provider-stream-url id))))
        (if (eq job supersonic-waveform--job)
            (supersonic-waveform--start-transcode url buckets cache-file job)
          (funcall (plist-get job :callback) nil)))
    (error
     ;; The process may have started before the error.
     (when (eq job supersonic-waveform--job)
       (supersonic-waveform-cancel)
       (message "[Supersonic] Failed to start waveform generation: %s" (error-message-string err)))
     (funcall (plist-get job :callback) nil))))

;;;
;;; Rendering
;;;

(defun supersonic-waveform--rgb (color)
  "Return COLOR as a list (R G B) of bytes, black if COLOR is invalid."
  (mapcar
   (lambda (v) (/ v 257))
   (or (and (stringp color)
            (color-values color))
       '(0 0 0))))

(defun supersonic-waveform--blend (fg bg alpha)
  "Blend the colors FG over BG with ALPHA, between 0 and 1."
  (cl-mapcar (lambda (f b) (round (+ (* f alpha) (* b (- 1 alpha))))) fg bg))

(defun supersonic-waveform--set-pixel (buf width x y r g b)
  "Set pixel X, Y of the PPM pixel data BUF, WIDTH wide, to R G B."
  (let ((offset (* 3 (+ x (* y width)))))
    (aset buf offset r)
    (aset buf (1+ offset) g)
    (aset buf (+ offset 2) b)))

(defun supersonic-waveform-bars (envelope progress width height)
  "Return the bars of ENVELOPE in a WIDTH by HEIGHT image.
PROGRESS, between 0 and 1, marks the played part.  Each bar is a
plist of :x0 and :x1, its columns; :played; and :rms-extent and
:peak-extent, how far it reaches from the middle."
  (let* ((peaks (car envelope))
         (rms (cdr envelope))
         (buckets (length peaks))
         (progress (or progress 0))
         (center (/ height 2)))
    (cl-loop
     for b below buckets
     collect
     (let ((x0 (/ (* b width) buckets)))
       (list
        :x0 x0
        :x1 (max (1+ x0) (/ (* (1+ b) width) buckets))
        :played (< (/ (float b) buckets) progress)
        :rms-extent (max 1 (round (* (/ (aref rms b) 255.0) center)))
        :peak-extent (round (* (/ (aref peaks b) 255.0) center)))))))

(defun supersonic-waveform-image (envelope progress)
  "Return the seekbar image of ENVELOPE.
PROGRESS, between 0 and 1, marks the played part, drawn in the
default foreground; the rest is drawn in the `shadow' face."
  (let* ((width supersonic-waveform-width)
         (height supersonic-waveform-height)
         (bg (supersonic-waveform--rgb (face-background 'default nil t)))
         (played (supersonic-waveform--rgb (face-foreground 'default nil t)))
         (unplayed (supersonic-waveform--rgb (face-foreground 'shadow nil t)))
         (played-peak (supersonic-waveform--blend played bg 0.4))
         (unplayed-peak (supersonic-waveform--blend unplayed bg 0.4))
         (buf (mapconcat #'identity (make-list (* width height) (apply #'unibyte-string bg)) ""))
         (center (/ height 2)))
    (dolist (bar (supersonic-waveform-bars envelope progress width height))
      (let* ((playedp (plist-get bar :played))
             (solid
              (if playedp
                  played
                unplayed))
             (translucent
              (if playedp
                  played-peak
                unplayed-peak))
             (sr (nth 0 solid))
             (sg (nth 1 solid))
             (sb (nth 2 solid))
             (tr (nth 0 translucent))
             (tg (nth 1 translucent))
             (tb (nth 2 translucent))
             (rms-extent (plist-get bar :rms-extent))
             (peak-extent (plist-get bar :peak-extent))
             (x (plist-get bar :x0)))
        (while (< x (plist-get bar :x1))
          (dotimes (i rms-extent)
            (supersonic-waveform--set-pixel buf width x (max 0 (- center i)) sr sg sb)
            (supersonic-waveform--set-pixel buf width x (min (1- height) (+ center i)) sr sg sb))
          (cl-loop for i from rms-extent below peak-extent
                   do (supersonic-waveform--set-pixel buf width x (max 0 (- center i)) tr tg tb)
                   (supersonic-waveform--set-pixel buf width x (min (1- height) (+ center i)) tr tg tb))
          (setq x (1+ x)))))
    (create-image (concat (string-to-unibyte (format "P6\n%d %d\n255\n" width height)) buf) 'pbm t :mask 'heuristic)))

(defvar supersonic-waveform--svg-bars-cache nil
  "The SVG markup last made by `supersonic-waveform--svg-bars-markup'.")

(defun supersonic-waveform--svg-bar-markup (bar center opacity)
  "Return the SVG markup of BAR, centered at CENTER, at OPACITY."
  (let ((x0 (plist-get bar :x0))
        (width (- (plist-get bar :x1) (plist-get bar :x0)))
        (rms-extent (plist-get bar :rms-extent))
        (peak-extent (plist-get bar :peak-extent))
        (rect "<rect width=\"%s\" height=\"%s\" x=\"%s\" y=\"%s\" fill-opacity=\"%s\" fill=\"white\"></rect>"))
    (concat
     (when (> peak-extent rms-extent)
       (format rect width (* 2 peak-extent) x0 (- center peak-extent) (* opacity 0.5)))
     (format rect width (* 2 rms-extent) x0 (- center rms-extent) opacity))))

(defun supersonic-waveform--svg-bars-markup (envelope width y height)
  "Return (PLAYED . UNPLAYED), vectors of the SVG markup of each bar.
ENVELOPE's bars fill the band WIDTH by HEIGHT at Y.  The result is
cached."
  (let ((cached (car supersonic-waveform--svg-bars-cache)))
    (unless (and (eq envelope (car cached))
                 (equal (list width y height) (cdr cached)))
      (let ((bars (supersonic-waveform-bars envelope 0 width height))
            (center (+ y (/ height 2))))
        (setq supersonic-waveform--svg-bars-cache
              (cons (list envelope width y height)
                    (cons (vconcat (mapcar (lambda (bar) (supersonic-waveform--svg-bar-markup bar center 0.9)) bars))
                          (vconcat (mapcar (lambda (bar) (supersonic-waveform--svg-bar-markup bar center 0.4)) bars))))))))
  (cdr supersonic-waveform--svg-bars-cache))

(defun supersonic-waveform-svg-bars (svg envelope progress width y height)
  "Draw ENVELOPE's bars into SVG, in the band WIDTH by HEIGHT at Y.
PROGRESS, between 0 and 1, marks the played part.  The bars are
white, as they are drawn onto cover art."
  (let* ((markup (supersonic-waveform--svg-bars-markup envelope width y height))
         (buckets (length (car markup)))
         (progress (or progress 0)))
    (dom-append-child
     svg
     (apply #'concat
            (cl-loop for b below buckets
                     collect (aref (if (< (/ (float b) buckets) progress) (car markup) (cdr markup)) b))))))

(defvar supersonic-waveform-seek-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'supersonic-waveform--seek-at-click)
    ;; Keep point where it is.
    (define-key map [down-mouse-1] #'ignore)
    map)
  "Keymap active on the waveform image.
\\<supersonic-waveform-seek-map>\\[supersonic-waveform--seek-at-click] seeks to the clicked position.")

(defun supersonic-waveform--seek-at-click (event)
  "Seek to the position of mouse EVENT on the waveform."
  (interactive "e")
  (let* ((posn (event-start event))
         (image (posn-image posn))
         (x (car (posn-object-x-y posn))))
    ;; `image-size' only exists with image support.
    (when (and image x (fboundp 'image-size))
      (let* ((width (car (image-size image t)))
             (ratio (max 0.0 (min 1.0 (/ (float x) width)))))
        (supersonic-playback-seek-fraction ratio)))))

(defun supersonic-waveform-seekable (display)
  "Return a string displaying DISPLAY, an image, that seeks when clicked."
  (propertize " " 'display display 'keymap supersonic-waveform-seek-map 'pointer 'hand 'help-echo
              "mouse-1: seek to this position"))

(defun supersonic-waveform-propertize (envelope progress)
  "Return a string displaying the seekbar of ENVELOPE at PROGRESS."
  (supersonic-waveform-seekable (supersonic-waveform-image envelope progress)))

(defun supersonic-waveform-unload-function ()
  "Stop the waveform job in progress before unloading."
  (supersonic-waveform-cancel)
  nil)

(provide 'supersonic-waveform)
;;; supersonic-waveform.el ends here
