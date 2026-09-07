;;; supersonic-waveform.el --- Waveform seekbar generation for supersonic.el -*- lexical-binding: t; -*-

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

;; Waveform seekbar generation for supersonic.el, ported from the
;; approach the (unrelated, same-name) Go client "supersonic" uses:
;; transcode the track to a small mono WAV via a disposable mpv
;; subprocess, walk the raw PCM samples in fixed-size buckets to get a
;; peak/RMS envelope, cache that on disk, and render it into a
;; clickable seekbar image.
;;
;; Two deliberate differences from that Go implementation, both to
;; avoid pulling in more than mpv itself as a dependency:
;;
;; - The WAV is parsed by hand instead of via a WAV-decoding library --
;;   the RIFF container is simple enough, and mpv's output format
;;   (mono, s16le, at a rate this file chooses) is under our control.
;; - The seekbar is rendered as a PPM (P6) image rather than an SVG or
;;   PNG, since `pbm' is the one raster image type Emacs always decodes
;;   itself with no external image library required (see
;;   `image-type-available-p'); SVG/PNG support depends on how Emacs
;;   was built.
;;
;; This file knows nothing about the now-playing buffer itself, the
;; same way `supersonic-art.el' doesn't: `supersonic-waveform-ensure'
;; hands back peak/RMS data via a callback, and `supersonic-waveform-image'
;; /`supersonic-waveform-propertize' render it; supersonic.el wires both
;; into its own buffer and decides when to call them.

;;; Code:
(require 'cl-lib)
(require 'supersonic-api)
(require 'supersonic-mpv)

;; fix byte-compiler complaints
(defvar supersonic-mpv)
(defvar supersonic-enable-waveform)
(defvar supersonic-waveform-cache-path)
(defvar supersonic-waveform-buckets)
(defvar supersonic-waveform-width)
(defvar supersonic-waveform-height)

(defvar supersonic-waveform--process nil
  "The mpv transcode process currently generating a waveform, if any.")

(defvar supersonic-waveform--outfile nil
  "The temporary WAV file `supersonic-waveform--process' is writing to.
Tracked separately from the process object so `supersonic-waveform-cancel'
can clean it up even though it's a plain temp file mpv writes to
directly, not something Emacs would otherwise know to delete.")

(defun supersonic-waveform-available-p ()
  "Return non-nil if a waveform can actually be shown right now."
  (and supersonic-enable-waveform (display-graphic-p) (image-type-available-p 'pbm)))

(defun supersonic-waveform-cache-file (id buckets)
  "Return the path ID's peak/RMS envelope is cached under at BUCKETS resolution.
BUCKETS is part of the file name for the same reason size is part of
`supersonic-art-cache-file': changing `supersonic-waveform-buckets'
must not hand old callers a cached envelope at the wrong resolution."
  (expand-file-name (format "%s-%d" id buckets) supersonic-waveform-cache-path))

(defun supersonic-waveform-cancel ()
  "Kill any in-flight waveform transcode, discarding its output file.
Called before starting a new job so a quick track change doesn't leave
a stale transcode running in the background, burning CPU and network
on a waveform nothing will ever show."
  (when (process-live-p supersonic-waveform--process)
    (delete-process supersonic-waveform--process))
  (when (and supersonic-waveform--outfile (file-exists-p supersonic-waveform--outfile))
    (ignore-errors (delete-file supersonic-waveform--outfile)))
  (setq supersonic-waveform--process nil supersonic-waveform--outfile nil))

;;;
;;; WAV parsing and peak/RMS analysis
;;;

(defun supersonic-waveform--le32 (buf pos)
  "Read a 4-byte little-endian unsigned integer from unibyte BUF at POS."
  (logior (aref buf pos) (ash (aref buf (1+ pos)) 8) (ash (aref buf (+ pos 2)) 16) (ash (aref buf (+ pos 3)) 24)))

(defun supersonic-waveform--find-data-chunk (buf)
  "Return (START . LEN) of the \"data\" chunk's payload in unibyte WAV BUF.
Walks the RIFF chunk list instead of assuming a fixed header size,
since mpv writes an extended \"fmt \" chunk (WAVE_FORMAT_EXTENSIBLE)
rather than the minimal 16-byte one.  Returns nil if no \"data\" chunk
is found."
  (let ((pos 12) (len (length buf))) ; skip "RIFF" <size> "WAVE"
    (catch 'found
      (while (< (+ pos 8) len)
        (let* ((id (substring buf pos (+ pos 4)))
               (size (supersonic-waveform--le32 buf (+ pos 4))))
          (if (string= id "data")
              (throw 'found (cons (+ pos 8) (min size (- len pos 8))))
            ;; Chunks are word-aligned: an odd-sized chunk has one byte of
            ;; padding after it that isn't reflected in its own size field.
            (setq pos (+ pos 8 size (if (cl-oddp size) 1 0))))))
      nil)))

(defun supersonic-waveform--normalize (magnitude)
  "Scale a 16-bit sample MAGNITUDE (0..32768) to a 0..255 byte."
  (min 255 (round (* (/ magnitude 32768.0) 255))))

(defun supersonic-waveform--analyze-samples (buf data-start data-len buckets)
  "Compute peak/RMS envelopes over BUCKETS chunks of mono s16le PCM.
BUF is the full unibyte WAV file contents; DATA-START/DATA-LEN mark the
sample data within it, as returned by `supersonic-waveform--find-data-chunk'.
Returns (PEAKS . RMS), each a BUCKETS-length unibyte string of 0..255
magnitudes."
  (let* ((total-samples (/ data-len 2))
         (per-bucket (max 1 (/ total-samples buckets)))
         (peaks (make-string buckets 0 nil))
         (rms (make-string buckets 0 nil)))
    (dotimes (b buckets)
      (let* ((start (* b per-bucket))
             (end (min total-samples (+ start per-bucket)))
             (peak 0)
             (sum-squares 0.0)
             (n 0))
        (cl-loop
         for i from start below end
         for offset = (+ data-start (* i 2))
         for lo = (aref buf offset)
         for hi = (aref buf (1+ offset))
         ;; 16-bit little-endian two's complement.
         for sample = (let ((unsigned (logior lo (ash hi 8))))
                        (if (>= unsigned 32768) (- unsigned 65536) unsigned))
         do (setq peak (max peak (abs sample)) sum-squares (+ sum-squares (* (float sample) sample)) n (1+ n)))
        (aset peaks b (supersonic-waveform--normalize peak))
        (aset rms b (supersonic-waveform--normalize (if (> n 0) (sqrt (/ sum-squares n)) 0)))))
    (cons peaks rms)))

(defun supersonic-waveform--analyze-file (path buckets)
  "Read the mono s16le WAV at PATH and return its (PEAKS . RMS) envelope.
Signals an error if PATH is not a valid WAV file or has no \"data\" chunk."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (let ((buf (buffer-string)))
      (unless (string= (substring buf 0 4) "RIFF")
        (error "Not a RIFF file: %s" path))
      (let ((chunk (supersonic-waveform--find-data-chunk buf)))
        (unless chunk
          (error "No \"data\" chunk found in %s" path))
        (supersonic-waveform--analyze-samples buf (car chunk) (cdr chunk) buckets)))))

;;;
;;; Disk cache
;;;

(defun supersonic-waveform--write-cache (file envelope)
  "Write (PEAKS . RMS) ENVELOPE to FILE as raw bytes, peaks then RMS."
  (unless (file-exists-p supersonic-waveform-cache-path)
    (mkdir supersonic-waveform-cache-path t))
  (let ((coding-system-for-write 'no-conversion))
    (write-region (concat (car envelope) (cdr envelope)) nil file nil 'no-message)))

(defun supersonic-waveform--read-cache (file buckets)
  "Read a (PEAKS . RMS) envelope back from FILE, BUCKETS bytes each.
Returns nil instead of signalling if FILE doesn't hold exactly
2*BUCKETS bytes, so a truncated or otherwise corrupt cache entry is
silently regenerated rather than crashing the caller."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let ((buf (buffer-string)))
      (when (= (length buf) (* 2 buckets))
        (cons (substring buf 0 buckets) (substring buf buckets))))))

;;;
;;; Transcode pipeline
;;;

(defun supersonic-waveform--transcode-sentinel (outfile buckets cache-file callback)
  "Return a process sentinel finishing the job that wrote to OUTFILE.
Reads and analyzes OUTFILE once the process exits, caches the result
to CACHE-FILE at BUCKETS resolution, and calls CALLBACK with it (or
with nil if the process failed, or OUTFILE turned out not to be a
readable WAV file)."
  (lambda (proc _event)
    (unless (process-live-p proc)
      (when (eq proc supersonic-waveform--process)
        (setq supersonic-waveform--process nil supersonic-waveform--outfile nil))
      (let ((envelope
             (and (eq (process-status proc) 'exit) (= (process-exit-status proc) 0) (file-exists-p outfile)
                  (ignore-errors (supersonic-waveform--analyze-file outfile buckets)))))
        (when envelope
          (ignore-errors (supersonic-waveform--write-cache cache-file envelope)))
        (when (file-exists-p outfile)
          (ignore-errors (delete-file outfile)))
        (funcall callback envelope)))))

(defun supersonic-waveform--start-transcode (id buckets cache-file callback)
  "Spawn the disposable mpv subprocess that transcodes ID to WAV.
Helper for `supersonic-waveform-ensure'; see
`supersonic-waveform--transcode-sentinel' for what happens once it exits."
  (unless (and supersonic-mpv (executable-find supersonic-mpv))
    (error "mpv not found"))
  (let* ((outfile (make-temp-file "supersonic-waveform-" nil ".wav"))
         (url (supersonic-build-url "/stream.view" `(("id" . ,id)))))
    (setq supersonic-waveform--outfile outfile)
    (setq supersonic-waveform--process
          (make-process
           :name "supersonic-waveform"
           :command (list supersonic-mpv "--no-config" "--no-terminal" "--really-quiet" "--no-video" "--ao=pcm"
                          (concat "--ao-pcm-file=" outfile) "--audio-samplerate=22050" "--audio-channels=mono"
                          "--audio-format=s16" url)
           :noquery t
           :sentinel (supersonic-waveform--transcode-sentinel outfile buckets cache-file callback)))))

(defun supersonic-waveform-ensure (id callback)
  "Ensure a (PEAKS . RMS) envelope for track ID exists, then call CALLBACK with it.
Reads a disk cache if one exists (see `supersonic-waveform-cache-file');
otherwise transcodes ID's stream through a disposable mpv subprocess
and analyzes the result, caching it for next time.  CALLBACK is called
with nil if generation fails for any reason (mpv missing, a transcode
error, a corrupt result) rather than left unnotified."
  (let* ((buckets supersonic-waveform-buckets)
         (cache-file (supersonic-waveform-cache-file id buckets)))
    (if (file-exists-p cache-file)
        (funcall callback (supersonic-waveform--read-cache cache-file buckets))
      (supersonic-waveform-cancel)
      (condition-case err
          (supersonic-waveform--start-transcode id buckets cache-file callback)
        (error
         (message "[Supersonic] Failed to start waveform generation: %s" (error-message-string err))
         (funcall callback nil))))))

;;;
;;; Rendering
;;;

(defun supersonic-waveform--rgb (color)
  "Return COLOR (a color name or hex string) as an (R G B) list, each 0..255.
Falls back to black if COLOR doesn't resolve to a real color -- either
`color-values' returning nil (an unset face attribute reported back as
\"unspecified-fg\"/\"unspecified-bg\") or COLOR not being a color at all
(nil, from a face attribute a theme leaves fully unset, which
`color-values' signals an error on rather than returning nil for) --
so a pathological theme dims the waveform instead of erroring out of
the whole now-playing buffer render."
  (mapcar (lambda (v) (/ v 257)) (or (ignore-errors (color-values color)) '(0 0 0))))

(defun supersonic-waveform--blend (fg bg alpha)
  "Alpha-blend (R G B) list FG over (R G B) list BG by ALPHA (0..1)."
  (cl-mapcar (lambda (f b) (round (+ (* f alpha) (* b (- 1 alpha))))) fg bg))

(defun supersonic-waveform--set-pixel (buf width x y rgb)
  "Set the pixel at X,Y in unibyte PPM pixel buffer BUF (WIDTH wide) to RGB."
  (let ((offset (* 3 (+ x (* y width)))))
    (aset buf offset (nth 0 rgb))
    (aset buf (1+ offset) (nth 1 rgb))
    (aset buf (+ offset 2) (nth 2 rgb))))

(defun supersonic-waveform-image (envelope progress)
  "Render (PEAKS . RMS) ENVELOPE into a seekbar image.
PROGRESS (0..1, or nil for 0) marks how much of the track counts as
already played, drawn in the buffer's ordinary foreground color, with
the rest in `shadow' -- the same two faces the rest of the now-playing
buffer already uses for emphasis vs. de-emphasis.  RMS pixels are
drawn solid; the extra reach out to the peak is blended halfway into
the background instead, mirroring the solid/translucent split
supersonic (the Go player this is ported from) draws its own seekbar
with."
  (let* ((width supersonic-waveform-width)
         (height supersonic-waveform-height)
         (peaks (car envelope))
         (rms (cdr envelope))
         (buckets (length peaks))
         (progress (or progress 0))
         (bg (supersonic-waveform--rgb (face-background 'default nil t)))
         (played (supersonic-waveform--rgb (face-foreground 'default nil t)))
         (unplayed (supersonic-waveform--rgb (face-foreground 'shadow nil t)))
         (played-peak (supersonic-waveform--blend played bg 0.4))
         (unplayed-peak (supersonic-waveform--blend unplayed bg 0.4))
         (buf (make-string (* width height 3) 0 nil))
         (center (/ height 2)))
    (dotimes (y height)
      (dotimes (x width)
        (supersonic-waveform--set-pixel buf width x y bg)))
    (dotimes (b buckets)
      (let* ((x0 (/ (* b width) buckets))
             (x1 (max (1+ x0) (/ (* (1+ b) width) buckets)))
             (playedp (< (/ (float b) buckets) progress))
             (solid (if playedp played unplayed))
             (translucent (if playedp played-peak unplayed-peak))
             (rms-extent (max 1 (round (* (/ (aref rms b) 255.0) center))))
             (peak-extent (round (* (/ (aref peaks b) 255.0) center))))
        (cl-loop
         for x from x0 below x1 do
         (dotimes (i rms-extent)
           (supersonic-waveform--set-pixel buf width x (max 0 (- center i)) solid)
           (supersonic-waveform--set-pixel buf width x (min (1- height) (+ center i)) solid))
         (cl-loop
          for i from rms-extent below peak-extent do
          (supersonic-waveform--set-pixel buf width x (max 0 (- center i)) translucent)
          (supersonic-waveform--set-pixel buf width x (min (1- height) (+ center i)) translucent)))))
    (create-image (concat (string-to-unibyte (format "P6\n%d %d\n255\n" width height)) buf) 'pbm t)))

(defvar supersonic-waveform-seek-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'supersonic-waveform--seek-at-click)
    map)
  "Keymap active on the waveform image; mouse-1 seeks to the click position.")

(defun supersonic-waveform--seek-at-click (event)
  "Seek mpv to the position in the track EVENT clicked within the waveform."
  (interactive "e")
  (let* ((posn (event-start event))
         (image (car (posn-object posn)))
         (x (car (posn-object-x-y posn))))
    (when (and image x)
      (let* ((width (car (image-size image t)))
             (ratio (max 0.0 (min 1.0 (/ (float x) width)))))
        (supersonic-mpv-command "seek" (number-to-string (* ratio 100)) "absolute-percent")))))

(defun supersonic-waveform-propertize (envelope progress)
  "Return a display string embedding ENVELOPE's seekbar image, clickable to seek.
PROGRESS is as in `supersonic-waveform-image'."
  (propertize " " 'display (supersonic-waveform-image envelope progress) 'keymap supersonic-waveform-seek-map
              'pointer 'hand 'help-echo "mouse-1: seek to this position"))

(provide 'supersonic-waveform)
;;; supersonic-waveform.el ends here
