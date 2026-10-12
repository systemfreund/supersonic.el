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

;; Waveform seekbar for supersonic.el, after the approach of the
;; (unrelated, same-name) Go client "supersonic": transcode the track
;; to a small mono WAV with a disposable mpv, walk the PCM samples in
;; buckets for a peak/RMS envelope, cache that, and render it into a
;; clickable seekbar image.
;;
;; Unlike the Go client, it needs nothing but mpv: the WAV is parsed by
;; hand, and the seekbar is a PPM image, since `pbm' is the one raster
;; type Emacs always decodes itself.
;;
;; The mpv here only transcodes, whichever backend is playing; only the
;; seekbar click goes through the playback facade.  This file knows
;; nothing about the now-playing buffer.

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
  "The temporary WAV file `supersonic-waveform--process' is writing to.
Kept so that `supersonic-waveform-cancel' can delete it.")

(defvar supersonic-waveform--job nil
  "Plist describing the waveform job currently in flight, or nil.
Keys are :id, the track, and :callback and :progress-callback, as
`supersonic-waveform-ensure' got them.  Kept for the job's whole life,
so that a repeat call for the same track -- which the now-playing
buffer makes on every re-render -- re-points the job at its callbacks
instead of restarting the transcode.  The callbacks are replaced by
mutating this plist, since the job's sentinel reads them from it.")

(defvar supersonic-waveform--generation 0
  "Bumped by `supersonic-waveform-cancel' to invalidate in-flight work.
The sample analysis after the transcode has no process to kill; this
is what stops it.")

(defun supersonic-waveform-available-p ()
  "Return non-nil if a waveform can actually be shown right now.
That includes the active provider having a `stream-url' to transcode."
  (and supersonic-enable-waveform
       (display-graphic-p)
       (image-type-available-p 'pbm)
       (supersonic-provider-supports-p 'stream-url)))

(defun supersonic-waveform-cache-file (id buckets)
  "Return the path ID's peak/RMS envelope is cached under at BUCKETS resolution.
BUCKETS and `supersonic-waveform-samplerate' are part of the name, so
that changing either never hands out an envelope measured otherwise.
ID is named via `supersonic-provider-cache-name'; the \"waveform-\"
prefix keeps it apart from `supersonic-art-cache-file' in the shared
`supersonic-cache-path'."
  (expand-file-name
   (format "waveform-%s-%d-%d" (supersonic-provider-cache-name id) buckets supersonic-waveform-samplerate)
   supersonic-cache-path))

(defun supersonic-waveform--delete-file (file)
  "Delete temporary or cache FILE, logging rather than signalling on failure.
Every caller is cleaning up after the real work, which a leftover file
must not abort: a temp file left behind is harmless, and a stale cache
entry is deleted again on the next visit to its track."
  (condition-case err
      (delete-file file)
    (file-error
     (supersonic--log "Could not delete %s: %s" file (error-message-string err)))))

(defun supersonic-waveform-cancel ()
  "Kill any in-flight waveform transcode and discard its output file.
Also stops in-flight sample analysis and forgets
`supersonic-waveform--job'.  Called before starting a new job, so that
a track change leaves nothing running.  The process is tagged as
cancelled first, so that its sentinel does not report a failure."
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
  "Return (START . LEN) of the \"data\" chunk's payload in unibyte WAV BUF.
Walks the RIFF chunk list instead of assuming a fixed header size,
since mpv writes an extended \"fmt \" chunk (WAVE_FORMAT_EXTENSIBLE)
rather than the minimal 16-byte one.  Returns nil if no \"data\" chunk
is found."
  (let ((pos 12)
        (len (length buf))) ; skip "RIFF" <size> "WAVE"
    (catch 'found
      (while (< (+ pos 8) len)
        (let* ((id (substring buf pos (+ pos 4)))
               (size (supersonic-waveform--le32 buf (+ pos 4))))
          (if (string= id "data")
              (throw 'found (cons (+ pos 8) (min size (- len pos 8))))
            ;; Chunks are word-aligned: an odd-sized chunk has one byte of
            ;; padding after it that isn't reflected in its own size field.
            (setq pos
                  (+ pos 8
                     size
                     (if (cl-oddp size)
                         1
                       0))))))
      nil)))

(defun supersonic-waveform--normalize (magnitude)
  "Scale a 16-bit sample MAGNITUDE (0..32768) to a 0..255 byte."
  (min 255 (round (* (/ magnitude 32768.0) 255))))

(defvar supersonic-waveform--analysis-tick-budget 0.02
  "Seconds of work per timer tick during sample analysis.
`supersonic-waveform--analyze-samples-async' spends at most this long
before yielding back to Emacs and resuming on the next tick.")

(defvar supersonic-waveform--progress-interval 0.1
  "Minimum seconds between two progress reports during sample analysis.
Each one has the caller redraw the seekbar, which costs about as much
as a slice of analysis.")

(defun supersonic-waveform--analyze-bucket (buf data-start per-bucket total-samples b)
  "Return (PEAK . RMS), each 0..255, for bucket B of mono s16le PCM.
Helper for `supersonic-waveform--analyze-samples-async'; BUF, DATA-START,
PER-BUCKET and TOTAL-SAMPLES are as computed there."
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
  "Compute peak/RMS envelopes over BUCKETS chunks of mono s16le PCM.
BUF is the unibyte WAV file contents; DATA-START/DATA-LEN mark the
samples in it, as `supersonic-waveform--find-data-chunk' returns them.
Calls ON-DONE with (PEAKS . RMS), each a BUCKETS-long unibyte string of
0..255 magnitudes.

Works in slices of `supersonic-waveform--analysis-tick-budget' seconds
on a zero-delay timer, so that Emacs stays responsive.  Process output
is drained before each reschedule: Emacs only reads it once no timer is
due, and a self-rearming zero-delay timer always is, so mpv's socket
and HTTP connections would starve otherwise.

Stops silently, without calling ON-DONE, once GENERATION no longer
equals `supersonic-waveform--generation'.

ON-PROGRESS, if given, is called with a copy of the partial
\(PEAKS . RMS), unprocessed buckets reading 0, at most once per
`supersonic-waveform--progress-interval'."
  (let* ((total-samples (/ data-len 2))
         (per-bucket (max 1 (/ total-samples buckets)))
         (peaks (make-string buckets 0 nil))
         (rms (make-string buckets 0 nil))
         (b 0)
         (last-progress 0.0))
    (cl-labels
     ((step
       ()
       (when (= generation supersonic-waveform--generation)
         (let ((deadline (+ (float-time) supersonic-waveform--analysis-tick-budget))
               (continue t))
           ;; The deadline is checked after a bucket, so that every
           ;; slice makes progress however small the budget.
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
               ;; Let process output through; see the docstring.
               (accept-process-output nil 0)
               (run-with-timer 0 nil #'step))
           (funcall on-done (cons peaks rms))))))
     (step))))

(defun supersonic-waveform--analyze-file-async (path buckets generation on-done &optional on-progress)
  "Read the mono s16le WAV at PATH and call ON-DONE with its envelope.
The envelope is a (PEAKS . RMS) pair at BUCKETS resolution, computed by
`supersonic-waveform--analyze-samples-async' with GENERATION and
ON-PROGRESS.  Signals an error right away if PATH is no WAV file with
a \"data\" chunk.

PATH is deleted as soon as it is read, so that a cancelled analysis
does not leak it.  It is read with `file-name-handler-alist' nil, so
that no handler for media files gets a say."
  (let* ((file-name-handler-alist nil)
         (buf
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (buffer-string))))
    (supersonic-waveform--delete-file path)
    ;; Length first, so that a short file gets this error rather than
    ;; `args-out-of-range'.
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
  "Write (PEAKS . RMS) ENVELOPE to FILE as raw bytes, peaks then RMS."
  (unless (file-exists-p supersonic-cache-path)
    (mkdir supersonic-cache-path t))
  (let ((coding-system-for-write 'no-conversion))
    (write-region (concat (car envelope) (cdr envelope)) nil file nil 'no-message)))

(defun supersonic-waveform--read-cache (file buckets)
  "Read a (PEAKS . RMS) envelope back from FILE, BUCKETS bytes each.
Returns nil instead of signalling if FILE doesn't hold exactly
2*BUCKETS bytes, so a truncated or otherwise corrupt cache entry
reads as a miss rather than crashing the caller;
`supersonic-waveform-ensure' is what then deletes and regenerates it."
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
  "Return a process sentinel finishing JOB, whose transcode wrote to OUTFILE.
Once the process exits, OUTFILE is analyzed, the envelope cached to
CACHE-FILE at BUCKETS resolution, and JOB's :callback called with it,
or with nil on failure, which is reported.  A process
`supersonic-waveform-cancel' killed is cleaned up silently.  The
callbacks are read from JOB when called, so that
`supersonic-waveform-ensure' can re-point them."
  (lambda (proc event)
    (unless (process-live-p proc)
      (when (eq proc supersonic-waveform--process)
        (setq
         supersonic-waveform--process nil
         supersonic-waveform--outfile nil)
        (remove-hook 'kill-emacs-hook #'supersonic-waveform-cancel))
      (cl-flet
       ((finish
         (envelope)
         (let ((file-name-handler-alist nil))
           (when envelope
             ;; A cache that cannot be written only means analyzing
             ;; again next time; the envelope is still good to show.
             (condition-case err
                 (supersonic-waveform--write-cache cache-file envelope)
               (file-error
                (supersonic--log "Could not cache waveform to %s: %s"
                                 cache-file (error-message-string err)))))
           ;; Only left over where analysis never ran.
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
  "Spawn the disposable mpv that transcodes the stream at URL to WAV.
For `supersonic-waveform-ensure', on behalf of JOB; see
`supersonic-waveform--transcode-sentinel', which BUCKETS and
CACHE-FILE are passed on to, for the rest.

The output file has no media extension: a media-file mode such as
ready-player.el may register a `file-name-handler-alist' entry for
those that hides the real bytes.

URL is written to mpv's stdin as a one-line playlist rather than put
in argv, where `ps' would show its credentials, such as Subsonic's
never-expiring token.  `--load-unsafe-playlists' lets mpv play
non-HTTP entries from it, such as the \"av://\" test streams."
  (unless (and supersonic-mpv (executable-find supersonic-mpv))
    (error "No mpv executable found"))
  ;; Nothing may signal between `make-temp-file' and the `setq' right
  ;; after it: the temp file would exist while nothing points at it,
  ;; stranded where not even `supersonic-waveform-cancel' could find it
  ;; again.  That is why URL comes in already resolved.
  ;;
  ;; So that a transcode in flight at exit leaves no temp file behind;
  ;; taken off again by `supersonic-waveform-cancel' or the sentinel.
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
           ;; A pipe, not a pty, so that `process-send-eof' closes it and
           ;; mpv gets on with playing the playlist.
           :connection-type 'pipe
           :noquery t
           :sentinel (supersonic-waveform--transcode-sentinel outfile buckets cache-file job)))
    (process-send-string supersonic-waveform--process (concat url "\n"))
    (process-send-eof supersonic-waveform--process)))

(defun supersonic-waveform-ensure (id callback &optional progress-callback)
  "Ensure a (PEAKS . RMS) envelope for track ID exists, then call CALLBACK with it.
Reads the disk cache if there is an entry; otherwise transcodes ID's
stream and analyzes it, caching the result.  An entry that cannot be
read is deleted and regenerated.  CALLBACK gets nil if generation
fails.

Calling this again for a track still being generated re-points the job
at the new callbacks, dropping the earlier ones.  PROGRESS-CALLBACK, if
given, gets the partial envelope as analysis goes; never on a cache
hit."
  (let* ((buckets supersonic-waveform-buckets)
         (cache-file (supersonic-waveform-cache-file id buckets))
         (cached
          (and (file-exists-p cache-file)
               (or (condition-case err
                       (supersonic-waveform--read-cache cache-file buckets)
                     ;; Gone or unreadable since `file-exists-p' said
                     ;; otherwise: a miss like any other.
                     (file-error
                      (supersonic--log "Could not read waveform cache %s: %s"
                                       cache-file (error-message-string err))
                      nil))
                   ;; Nothing usable in it: delete it, so that it is
                   ;; regenerated.
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
        ;; Installed right away, before the stream URL is even asked
        ;; for, so a repeat call for this track in the meantime re-points
        ;; this job rather than starting a second one.
        (setq supersonic-waveform--job job)
        (supersonic-waveform--transcode-when-streamable id buckets cache-file job)
        nil)))))

(aio-defun
 supersonic-waveform--transcode-when-streamable (id buckets cache-file job)
 "Transcode track ID for JOB once the provider has named its stream URL.
BUCKETS and CACHE-FILE are passed on to
`supersonic-waveform--start-transcode'.  The URL arrives
asynchronously, so JOB may no longer be `supersonic-waveform--job' by
then -- cancelled, or replaced by a job for another track.  Nothing is
spawned for it in that case, and its callback is called with nil, the
same as a transcode cancelled while running would.  A failure to get
the URL or to start the transcode is reported and likewise ends in
nil."
 (condition-case err
     (let ((url (aio-await (supersonic-provider-stream-url id))))
       (if (eq job supersonic-waveform--job)
           (supersonic-waveform--start-transcode url buckets cache-file job)
         (funcall (plist-get job :callback) nil)))
   (error
    ;; Cancel on the way out: `supersonic-waveform--start-transcode' can
    ;; fail with the process already spawned (writing the URL to a
    ;; process that died on the spot), and a job left behind with nothing
    ;; running would have the next call for this same track re-point it
    ;; and then wait on it forever.
    (when (eq job supersonic-waveform--job)
      (supersonic-waveform-cancel)
      (message "[Supersonic] Failed to start waveform generation: %s" (error-message-string err)))
    (funcall (plist-get job :callback) nil))))

;;;
;;; Rendering
;;;

(defun supersonic-waveform--rgb (color)
  "Return COLOR (a color name or hex string) as an (R G B) list, each 0..255.
Falls back to black for an unset or invalid COLOR, so that an odd
theme dims the waveform rather than breaking the render."
  (mapcar
   (lambda (v) (/ v 257))
   (or (and (stringp color)
            (color-values color))
       '(0 0 0))))

(defun supersonic-waveform--blend (fg bg alpha)
  "Alpha-blend (R G B) list FG over (R G B) list BG by ALPHA (0..1)."
  (cl-mapcar (lambda (f b) (round (+ (* f alpha) (* b (- 1 alpha))))) fg bg))

(defun supersonic-waveform--set-pixel (buf width x y r g b)
  "Set the pixel at X,Y in unibyte PPM pixel buffer BUF (WIDTH wide) to R G B.
Takes the channels apart rather than as a list, as it is called per
pixel."
  (let ((offset (* 3 (+ x (* y width)))))
    (aset buf offset r)
    (aset buf (1+ offset) g)
    (aset buf (+ offset 2) b)))

(defun supersonic-waveform-bars (envelope progress width height)
  "Return each bucket of (PEAKS . RMS) ENVELOPE as a bar geometry plist.
Shared by `supersonic-waveform-image' and
`supersonic-waveform-svg-bars'.  PROGRESS (0..1, or nil for 0) is the
played/unplayed boundary, at WIDTH x HEIGHT.

Each element is a plist: :x0/:x1, the bar's half-open pixel column
range; :played; and :rms-extent/:peak-extent, how many pixels out from
the vertical center the bar reaches, :peak-extent >= :rms-extent,
neither past HEIGHT/2."
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
  "Render (PEAKS . RMS) ENVELOPE into a seekbar image.
PROGRESS (0..1, or nil for 0) marks how much of the track counts as
played, drawn in the default foreground, the rest in `shadow'.  RMS is
drawn solid, the reach out to the peak blended into the background.
The background is masked out, so that whatever is behind the image,
such as `hl-line', shows through."
  (let* ((width supersonic-waveform-width)
         (height supersonic-waveform-height)
         (bg (supersonic-waveform--rgb (face-background 'default nil t)))
         (played (supersonic-waveform--rgb (face-foreground 'default nil t)))
         (unplayed (supersonic-waveform--rgb (face-foreground 'shadow nil t)))
         (played-peak (supersonic-waveform--blend played bg 0.4))
         (unplayed-peak (supersonic-waveform--blend unplayed bg 0.4))
         ;; Pre-filled with the background, far cheaper than pixel by
         ;; pixel, as this renders once a second.
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
             ;; Destructured once per bucket instead of per pixel below.
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
          (cl-loop
           for
           i
           from
           rms-extent
           below
           peak-extent
           do
           (supersonic-waveform--set-pixel buf width x (max 0 (- center i)) tr tg tb)
           (supersonic-waveform--set-pixel buf width x (min (1- height) (+ center i)) tr tg tb))
          (setq x (1+ x)))))
    (create-image (concat (string-to-unibyte (format "P6\n%d %d\n255\n" width height)) buf) 'pbm t :mask 'heuristic)))

(defun supersonic-waveform-svg-bars (svg bars y height)
  "Draw BARS (see `supersonic-waveform-bars') into SVG as rectangles.
Y and HEIGHT mark the band BARS was computed for.  Drawn in white,
with played and unplayed, RMS and peak told apart by opacity, since
this goes onto cover art, where a fixed light color reads best."
  (let ((center (+ y (/ height 2))))
    (dolist (bar bars)
      (let* ((x0 (plist-get bar :x0))
             (width (- (plist-get bar :x1) x0))
             (rms-extent (plist-get bar :rms-extent))
             (peak-extent (plist-get bar :peak-extent))
             (opacity (if (plist-get bar :played) 0.9 0.4)))
        (when (> peak-extent rms-extent)
          (svg-rectangle
           svg x0 (- center peak-extent) width (* 2 peak-extent) :fill "white" :fill-opacity (* opacity 0.5)))
        (svg-rectangle svg x0 (- center rms-extent) width (* 2 rms-extent) :fill "white" :fill-opacity opacity)))))

(defvar supersonic-waveform-seek-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'supersonic-waveform--seek-at-click)
    ;; Keep `mouse-drag-region' from moving point, and the cursor, onto
    ;; the image.
    (define-key map [down-mouse-1] #'ignore)
    map)
  "Keymap active on the waveform image.
\\<supersonic-waveform-seek-map>\\[supersonic-waveform--seek-at-click] seeks to the clicked position.")

(defun supersonic-waveform--seek-at-click (event)
  "Seek to the position in the track EVENT clicked within the waveform.
The image's width stands for the whole track."
  (interactive "e")
  (let* ((posn (event-start event))
         (image (posn-image posn))
         (x (car (posn-object-x-y posn))))
    (when (and image x)
      (let* ((width (car (image-size image t)))
             (ratio (max 0.0 (min 1.0 (/ (float x) width)))))
        (supersonic-playback-seek-fraction ratio)))))

(defun supersonic-waveform-seekable (display)
  "Return a display string showing DISPLAY (an image spec), clickable to seek.
Shared by `supersonic-waveform-propertize' and the art overlay with a
waveform lane."
  (propertize " " 'display display 'keymap supersonic-waveform-seek-map 'pointer 'hand 'help-echo
              "mouse-1: seek to this position"))

(defun supersonic-waveform-propertize (envelope progress)
  "Return a display string embedding ENVELOPE's seekbar image, clickable to seek.
PROGRESS is as in `supersonic-waveform-image'."
  (supersonic-waveform-seekable (supersonic-waveform-image envelope progress)))

(defun supersonic-waveform-unload-function ()
  "Cancel any waveform job, which takes its `kill-emacs-hook' entry off too.
Called by `unload-feature', which would otherwise leave that hook
holding a reference to a function that no longer exists.  Returns nil
so `unload-feature' still goes on to remove the definitions itself."
  (supersonic-waveform-cancel)
  nil)

(provide 'supersonic-waveform)
;;; supersonic-waveform.el ends here
