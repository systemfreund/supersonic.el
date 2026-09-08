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
;; The mpv subprocess here is only ever a transcoder: it is spawned to
;; turn a stream into PCM and exits, independently of which playback
;; backend is actually playing anything (see `supersonic-playback.el').
;; The one place this file does touch playback is the seekbar click,
;; which goes through that facade.
;;
;; This file knows nothing about the now-playing buffer itself, the
;; same way `supersonic-art.el' doesn't: `supersonic-waveform-ensure'
;; hands back peak/RMS data via a callback, and `supersonic-waveform-image'
;; /`supersonic-waveform-propertize' render it; supersonic.el wires both
;; into its own buffer and decides when to call them.

;;; Code:
(require 'cl-lib)
(require 'supersonic-api)
(require 'supersonic-playback)

;; fix byte-compiler complaints
(defvar supersonic-mpv)
(defvar supersonic-enable-waveform)
(defvar supersonic-cache-path)
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

(defvar supersonic-waveform--generation 0
  "Bumped by `supersonic-waveform-cancel' to invalidate in-flight work.
The mpv transcode process itself can just be killed, but the chunked
sample analysis that runs after it exits (see
`supersonic-waveform--analyze-samples-async') has no process object of
its own -- this is what lets `supersonic-waveform-cancel' stop a stale
analysis (for a track the user has since moved on from) from grinding
on in the background regardless.")

(defun supersonic-waveform-available-p ()
  "Return non-nil if a waveform can actually be shown right now."
  (and supersonic-enable-waveform (display-graphic-p) (image-type-available-p 'pbm)))

(defun supersonic-waveform-cache-file (id buckets)
  "Return the path ID's peak/RMS envelope is cached under at BUCKETS resolution.
BUCKETS is part of the file name for the same reason size is part of
`supersonic-art-cache-file': changing `supersonic-waveform-buckets'
must not hand old callers a cached envelope at the wrong resolution.
Prefixed with \"waveform-\": `supersonic-cache-path' is shared with
`supersonic-art-cache-file', whose own ID could otherwise coincide
with this one (e.g. a track and its own cover art id) and collide on
the same file name."
  (expand-file-name (format "waveform-%s-%d" id buckets) supersonic-cache-path))

(defun supersonic-waveform-cancel ()
  "Kill any in-flight waveform transcode, discarding its output file,
and invalidate any in-flight chunked sample analysis too.
Called before starting a new job so a quick track change doesn't leave
a stale transcode -- or a stale analysis grinding through a track
nobody cares about anymore, tying up Emacs's timer queue for however
much longer that would have taken -- running in the background.  Tags
the process as deliberately cancelled before killing it, so
`supersonic-waveform--transcode-sentinel' knows not to report it as a
failure -- killing it is the whole point here, not something gone
wrong.  Bumps `supersonic-waveform--generation' unconditionally, since
a stale analysis (the mpv process having already exited by the time it
starts) has no process object left to kill in the first place."
  (setq supersonic-waveform--generation (1+ supersonic-waveform--generation))
  (when (process-live-p supersonic-waveform--process)
    (process-put supersonic-waveform--process 'supersonic-waveform-cancelled t)
    (delete-process supersonic-waveform--process))
  (when (and supersonic-waveform--outfile (file-exists-p supersonic-waveform--outfile))
    (ignore-errors
      (delete-file supersonic-waveform--outfile)))
  (setq
   supersonic-waveform--process nil
   supersonic-waveform--outfile nil))

;; A transcode in flight when Emacs exits would otherwise leave its temp
;; file behind: mpv is still writing to it, so nothing has deleted it yet.
;; Only covers a graceful exit -- there is no hook for being killed.
(add-hook 'kill-emacs-hook #'supersonic-waveform-cancel)

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
  "Seconds `supersonic-waveform--analyze-samples-async' spends per timer
tick before yielding back to Emacs and resuming on the next one.")

(defvar supersonic-waveform--progress-interval 0.1
  "Minimum seconds between two ON-PROGRESS calls from
`supersonic-waveform--analyze-samples-async'.  Each one has the caller
re-render the seekbar image, which costs about as much as a slice of
analysis itself; ten redraws a second look no different from forty.")

(defun supersonic-waveform--analyze-bucket (buf data-start per-bucket total-samples b)
  "Return (PEAK . RMS), each 0..255, for bucket B of mono s16le PCM.
Helper for `supersonic-waveform--analyze-samples-async'; BUF, DATA-START,
PER-BUCKET and TOTAL-SAMPLES are as computed there."
  (let* ((start (* b per-bucket))
         (end (min total-samples (+ start per-bucket)))
         (peak 0)
         (sum-squares 0.0)
         (n 0))
    (cl-loop
     for
     i
     from
     start
     below
     end
     for
     offset
     =
     (+ data-start (* i 2))
     for
     lo
     =
     (aref buf offset)
     for
     hi
     =
     (aref buf (1+ offset))
     ;; 16-bit little-endian two's complement.
     for
     sample
     =
     (let ((unsigned (logior lo (ash hi 8))))
       (if (>= unsigned 32768)
           (- unsigned 65536)
         unsigned))
     do
     (setq
      peak (max peak (abs sample))
      sum-squares (+ sum-squares (* (float sample) sample))
      n (1+ n)))
    (cons
     (supersonic-waveform--normalize peak)
     (supersonic-waveform--normalize
      (if (> n 0)
          (sqrt (/ sum-squares n))
        0)))))

(defun supersonic-waveform--analyze-samples-async
    (buf data-start data-len buckets generation on-done &optional on-progress)
  "Compute peak/RMS envelopes over BUCKETS chunks of mono s16le PCM,
without blocking Emacs while doing it. BUF is the full unibyte WAV file
contents; DATA-START/DATA-LEN mark the sample data within it, as
returned by `supersonic-waveform--find-data-chunk'. Calls ON-DONE with
\(PEAKS . RMS), each a BUCKETS-length unibyte string of 0..255
magnitudes, once every bucket has been processed.

Walking a multi-minute track's raw PCM sample-by-sample in Lisp is
real work; doing it all in one synchronous pass froze Emacs solid
until it finished. Instead this processes buckets in small time-boxed
slices (`supersonic-waveform--analysis-tick-budget' each), yielding
back to Emacs between slices via a zero-delay timer so redisplay and
input keep running throughout.

Yielding via a timer alone is not enough, though: process output has
to be drained explicitly (`accept-process-output') before each
reschedule.  When Emacs is idle in its command loop, it runs every due
timer -- redisplaying in between -- and only goes on to read process
output once no timer is due anymore.  A zero-delay timer that re-arms
itself is *always* due, so without the explicit drain Emacs would spin
on this chain until it finished, never reading a byte from mpv's IPC
socket or an HTTP connection in the meantime (keyboard input still
interrupts that loop, which is why a keypress would still reach mpv
while the now-playing buffer sat frozen waiting for replies that never
got delivered).

ON-PROGRESS calls are rate-limited to one per
`supersonic-waveform--progress-interval' (the first slice always
reports): rendering a seekbar image costs about as much as a whole
slice of analysis, and redrawing it dozens of times a second buys
nothing over redrawing it ten.

GENERATION must still equal `supersonic-waveform--generation' at the
start of every slice, checked there and nowhere in between -- once
`supersonic-waveform-cancel' bumps that counter out from under it, the
current slice is still allowed to finish, but no further slice is
scheduled and ON-DONE is never called.  Silent by design: this is how
a stale analysis (for a track the user has since moved on from) is
made to stop, not a failure to report.

If given, ON-PROGRESS is called with the same shape of (PEAKS . RMS)
after every slice but the last -- still-unprocessed buckets read as 0
in it -- so a caller can redraw the seekbar as it fills in instead of
only once the whole track has been analyzed. Each call gets its own
copies, safe to hold onto after this function has moved on to the next
slice."
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
           ;; Checking the deadline only after processing a bucket
           ;; (rather than in a plain `while' condition checked up
           ;; front) matters at the low end: with a small or zero
           ;; budget, elapsed time can already exceed the deadline
           ;; before a single bucket ran, which would make zero
           ;; progress per tick and never finish.
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
               ;; Let pending process output (mpv IPC replies, HTTP
               ;; responses, subprocess sentinels) through before the
               ;; next slice -- see the docstring for why the timer
               ;; reschedule alone would never get around to that.
               (accept-process-output nil 0)
               (run-with-timer 0 nil #'step))
           (funcall on-done (cons peaks rms))))))
     (step))))

(defun supersonic-waveform--analyze-file-async (path buckets generation on-done &optional on-progress)
  "Read the mono s16le WAV at PATH and call ON-DONE with its (PEAKS . RMS)
envelope. Signals an error synchronously -- before ON-DONE ever enters
the picture -- if PATH is not a valid WAV file or has no \"data\" chunk.
The sample-crunching itself happens via
`supersonic-waveform--analyze-samples-async', spread across several
event-loop turns rather than in one uninterrupted pass, reporting
partial results via ON-PROGRESS if given and stopping early if
GENERATION goes stale -- see that function for why.

Deletes PATH as soon as its contents have been read, before any of
that: it is a disposable temp file, nothing reads it again once the
bytes are in memory, and waiting until analysis finishes would leak it
whenever analysis gets cancelled instead.

Reads with `file-name-handler-alist' bound to nil: PATH is our own
disposable temp file, not something a handler installed for the user's
own purposes (e.g. a media-file minor mode intercepting file
operations on recognized audio extensions) should ever get a say in --
see `supersonic-waveform--start-transcode' for why PATH deliberately
doesn't have one of those extensions in the first place."
  (let* ((file-name-handler-alist nil)
         (buf
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (buffer-string))))
    ;; PATH's bytes are in memory now, so the file itself is dead weight
    ;; from here on -- delete it immediately rather than once analysis is
    ;; done.  A cancelled analysis never reaches its completion path at
    ;; all (see `supersonic-waveform--analyze-samples-async'), so deleting
    ;; there leaked one multi-megabyte temp file per interrupted track.
    (ignore-errors
      (delete-file path))
    (unless (string= (substring buf 0 4) "RIFF")
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

(defun supersonic-waveform--transcode-sentinel (outfile buckets cache-file callback &optional progress-callback)
  "Return a process sentinel finishing the job that wrote to OUTFILE.
Reads and analyzes OUTFILE once the process exits -- asynchronously,
via `supersonic-waveform--analyze-file-async', so a long track's worth
of sample-crunching can't block Emacs -- caches the result to
CACHE-FILE at BUCKETS resolution, and calls CALLBACK with it (or with
nil if the process failed, or OUTFILE turned out not to be a readable
WAV file).  Any such failure is also reported via `message', rather
than only manifesting as \"no waveform ever showed up\" with nothing
to explain why.  A process `supersonic-waveform-cancel' killed on
purpose (a track change interrupting an in-flight transcode, say) is
the one exception: that's an ordinary part of switching tracks, not a
failure, so it's cleaned up silently instead.

PROGRESS-CALLBACK, if given, is passed through as
`supersonic-waveform--analyze-file-async''s ON-PROGRESS.  The
`supersonic-waveform--generation' snapshot taken here, right as
analysis begins, is what lets a later `supersonic-waveform-cancel'
stop it -- see that function's GENERATION parameter."
  (lambda (proc event)
    (unless (process-live-p proc)
      (when (eq proc supersonic-waveform--process)
        (setq
         supersonic-waveform--process nil
         supersonic-waveform--outfile nil))
      (let ((file-name-handler-alist nil))
        (cl-flet
         ((finish
           (envelope)
           (let ((file-name-handler-alist nil))
             (when envelope
               (ignore-errors
                 (supersonic-waveform--write-cache cache-file envelope)))
             (when (file-exists-p outfile)
               (ignore-errors
                 (delete-file outfile))))
           (funcall callback envelope)))
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
                                                        progress-callback)
             (error
              (message "[Supersonic] Failed to generate waveform: %s" (error-message-string err))
              (finish nil))))))))))

(defun supersonic-waveform--start-transcode (id buckets cache-file callback &optional progress-callback)
  "Spawn the disposable mpv subprocess that transcodes ID to WAV.
Helper for `supersonic-waveform-ensure'; see
`supersonic-waveform--transcode-sentinel' for what happens once it
exits, and where PROGRESS-CALLBACK ends up.  The output file
deliberately does NOT get a \".wav\" (or other media-file) extension: a
media-file minor mode (e.g. ready-player.el) can register a
`file-name-handler-alist' entry for such extensions that intercepts
reads of the file and hands back empty/placeholder content instead of
the real bytes, on the assumption that nothing needs the raw data of a
file it's offering to play instead."
  (unless (and supersonic-mpv (executable-find supersonic-mpv))
    (error "mpv not found"))
  ;; Build the url first: `supersonic-build-url' signals when there are no
  ;; usable credentials, and between `make-temp-file' and the `setq' below
  ;; the temp file exists while nothing yet points at it -- an error thrown
  ;; in that window would strand it where not even
  ;; `supersonic-waveform-cancel' could find it again.
  (let* ((url (supersonic-build-url "/stream.view" `(("id" . ,id))))
         (outfile (make-temp-file "supersonic-waveform-" nil ".tmp")))
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
            "--audio-samplerate=6000"
            "--audio-channels=mono"
            "--audio-format=s16"
            url)
           :noquery t
           :sentinel (supersonic-waveform--transcode-sentinel outfile buckets cache-file callback progress-callback)))))

(defun supersonic-waveform-ensure (id callback &optional progress-callback)
  "Ensure a (PEAKS . RMS) envelope for track ID exists, then call CALLBACK with it.
Reads a disk cache if one exists (see `supersonic-waveform-cache-file');
otherwise transcodes ID's stream through a disposable mpv subprocess
and analyzes the result, caching it for next time.  CALLBACK is called
with nil if generation fails for any reason (mpv missing, a transcode
error, a corrupt result) rather than left unnotified.

If given, PROGRESS-CALLBACK is called with a (PEAKS . RMS) envelope of
whatever has been analyzed so far, repeatedly, before CALLBACK's final
call -- letting a caller redraw the seekbar as it fills in rather than
only once the whole track is done.  Never called on a cache hit, since
there's nothing partial about that case."
  (let* ((buckets supersonic-waveform-buckets)
         (cache-file (supersonic-waveform-cache-file id buckets)))
    (if (file-exists-p cache-file)
        (funcall callback (supersonic-waveform--read-cache cache-file buckets))
      (supersonic-waveform-cancel)
      (condition-case err
          (supersonic-waveform--start-transcode id buckets cache-file callback progress-callback)
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
  (mapcar
   (lambda (v) (/ v 257))
   (or (ignore-errors
         (color-values color))
       '(0 0 0))))

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
with.

The flat background fill is masked out (`:mask' set to `heuristic',
keying off the corner pixel -- always pure background, since even a
full-height peak bar leaves a margin at the very top and bottom row)
rather than left as a solid rectangle, so whatever is actually behind
the seekbar in the buffer -- the real background, `hl-line', an active
region -- shows through instead of whatever `face-background' happened
to report when this image was generated."
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
             (solid
              (if playedp
                  played
                unplayed))
             (translucent
              (if playedp
                  played-peak
                unplayed-peak))
             (rms-extent (max 1 (round (* (/ (aref rms b) 255.0) center))))
             (peak-extent (round (* (/ (aref peaks b) 255.0) center))))
        (cl-loop
         for x from x0 below x1 do
         (dotimes (i rms-extent)
           (supersonic-waveform--set-pixel buf width x (max 0 (- center i)) solid)
           (supersonic-waveform--set-pixel buf width x (min (1- height) (+ center i)) solid))
         (cl-loop
          for
          i
          from
          rms-extent
          below
          peak-extent
          do
          (supersonic-waveform--set-pixel buf width x (max 0 (- center i)) translucent)
          (supersonic-waveform--set-pixel buf width x (min (1- height) (+ center i)) translucent)))))
    (create-image (concat (string-to-unibyte (format "P6\n%d %d\n255\n" width height)) buf) 'pbm t :mask 'heuristic)))

(defvar supersonic-waveform-seek-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'supersonic-waveform--seek-at-click)
    ;; The global `down-mouse-1' binding is `mouse-drag-region', which moves
    ;; point to the click position as its very first act (to support
    ;; selecting a region by dragging) -- visible here as the cursor box
    ;; rendering right on top of the seekbar image after every click.
    ;; Overriding it locally with a no-op keeps that from ever running for
    ;; clicks on the image, so point (and the cursor drawn at it) never
    ;; moves there in the first place.
    (define-key map [down-mouse-1] #'ignore)
    map)
  "Keymap active on the waveform image; mouse-1 seeks to the click position.")

(defun supersonic-waveform--seek-at-click (event)
  "Seek to the position in the track EVENT clicked within the waveform.
The image's width stands for the whole track, so the click's x offset
within it is a fraction of the track -- which is exactly what
`supersonic-playback-seek-fraction' takes, whichever backend is
playing."
  (interactive "e")
  (let* ((posn (event-start event))
         (image (posn-image posn))
         (x (car (posn-object-x-y posn))))
    (when (and image x)
      (let* ((width (car (image-size image t)))
             (ratio (max 0.0 (min 1.0 (/ (float x) width)))))
        (supersonic-playback-seek-fraction ratio)))))

(defun supersonic-waveform-propertize (envelope progress)
  "Return a display string embedding ENVELOPE's seekbar image, clickable to seek.
PROGRESS is as in `supersonic-waveform-image'."
  (propertize " "
              'display
              (supersonic-waveform-image envelope progress)
              'keymap
              supersonic-waveform-seek-map
              'pointer
              'hand
              'help-echo
              "mouse-1: seek to this position"))

(provide 'supersonic-waveform)
;;; supersonic-waveform.el ends here
