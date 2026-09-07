;;; supersonic-tests.el --- Tests for supersonic.el's mpv queueing  -*- lexical-binding: t; -*-

;; This is free software; see supersonic.el for licensing details.

;;; Commentary:

;; Exercises supersonic.el's mpv queueing/scrobbling-id-mapping logic
;; against a real (but headless, synthetic-audio) mpv process, since
;; that is what the package actually talks to; nothing here needs a
;; real supersonic server.  Run with:
;;
;;   make test
;;
;; or directly:
;;
;;   cask emacs -Q --batch -L . -l ert -l supersonic.el -l supersonic-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Skipped automatically if mpv is not installed.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'supersonic)
(require 'aio)

(defvar supersonic-tests--track-1 "av://lavfi:sine=frequency=440:duration=2")
(defvar supersonic-tests--track-2 "av://lavfi:sine=frequency=660:duration=2")
(defvar supersonic-tests--track-3 "av://lavfi:sine=frequency=880:duration=2")

(defmacro supersonic-tests--with-mpv (&rest body)
  "Run BODY with a real mpv running, treating ids as raw play urls.
Skips the test if mpv is not available.  Always kills mpv afterwards,
even if BODY signals."
  `(if (not (and supersonic-mpv (executable-find supersonic-mpv)))
       (ert-skip "mpv not found")
     (cl-letf (((symbol-function 'supersonic-build-url)
                (lambda (_endpoint extra-query) (alist-get "id" extra-query nil nil #'equal))))
       (unwind-protect
           (progn
             ,@body)
         (supersonic-mpv-kill)))))

(defun supersonic-tests--wait-for (predicate &optional timeout)
  "Busy-wait until PREDICATE is non-nil or TIMEOUT (default 5s) elapses.
Returns the final value of PREDICATE."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (sleep-for 0.05))
    (funcall predicate)))

(ert-deftest supersonic-tests-load-track-rolls-back-on-send-failure ()
  "`supersonic--mpv-load-track' does not advance `supersonic-mpv--entry-counter'
or register an entry in `supersonic--playlist' when the `loadfile'
command could not actually be sent to mpv (e.g. no live IPC queue),
so the client-side counter never runs ahead of what mpv has actually
seen."
  (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url")))
    (let ((supersonic-mpv--queue nil)
          (supersonic-mpv--entry-counter 5)
          (supersonic--playlist (make-hash-table)))
      (should-error (supersonic--mpv-load-track "some-id" "replace"))
      (should (= 5 supersonic-mpv--entry-counter))
      (should (= 0 (hash-table-count supersonic--playlist))))))

(ert-deftest supersonic-tests-start-assigns-sequential-ids ()
  "`supersonic-mpv-start' maps mpv's playlist entry ids 1..n, in order."
  (supersonic-tests--with-mpv
   (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2 supersonic-tests--track-3))
   (should (supersonic-tests--wait-for (lambda () (= 3 (hash-table-count supersonic--playlist)))))
   (should (equal supersonic-tests--track-1 (gethash 1 supersonic--playlist)))
   (should (equal supersonic-tests--track-2 (gethash 2 supersonic--playlist)))
   (should (equal supersonic-tests--track-3 (gethash 3 supersonic--playlist)))))

(ert-deftest supersonic-tests-enqueue-appends-without-restarting ()
  "`supersonic-mpv-enqueue' appends to the running queue instead of replacing it."
  (supersonic-tests--with-mpv
   (supersonic-mpv-start (list supersonic-tests--track-1))
   (should (supersonic-tests--wait-for (lambda () (= 1 (hash-table-count supersonic--playlist)))))
   (supersonic-mpv-enqueue (list supersonic-tests--track-2))
   (should (supersonic-tests--wait-for (lambda () (= 2 (hash-table-count supersonic--playlist)))))
   (should (equal supersonic-tests--track-1 (gethash 1 supersonic--playlist)))
   (should (equal supersonic-tests--track-2 (gethash 2 supersonic--playlist)))))

(ert-deftest supersonic-tests-start-resumes-if-paused ()
  "`supersonic-mpv-start' resumes playback even if mpv was left paused."
  (supersonic-tests--with-mpv
   (supersonic-mpv-start (list supersonic-tests--track-1))
   (should (supersonic-tests--wait-for (lambda () (= 1 (hash-table-count supersonic--playlist)))))
   (supersonic-toggle-playing)
   (should (supersonic-tests--wait-for (lambda () (eq supersonic--paused t))))
   (supersonic-mpv-start (list supersonic-tests--track-2))
   (should (supersonic-tests--wait-for (lambda () (eq supersonic--paused nil))))))

(ert-deftest supersonic-tests-command-with-callback-round-trips ()
  "`supersonic-mpv-command-with-callback' delivers the matching reply."
  (supersonic-tests--with-mpv
   (supersonic-mpv-start (list supersonic-tests--track-1))
   (should (supersonic-tests--wait-for (lambda () (= 1 (hash-table-count supersonic--playlist)))))
   (let ((result 'pending))
     (supersonic-mpv-command-with-callback (lambda (response) (setq result response)) "get_property" "playlist")
     (should (supersonic-tests--wait-for (lambda () (not (eq result 'pending)))))
     (should (equal "success" (alist-get 'error result)))
     (should (= 1 (length (alist-get 'data result)))))))

(ert-deftest supersonic-tests-command-after-mpv-exit-reports-not-running ()
  "Commands sent right after mpv (`--idle=once') exits on its own fail
gracefully instead of writing to the now-dead IPC socket -- see #15.
`supersonic--mpv-socket''s sentinel is what notices the exit here, so
this also exercises that it actually clears `supersonic-mpv--socket'
rather than leaving it pointing at a dead process."
  (supersonic-tests--with-mpv
   (supersonic-mpv-start (list "av://lavfi:sine=frequency=440:duration=1"))
   (should (supersonic-tests--wait-for (lambda () (= 1 (hash-table-count supersonic--playlist)))))
   (should (supersonic-tests--wait-for (lambda () (not (supersonic-mpv-live-p))) 5))
   (should-not (supersonic-mpv--send "irrelevant"))
   (should-not supersonic-mpv--socket)
   (should-not (supersonic-mpv-command "get_property" "time-pos"))
   (let ((result 'pending))
     (supersonic-mpv-command-with-callback (lambda (response) (setq result response)) "get_property" "time-pos")
     ;; No live socket to ever deliver a reply on, so the callback must
     ;; never have been registered -- otherwise it leaks in
     ;; `supersonic-mpv--pending-requests' forever.
     (should (eq result 'pending))
     (should (= 0 (hash-table-count supersonic-mpv--pending-requests))))))

(ert-deftest supersonic-tests-queue-parse-marks-current-track ()
  "`supersonic-queue-parse' marks whichever entry mpv reports as current."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
     (should (supersonic-tests--wait-for (lambda () (= 2 (hash-table-count supersonic--playlist)))))
     (let ((result 'pending))
       (supersonic-mpv-command-with-callback (lambda (response) (setq result response)) "get_property" "playlist")
       (should (supersonic-tests--wait-for (lambda () (not (eq result 'pending)))))
       (let ((entries (aio-wait-for (supersonic-queue-parse (alist-get 'data result)))))
         (should (equal "▶" (aref (nth 1 (car entries)) 0)))
         (should (equal "" (aref (nth 1 (cadr entries)) 0))))))))

(ert-deftest supersonic-tests-queue-parse-includes-song-metadata ()
  "`supersonic-queue-parse' fills in title, artist and album from the song lookup."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda
               (url)
               `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test Artist") ("album" . "Test Album")))))))
     (supersonic-mpv-start (list supersonic-tests--track-1))
     (should (supersonic-tests--wait-for (lambda () (= 1 (hash-table-count supersonic--playlist)))))
     (let ((result 'pending))
       (supersonic-mpv-command-with-callback (lambda (response) (setq result response)) "get_property" "playlist")
       (should (supersonic-tests--wait-for (lambda () (not (eq result 'pending)))))
       (let ((entry (car (aio-wait-for (supersonic-queue-parse (alist-get 'data result))))))
         (should (equal supersonic-tests--track-1 (aref (nth 1 entry) 1)))
         (should (equal "Test Artist" (aref (nth 1 entry) 2)))
         (should (equal "Test Album" (aref (nth 1 entry) 3))))))))

(ert-deftest supersonic-tests-queue-buffer-follows-track-changes ()
  "An open queue buffer refreshes itself as mpv advances, with no manual refresh."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (let ((buff (get-buffer-create supersonic-queue-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff
               (supersonic-queue-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
             ;; `supersonic-mpv-start' should have populated the buffer already,
             ;; without anyone calling `supersonic-queue-refresh'.
             (should
              (supersonic-tests--wait-for (lambda () (= 2 (length (buffer-local-value 'tabulated-list-entries buff))))))
             (should (equal "▶" (aref (nth 1 (car (buffer-local-value 'tabulated-list-entries buff))) 0)))
             ;; Once mpv auto-advances to track 2 (track 1 is 2s long), the
             ;; buffer should follow along on its own.
             (should
              (supersonic-tests--wait-for (lambda ()
                                            (equal
                                             "▶"
                                             (aref (nth 1 (cadr (buffer-local-value 'tabulated-list-entries buff))) 0)))
                                          6)))
         (kill-buffer buff))))))

(ert-deftest supersonic-tests-queue-buffer-follows-full-replacement ()
  "An open queue buffer reflects a full `supersonic-mpv-start' replacement,
dropping the previous queue's entries rather than appending to them."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (let ((buff (get-buffer-create supersonic-queue-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff
               (supersonic-queue-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
             (should
              (supersonic-tests--wait-for (lambda () (= 2 (length (buffer-local-value 'tabulated-list-entries buff))))))
             ;; Replace the running queue outright with a single, different track.
             (supersonic-mpv-start (list supersonic-tests--track-3))
             (should
              (supersonic-tests--wait-for (lambda () (= 1 (length (buffer-local-value 'tabulated-list-entries buff))))))
             (let ((entry (car (buffer-local-value 'tabulated-list-entries buff))))
               (should (equal supersonic-tests--track-3 (aref (nth 1 entry) 1)))
               (should (equal "▶" (aref (nth 1 entry) 0)))))
         (kill-buffer buff))))))

(ert-deftest supersonic-tests-seeking-does-not-open-the-transient ()
  "Seeking only seeks.  It used to end by calling the `supersonic'
transient, which meant the menu popped up whenever the seek commands
were invoked from anywhere else -- e.g. from the now-playing buffer.
The menu now stays open on its own via the suffixes' `:transient t'."
  (let ((commands nil)
        (opened-transient nil))
    (cl-letf (((symbol-function 'supersonic-mpv-command) (lambda (&rest args) (push args commands)))
              ((symbol-function 'supersonic) (lambda (&rest _) (setq opened-transient t))))
      (supersonic-seek-forward)
      (supersonic-seek-back)
      (should-not opened-transient)
      (should (equal '(("seek" "-30" "relative") ("seek" "30" "relative")) commands)))))

(ert-deftest supersonic-tests-transient-stays-open-while-seeking ()
  "The seek suffixes are marked `:transient t', so the menu survives them
and several seeks in a row can be done without reopening it."
  (dolist (key '("F" "B"))
    (should (plist-get (cdr (transient-get-suffix 'supersonic key)) :transient))))

(ert-deftest supersonic-tests-art-cache-file-is-per-size ()
  "Art cached for one display size does not stand in for another size,
so that the list and now-playing buffers can show the same cover at
their own resolutions -- and so that changing either size setting
actually re-fetches instead of reusing the old resolution forever."
  (let ((supersonic-art-cache-path "/tmp/supersonic-tests-cache"))
    (should-not (equal (supersonic-art-cache-file "art-1" 100) (supersonic-art-cache-file "art-1" 300)))
    (should (equal (supersonic-art-cache-file "art-1" 100) (supersonic-art-cache-file "art-1" 100)))))

(ert-deftest supersonic-tests-alist-to-query-builds-query-string ()
  "`supersonic-alist->query' joins an alist into a leading-`?', `&'-separated
query string, and returns the empty string (not a bare \"?\") for an
empty alist, so `supersonic-build-url' never appends a dangling `?' to
a url that has no extra query parameters."
  (should (equal "" (supersonic-alist->query '())))
  (should (equal "?a=1" (supersonic-alist->query '(("a" . "1")))))
  (should (equal "?a=1&b=2" (supersonic-alist->query '(("a" . "1") ("b" . "2"))))))

(ert-deftest supersonic-tests-fetch-art-creates-cache-directory ()
  "`supersonic--fetch-art' creates the cache directory itself, so callers
that fetch a single image (the now-playing buffer) get art on a fresh
install too, instead of only those that populate a whole list."
  (let ((supersonic-art-cache-path
         (expand-file-name (make-temp-name "supersonic-tests-cache-") temporary-file-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url")))
          (supersonic-tests--with-stubbed-response "cover-art-bytes"
            (aio-wait-for (supersonic--fetch-art "art-1" 300))
            (should (file-exists-p (supersonic-art-cache-file "art-1" 300)))
            (should-not (file-exists-p (supersonic-art-cache-file "art-1" 100)))))
      (when (file-exists-p supersonic-art-cache-path)
        (delete-directory supersonic-art-cache-path t)))))

(ert-deftest supersonic-tests-art-fetches-run-capped-in-parallel ()
  "Cover art fetches run concurrently, but never more of them at a time
than the semaphore `supersonic-get-images' hands them allows -- a list
buffer asks for every row's art at once, and without the cap that is one
open connection per row."
  (let ((supersonic-art-cache-path
         (expand-file-name (make-temp-name "supersonic-tests-cache-") temporary-file-directory))
        (in-flight 0)
        (peak 0)
        (sem (aio-sem 2)))
    (unwind-protect
        (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url"))
                  ((symbol-function 'aio-url-retrieve)
                   (aio-lambda
                    (_url) (cl-incf in-flight) (setq peak (max peak in-flight))
                    ;; Stay "on the wire" long enough for the other fetches
                    ;; to pile up behind the semaphore.
                    (aio-await (aio-sleep 0.05)) (cl-decf in-flight)
                    (let ((buff (generate-new-buffer " *supersonic-tests-response*")))
                      (with-current-buffer buff
                        (insert "HTTP/1.1 200 OK\n\n")
                        (setq-local url-http-end-of-headers (1- (point)))
                        (insert "cover-art-bytes"))
                      (cons nil buff)))))
          (let ((pending
                 (mapcar
                  (lambda (id) (supersonic--fetch-art-throttled sem id 100))
                  '("art-1" "art-2" "art-3" "art-4" "art-5" "art-6"))))
            (dolist (promise pending)
              (aio-wait-for promise)))
          (should (= peak 2))
          ;; Capped, but every entry still got fetched.
          (should (file-exists-p (supersonic-art-cache-file "art-6" 100))))
      (when (file-exists-p supersonic-art-cache-path)
        (delete-directory supersonic-art-cache-path t)))))

(defun supersonic-tests--bytes (values)
  "Return VALUES (a list of 0..255 ints) as a unibyte string."
  (let ((s (make-string (length values) 0 nil))
        (i 0))
    (dolist (v values)
      (aset s i v)
      (setq i (1+ i)))
    s))

(defun supersonic-tests--le32 (n)
  "Return N as a 4-byte little-endian unibyte string."
  (supersonic-tests--bytes
   (list (logand n 255) (logand (ash n -8) 255) (logand (ash n -16) 255) (logand (ash n -24) 255))))

(defun supersonic-tests--wav (samples)
  "Build a minimal mono s16le WAV wrapping SAMPLES (signed 16-bit ints).
Uses a 40-byte extended \"fmt \" chunk, like the WAVE_FORMAT_EXTENSIBLE
one mpv actually writes, rather than the minimal 16-byte PCM one, so
tests against this catch a data-chunk scan that only handles the
minimal case."
  (let ((sample-bytes
         (supersonic-tests--bytes
          (cl-mapcan
           (lambda (s)
             (let ((u (if (< s 0) (+ s 65536) s)))
               (list (logand u 255) (logand (ash u -8) 255))))
           samples))))
    (concat (string-to-unibyte "RIFF") (supersonic-tests--le32 0) (string-to-unibyte "WAVE") (string-to-unibyte "fmt ")
            (supersonic-tests--le32 40) (make-string 40 0 nil) (string-to-unibyte "data")
            (supersonic-tests--le32 (length sample-bytes)) sample-bytes)))

(ert-deftest supersonic-tests-waveform-cache-file-is-per-bucket-count ()
  "Waveform cache is keyed on the bucket count, mirroring
`supersonic-tests-art-cache-file-is-per-size': raising
`supersonic-waveform-buckets' must not resurrect a stale,
wrong-resolution envelope cached before the change."
  (let ((supersonic-waveform-cache-path "/tmp/supersonic-tests-waveform-cache"))
    (should-not (equal (supersonic-waveform-cache-file "id-1" 200) (supersonic-waveform-cache-file "id-1" 300)))
    (should (equal (supersonic-waveform-cache-file "id-1" 200) (supersonic-waveform-cache-file "id-1" 200)))))

(ert-deftest supersonic-tests-waveform-find-data-chunk-skips-extended-fmt-chunk ()
  "`supersonic-waveform--find-data-chunk' finds \"data\" behind a 40-byte
extended \"fmt \" chunk -- what mpv actually writes -- rather than
assuming the minimal 16-byte PCM one."
  (let* ((wav (supersonic-tests--wav '(100 -100 200)))
         (chunk (supersonic-waveform--find-data-chunk wav)))
    (should chunk)
    (should (= 6 (cdr chunk))) ; 3 samples * 2 bytes
    (should (equal (string-to-unibyte "data") (substring wav (- (car chunk) 8) (- (car chunk) 4))))))

(ert-deftest supersonic-tests-waveform-analyze-samples-computes-peak-and-rms ()
  "Peak/RMS are computed independently per bucket and normalized to a
0..255 byte -- full-scale samples in one half of the buffer, silence in
the other."
  (let* ((wav (supersonic-tests--wav (append (make-list 4 32767) (make-list 4 0))))
         (chunk (supersonic-waveform--find-data-chunk wav))
         (envelope (supersonic-waveform--analyze-samples wav (car chunk) (cdr chunk) 2)))
    (should (= 255 (aref (car envelope) 0)))
    (should (= 255 (aref (cdr envelope) 0)))
    (should (= 0 (aref (car envelope) 1)))
    (should (= 0 (aref (cdr envelope) 1)))))

(ert-deftest supersonic-tests-waveform-analyze-file-reads-wav-off-disk ()
  "`supersonic-waveform--analyze-file' works against a real file, not
just an in-memory buffer -- the shape `supersonic-waveform-ensure'
actually calls it in, on whatever mpv wrote to disk."
  (let ((file (make-temp-file "supersonic-tests-wav-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region (supersonic-tests--wav (make-list 8 32767)) nil file nil 'no-message))
          (should (= 255 (aref (car (supersonic-waveform--analyze-file file 1)) 0))))
      (delete-file file))))

(ert-deftest supersonic-tests-waveform-cache-round-trips ()
  "A written envelope reads back byte-identical, and a cache file that
doesn't hold exactly 2*BUCKETS bytes (e.g. left truncated by an
interrupted write) is rejected instead of handed back as if valid."
  (let* ((dir (make-temp-file "supersonic-tests-wf-cache-" t))
         (file (expand-file-name "entry" dir))
         (peaks (supersonic-tests--bytes '(1 2 3)))
         (rms (supersonic-tests--bytes '(4 5 6))))
    (unwind-protect
        (progn
          (supersonic-waveform--write-cache file (cons peaks rms))
          (should (equal (cons peaks rms) (supersonic-waveform--read-cache file 3)))
          (should-not (supersonic-waveform--read-cache file 4)))
      (delete-directory dir t))))

(ert-deftest supersonic-tests-waveform-ensure-reads-existing-cache-without-spawning-mpv ()
  "A cached envelope is served straight off disk without starting a
transcode at all."
  (let ((supersonic-waveform-cache-path (make-temp-file "supersonic-tests-wf-cache-" t))
        (supersonic-waveform-buckets 3))
    (unwind-protect
        (cl-letf (((symbol-function 'supersonic-waveform--start-transcode)
                   (lambda (&rest _) (error "should not be called"))))
          (let ((envelope (cons (supersonic-tests--bytes '(1 2 3)) (supersonic-tests--bytes '(4 5 6)))))
            (supersonic-waveform--write-cache (supersonic-waveform-cache-file "id-1" 3) envelope)
            (let (result)
              (supersonic-waveform-ensure "id-1" (lambda (e) (setq result e)))
              (should (equal envelope result)))))
      (delete-directory supersonic-waveform-cache-path t))))

(ert-deftest supersonic-tests-waveform-ensure-transcodes-and-caches ()
  "A cache miss spawns a disposable mpv to transcode+analyze the track,
then caches the result to disk for next time."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-build-url)
              (lambda (_endpoint _extra-query) "av://lavfi:sine=frequency=440:duration=2")))
     (let ((supersonic-waveform-cache-path (make-temp-file "supersonic-tests-wf-cache-" t))
           (supersonic-waveform-buckets 5)
           (result 'pending))
       (unwind-protect
           (progn
             (supersonic-waveform-ensure "track-1" (lambda (envelope) (setq result envelope)))
             (should (supersonic-tests--wait-for (lambda () (not (eq result 'pending))) 10))
             (should result)
             (should (= 5 (length (car result))))
             (should (file-exists-p (supersonic-waveform-cache-file "track-1" 5))))
         (delete-directory supersonic-waveform-cache-path t))))))

(ert-deftest supersonic-tests-waveform-cancel-kills-in-flight-transcode ()
  "`supersonic-waveform-cancel' kills the transcode process and forgets
its output file, so a quick track change never leaves either behind."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-build-url)
              (lambda (_endpoint _extra-query) "av://lavfi:sine=frequency=440:duration=30")))
     (let ((supersonic-waveform-cache-path (make-temp-file "supersonic-tests-wf-cache-" t)))
       (unwind-protect
           (progn
             (supersonic-waveform-ensure "long-track" #'ignore)
             (should (process-live-p supersonic-waveform--process))
             (let ((outfile supersonic-waveform--outfile))
               (supersonic-waveform-cancel)
               (should-not (process-live-p supersonic-waveform--process))
               (should-not (and outfile (file-exists-p outfile)))))
         (delete-directory supersonic-waveform-cache-path t))))))

(ert-deftest supersonic-tests-waveform-transcode-outfile-avoids-media-extension ()
  "The transcode output file must not use a media-file extension like
\"wav\": a package that intercepts media-file reads via
`file-name-handler-alist' (e.g. ready-player.el, which really does
this) can silently hand back empty content instead of the real bytes
for such a path -- breaking analysis with no error at all, since mpv
itself still succeeds regardless. This bit a real user; guard against
it coming back."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/mpv"))
            ((symbol-function 'supersonic-build-url) (lambda (&rest _) "dummy://url"))
            ((symbol-function 'make-process) (lambda (&rest _) nil)))
    (let ((supersonic-mpv "mpv"))
      (unwind-protect
          (progn
            (supersonic-waveform--start-transcode "id" 10 "cache-file" #'ignore)
            (should-not
             (member (file-name-extension supersonic-waveform--outfile) '("wav" "mp3" "ogg" "flac" "m4a" "opus"))))
        (supersonic-waveform-cancel)))))

(ert-deftest supersonic-tests-waveform-image-produces-well-formed-ppm ()
  "The rendered seekbar is a well-formed PPM: a header plus exactly
WIDTH*HEIGHT*3 bytes of pixel data -- and building it doesn't error even
against the placeholder \"unspecified-fg\"/\"unspecified-bg\" colors a
frameless batch Emacs reports, thanks to `supersonic-waveform--rgb''s
fallback."
  ;; A `--without-x'-style build (e.g. the headless Emacs CI runs tests
  ;; against) has no image support compiled in at all, `pbm' included --
  ;; not something this package can work around, so skip like the mpv
  ;; tests do when their own prerequisite is missing.
  (if (not (image-type-available-p 'pbm))
      (ert-skip "pbm image type not available")
    (let* ((supersonic-waveform-width 12)
           (supersonic-waveform-height 6)
           (peaks (supersonic-tests--bytes (make-list 4 200)))
           (rms (supersonic-tests--bytes (make-list 4 100)))
           (img (supersonic-waveform-image (cons peaks rms) 0.5)))
      (should (eq 'pbm (plist-get (cdr img) :type)))
      (should (= (+ (length "P6\n12 6\n255\n") (* 12 6 3)) (length (plist-get (cdr img) :data)))))))

(ert-deftest supersonic-tests-waveform-seek-at-click-reads-image-via-posn-image ()
  "Clicking the waveform must read the clicked image via `posn-image',
not `car' of `posn-object': for an image, `posn-object' returns the
image spec itself -- a list whose car is the literal symbol `image',
not a (IMAGE . POS) cons -- so `car'ing it yields that bare symbol
instead of the spec, and `image-size' then errors with \"Invalid image
specification\".  This bit a real user: clicking the seekbar did
nothing but log that error."
  (if (not (image-type-available-p 'pbm))
      (ert-skip "pbm image type not available")
    (let* ((peaks (supersonic-tests--bytes '(200 200)))
           (rms (supersonic-tests--bytes '(100 100)))
           (image (supersonic-waveform-image (cons peaks rms) 0))
           ;; Mirrors the position list `event-start' hands back: nth 7 is
           ;; the image (`posn-image'), nth 8 is (DX . DY) relative to it
           ;; (`posn-object-x-y') -- see `posn-image' and
           ;; `posn-object-x-y' in subr.el.
           (posn (list nil nil nil nil nil nil nil image '(4 . 0)))
           (event (list 'down-mouse-1 posn))
           (command nil))
      ;; `image-size' needs a window-system frame even just to measure a
      ;; pixel count, which a batch Emacs never has -- stub it rather than
      ;; skip the whole test, since the point here is `posn-image' plumbing,
      ;; not image measurement.
      (cl-letf (((symbol-function 'supersonic-mpv-command) (lambda (&rest args) (setq command args)))
                ((symbol-function 'image-size) (lambda (spec &optional _pixels _frame) (should (eq spec image)) '(10 . 4))))
        (supersonic-waveform--seek-at-click event)
        (should (equal '("seek" "40.0" "absolute-percent") command))))))

(ert-deftest supersonic-tests-waveform-available-p-requires-enable-and-graphic-frame ()
  "The waveform seekbar needs both the user opt-in and a graphic frame --
the same gating `supersonic-enable-art' has for cover art.  Stubs
`image-type-available-p' too, so this exercises just that gating
regardless of whether the Emacs actually running these tests was built
with image support at all."
  (let ((supersonic-enable-waveform nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _display) t))
              ((symbol-function 'image-type-available-p) (lambda (&optional _type) t)))
      (should-not (supersonic-waveform-available-p))
      (setq supersonic-enable-waveform t)
      (should (supersonic-waveform-available-p)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _display) nil))
              ((symbol-function 'image-type-available-p) (lambda (&optional _type) t)))
      (setq supersonic-enable-waveform t)
      (should-not (supersonic-waveform-available-p)))))

(ert-deftest supersonic-tests-scrobble-does-not-leak-its-response-buffer ()
  "`supersonic-scrobble' kills the buffer `url-retrieve' hands its
callback.  Nothing reads that reply, and nothing else cleans it up, so
without this every scrobbled track would leave a ` *http host:port*'
buffer behind for the rest of the session."
  (let ((supersonic-scrobble-plays t)
        (response nil))
    (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url"))
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq response (generate-new-buffer " *supersonic-tests-response*"))
                 (with-current-buffer response
                   (funcall callback nil)))))
      (supersonic-scrobble "track-1")
      (should response)
      (should-not (buffer-live-p response)))))

(defun supersonic-tests--buffer-matches (buff regexp)
  "Return non-nil if BUFF's contents match REGEXP."
  (with-current-buffer buff
    (string-match-p regexp (buffer-string))))

(ert-deftest supersonic-tests-now-playing-buffer-follows-track-changes ()
  "An open now-playing buffer refreshes itself as mpv advances, with no
manual refresh."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url)))))))
     (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff
               (supersonic-now-playing-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
             (should
              (supersonic-tests--wait-for
               (lambda () (supersonic-tests--buffer-matches buff (regexp-quote supersonic-tests--track-1)))))
             ;; Once mpv auto-advances to track 2 (track 1 is 2s long), the
             ;; buffer should follow along on its own.
             (should
              (supersonic-tests--wait-for (lambda ()
                                            (supersonic-tests--buffer-matches
                                             buff (regexp-quote supersonic-tests--track-2)))
                                          6)))
         (kill-buffer buff))))))

(ert-deftest supersonic-tests-now-playing-position-is-as-short-as-possible ()
  "The position line drops the hours until a track actually runs that long,
and shows position and duration in the same shape."
  (should (equal "00:14 / 05:01" (supersonic-now-playing--position 14 301)))
  (should (equal "00:00 / 05:01" (supersonic-now-playing--position 0 301)))
  ;; mpv reports the position as a float.
  (should (equal "00:14 / 05:01" (supersonic-now-playing--position 14.7 301)))
  (should (equal "0:00:14 / 1:01:01" (supersonic-now-playing--position 14 3661)))
  ;; Either half on its own, for tracks mpv or the server is vague about.
  (should (equal "05:01" (supersonic-now-playing--position nil 301)))
  (should (equal "00:14" (supersonic-now-playing--position 14 nil)))
  (should-not (supersonic-now-playing--position nil nil)))

(ert-deftest supersonic-tests-now-playing-position-advances-on-its-own ()
  "The position ticks along with playback, without a manual refresh and
without re-rendering the buffer, and stops ticking once mpv is gone."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url) ("duration" . 10)))))))
     (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff
               (supersonic-now-playing-mode))
             ;; The tick keeps quiet unless the buffer is on display.
             (set-window-buffer (selected-window) buff)
             (supersonic-mpv-start (list "av://lavfi:sine=frequency=440:duration=10"))
             (should (supersonic-tests--wait-for (lambda () (supersonic-tests--buffer-matches buff "00:00 / 00:10"))))
             (should
              (supersonic-tests--wait-for (lambda () (supersonic-tests--buffer-matches buff "00:0[1-9] / 00:10")) 6))
             (supersonic-mpv-kill)
             (should-not supersonic-now-playing--timer))
         (supersonic-now-playing--stop-timer)
         (kill-buffer buff))))))

(ert-deftest supersonic-tests-now-playing-buffer-follows-pause-toggle ()
  "An open now-playing buffer reflects pausing and resuming, which mpv
reports as a property change rather than as a track event."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url)))))))
     (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff
               (supersonic-now-playing-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1))
             (should
              (supersonic-tests--wait-for
               (lambda () (supersonic-tests--buffer-matches buff (regexp-quote "(playing)")))))
             (supersonic-toggle-playing)
             (should
              (supersonic-tests--wait-for
               (lambda () (supersonic-tests--buffer-matches buff (regexp-quote "(paused)")))))
             (supersonic-toggle-playing)
             (should
              (supersonic-tests--wait-for
               (lambda () (supersonic-tests--buffer-matches buff (regexp-quote "(playing)"))))))
         (kill-buffer buff))))))

(ert-deftest supersonic-tests-now-playing-falls-back-to-track-id ()
  "A failing getSong.view lookup leaves the now-playing buffer showing the
track id instead of claiming that nothing is playing."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json) (aio-lambda (_url) (error "Failed to fetch: connection refused"))))
     (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff
               (supersonic-now-playing-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1))
             (should
              (supersonic-tests--wait-for
               (lambda () (supersonic-tests--buffer-matches buff (regexp-quote supersonic-tests--track-1))))))
         (kill-buffer buff))))))

(defun supersonic-tests--waveform-image-shown-p (buff)
  "Return non-nil if BUFF's waveform field holds a rendered image, not
just the placeholder `supersonic-now-playing--render' inserts for it."
  (with-current-buffer buff
    (save-excursion
      (goto-char (point-min))
      (let ((match (text-property-search-forward 'supersonic-now-playing-field 'waveform t)))
        (and match (get-text-property (prop-match-beginning match) 'display))))))

(ert-deftest supersonic-tests-now-playing-buffer-shows-waveform-once-ready ()
  "The now-playing buffer patches in a waveform seekbar once
`supersonic-waveform-ensure' delivers an envelope for the current
track, without disturbing anything else already rendered."
  (skip-unless (image-type-available-p 'pbm))
  (supersonic-tests--with-mpv
   ;; Play a plain, filesystem-safe track id (unlike the raw `av://...'
   ;; urls `supersonic-tests--with-mpv' otherwise treats as ids), so it
   ;; can double as the waveform cache key below; re-stub `supersonic-build-url'
   ;; to still resolve it to a real playable url for mpv.  Long enough
   ;; that mpv is still around (and its IPC socket still alive) for the
   ;; whole test -- a track ending mid-assertion is exactly what
   ;; `supersonic-tests-now-playing-position-advances-on-its-own' exercises
   ;; on purpose elsewhere, not something this test is about.
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url) ("duration" . 30))))))
             ((symbol-function 'supersonic-build-url)
              (lambda (_endpoint _extra-query) "av://lavfi:sine=frequency=440:duration=30"))
             ((symbol-function 'display-graphic-p) (lambda (&optional _display) t)))
     (let ((supersonic-enable-waveform t)
           (supersonic-waveform-cache-path (make-temp-file "supersonic-tests-wf-cache-" t))
           (supersonic-waveform-buckets 4)
           (buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff
               (supersonic-now-playing-mode))
             ;; Pre-seed the cache so the buffer gets a waveform without
             ;; needing a real transcode.
             (supersonic-waveform--write-cache
              (supersonic-waveform-cache-file "track-1" 4)
              (cons (supersonic-tests--bytes '(10 20 30 40)) (supersonic-tests--bytes '(5 10 15 20))))
             (supersonic-mpv-start (list "track-1"))
             (should (supersonic-tests--wait-for (lambda () (supersonic-tests--waveform-image-shown-p buff)))))
         (kill-buffer buff))))))

(ert-deftest supersonic-tests-refresh-shows-error-on-network-failure ()
  "A refresh function surfaces network/HTTP failures to the user instead
of silently doing nothing, e.g. when `supersonic-host' is misconfigured
with the wrong scheme (https vs. http) and the request fails."
  (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url"))
            ((symbol-function 'supersonic-get-json)
             (aio-lambda (_url) (error "Failed to fetch %s: connection refused" "url"))))
    (let ((buff (get-buffer-create "*supersonic-tests-artists*")))
      (unwind-protect
          (progn
            (aio-wait-for (supersonic-artists-refresh buff))
            (with-current-buffer buff
              (should (string-match-p "Error" (buffer-string)))
              (should (string-match-p "connection refused" (buffer-string)))))
        (kill-buffer buff)))))

(ert-deftest supersonic-tests-signal-if-failed-raises-on-failed-status ()
  "`supersonic--signal-if-failed' raises an `error' carrying the server's
message when a \"subsonic-response\" reports status \"failed\", e.g. the
\"Wrong username or password\" the server sends back for a bad
auth-source entry -- this used to pass through silently as if it were
an ordinary, empty result."
  (should-error
   (supersonic--signal-if-failed
    '(("subsonic-response" ("status" . "failed") ("error" ("code" . 40) ("message" . "Wrong username or password.")))))
   :type 'error)
  (condition-case err
      (supersonic--signal-if-failed
       '(("subsonic-response"
          ("status" . "failed")
          ("error" ("code" . 40) ("message" . "Wrong username or password.")))))
    (error
     (should (string-match-p "Wrong username or password" (error-message-string err)))))
  ;; A successful response must not raise.
  (supersonic--signal-if-failed '(("subsonic-response" ("status" . "ok") ("song" ("id" . "1"))))))

(defmacro supersonic-tests--with-stubbed-response (body-json &rest body)
  "Run BODY with `aio-url-retrieve' stubbed to a 200 OK reply of BODY-JSON.
Mimics the buffer shape (headers, then `url-http-end-of-headers', then
the body) that `supersonic-get-json' expects to parse, so BODY can
exercise it end to end without a real supersonic server."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'aio-url-retrieve)
              (aio-lambda
               (_url)
               (let ((buff (generate-new-buffer " *supersonic-tests-response*")))
                 (with-current-buffer buff
                   (insert "HTTP/1.1 200 OK\n\n")
                   (setq-local url-http-end-of-headers (1- (point)))
                   (insert ,body-json))
                 (cons nil buff)))))
     ,@body))

(ert-deftest supersonic-tests-get-json-raises-on-bad-credentials ()
  "`supersonic-get-json' surfaces a Subsonic-level \"failed\" response --
e.g. the \"Wrong username or password\" a server sends back for a bad
auth-source entry -- as a visible `error' instead of returning it as an
ordinary, empty-looking result."
  (supersonic-tests--with-stubbed-response
      "{\"subsonic-response\":{\"status\":\"failed\",\"error\":{\"code\":40,\"message\":\"Wrong username or password.\"}}}"
    (should-error (aio-wait-for (supersonic-get-json "dummy://url")) :type 'error)))

(ert-deftest supersonic-tests-refresh-shows-error-on-bad-credentials ()
  "A refresh function surfaces a Subsonic-level \"failed\" response (e.g.
wrong username/password from auth-source) to the user instead of
silently rendering an empty list, mirroring
`supersonic-tests-refresh-shows-error-on-network-failure' but for a
failure that arrives inside a 200 OK body rather than as an HTTP/network
error."
  (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url")))
    (supersonic-tests--with-stubbed-response
        "{\"subsonic-response\":{\"status\":\"failed\",\"error\":{\"code\":40,\"message\":\"Wrong username or password.\"}}}"
      (let ((buff (get-buffer-create "*supersonic-tests-artists*")))
        (unwind-protect
            (progn
              (aio-wait-for (supersonic-artists-refresh buff))
              (with-current-buffer buff
                (should (string-match-p "Error" (buffer-string)))
                (should (string-match-p "Wrong username or password" (buffer-string)))))
          (kill-buffer buff))))))

(ert-deftest supersonic-tests-auth-picks-up-host-change-at-runtime ()
  "`supersonic-auth' re-resolves against `auth-source-search' as soon as
`supersonic-host' changes, instead of keeping whatever was looked up
the first time it was called (which used to be frozen in for the rest
of the Emacs session, e.g. via a `defvar' initializer evaluated once
at load time)."
  (let ((supersonic-host "host-a"))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args) (list (list :host (plist-get args :host) :user "u" :secret (lambda () "p"))))))
      (should (equal "host-a" (plist-get (supersonic-auth) :host)))
      (setq supersonic-host "host-b")
      (should (equal "host-b" (plist-get (supersonic-auth) :host))))))

(ert-deftest supersonic-tests-auth-picks-up-corrected-credentials-at-runtime ()
  "`supersonic-auth' re-resolves against `auth-source-search' on every call,
so correcting a wrong username/password in the authinfo entry (and
forgetting `auth-source''s own cache, e.g. via
`auth-source-forget-all-cached') takes effect on the next request --
even though `supersonic-host' itself never changed -- instead of
keeping whatever supersonic itself first memoized for that host."
  (let ((supersonic-host "host-a")
        (user "wrong-user"))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args) (list (list :host (plist-get args :host) :user user :secret (lambda () "p"))))))
      (should (equal "wrong-user" (plist-get (supersonic-auth) :user)))
      (setq user "right-user")
      (should (equal "right-user" (plist-get (supersonic-auth) :user))))))

(ert-deftest supersonic-tests-tracks-parse-follows-browse-by-tags-toggle ()
  "`supersonic-tracks-parse' reads the track list from whichever json path
matches the *current* `supersonic-browse-by-tags' value, staying
consistent with `supersonic-tracks-json' (which picks the endpoint
the same way) rather than parsing at a path cached from whatever
`supersonic-browse-by-tags' was when the package was loaded."
  (let ((tag-response '(("subsonic-response" ("album" ("song" (("title" . "Tag Song") ("id" . "1")))))))
        (folder-response '(("subsonic-response" ("directory" ("child" (("title" . "Folder Song") ("id" . "2"))))))))
    (let ((supersonic-browse-by-tags t))
      (should (equal "Tag Song" (aref (nth 1 (car (supersonic-tracks-parse tag-response))) 0))))
    (let ((supersonic-browse-by-tags nil))
      (should (equal "Folder Song" (aref (nth 1 (car (supersonic-tracks-parse folder-response))) 0))))))

(ert-deftest supersonic-tests-artists-parse-flattens-index-buckets ()
  "`supersonic-artists-parse' flattens every letter bucket of the
\"index\" array into a single list of (id [name]) tabulated-list
entries, in bucket order, covering every bucket rather than just the
first."
  (let ((data
         '(("subsonic-response"
            ("artists"
             ("index"
              (("artist" (("id" . "1") ("name" . "Alice")) (("id" . "2") ("name" . "Bob"))))
              (("artist" (("id" . "3") ("name" . "Carl"))))))))))
    (should
     (equal
      '(("1" ["Alice"]) ("2" ["Bob"]) ("3" ["Carl"]))
      (supersonic-artists-parse data)))))

(ert-deftest supersonic-tests-albums-buffer-becomes-current ()
  "`supersonic-albums' leaves the freshly created album-list buffer as
the current/displayed buffer, instead of popping back to whichever
buffer the command happened to be invoked from.

Regression test for a bug where `set-buffer' (which switches the
current buffer for the rest of the function) was replaced by
`supersonic--init-list-buffer', which only switches buffers via
`with-current-buffer' and unwinds *before* `supersonic-albums'
returns.  `(current-buffer)' at the end of `supersonic-albums' then
picked up the original buffer again, so e.g. `supersonic-random-albums'
silently reopened whatever buffer it was called from instead of the
new albums list."
  (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url"))
            ((symbol-function 'supersonic-get-json) (aio-lambda (_url) nil)))
    (let ((origin (get-buffer-create "*supersonic-tests-origin*")))
      (unwind-protect
          (with-current-buffer origin
            (supersonic-albums nil "random")
            (should (eq (current-buffer) (get-buffer "*supersonic-albums*"))))
        (kill-buffer origin)
        (when (get-buffer "*supersonic-albums*")
          (kill-buffer "*supersonic-albums*"))))))

(ert-deftest supersonic-tests-artist-albums-buffer-becomes-current ()
  "Same regression as `supersonic-tests-albums-buffer-becomes-current',
for the artist-ID branch of `supersonic-albums' (i.e. `supersonic-open-album')."
  (cl-letf (((symbol-function 'supersonic-build-url) (lambda (_endpoint _extra-query) "dummy://url"))
            ((symbol-function 'supersonic-get-json) (aio-lambda (_url) nil)))
    (let ((origin (get-buffer-create "*supersonic-tests-origin*")))
      (unwind-protect
          (with-current-buffer origin
            (supersonic-albums "42" nil)
            (should (eq (current-buffer) (get-buffer "*supersonic-artist-albums*"))))
        (kill-buffer origin)
        (when (get-buffer "*supersonic-artist-albums*")
          (kill-buffer "*supersonic-artist-albums*"))))))

(provide 'supersonic-tests)

;;; supersonic-tests.el ends here
