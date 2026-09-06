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
                (lambda (_endpoint extra-query)
                  (alist-get "id" extra-query nil nil #'equal))))
       (unwind-protect
           (progn ,@body)
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
   (supersonic-mpv-start
    (list supersonic-tests--track-1 supersonic-tests--track-2 supersonic-tests--track-3))
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

(ert-deftest supersonic-tests-command-with-callback-round-trips ()
  "`supersonic-mpv-command-with-callback' delivers the matching reply."
  (supersonic-tests--with-mpv
   (supersonic-mpv-start (list supersonic-tests--track-1))
   (should (supersonic-tests--wait-for (lambda () (= 1 (hash-table-count supersonic--playlist)))))
   (let ((result 'pending))
     (supersonic-mpv-command-with-callback
      (lambda (response) (setq result response))
      "get_property" "playlist")
     (should (supersonic-tests--wait-for (lambda () (not (eq result 'pending)))))
     (should (equal "success" (alist-get 'error result)))
     (should (= 1 (length (alist-get 'data result)))))))

(ert-deftest supersonic-tests-queue-parse-marks-current-track ()
  "`supersonic-queue-parse' marks whichever entry mpv reports as current."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url)
                `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
     (should (supersonic-tests--wait-for (lambda () (= 2 (hash-table-count supersonic--playlist)))))
     (let ((result 'pending))
       (supersonic-mpv-command-with-callback
        (lambda (response) (setq result response))
        "get_property" "playlist")
       (should (supersonic-tests--wait-for (lambda () (not (eq result 'pending)))))
       (let ((entries (aio-wait-for (supersonic-queue-parse (alist-get 'data result)))))
         (should (equal "▶" (aref (nth 1 (car entries)) 0)))
         (should (equal "" (aref (nth 1 (cadr entries)) 0))))))))

(ert-deftest supersonic-tests-queue-parse-includes-song-metadata ()
  "`supersonic-queue-parse' fills in title, artist and album from the song lookup."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url)
                `(("subsonic-response"
                   ("song" ("title" . ,url) ("artist" . "Test Artist") ("album" . "Test Album")))))))
     (supersonic-mpv-start (list supersonic-tests--track-1))
     (should (supersonic-tests--wait-for (lambda () (= 1 (hash-table-count supersonic--playlist)))))
     (let ((result 'pending))
       (supersonic-mpv-command-with-callback
        (lambda (response) (setq result response))
        "get_property" "playlist")
       (should (supersonic-tests--wait-for (lambda () (not (eq result 'pending)))))
       (let ((entry (car (aio-wait-for (supersonic-queue-parse (alist-get 'data result))))))
         (should (equal supersonic-tests--track-1 (aref (nth 1 entry) 1)))
         (should (equal "Test Artist" (aref (nth 1 entry) 2)))
         (should (equal "Test Album" (aref (nth 1 entry) 3))))))))

(ert-deftest supersonic-tests-queue-buffer-follows-track-changes ()
  "An open queue buffer refreshes itself as mpv advances, with no manual refresh."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url)
                `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (let ((buff (get-buffer-create supersonic-queue-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff (supersonic-queue-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
             ;; `supersonic-mpv-start' should have populated the buffer already,
             ;; without anyone calling `supersonic-queue-refresh'.
             (should
              (supersonic-tests--wait-for
               (lambda () (= 2 (length (buffer-local-value 'tabulated-list-entries buff))))))
             (should
              (equal "▶" (aref (nth 1 (car (buffer-local-value 'tabulated-list-entries buff))) 0)))
             ;; Once mpv auto-advances to track 2 (track 1 is 2s long), the
             ;; buffer should follow along on its own.
             (should
              (supersonic-tests--wait-for
               (lambda ()
                 (equal "▶"
                        (aref (nth 1 (cadr (buffer-local-value 'tabulated-list-entries buff))) 0)))
               6)))
         (kill-buffer buff))))))

(ert-deftest supersonic-tests-queue-buffer-follows-full-replacement ()
  "An open queue buffer reflects a full `supersonic-mpv-start' replacement,
dropping the previous queue's entries rather than appending to them."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url)
                `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (let ((buff (get-buffer-create supersonic-queue-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff (supersonic-queue-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
             (should
              (supersonic-tests--wait-for
               (lambda () (= 2 (length (buffer-local-value 'tabulated-list-entries buff))))))
             ;; Replace the running queue outright with a single, different track.
             (supersonic-mpv-start (list supersonic-tests--track-3))
             (should
              (supersonic-tests--wait-for
               (lambda () (= 1 (length (buffer-local-value 'tabulated-list-entries buff))))))
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
    (cl-letf (((symbol-function 'supersonic-mpv-command)
               (lambda (&rest args) (push args commands)))
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
    (should-not
      (equal
        (supersonic-art-cache-file "art-1" 100)
        (supersonic-art-cache-file "art-1" 300)))
    (should
      (equal
        (supersonic-art-cache-file "art-1" 100)
        (supersonic-art-cache-file "art-1" 100)))))

(ert-deftest supersonic-tests-fetch-art-creates-cache-directory ()
  "`supersonic--fetch-art' creates the cache directory itself, so callers
that fetch a single image (the now-playing buffer) get art on a fresh
install too, instead of only those that populate a whole list."
  (let ((supersonic-art-cache-path
          (expand-file-name (make-temp-name "supersonic-tests-cache-") temporary-file-directory)))
    (unwind-protect
        (cl-letf (((symbol-function 'supersonic-build-url)
                   (lambda (_endpoint _extra-query) "dummy://url")))
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
        (cl-letf (((symbol-function 'supersonic-build-url)
                   (lambda (_endpoint _extra-query) "dummy://url"))
                  ((symbol-function 'aio-url-retrieve)
                   (aio-lambda (_url)
                     (cl-incf in-flight)
                     (setq peak (max peak in-flight))
                     ;; Stay "on the wire" long enough for the other fetches
                     ;; to pile up behind the semaphore.
                     (aio-await (aio-sleep 0.05))
                     (cl-decf in-flight)
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

(ert-deftest supersonic-tests-scrobble-does-not-leak-its-response-buffer ()
  "`supersonic-scrobble' kills the buffer `url-retrieve' hands its
callback.  Nothing reads that reply, and nothing else cleans it up, so
without this every scrobbled track would leave a ` *http host:port*'
buffer behind for the rest of the session."
  (let ((supersonic-scrobble-plays t)
        (response nil))
    (cl-letf (((symbol-function 'supersonic-build-url)
               (lambda (_endpoint _extra-query) "dummy://url"))
              ((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (setq response (generate-new-buffer " *supersonic-tests-response*"))
                 (with-current-buffer response (funcall callback nil)))))
      (supersonic-scrobble "track-1")
      (should response)
      (should-not (buffer-live-p response)))))

(defun supersonic-tests--buffer-matches (buff regexp)
  "Return non-nil if BUFF's contents match REGEXP."
  (with-current-buffer buff (string-match-p regexp (buffer-string))))

(ert-deftest supersonic-tests-now-playing-buffer-follows-track-changes ()
  "An open now-playing buffer refreshes itself as mpv advances, with no
manual refresh."
  (supersonic-tests--with-mpv
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (url) `(("subsonic-response" ("song" ("title" . ,url)))))))
     (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff (supersonic-now-playing-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1 supersonic-tests--track-2))
             (should
              (supersonic-tests--wait-for
               (lambda ()
                 (supersonic-tests--buffer-matches
                  buff (regexp-quote supersonic-tests--track-1)))))
             ;; Once mpv auto-advances to track 2 (track 1 is 2s long), the
             ;; buffer should follow along on its own.
             (should
              (supersonic-tests--wait-for
               (lambda ()
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
              (aio-lambda (url)
                `(("subsonic-response" ("song" ("title" . ,url) ("duration" . 10)))))))
     (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff (supersonic-now-playing-mode))
             ;; The tick keeps quiet unless the buffer is on display.
             (set-window-buffer (selected-window) buff)
             (supersonic-mpv-start (list "av://lavfi:sine=frequency=440:duration=10"))
             (should
              (supersonic-tests--wait-for
               (lambda () (supersonic-tests--buffer-matches buff "00:00 / 00:10"))))
             (should
              (supersonic-tests--wait-for
               (lambda () (supersonic-tests--buffer-matches buff "00:0[1-9] / 00:10"))
               6))
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
             (with-current-buffer buff (supersonic-now-playing-mode))
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
   (cl-letf (((symbol-function 'supersonic-get-json)
              (aio-lambda (_url) (error "Failed to fetch: connection refused"))))
     (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff (supersonic-now-playing-mode))
             (supersonic-mpv-start (list supersonic-tests--track-1))
             (should
              (supersonic-tests--wait-for
               (lambda ()
                 (supersonic-tests--buffer-matches
                  buff (regexp-quote supersonic-tests--track-1))))))
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
      '(("subsonic-response"
          ("status" . "failed")
          ("error" ("code" . 40) ("message" . "Wrong username or password.")))))
    :type 'error)
  (condition-case err
    (supersonic--signal-if-failed
      '(("subsonic-response"
          ("status" . "failed")
          ("error" ("code" . 40) ("message" . "Wrong username or password.")))))
    (error
      (should (string-match-p "Wrong username or password" (error-message-string err)))))
  ;; A successful response must not raise.
  (supersonic--signal-if-failed
    '(("subsonic-response" ("status" . "ok") ("song" ("id" . "1"))))))

(defmacro supersonic-tests--with-stubbed-response (body-json &rest body)
  "Run BODY with `aio-url-retrieve' stubbed to a 200 OK reply of BODY-JSON.
Mimics the buffer shape (headers, then `url-http-end-of-headers', then
the body) that `supersonic-get-json' expects to parse, so BODY can
exercise it end to end without a real supersonic server."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'aio-url-retrieve)
              (aio-lambda (_url)
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
    (should-error
      (aio-wait-for (supersonic-get-json "dummy://url"))
      :type 'error)))

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
               (lambda (&rest args)
                 (list (list :host (plist-get args :host) :user "u" :secret (lambda () "p"))))))
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
               (lambda (&rest args)
                 (list (list :host (plist-get args :host) :user user :secret (lambda () "p"))))))
      (should (equal "wrong-user" (plist-get (supersonic-auth) :user)))
      (setq user "right-user")
      (should (equal "right-user" (plist-get (supersonic-auth) :user))))))

(ert-deftest supersonic-tests-tracks-parse-follows-browse-by-tags-toggle ()
  "`supersonic-tracks-parse' reads the track list from whichever json path
matches the *current* `supersonic-browse-by-tags' value, staying
consistent with `supersonic-tracks-json' (which picks the endpoint
the same way) rather than parsing at a path cached from whatever
`supersonic-browse-by-tags' was when the package was loaded."
  (let ((tag-response
          '(("subsonic-response" ("album" ("song" (("title" . "Tag Song") ("id" . "1")))))))
        (folder-response
          '(("subsonic-response" ("directory" ("child" (("title" . "Folder Song") ("id" . "2"))))))))
    (let ((supersonic-browse-by-tags t))
      (should (equal "Tag Song" (aref (nth 1 (car (supersonic-tracks-parse tag-response))) 0))))
    (let ((supersonic-browse-by-tags nil))
      (should (equal "Folder Song" (aref (nth 1 (car (supersonic-tracks-parse folder-response))) 0))))))

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
        (when (get-buffer "*supersonic-albums*") (kill-buffer "*supersonic-albums*"))))))

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
        (when (get-buffer "*supersonic-artist-albums*") (kill-buffer "*supersonic-artist-albums*"))))))

(provide 'supersonic-tests)

;;; supersonic-tests.el ends here
