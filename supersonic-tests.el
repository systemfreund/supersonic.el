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

(ert-deftest supersonic-tests-auth-picks-up-host-change-at-runtime ()
  "`supersonic-auth' re-resolves against `auth-source-search' as soon as
`supersonic-host' changes, instead of keeping whatever was looked up
the first time it was called (which used to be frozen in for the rest
of the Emacs session, e.g. via a `defvar' initializer evaluated once
at load time)."
  (let ((supersonic-host "host-a")
        (supersonic--auth-cache nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (list (list :host (plist-get args :host) :user "u" :secret (lambda () "p"))))))
      (should (equal "host-a" (plist-get (supersonic-auth) :host)))
      (setq supersonic-host "host-b")
      (should (equal "host-b" (plist-get (supersonic-auth) :host))))))

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

(provide 'supersonic-tests)

;;; supersonic-tests.el ends here
