;;; subsonic-tests.el --- Tests for subsonic.el's mpv queueing  -*- lexical-binding: t; -*-

;; This is free software; see subsonic.el for licensing details.

;;; Commentary:

;; Exercises subsonic.el's mpv queueing/scrobbling-id-mapping logic
;; against a real (but headless, synthetic-audio) mpv process, since
;; that is what the package actually talks to; nothing here needs a
;; real subsonic server.  Run with:
;;
;;   make test
;;
;; or directly:
;;
;;   emacs -Q --batch -L . -l ert -l subsonic.el -l subsonic-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Skipped automatically if mpv is not installed.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subsonic)

(defvar subsonic-tests--track-1 "av://lavfi:sine=frequency=440:duration=2")
(defvar subsonic-tests--track-2 "av://lavfi:sine=frequency=660:duration=2")
(defvar subsonic-tests--track-3 "av://lavfi:sine=frequency=880:duration=2")

(defmacro subsonic-tests--with-mpv (&rest body)
  "Run BODY with a real mpv running, treating ids as raw play urls.
Skips the test if mpv is not available.  Always kills mpv afterwards,
even if BODY signals."
  `(if (not (and subsonic-mpv (executable-find subsonic-mpv)))
       (ert-skip "mpv not found")
     (cl-letf (((symbol-function 'subsonic-build-url)
                (lambda (_endpoint extra-query)
                  (alist-get "id" extra-query nil nil #'equal))))
       (unwind-protect
           (progn ,@body)
         (subsonic-mpv-kill)))))

(defun subsonic-tests--wait-for (predicate &optional timeout)
  "Busy-wait until PREDICATE is non-nil or TIMEOUT (default 5s) elapses.
Returns the final value of PREDICATE."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (sleep-for 0.05))
    (funcall predicate)))

(ert-deftest subsonic-tests-start-assigns-sequential-ids ()
  "`subsonic-mpv-start' maps mpv's playlist entry ids 1..n, in order."
  (subsonic-tests--with-mpv
   (subsonic-mpv-start
    (list subsonic-tests--track-1 subsonic-tests--track-2 subsonic-tests--track-3))
   (should (subsonic-tests--wait-for (lambda () (= 3 (hash-table-count subsonic--playlist)))))
   (should (equal subsonic-tests--track-1 (gethash 1 subsonic--playlist)))
   (should (equal subsonic-tests--track-2 (gethash 2 subsonic--playlist)))
   (should (equal subsonic-tests--track-3 (gethash 3 subsonic--playlist)))))

(ert-deftest subsonic-tests-enqueue-appends-without-restarting ()
  "`subsonic-mpv-enqueue' appends to the running queue instead of replacing it."
  (subsonic-tests--with-mpv
   (subsonic-mpv-start (list subsonic-tests--track-1))
   (should (subsonic-tests--wait-for (lambda () (= 1 (hash-table-count subsonic--playlist)))))
   (subsonic-mpv-enqueue (list subsonic-tests--track-2))
   (should (subsonic-tests--wait-for (lambda () (= 2 (hash-table-count subsonic--playlist)))))
   (should (equal subsonic-tests--track-1 (gethash 1 subsonic--playlist)))
   (should (equal subsonic-tests--track-2 (gethash 2 subsonic--playlist)))))

(ert-deftest subsonic-tests-command-with-callback-round-trips ()
  "`subsonic-mpv-command-with-callback' delivers the matching reply."
  (subsonic-tests--with-mpv
   (subsonic-mpv-start (list subsonic-tests--track-1))
   (should (subsonic-tests--wait-for (lambda () (= 1 (hash-table-count subsonic--playlist)))))
   (let ((result 'pending))
     (subsonic-mpv-command-with-callback
      (lambda (response) (setq result response))
      "get_property" "playlist")
     (should (subsonic-tests--wait-for (lambda () (not (eq result 'pending)))))
     (should (equal "success" (alist-get 'error result)))
     (should (= 1 (length (alist-get 'data result)))))))

(ert-deftest subsonic-tests-queue-parse-marks-current-track ()
  "`subsonic-queue-parse' marks whichever entry mpv reports as current."
  (subsonic-tests--with-mpv
   (cl-letf (((symbol-function 'subsonic-get-json)
              (lambda (url)
                `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (subsonic-mpv-start (list subsonic-tests--track-1 subsonic-tests--track-2))
     (should (subsonic-tests--wait-for (lambda () (= 2 (hash-table-count subsonic--playlist)))))
     (let ((result 'pending))
       (subsonic-mpv-command-with-callback
        (lambda (response) (setq result response))
        "get_property" "playlist")
       (should (subsonic-tests--wait-for (lambda () (not (eq result 'pending)))))
       (let ((entries (subsonic-queue-parse (alist-get 'data result))))
         (should (equal "▶" (aref (nth 1 (car entries)) 0)))
         (should (equal "" (aref (nth 1 (cadr entries)) 0))))))))

(ert-deftest subsonic-tests-queue-parse-includes-song-metadata ()
  "`subsonic-queue-parse' fills in title, artist and album from the song lookup."
  (subsonic-tests--with-mpv
   (cl-letf (((symbol-function 'subsonic-get-json)
              (lambda (url)
                `(("subsonic-response"
                   ("song" ("title" . ,url) ("artist" . "Test Artist") ("album" . "Test Album")))))))
     (subsonic-mpv-start (list subsonic-tests--track-1))
     (should (subsonic-tests--wait-for (lambda () (= 1 (hash-table-count subsonic--playlist)))))
     (let ((result 'pending))
       (subsonic-mpv-command-with-callback
        (lambda (response) (setq result response))
        "get_property" "playlist")
       (should (subsonic-tests--wait-for (lambda () (not (eq result 'pending)))))
       (let ((entry (car (subsonic-queue-parse (alist-get 'data result)))))
         (should (equal subsonic-tests--track-1 (aref (nth 1 entry) 1)))
         (should (equal "Test Artist" (aref (nth 1 entry) 2)))
         (should (equal "Test Album" (aref (nth 1 entry) 3))))))))

(ert-deftest subsonic-tests-queue-buffer-follows-track-changes ()
  "An open queue buffer refreshes itself as mpv advances, with no manual refresh."
  (subsonic-tests--with-mpv
   (cl-letf (((symbol-function 'subsonic-get-json)
              (lambda (url)
                `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (let ((buff (get-buffer-create subsonic-queue-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff (subsonic-queue-mode))
             (subsonic-mpv-start (list subsonic-tests--track-1 subsonic-tests--track-2))
             ;; `subsonic-mpv-start' should have populated the buffer already,
             ;; without anyone calling `subsonic-queue-refresh'.
             (should
              (subsonic-tests--wait-for
               (lambda () (= 2 (length (buffer-local-value 'tabulated-list-entries buff))))))
             (should
              (equal "▶" (aref (nth 1 (car (buffer-local-value 'tabulated-list-entries buff))) 0)))
             ;; Once mpv auto-advances to track 2 (track 1 is 2s long), the
             ;; buffer should follow along on its own.
             (should
              (subsonic-tests--wait-for
               (lambda ()
                 (equal "▶"
                        (aref (nth 1 (cadr (buffer-local-value 'tabulated-list-entries buff))) 0)))
               6)))
         (kill-buffer buff))))))

(ert-deftest subsonic-tests-queue-buffer-follows-full-replacement ()
  "An open queue buffer reflects a full `subsonic-mpv-start' replacement,
dropping the previous queue's entries rather than appending to them."
  (subsonic-tests--with-mpv
   (cl-letf (((symbol-function 'subsonic-get-json)
              (lambda (url)
                `(("subsonic-response" ("song" ("title" . ,url) ("artist" . "Test")))))))
     (let ((buff (get-buffer-create subsonic-queue-buffer-name)))
       (unwind-protect
           (progn
             (with-current-buffer buff (subsonic-queue-mode))
             (subsonic-mpv-start (list subsonic-tests--track-1 subsonic-tests--track-2))
             (should
              (subsonic-tests--wait-for
               (lambda () (= 2 (length (buffer-local-value 'tabulated-list-entries buff))))))
             ;; Replace the running queue outright with a single, different track.
             (subsonic-mpv-start (list subsonic-tests--track-3))
             (should
              (subsonic-tests--wait-for
               (lambda () (= 1 (length (buffer-local-value 'tabulated-list-entries buff))))))
             (let ((entry (car (buffer-local-value 'tabulated-list-entries buff))))
               (should (equal subsonic-tests--track-3 (aref (nth 1 entry) 1)))
               (should (equal "▶" (aref (nth 1 entry) 0)))))
         (kill-buffer buff))))))

(provide 'subsonic-tests)

;;; subsonic-tests.el ends here
