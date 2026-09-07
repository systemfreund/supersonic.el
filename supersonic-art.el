;;; supersonic-art.el --- Cover art cache layer for supersonic.el -*- lexical-binding: t; -*-

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

;; Cover art caching for supersonic.el: naming/locating the on-disk
;; cache, fetching art from the Subsonic server with bounded
;; concurrency, and painting it into a tabulated-list buffer column.

;;; Code:
(require 'url)
(require 'aio)
(require 'supersonic-api)

;; fix byte-compiler complaints
(defvar supersonic-enable-art)
(defvar supersonic-art-cache-path)
(defvar supersonic-list-art-size)
(defvar supersonic-art-fetch-concurrency)
(defvar url-http-end-of-headers)

(defun supersonic-art-cache-file (id size)
  "Return the path cover art ID is cached under when fetched at SIZE.
The size is part of the file name because the same art is shown at
different sizes in different buffers (see `supersonic-list-art-size'
and `supersonic-now-playing-art-size'): sharing one file per art id
would hand whichever buffer asked second the other one's resolution,
and would silently keep serving the old resolution after either
setting is changed."
  (expand-file-name (format "%s-%d" id size) supersonic-art-cache-path))

(defun supersonic-image-propertize (id size)
  "Generate a property displaying cover art ID at SIZE pixels high."
  (propertize " " 'display (create-image (supersonic-art-cache-file id size) nil nil :height size)))

(aio-defun
 supersonic--fetch-art (id size)
 "Ensure cover art ID is cached on disk at SIZE, fetching it if necessary.
Returns a promise that resolves once the fetch has settled; callers
should re-check `file-exists-p' afterwards rather than assume success,
since a failed fetch resolves without signalling here."
 (unless (file-exists-p (supersonic-art-cache-file id size))
   (unless (file-exists-p supersonic-art-cache-path)
     (mkdir supersonic-art-cache-path))
   (pcase-let ((`(,status . ,buffer)
                (aio-await
                 (aio-url-retrieve
                  (supersonic-build-url "/getCoverArt.view" `(("id" . ,id) ("size" . ,(int-to-string size))))))))
     (unwind-protect
         (unless (plist-get status :error)
           (with-current-buffer buffer
             ;; Cover art is arbitrary binary image data, not text -- write the bytes as-is instead of letting Emacs
             ;; guess (and possibly prompt for) a coding system.
             (let ((coding-system-for-write 'no-conversion))
               (write-region (1+ url-http-end-of-headers) (point-max) (supersonic-art-cache-file id size)
                             nil
                             'no-message))))
       (kill-buffer buffer)))))

(aio-defun
 supersonic--fetch-art-throttled (sem id size)
 "Fetch cover art ID at SIZE once SEM hands out a slot.
Callers create every fetch up front, so the semaphore -- not the number
of entries -- is what decides how many requests are on the wire at once
(see `supersonic-art-fetch-concurrency').  Failures are swallowed here
rather than left to reject, so that a slot is always given back and one
unreachable cover doesn't hold up the rest."
 (aio-await (aio-sem-wait sem)) (aio-await (aio-catch (supersonic--fetch-art id size))) (aio-sem-post sem))

(aio-defun
 supersonic-get-images (entries n buff)
 "Fetch/cache cover art for ENTRIES and paint it into column N of BUFF.
Fetches are fired up front, before anything is awaited, but only
`supersonic-art-fetch-concurrency' of them run at a time; individual
failures are tolerated, leaving those entries without art rather than
aborting the rest.  BUFF is (re)printed once every fetch has settled,
so callers don't need to print again themselves."
 (if (or (not supersonic-enable-art) (not (display-graphic-p)))
     (dolist (entry entries)
       (aset (nth 1 entry) n ""))
   (let* ((sem (aio-sem supersonic-art-fetch-concurrency))
          (pending
           (mapcar
            (lambda (entry)
              (cons entry (supersonic--fetch-art-throttled sem (car entry) supersonic-list-art-size)))
            entries)))
     (dolist (item pending)
       (aio-await (cdr item))
       (let ((entry (car item)))
         (when (file-exists-p (supersonic-art-cache-file (car entry) supersonic-list-art-size))
           (aset (nth 1 entry) n (supersonic-image-propertize (car entry) supersonic-list-art-size)))))))
 (when (buffer-live-p buff)
   (with-current-buffer buff
     (when (derived-mode-p 'tabulated-list-mode)
       (tabulated-list-print t)))))

(provide 'supersonic-art)
;;; supersonic-art.el ends here
