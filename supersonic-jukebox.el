;;; supersonic-jukebox.el --- Subsonic jukeboxControl playback backend -*- lexical-binding: t; -*-

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

;; Playback backend that plays on the Subsonic server's jukebox, out
;; of the server's speakers.  Set `supersonic-playback-backend' to
;; `jukebox'; this file is loaded on first use.
;;
;; The jukebox is polled, see `supersonic-poller.el'.  Servers don't
;; scrobble jukebox playback, so track changes are scrobbled here.

;;; Code:
(require 'seq)
(require 'aio)
(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-provider)
(require 'supersonic-playback)
(require 'supersonic-poller)

;;;
;;; Talking to jukeboxControl
;;;

(defun supersonic-jukebox--url (action &optional extra-query)
  "Return the jukeboxControl URL for ACTION with EXTRA-QUERY."
  (supersonic-build-url "/jukeboxControl.view" (cons (cons "action" action) extra-query)))

(aio-defun supersonic-jukebox--request (action &optional extra-query)
  "Return a promise resolving to the response to jukeboxControl ACTION.
EXTRA-QUERY is an alist of further query parameters."
  (aio-await (supersonic-get-json (supersonic-jukebox--url action extra-query))))

(defun supersonic-jukebox--id-query (ids)
  "Return a query alist with an \"id\" parameter for each of IDS."
  (mapcar (lambda (id) (cons "id" id)) ids))

;;;
;;; The cached snapshot
;;;

(defun supersonic-jukebox--parse-snapshot (playlist polled-at)
  "Return the snapshot plist for PLAYLIST, received at POLLED-AT.
PLAYLIST is a parsed jukeboxPlaylist.  See `supersonic-jukebox--poller'."
  (let* ((entries (assoc-default "entry" playlist))
         (current-index (truncate (or (assoc-default "currentIndex" playlist) -1))))
    (list
     :entries (mapcar (lambda (entry) (assoc-default "id" entry)) entries)
     :current-index current-index
     :playing (eq (assoc-default "playing" playlist) t)
     :position (assoc-default "position" playlist)
     :duration (and (>= current-index 0) (assoc-default "duration" (nth current-index entries)))
     :polled-at polled-at)))

(defun supersonic-jukebox--current-track (snapshot)
  "Return the id of SNAPSHOT's current track, or nil if there is none."
  (let ((index (plist-get snapshot :current-index)))
    (and index (>= index 0) (nth index (plist-get snapshot :entries)))))

(defun supersonic-jukebox--summarize (snapshot)
  "Return the summary of SNAPSHOT that `supersonic-poller-announce' compares."
  (list :track-id (supersonic-jukebox--current-track snapshot)
        :paused (not (plist-get snapshot :playing))
        :entries (plist-get snapshot :entries)))

(aio-defun supersonic-jukebox--fetch ()
  "Return a promise resolving to a snapshot of the jukebox's state."
  (let* ((response (aio-await (supersonic-jukebox--request "get")))
         (polled-at (float-time))
         (playlist (supersonic-recursive-assoc response '("subsonic-response" "jukeboxPlaylist"))))
    (supersonic-jukebox--parse-snapshot playlist polled-at)))

(defun supersonic-jukebox--make-poller ()
  "Return a poller for the jukebox that has not polled yet."
  (supersonic-poller-create
   :backend 'jukebox
   :interval 'supersonic-jukebox-poll-interval
   :fetch #'supersonic-jukebox--fetch
   :summarize #'supersonic-jukebox--summarize
   :description "poll the jukebox"))

(defvar supersonic-jukebox--poller (supersonic-jukebox--make-poller)
  "The poller of the jukebox.
Its snapshot is a plist of `:entries', the track ids of the playlist;
`:current-index', the index of the current entry or -1; `:playing';
`:position' in seconds as of `:polled-at'; and `:duration' of the
current track, or nil.")

(defun supersonic-jukebox--snapshot ()
  "Return the jukebox's latest snapshot, or nil."
  (supersonic-poller-snapshot supersonic-jukebox--poller))

(defun supersonic-jukebox--position ()
  "Return the jukebox's estimated current position."
  (let ((snapshot (supersonic-jukebox--snapshot)))
    (supersonic-poller-position snapshot (plist-get snapshot :playing))))

(defun supersonic-jukebox--poll ()
  "Return a promise of polling the jukebox."
  (supersonic-poller-poll supersonic-jukebox--poller))

(defun supersonic-jukebox-live-p ()
  "Return non-nil if the jukebox backend's last poll succeeded."
  (supersonic-poller-live supersonic-jukebox--poller))

;;;
;;; The facade's status/queue/live-p operations
;;;

(aio-defun supersonic-jukebox-status (key)
  "Return a promise resolving to the jukebox's current KEY.
The value comes from the latest poll."
  (pcase key
    ('track-id (supersonic-jukebox--current-track (supersonic-jukebox--snapshot)))
    ('position (supersonic-jukebox--position))
    ('paused (not (plist-get (supersonic-jukebox--snapshot) :playing)))))

(aio-defun supersonic-jukebox-queue ()
  "Return a promise resolving to the jukebox's playlist.
The value comes from the latest poll."
  (let ((current-index (plist-get (supersonic-jukebox--snapshot) :current-index)))
    (seq-map-indexed
     (lambda (id index) (list :track-id id :current (eql index current-index)))
     (plist-get (supersonic-jukebox--snapshot) :entries))))

;;;
;;; The facade's start/enqueue/toggle-play/next operations
;;;

;; Nobody awaits these, so each reports its own errors.

(aio-defun supersonic-jukebox--start (ids)
  "Replace the jukebox playlist with IDS and start playing."
  (supersonic--with-async-error-handling nil "start jukebox playback"
    (aio-await (supersonic-jukebox--request "set" (supersonic-jukebox--id-query ids)))
    (aio-await (supersonic-jukebox--request "start"))
    (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-start (ids)
  "Replace the jukebox playlist with IDS and start playing immediately."
  (ignore (supersonic-jukebox--start ids)))

(aio-defun supersonic-jukebox--enqueue (ids)
  "Append IDS to the jukebox playlist.
Start playing them if the playlist was empty or played to its end."
  (supersonic--with-async-error-handling nil "enqueue on the jukebox"
    (aio-await (supersonic-jukebox--poll))
    (let* ((snapshot (supersonic-jukebox--snapshot))
           (length (length (plist-get snapshot :entries)))
           (finished (and (not (plist-get snapshot :playing))
                          (eql (plist-get snapshot :current-index) (1- length))
                          (eql (plist-get snapshot :position) 0))))
      (aio-await (supersonic-jukebox--request "add" (supersonic-jukebox--id-query ids)))
      (cond
       ((zerop length)
        (aio-await (supersonic-jukebox--request "start")))
       (finished
        (aio-await (supersonic-jukebox--request "skip" `(("index" . ,(number-to-string length)))))
        (aio-await (supersonic-jukebox--request "start")))))
    (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-enqueue (ids)
  "Append IDS to the jukebox playlist, playing them if nothing is left to play."
  (ignore (supersonic-jukebox--enqueue ids)))

(aio-defun supersonic-jukebox--toggle-play ()
  "Start or stop the jukebox."
  (supersonic--with-async-error-handling nil "toggle jukebox playback"
    (aio-await
     (supersonic-jukebox--request
      (if (plist-get (supersonic-jukebox--snapshot) :playing)
          "stop"
        "start")))
    (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-toggle-play ()
  "Toggle between playing and paused on the jukebox."
  (ignore (supersonic-jukebox--toggle-play)))

(aio-defun supersonic-jukebox--next ()
  "Skip the jukebox to the next entry, if there is one."
  (supersonic--with-async-error-handling nil "skip to the next jukebox track"
    (let* ((snapshot (supersonic-jukebox--snapshot))
           (index (1+ (or (plist-get snapshot :current-index) -1)))
           (length (length (plist-get snapshot :entries))))
      (when (< index length)
        (aio-await (supersonic-jukebox--request "skip" `(("index" . ,(number-to-string index)))))))
    (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-next ()
  "Skip to the next track in the jukebox playlist."
  (ignore (supersonic-jukebox--next)))

(aio-defun supersonic-jukebox--prev ()
  "Skip the jukebox to the previous entry, if there is one."
  (supersonic--with-async-error-handling nil "skip to the previous jukebox track"
    (let ((index (1- (or (plist-get (supersonic-jukebox--snapshot) :current-index) -1))))
      (when (>= index 0)
        (aio-await (supersonic-jukebox--request "skip" `(("index" . ,(number-to-string index)))))))
    (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-prev ()
  "Go to the previous track in the jukebox playlist."
  (ignore (supersonic-jukebox--prev)))

(aio-defun supersonic-jukebox--seek (offset)
  "Seek OFFSET seconds from the jukebox's current position."
  (supersonic--with-async-error-handling nil "seek the jukebox"
    (let* ((index (or (plist-get (supersonic-jukebox--snapshot) :current-index) -1))
           (position (or (supersonic-jukebox--position) 0))
           (target (max 0 (round (+ position offset)))))
      (aio-await
       (supersonic-jukebox--request
        "skip" `(("index" . ,(number-to-string index)) ("offset" . ,(number-to-string target))))))
    (aio-await (supersonic-poller-finish-seek supersonic-jukebox--poller))))

(defun supersonic-jukebox-seek (offset)
  "Seek OFFSET seconds relative to the current position on the jukebox."
  (ignore (supersonic-jukebox--seek offset)))

(aio-defun supersonic-jukebox--seek-fraction (fraction)
  "Seek the jukebox to FRACTION, between 0.0 and 1.0, of the current track."
  (supersonic--with-async-error-handling nil "seek the jukebox"
    (let* ((snapshot (supersonic-jukebox--snapshot))
           (index (or (plist-get snapshot :current-index) -1))
           (duration (or (plist-get snapshot :duration) 0))
           (target (round (* fraction duration))))
      (aio-await
       (supersonic-jukebox--request
        "skip" `(("index" . ,(number-to-string index)) ("offset" . ,(number-to-string target))))))
    (aio-await (supersonic-poller-finish-seek supersonic-jukebox--poller))))

(defun supersonic-jukebox-seek-fraction (fraction)
  "Seek the jukebox to FRACTION, between 0.0 and 1.0, of the current track."
  (ignore (supersonic-jukebox--seek-fraction fraction)))

(aio-defun supersonic-jukebox--stop ()
  "Stop the jukebox."
  (supersonic--with-async-error-handling nil "stop the jukebox"
    (aio-await (supersonic-jukebox--request "stop"))
    (aio-await (supersonic-jukebox--poll))))

(defun supersonic-jukebox-stop ()
  "Stop playback on the jukebox."
  (ignore (supersonic-jukebox--stop)))

(supersonic-poller-register 'supersonic-jukebox--poller)

(supersonic-playback-register-backend
 'jukebox
 '((start . supersonic-jukebox-start)
   (enqueue . supersonic-jukebox-enqueue)
   (toggle-play . supersonic-jukebox-toggle-play)
   (next . supersonic-jukebox-next)
   (prev . supersonic-jukebox-prev)
   (stop . supersonic-jukebox-stop)
   (seek . supersonic-jukebox-seek)
   (seek-fraction . supersonic-jukebox-seek-fraction)
   (live-p . supersonic-jukebox-live-p)
   (status . supersonic-jukebox-status)
   (queue . supersonic-jukebox-queue))
 :providers '(subsonic))

(provide 'supersonic-jukebox)

;;; supersonic-jukebox.el ends here
