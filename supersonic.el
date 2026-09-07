;;; supersonic.el --- Browse and play music from subsonic servers with mpv -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
;; URL: https://github.com/systemfreund/supersonic.el
;; Version: 0.2.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "27.1") (transient "0.2") (aio "1.0"))

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

;; This package is meant to act as a simple subsonic frontend that
;; uses mpv for playing the actual music.
;;
;; mpv is driven over its JSON IPC protocol through a Unix-domain
;; socket (`--input-ipc-server'), so this only works on platforms that
;; support those -- i.e. not native Windows, where mpv's IPC transport
;; is a named pipe with a different naming scheme instead.

;;; Code:
(require 'json)
(require 'url)
(require 'seq)
(require 'aio)

(require 'transient)

(defgroup supersonic nil
  "Customization group for mpv."
  :prefix "supersonic-"
  :group 'external)

(defcustom supersonic-host ""
  "URL of the supersonic service, e.g. \"http://host:4533\".
Must match the \"machine\" field of the corresponding authinfo
entry verbatim; that entry's host is what is actually used to
build request URLs.  May be given without a scheme (\"http://\" or
\"https://\"), in which case \"https://\" is assumed."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-mpv (executable-find "mpv")
  "Path to the mpv executable."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-default-volume 100
  "Default  volume for mpv to use."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-cache-path (expand-file-name "supersonic-cache" user-emacs-directory)
  "Path to store cached cover art and waveform peak/RMS envelopes.
Shared by both `supersonic-art-cache-file' and
`supersonic-waveform-cache-file', which prefix their file names
distinctly enough (\"art-\"/\"waveform-\") that the two never collide,
even though both are keyed on a Subsonic id that could otherwise
coincide (e.g. a track and its own cover art id)."
  :type 'string
  :group 'supersonic)

(defcustom supersonic-enable-art nil
  "Enable displaying album art in supported frames."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-list-art-size 100
  "Height in pixels of the cover art in the album and podcast lists.
The art is also downloaded at this size, so raising it costs a
re-download of anything already cached at the old size."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-now-playing-art-size 300
  "Height in pixels of the cover art in the now-playing buffer.
The art is also downloaded at this size, so raising it costs a
re-download of anything already cached at the old size."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-art-fetch-concurrency 6
  "How many cover art downloads may be in flight at the same time.
A list buffer asks for every row's art at once; without a cap that
opens one connection per row, so a 50 album list hits the server with
50 simultaneous requests and leaves 50 idle keep-alive connections in
`url-http-open-connections' afterwards.  Six is what browsers and
`url-queue-parallel-processes' settle on per host."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-enable-waveform nil
  "Enable a clickable waveform seekbar in the now-playing buffer.
Requires a graphic frame and the `pbm' image type (see
`supersonic-waveform-available-p'), the same restriction
`supersonic-enable-art' has.  Generating a waveform means transcoding
the whole track through a disposable mpv process, so the first time a
track is shown its seekbar appears a beat after the rest of the
buffer; it is cached on disk afterwards (see `supersonic-cache-path')."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-waveform-buckets 300
  "Number of peak/RMS samples computed across a track.
Also the horizontal resolution of the rendered seekbar.  The cache is
keyed on this value (see `supersonic-waveform-cache-file'), so raising
it costs a re-analysis of anything already cached, not just a redraw."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-waveform-width 500
  "Width in pixels of the rendered waveform seekbar."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-waveform-height 48
  "Height in pixels of the rendered waveform seekbar."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-now-playing-interval 1
  "Seconds between playback position updates in the now-playing buffer.
Each update is a single query to the local mpv socket, and only runs
while that buffer is both open and on display."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-album-list-count 50
  "Number of albums to display in random/newest albums etc."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-browse-by-tags t
  "Browse by folder or by idv3 tags."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-scrobble-plays nil
  "Request that the supersonic server scrobble played tracks."
  :type 'boolean
  :group 'supersonic)

(defcustom supersonic-mpv-timeout 0.5
  "How long to wait when starting or killing the mpv process."
  :type 'float
  :group 'supersonic)

(require 'supersonic-api)
(require 'supersonic-art)
(require 'supersonic-mpv)
(require 'supersonic-waveform)

;;;
;;; Queue
;;;

(defconst supersonic-queue-buffer-name "*supersonic-queue*"
  "Name of the buffer used by `supersonic-show-queue'.")

(defun supersonic-queue-buffer ()
  "Return the play queue buffer if it is currently live, else nil."
  (let ((buff (get-buffer supersonic-queue-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-queue-maybe-refresh ()
  "Refresh the play queue buffer from mpv's playlist, if it is open.
Called whenever the queue is likely to have changed: after
starting/enqueueing tracks and whenever mpv reports a track
starting or ending."
  (let ((buff (supersonic-queue-buffer)))
    (when buff
      (supersonic-queue-fetch-and-render buff))))

(aio-defun
 supersonic-queue-parse (playlist)
 "Turn mpv's PLAYLIST (from a \"get_property playlist\" reply) into
tabulated-list entries.
Fetches each entry's song metadata concurrently (fired up front, below,
before anything is awaited) and tolerates individual lookup failures
via `aio-catch', falling back to the \"?\" placeholder row instead of
aborting the whole render."
 (let* ((pending
         (mapcar
          (lambda (entry)
            (let* ((mpv-id (alist-get 'id entry))
                   (track-id (gethash mpv-id supersonic--playlist)))
              (list
               entry mpv-id track-id
               (and track-id
                    (aio-catch (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,track-id)))))))))
          playlist)))
   ;; A plain `mapcar' lambda would call `aio-await' through an ordinary `funcall', outside of this function's own
   ;; generator machinery, which `generator.el' cannot transform -- so this collects results via a `dolist', which
   ;; (like `while') stays inline and awaits correctly.
   (let (rows)
     (dolist (item pending)
       (pcase-let ((`(,entry ,mpv-id ,track-id ,promise) item))
         (let* ((outcome (and promise (aio-await promise)))
                (song
                 (and outcome
                      (eq (car outcome) :success)
                      (supersonic-recursive-assoc (cdr outcome) '("subsonic-response" "song")))))
           (push (list
                  (or track-id (format "%s" mpv-id))
                  (vector
                   (if (alist-get 'current entry)
                       "▶"
                     "")
                   (if song
                       (assoc-default "title" song)
                     "?")
                   (if song
                       (or (assoc-default "artist" song) "")
                     "")
                   (if song
                       (or (assoc-default "album" song) "")
                     "")))
                 rows))))
     (nreverse rows))))

(defun supersonic-queue-fetch-and-render (buff)
  "Query mpv for its current playlist and render it into BUFF."
  (if (supersonic-mpv-live-p)
      (supersonic-mpv-command-with-callback
       (aio-lambda
        (response)
        (when (buffer-live-p buff)
          (let ((entries (aio-await (supersonic-queue-parse (alist-get 'data response)))))
            (when (buffer-live-p buff)
              (with-current-buffer buff
                (setq tabulated-list-entries entries)
                (tabulated-list-print t))))))
       "get_property" "playlist")
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries nil)
        (tabulated-list-print t)))))

(defun supersonic-queue-refresh ()
  "Refresh the play queue buffer from mpv's current playlist."
  (interactive)
  (supersonic-queue-fetch-and-render (current-buffer)))

(defvar supersonic-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-queue-refresh)
    map))

(define-derived-mode
 supersonic-queue-mode
 tabulated-list-mode
 "Supersonic Queue"
 (setq tabulated-list-format [("" 2 nil) ("Title" 40 t) ("Artist" 25 t) ("Album" 25 t)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

;;;###autoload
(defun supersonic-show-queue ()
  "Open a buffer showing mpv's current play queue."
  (interactive)
  (let ((buff (get-buffer-create supersonic-queue-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-queue-mode)
        (supersonic-queue-mode)))
    (supersonic-queue-fetch-and-render buff)
    (pop-to-buffer-same-window buff)))

;;;
;;; Now playing
;;;

(defconst supersonic-now-playing-buffer-name "*supersonic-now-playing*"
  "Name of the buffer used by `supersonic-show-now-playing'.")

(defvar supersonic-now-playing--timer nil
  "Timer ticking the playback position shown in the now-playing buffer.")

(defvar-local supersonic-now-playing--duration nil
  "Duration in seconds of the track the now-playing buffer is showing.
Kept around so the position can be re-rendered on its own tick, without
another round of metadata lookups just to learn what to count towards.")

(defvar-local supersonic-now-playing--track-id nil
  "Id of the track the now-playing buffer is currently showing, or nil.
Set by `supersonic-now-playing--render'; consulted by
`supersonic-now-playing--maybe-fetch-waveform' to discard a
`supersonic-waveform-ensure' callback that arrives after the buffer
has already moved on to a different track (a full-track transcode can
take a few seconds, plenty of time for that to happen).")

(defvar-local supersonic-now-playing--waveform nil
  "(TRACK-ID . ENVELOPE) for the waveform seekbar currently shown, or nil.
ENVELOPE lets `supersonic-now-playing--tick' recolor the seekbar's
played/unplayed split every second without asking
`supersonic-waveform-ensure' again.")

(defvar-local supersonic-now-playing--waveform-requested nil
  "Non-nil once a waveform fetch has actually been kicked off for the
current track, successful or not.
`supersonic-now-playing--maybe-fetch-waveform' skips generating a
waveform for a buffer nobody is looking at -- real CPU and network
work otherwise wasted on nothing -- so this is what
`supersonic-now-playing--tick' checks to know whether it still owes
the current track a first attempt once the buffer becomes visible,
without retrying one that already ran (and maybe failed).")

(defun supersonic-now-playing-buffer ()
  "Return the now-playing buffer if it is currently live, else nil."
  (let ((buff (get-buffer supersonic-now-playing-buffer-name)))
    (and buff (buffer-live-p buff) buff)))

(defun supersonic-now-playing--start-timer ()
  "Start ticking the playback position, unless that is already happening."
  (unless supersonic-now-playing--timer
    (setq supersonic-now-playing--timer
          (run-at-time
           supersonic-now-playing-interval supersonic-now-playing-interval #'supersonic-now-playing--tick))))

(defun supersonic-now-playing--stop-timer ()
  "Stop ticking the playback position."
  (when supersonic-now-playing--timer
    (cancel-timer supersonic-now-playing--timer)
    (setq supersonic-now-playing--timer nil)))

(defun supersonic-now-playing--tick ()
  "Update the playback position in the now-playing buffer.
Asks mpv where it is rather than counting seconds locally, so seeking
and pausing need no special handling here.  Stops itself once there is
nothing left to update, and keeps quiet while the buffer is not on
display.  Also picks up a waveform fetch
`supersonic-now-playing--maybe-fetch-waveform' skipped earlier for
exactly that reason, the first tick after the buffer becomes visible
again (see `supersonic-now-playing--waveform-requested')."
  (let ((buff (supersonic-now-playing-buffer)))
    (cond
     ((or (not buff) (not (supersonic-mpv-live-p)))
      (supersonic-now-playing--stop-timer))
     ((not (get-buffer-window buff t)))
     (t
      (supersonic-mpv-command-with-callback
       (lambda (response)
         (let ((position (alist-get 'data response)))
           (supersonic-now-playing--update-field
            buff
            'duration
            (supersonic-now-playing--position position (buffer-local-value 'supersonic-now-playing--duration buff)))
           (supersonic-now-playing--recolor-waveform buff position)
           (unless (buffer-local-value 'supersonic-now-playing--waveform-requested buff)
             (supersonic-now-playing--maybe-fetch-waveform
              buff
              (buffer-local-value 'supersonic-now-playing--track-id buff)
              position
              (buffer-local-value 'supersonic-now-playing--duration buff)))))
       "get_property" "time-pos")))))

(defun supersonic-now-playing-maybe-refresh ()
  "Refresh the now-playing buffer from mpv's state, if it is open.
Called at the same points as `supersonic-queue-maybe-refresh', plus
whenever mpv reports that playback was paused or resumed."
  (let ((buff (supersonic-now-playing-buffer)))
    (when buff
      (supersonic-now-playing-fetch-and-render buff))))

;; Wired up from the outside rather than supersonic-mpv.el calling these
;; directly, so that file stays independent of this one's buffers -- see
;; `supersonic-mpv-track-change-hook'/`supersonic-mpv-playback-state-change-hook'.
(add-hook 'supersonic-mpv-track-change-hook #'supersonic-queue-maybe-refresh)
(add-hook 'supersonic-mpv-track-change-hook #'supersonic-now-playing-maybe-refresh)
(add-hook 'supersonic-mpv-playback-state-change-hook #'supersonic-now-playing-maybe-refresh)

(defun supersonic-now-playing--insert-field (label value &optional field)
  "Insert one \"LABEL: VALUE\" metadata row, skipping it if VALUE is nil.
FIELD, if given, tags VALUE so `supersonic-now-playing--update-field'
can replace it later without re-rendering the whole buffer."
  (when value
    (insert
     (propertize (format "%-10s" (concat label ":")) 'face 'shadow)
     (if field
         (propertize value 'supersonic-now-playing-field field)
       value)
     "\n")))

(defun supersonic-now-playing--update-field (buff field value)
  "Replace the text tagged FIELD in BUFF with VALUE, leaving the rest alone.
The playback position ticks once a second; re-rendering everything that
often would rebuild the cover art image and yank point back to the top
of the buffer each time."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (save-excursion
        (goto-char (point-min))
        (let ((match (text-property-search-forward 'supersonic-now-playing-field field t)))
          (when match
            (let ((inhibit-read-only t))
              (delete-region (prop-match-beginning match) (prop-match-end match))
              (goto-char (prop-match-beginning match))
              (insert (propertize value 'supersonic-now-playing-field field)))))))))

(defun supersonic-now-playing--progress-ratio (position duration)
  "Return POSITION/DURATION clamped to 0..1, or 0 if either is unavailable."
  (if (and position duration (> duration 0))
      (max 0.0 (min 1.0 (/ position duration)))
    0.0))

(defun supersonic-now-playing--recolor-waveform (buff position)
  "Redraw BUFF's waveform seekbar with POSITION as the new played/unplayed split.
No-op unless a waveform is already showing for BUFF's current track --
there's nothing to recolor before `supersonic-waveform-ensure''s
callback has delivered the first envelope."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (when supersonic-now-playing--waveform
        (supersonic-now-playing--update-field
         buff 'waveform
         (supersonic-waveform-propertize
          (cdr supersonic-now-playing--waveform)
          (supersonic-now-playing--progress-ratio position supersonic-now-playing--duration)))))))

(defun supersonic-now-playing--insert-button (label command)
  "Insert a button reading LABEL that runs COMMAND when activated."
  (insert-text-button label 'action (lambda (_button) (call-interactively command)) 'follow-link t))

(defun supersonic-now-playing--format-time (seconds longest)
  "Format SECONDS as a clock string no longer than it has to be.
LONGEST is the longest value the same line will show, so that a
position and the duration it counts towards stay the same shape: hours
only appear once LONGEST reaches an hour, e.g. \"05:01\" for a five
minute track but \"1:05:01\" once an hour is on the clock."
  (let ((seconds (max 0 (truncate (or seconds 0)))))
    (if (>= (or longest 0) 3600)
        (format-seconds "%h:%.2m:%.2s" seconds)
      (format-seconds "%.2m:%.2s" seconds))))

(defun supersonic-now-playing--position (position duration)
  "Format POSITION and DURATION (seconds, either may be nil) for display."
  (cond
   ((and position duration)
    (format "%s / %s"
            (supersonic-now-playing--format-time position duration)
            (supersonic-now-playing--format-time duration duration)))
   (duration
    (supersonic-now-playing--format-time duration duration))
   (position
    (supersonic-now-playing--format-time position position))))

(defun supersonic-now-playing--format (song)
  "Return SONG's file format as \"MP3 (audio/mpeg)\", or nil if unknown."
  (let ((suffix (assoc-default "suffix" song))
        (type (assoc-default "contentType" song)))
    (cond
     ((and suffix type)
      (format "%s (%s)" (upcase suffix) type))
     (suffix
      (upcase suffix))
     (type
      type))))

(defun supersonic-now-playing--art (song)
  "Return a display string for SONG's cover art, or nil if there is none.
Expects the art to be cached already, which
`supersonic-now-playing-fetch-and-render' takes care of before it
renders."
  (let ((art-id (assoc-default "coverArt" song)))
    (when (and supersonic-enable-art
               (display-graphic-p)
               art-id
               (file-exists-p (supersonic-art-cache-file art-id supersonic-now-playing-art-size)))
      (supersonic-image-propertize art-id supersonic-now-playing-art-size))))

(defun supersonic-now-playing--render (buff song paused position track-id)
  "Render SONG into BUFF, marked as paused or playing according to PAUSED.
POSITION is how many seconds into SONG playback currently is.  SONG is a
\"song\" alist as returned by getSong.view; if it is nil, BUFF shows a
placeholder saying that nothing is playing.  TRACK-ID is SONG's track
id (kept separate from SONG since the fallback placeholder built for a
failed metadata lookup has no \"id\" field of its own to read it back
from) -- see `supersonic-now-playing--track-id'."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (let ((inhibit-read-only t)
            (art (and song (supersonic-now-playing--art song)))
            (duration (and song (assoc-default "duration" song)))
            (size (and song (assoc-default "size" song))))
        (setq supersonic-now-playing--duration duration)
        (setq supersonic-now-playing--track-id track-id)
        ;; Whatever waveform was cached here belonged to the previous
        ;; track (or there wasn't one); `supersonic-now-playing--maybe-fetch-waveform'
        ;; repopulates it for the new one once it's ready.
        (setq supersonic-now-playing--waveform nil)
        (setq supersonic-now-playing--waveform-requested nil)
        (if song
            (supersonic-now-playing--start-timer)
          (supersonic-now-playing--stop-timer))
        (erase-buffer)
        (if (not song)
            (insert "Nothing is playing.\n")
          (progn
            (when art
              (insert art "\n\n"))
            (insert
             (propertize (or (assoc-default "title" song) "?") 'face 'bold) "  "
             (propertize (if paused
                             "(paused)"
                           "(playing)")
                         'face 'shadow)
             "\n\n")
            (supersonic-now-playing--insert-button "|◀◀" #'supersonic-prev-track)
            (insert "  ")
            (supersonic-now-playing--insert-button
             (if paused
                 " ▶ "
               " ⏸ ")
             #'supersonic-toggle-playing)
            (insert "  ")
            (supersonic-now-playing--insert-button "▶▶|" #'supersonic-skip-track)
            (insert "  ")
            (supersonic-now-playing--insert-button "◀◀" #'supersonic-seek-back)
            (insert "  ")
            (supersonic-now-playing--insert-button "▶▶" #'supersonic-seek-forward)
            (insert "\n\n")
            (when (supersonic-waveform-available-p)
              (insert (propertize " " 'supersonic-now-playing-field 'waveform) "\n\n"))
            (supersonic-now-playing--insert-field "Title" (assoc-default "title" song))
            (supersonic-now-playing--insert-field "Artist" (assoc-default "artist" song))
            (supersonic-now-playing--insert-field "Album" (assoc-default "album" song))
            (supersonic-now-playing--insert-field "Duration" (supersonic-now-playing--position position duration)
                                                  'duration)
            (supersonic-now-playing--insert-field "Format" (supersonic-now-playing--format song))
            (supersonic-now-playing--insert-field "Size" (and size (format "%.2f MB" (/ size 1048576.0))))))
        (goto-char (point-min))))))

(aio-defun
 supersonic-now-playing-fetch-and-render (buff)
 "Query mpv for the track it is currently on and render it into BUFF.
Tolerates a failing metadata lookup the way `supersonic-queue-parse'
does: rather than blanking a view that refreshes on every track change,
it falls back to showing the bare track id."
 (if (supersonic-mpv-live-p)
     (let* ((playlist (aio-await (supersonic-mpv-get-property "playlist")))
            (entry (seq-find (lambda (item) (alist-get 'current item)) playlist))
            (track-id (and entry (gethash (alist-get 'id entry) supersonic--playlist))))
       (if track-id
           (let* ((outcome
                   (aio-await
                    (aio-catch (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,track-id)))))))
                  (song
                   (if (eq (car outcome) :success)
                       (supersonic-recursive-assoc (cdr outcome) '("subsonic-response" "song"))
                     `(("title" . ,track-id)))))
             (when (and supersonic-enable-art (assoc-default "coverArt" song))
               (aio-await
                (aio-catch (supersonic--fetch-art (assoc-default "coverArt" song) supersonic-now-playing-art-size))))
             ;; Asked for last, so the position is as fresh as possible: the
             ;; art fetch above can take a while on a cold cache.
             (let ((position (aio-await (supersonic-mpv-get-property "time-pos"))))
               (supersonic-now-playing--render buff song supersonic--paused position track-id)
               (supersonic-now-playing--maybe-fetch-waveform buff track-id position (assoc-default "duration" song))))
         (supersonic-now-playing--render buff nil nil nil nil)))
   (supersonic-now-playing--render buff nil nil nil nil)))

(defun supersonic-now-playing--show-waveform (buff track-id envelope position duration)
  "Patch BUFF's waveform field to display ENVELOPE for TRACK-ID, if still current.
Shared by `supersonic-waveform-ensure''s final callback and its
progress callback -- see `supersonic-now-playing--maybe-fetch-waveform'
-- so the seekbar fills in gradually as buckets finish analyzing
instead of only popping in once the whole track is done."
  (when (and envelope (buffer-live-p buff))
    (with-current-buffer buff
      ;; The buffer may have moved on to a different track by the time a
      ;; full-track transcode finishes; discard a now-stale result instead
      ;; of showing another track's waveform under this one.
      (when (equal track-id supersonic-now-playing--track-id)
        (setq supersonic-now-playing--waveform (cons track-id envelope))
        (supersonic-now-playing--update-field
         buff 'waveform
         (supersonic-waveform-propertize envelope (supersonic-now-playing--progress-ratio position duration)))))))

(defun supersonic-now-playing--maybe-fetch-waveform (buff track-id position duration)
  "Kick off waveform generation for TRACK-ID and patch it into BUFF as it
becomes available.  No-op unless `supersonic-waveform-available-p' and
BUFF is actually on display -- a full-track transcode is real CPU and
network work, not worth spending on a buffer nobody is looking at (see
`supersonic-now-playing--tick' for how a buffer that becomes visible
again still gets one).  Fires and forgets rather than being awaited by
the caller, so a cold-cache waveform never delays the rest of the
buffer from appearing; POSITION/DURATION only matter for coloring the
seekbar's played/unplayed split.  The seekbar fills in progressively,
bucket by bucket, rather than only appearing once the whole track has
been analyzed -- see `supersonic-waveform-ensure''s PROGRESS-CALLBACK."
  (when (and (supersonic-waveform-available-p) (buffer-live-p buff) (get-buffer-window buff t))
    (with-current-buffer buff
      (setq supersonic-now-playing--waveform-requested t))
    (supersonic-waveform-ensure
     track-id
     (lambda (envelope) (supersonic-now-playing--show-waveform buff track-id envelope position duration))
     (lambda (envelope) (supersonic-now-playing--show-waveform buff track-id envelope position duration)))))

(defun supersonic-now-playing-refresh ()
  "Refresh the now-playing buffer from mpv's current state."
  (interactive)
  (supersonic-now-playing-fetch-and-render (current-buffer)))

(defvar supersonic-now-playing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-now-playing-refresh)
    (define-key map (kbd "SPC") #'supersonic-toggle-playing)
    (define-key map (kbd "n") #'supersonic-skip-track)
    (define-key map (kbd "p") #'supersonic-prev-track)
    (define-key map (kbd "f") #'supersonic-seek-forward)
    (define-key map (kbd "b") #'supersonic-seek-back)
    map))

(define-derived-mode
 supersonic-now-playing-mode
 special-mode
 "Supersonic Now Playing"
 "Major mode for the buffer opened by `supersonic-show-now-playing'.")

;;;###autoload
(defun supersonic-show-now-playing ()
  "Open a buffer showing the track mpv is currently on.
The buffer follows mpv on its own -- track changes, pausing and
resuming are all reflected without a manual refresh, the same way the
play queue buffer keeps itself current."
  (interactive)
  (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-now-playing-mode)
        (supersonic-now-playing-mode)))
    (ignore (supersonic-now-playing-fetch-and-render buff))
    (pop-to-buffer-same-window buff)))

(defun supersonic-get-id-as-string (data)
  "Return DATA's \"id\" field as a string, converting from a number if necessary."
  (let ((id (assoc-default "id" data)))
    (if (numberp id)
        (number-to-string id)
      id)))

;;;
;;; Search
;;;
(defun supersonic-search-parse (data)
  "Retrieve a list of search results from some parsed json DATA."
  (let* ((search-results (supersonic-recursive-assoc data '("subsonic-response" "searchResult3")))
         (result
          (append
           (mapcar
            (lambda (artist)
              (list
               `(,(supersonic-get-id-as-string artist) . "artist") (vector "Artist" (assoc-default "name" artist))))
            (assoc-default "artist" search-results))
           (mapcar
            (lambda (album)
              (list `(,(supersonic-get-id-as-string album) . "album") (vector "Album" (assoc-default "name" album))))
            (assoc-default "album" search-results))
           (mapcar
            (lambda (song)
              (list `(,(supersonic-get-id-as-string song) . "song") (vector "Song" (assoc-default "title" song))))
            (assoc-default "song" search-results)))))
    result))

(aio-defun
 supersonic-search-refresh (query buff) "Refresh the list of search results from QUERY into BUFF."
 (supersonic--with-async-error-handling
  buff "search"
  (let ((data (aio-await (supersonic-get-json (supersonic-build-url "/search3.view" `(("query" . ,query)))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (supersonic-search-parse data))
        (tabulated-list-print t))))))

(defun supersonic-open-search-appropriate-result (result)
  "Opens the RESULT from a search in the appropriate buffer."
  (let ((type (cdr result)))
    (cond
     ((string-equal type "artist")
      (supersonic-albums (car result)))
     ((string-equal type "album")
      (supersonic-tracks (car result)))
     ((string-equal type "song")
      (supersonic-mpv-start (list (car result)))))))

(defun supersonic-open-search-result ()
  "Open a view of the result from the result at point."
  (interactive)
  (supersonic-open-search-appropriate-result (tabulated-list-get-id)))

(defvar supersonic-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-search-result)
    map))

;;;###autoload
(defun supersonic-search ()
  "List supersonic search results."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-search*")))
    (supersonic--init-list-buffer new-buff #'supersonic-search-mode "Searching...")
    (ignore (supersonic-search-refresh (url-hexify-string (read-string "Query: ")) new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-search-mode
 tabulated-list-mode
 "Supersonic search mode"
 ;;  type: artist|album|track
 (setq tabulated-list-format [("Type" 10 t) ("Name" 30 t)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))


;;;
;;; Tracks
;;;

(defun supersonic-get-tracklist-id (id)
  "Get a tracklist for a given ID."
  (reverse
   (seq-reduce
    (lambda (accu current)
      (if (equal (car current) id)
          (list (car current))
        (if (null accu)
            '()
          (cons (car current) accu))))
    tabulated-list-entries '())))

(defun supersonic--tracks-extract-path ()
  "Return the json path to a track list, per current `supersonic-browse-by-tags'.
Computed fresh on every call rather than cached, so that toggling
`supersonic-browse-by-tags' at runtime stays consistent with
`supersonic-tracks-json', which also reads it live to pick the
endpoint -- a cached path here would otherwise go stale and parse
the response at the wrong key."
  (if supersonic-browse-by-tags
      '("subsonic-response" "album" "song")
    '("subsonic-response" "directory" "child")))

(defun supersonic-tracks-parse (data)
  "Parse tracks from json DATA."
  (let* ((tracks (supersonic-recursive-assoc data (supersonic--tracks-extract-path)))
         (result
          (mapcar
           (lambda (track)
             (let* ((duration (assoc-default "duration" track)))
               (list
                (supersonic-get-id-as-string track)
                (vector
                 (assoc-default "title" track)
                 (format-seconds "%m:%.2s" duration)
                 (format "%d" (or (assoc-default "track" track) 0))))))
           tracks)))
    result))

(aio-defun
 supersonic-tracks-json (id) "Fetch the raw getAlbum/getMusicDirectory json response for ID."
 (aio-await
  (supersonic-get-json
   (if supersonic-browse-by-tags
       (supersonic-build-url "/getAlbum.view" `(("id" . ,id)))
     (supersonic-build-url "/getMusicDirectory.view" `(("id" . ,id)))))))

(aio-defun
 supersonic-get-album-track-ids
 (id)
 "Return the list of track ids for album/directory ID."
 (mapcar #'car (supersonic-tracks-parse (aio-await (supersonic-tracks-json id)))))

(aio-defun
 supersonic-tracks-refresh (id buff) "Refresh the list of tracks from ID into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch tracks"
  (let ((data (aio-await (supersonic-tracks-json id))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (supersonic-tracks-parse data))
        (tabulated-list-print t))))))

(defun supersonic-play-tracks ()
  "Play all the tracks after the point in the list."
  (interactive)
  (supersonic-mpv-start (supersonic-get-tracklist-id (tabulated-list-get-id))))

(defun supersonic-enqueue-tracks ()
  "Add all the tracks after the point in the list to the play queue."
  (interactive)
  (let ((ids (supersonic-get-tracklist-id (tabulated-list-get-id))))
    (supersonic-mpv-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids))))

(defvar supersonic-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-play-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-tracks)
    map))

(defun supersonic-tracks (id)
  "Create a buffer with a list of tracks from ID."
  (let ((new-buff (get-buffer-create "*supersonic-tracks*")))
    (supersonic--init-list-buffer new-buff #'supersonic-tracks-mode "Loading tracks...")
    (ignore (supersonic-tracks-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-tracks-mode
 tabulated-list-mode
 "Supersonic Tracks"
 (setq tabulated-list-format [("Title" 30 t) ("Duration" 10 t) ("Track" 10 t)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

;;;
;;; Albums
;;;

(defun supersonic-albums-parse (data)
  "Retrieve a list of albums from some parsed json DATA."
  (let* ((albums (supersonic-recursive-assoc data '("subsonic-response" "artist" "album")))
         (result
          (mapcar
           (lambda (album)
             (list
              (supersonic-get-id-as-string album)
              (vector (format "%d" (or (assoc-default "year" album) 0)) (assoc-default "name" album) "")))
           albums)))
    result))

(defun supersonic-albums-type-parse (data)
  "Retrieve a list of albums from some parsed json DATA."
  (let* ((albums (supersonic-recursive-assoc data '("subsonic-response" "albumList2" "album")))
         (result
          (mapcar
           (lambda (album)
             (list
              (supersonic-get-id-as-string album)
              (vector (assoc-default "name" album) (assoc-default "artist" album) "")))
           albums)))
    result))

(aio-defun
 supersonic-albums-refresh (id buff) "Refresh the albums list for a given artist ID into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch albums"
  (let ((data (aio-await (supersonic-get-json (supersonic-build-url "/getArtist.view" `(("id" . ,id)))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (supersonic-albums-parse data))
        (tabulated-list-print t)
        (supersonic-get-images tabulated-list-entries 2 buff))))))


(aio-defun
 supersonic-albums-refresh-type (type buff) "Refresh the albums list for a given albumlist TYPE into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch albums"
  (let ((data
         (aio-await
          (supersonic-get-json
           (supersonic-build-url
            "/getAlbumList2.view" `(("type" . ,type) ("size" . ,(number-to-string supersonic-album-list-count))))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (supersonic-albums-type-parse data))
        (tabulated-list-print t)
        (supersonic-get-images tabulated-list-entries 2 buff))))))

(defun supersonic-open-tracks ()
  "Open a list of tracks at point."
  (interactive)
  (supersonic-tracks (tabulated-list-get-id)))

(aio-defun
 supersonic-enqueue-album () "Add all the tracks of the album at point to the play queue." (interactive)
 (supersonic--with-async-error-handling
  nil "enqueue album"
  (let* ((track-id (tabulated-list-get-id))
         (ids (aio-await (supersonic-get-album-track-ids track-id))))
    (supersonic-mpv-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids)))))

(defvar supersonic-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map))

(defvar supersonic-album-type-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map))

;;;###autoload
(defun supersonic-recent-albums ()
  "Show a list of recently played albums."
  (interactive)
  (supersonic-albums nil "recent"))

;;;###autoload
(defun supersonic-random-albums ()
  "Show a list of random albums."
  (interactive)
  (supersonic-albums nil "random"))

;;;###autoload
(defun supersonic-newest-albums ()
  "Show a list of recently added albums."
  (interactive)
  (supersonic-albums nil "newest"))

(defun supersonic-albums (&optional id type)
  "Open a buffer of albums for artist ID or list TYPE."
  (cond
   (id
    (let ((new-buff (get-buffer-create "*supersonic-artist-albums*")))
      (supersonic--init-list-buffer new-buff #'supersonic-album-mode "Loading albums...")
      (ignore (supersonic-albums-refresh id new-buff))
      (pop-to-buffer-same-window new-buff)))
   (type
    (let ((new-buff (get-buffer-create "*supersonic-albums*")))
      (supersonic--init-list-buffer new-buff #'supersonic-album-type-mode "Loading albums...")
      (ignore (supersonic-albums-refresh-type type new-buff))
      (pop-to-buffer-same-window new-buff)))))

(define-derived-mode
 supersonic-album-type-mode
 tabulated-list-mode
 "Supersonic Album List"
 (setq tabulated-list-format [("Albums" 30 t) ("Artists" 30 t) ("Art" 30 nil)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

(define-derived-mode
 supersonic-album-mode
 tabulated-list-mode
 "Supersonic Albums"
 (setq tabulated-list-format [("Year" 5 t) ("Albums" 40 t) ("Art" 30 nil)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

;;;
;;; Artists
;;;
(defun supersonic-open-album ()
  "Open the albums for the artist at point."
  (interactive)
  (supersonic-albums (tabulated-list-get-id)))

(defun supersonic-artists-parse (data)
  "Retrieve a list of artists from some parsed json DATA."
  (let ((artists (supersonic-recursive-assoc data '("subsonic-response" "artists" "index"))))
    (mapcan
     (lambda (artist-index)
       (mapcar
        (lambda (artist)
          (list (supersonic-get-id-as-string artist) (vector (assoc-default "name" artist))))
        (assoc-default "artist" artist-index)))
     artists)))

(aio-defun
 supersonic-artists-refresh (buff) "Refresh the list of artists into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch artists"
  (let ((data (aio-await (supersonic-get-json (supersonic-build-url "/getArtists.view" '())))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (supersonic-artists-parse data))
        (tabulated-list-print t))))))

(defun supersonic-artists-revert ()
  "Refresh the artists buffer from the Subsonic server."
  (interactive)
  (supersonic-artists-refresh (current-buffer)))

(defvar supersonic-artist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-album)
    (define-key map (kbd "g") #'supersonic-artists-revert)
    map))

;;;###autoload
(defun supersonic-artists ()
  "List artists."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-artists*")))
    (supersonic--init-list-buffer new-buff #'supersonic-artist-mode "Loading artists...")
    (ignore (supersonic-artists-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-artist-mode
 tabulated-list-mode
 "Supersonic Artists"
 (setq tabulated-list-format [("Artist" 30 t)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

;;;
;;; Podcasts
;;;
(defun supersonic-podcasts-parse (data)
  "Retrieve a list of podcasts from some parsed json DATA."
  (let* ((podcasts (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel")))
         (result
          (mapcar
           (lambda (channel)
             (list (supersonic-get-id-as-string channel) (vector (assoc-default "title" channel) "")))
           podcasts)))
    result))

(aio-defun
 supersonic-podcasts-refresh (buff) "Refresh the list of podcasts into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch podcasts"
  (let ((data
         (aio-await (supersonic-get-json (supersonic-build-url "/getPodcasts.view" '(("includeEpisodes" . "false")))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (supersonic-podcasts-parse data))
        (tabulated-list-print t)
        (supersonic-get-images tabulated-list-entries 1 buff))))))


(defun supersonic-open-podcast-episodes ()
  "Open a view of podcasts episodes from the podcast at point."
  (interactive)
  (supersonic-podcast-episodes (tabulated-list-get-id)))

(aio-defun
 supersonic-add-podcast () "Add a new podcast." (interactive)
 (supersonic--with-async-error-handling
  nil "add podcast"
  (aio-await
   (supersonic-get-json
    (supersonic-build-url "/createPodcastChannel.view" `(("url" . ,(url-hexify-string (read-string "feed url: ")))))))
  (message "Podcast added")))

(transient-define-prefix
 supersonic-podcast-help () "Help transient for podcasts."
 ["Supersonic podcast help"
  ("a" "Add a podcast" supersonic-add-podcast)
  ("RET" "Open a podcast" supersonic-open-podcast-episodes)])

(defvar supersonic-podcast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-podcast-episodes)
    (define-key map (kbd "?") #'supersonic-podcast-help)
    (define-key map (kbd "a") #'supersonic-add-podcast)
    map))

;;;###autoload
(defun supersonic-podcasts ()
  "List podcasts."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-podcasts*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-mode "Loading podcasts...")
    (ignore (supersonic-podcasts-refresh new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-podcast-mode
 tabulated-list-mode
 "Supersonic Podcasts"
 (setq tabulated-list-format [("Podcasts" 30 t) ("Art" 20 nil)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

;;;
;;; Podcast episodes
;;;

(defun supersonic-podcast-episodes-parse (data)
  "Retrieve a list of podcast episodes from some parsed json DATA."
  (let* ((episodes
          (assoc-default "episode" (car (supersonic-recursive-assoc data '("subsonic-response" "podcasts" "channel")))))
         (result
          (mapcar
           (lambda (episode)
             (list
              (supersonic-get-id-as-string episode)
              (vector
               (assoc-default "title" episode)
               (format-seconds "%h:%.2m:%.2s" (assoc-default "duration" episode))
               (assoc-default "status" episode))))
           episodes)))
    result))

(defun supersonic-play-podcast ()
  "Play a podcast episode at point."
  (interactive)
  (supersonic-mpv-start (list (tabulated-list-get-id))))

(aio-defun
 supersonic-podcasts-episode-refresh (id buff) "Refresh the list of podcast episodes for a podcast ID into BUFF."
 (supersonic--with-async-error-handling
  buff "fetch episodes"
  (let ((data
         (aio-await
          (supersonic-get-json
           (supersonic-build-url "/getPodcasts.view" `(("id" . ,id) ("includeEpisodes" . "true")))))))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (setq tabulated-list-entries (supersonic-podcast-episodes-parse data))
        (tabulated-list-print t))))))

(aio-defun
 supersonic-download-podcast-episode () "Tell the supersonic server to download an episode at point." (interactive)
 (supersonic--with-async-error-handling
  nil "download episode"
  (let ((id (tabulated-list-get-id)))
    (aio-await (supersonic-get-json (supersonic-build-url "/downloadPodcastEpisode.view" `(("id" . ,id)))))
    (message "Episode download started"))))

(transient-define-prefix
 supersonic-podcast-episode-help () "Help transient for podcast episodes."
 ["Supersonic podcast episode help"
  ("d" "Download" supersonic-download-podcast-episode)
  ("RET" "Start playing" supersonic-play-podcast)])


(defvar supersonic-podcast-episodes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'supersonic-podcast-episode-help)
    (define-key map (kbd "RET") #'supersonic-play-podcast)
    (define-key map (kbd "d") #'supersonic-download-podcast-episode)
    map))

(defun supersonic-podcast-episodes (id)
  "Open a buffer with a list of podcast episodes from podcast ID."
  (let ((new-buff (get-buffer-create "*supersonic-podcast-episodes*")))
    (supersonic--init-list-buffer new-buff #'supersonic-podcast-episodes-mode "Loading episodes...")
    (ignore (supersonic-podcasts-episode-refresh id new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-podcast-episodes-mode
 tabulated-list-mode
 "Supersonic Podcast Episodes"
 (setq tabulated-list-format [("Title" 50 t) ("Duration" 10 t) ("Status" 24 t)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

;;;###autoload (autoload 'supersonic "supersonic" nil t)
(transient-define-prefix
 supersonic () "Help transient for supersonic."
 [["Supersonic"
   ("a" "Artists" supersonic-artists)
   ("e" "Recent Albums" supersonic-recent-albums)
   ("r" "Random Albums" supersonic-random-albums)
   ("n" "Newest Albums" supersonic-newest-albums)
   ("s" "Search" supersonic-search)
   ("p" "Podcasts" supersonic-podcasts)]
  ["Controls"
   ("Q" "Show queue" supersonic-show-queue)
   ("N" "Now playing" supersonic-show-now-playing)
   ("SPC" "Toggle playing" supersonic-toggle-playing)
   ("f" "Skip track" supersonic-skip-track)
   ("b" "Previous track" supersonic-prev-track)
   ("F" "Seek forward" supersonic-seek-forward :transient t)
   ("B" "Seek back" supersonic-seek-back :transient t)]])

(provide 'supersonic)

;;; supersonic.el ends here
