;;; supersonic.el --- Browse and play music from subsonic servers with mpv -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
;; URL: https://github.com/systemfreund/supersonic.el
;; Version: 0.2.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "28.1") (transient "0.2") (aio "1.0"))

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
;; `prop-match-beginning'/`prop-match-end' are plain struct accessors
;; from this file, not autoloads, so requiring it is not optional even
;; though `text-property-search-forward' itself is autoloaded.
(require 'text-property-search)

(require 'transient)

(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-art)
(require 'supersonic-playback)
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
  "Refresh the play queue buffer from the active backend's play queue, if it is open.
Called whenever the queue is likely to have changed: after
starting/enqueueing tracks and whenever the active backend reports a
track starting or ending."
  (let ((buff (supersonic-queue-buffer)))
    (when buff
      (supersonic-queue-fetch-and-render buff))))

(aio-defun
 supersonic-queue-parse (entries)
 "Turn ENTRIES, as resolved by `supersonic-playback-queue', into
tabulated-list entries.
Fetches each entry's song metadata concurrently (fired up front, below,
before anything is awaited) and tolerates individual lookup failures
via `aio-catch', falling back to the \"?\" placeholder row instead of
aborting the whole render."
 (let* ((pending
         (seq-map-indexed
          (lambda (entry index)
            (let ((track-id (plist-get entry :track-id)))
              (list
               index track-id (plist-get entry :current)
               (and track-id
                    (aio-catch (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,track-id)))))))))
          entries)))
   ;; A plain `mapcar' lambda would call `aio-await' through an ordinary `funcall', outside of this function's own
   ;; generator machinery, which `generator.el' cannot transform -- so this collects results via a `dolist', which
   ;; (like `while') stays inline and awaits correctly.
   (let (rows)
     (dolist (item pending)
       (pcase-let ((`(,index ,track-id ,current ,promise) item))
         (let* ((outcome (and promise (aio-await promise)))
                (song
                 (and outcome
                      (eq (car outcome) :success)
                      (supersonic-recursive-assoc (cdr outcome) '("subsonic-response" "song")))))
           (push (list
                  ;; A missing track id would only happen if a backend handed back an entry it could not resolve;
                  ;; fall back to the entry's position so the row still gets a usable, if display-only, id.
                  (or track-id (number-to-string index))
                  (vector
                   (if current
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

(aio-defun
 supersonic-queue-fetch-and-render
 (buff)
 "Query the active playback backend for its play queue and render it into BUFF."
 (when (buffer-live-p buff)
   (let ((entries (aio-await (supersonic-queue-parse (aio-await (supersonic-playback-queue))))))
     (when (buffer-live-p buff)
       (with-current-buffer buff
         (setq tabulated-list-entries entries)
         (tabulated-list-print t))))))

(defun supersonic-queue-refresh ()
  "Refresh the play queue buffer from the active backend's current play queue."
  (interactive)
  (supersonic-queue-fetch-and-render (current-buffer)))

(defvar supersonic-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'supersonic-queue-refresh)
    map)
  "Keymap for `supersonic-queue-mode'.")

(define-derived-mode
 supersonic-queue-mode
 tabulated-list-mode
 "Supersonic Queue"
 "Major mode for the play queue opened by `supersonic-show-queue'.
Mirrors the active backend's play queue, refreshed whenever it may have changed."
 (setq tabulated-list-format [("" 2 nil) ("Title" 40 t) ("Artist" 25 t) ("Album" 25 t)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

;;;###autoload
(defun supersonic-show-queue ()
  "Open a buffer showing the active backend's current play queue."
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

(defvar supersonic-now-playing--animation-timer nil
  "Timer advancing `supersonic-now-playing-cycle-fields'.
Only runs while that list is non-nil and a track is playing; see
`supersonic-now-playing--animation-tick'.")

(defvar-local supersonic-now-playing--title nil
  "Title of the track the now-playing buffer is currently showing, or nil.
Kept around, alongside `supersonic-now-playing--artist' and
`supersonic-now-playing--album', so
`supersonic-now-playing--animation-tick' can rotate between them
without another metadata lookup.")

(defvar-local supersonic-now-playing--artist nil
  "Artist of the track the now-playing buffer is currently showing, or nil.
See `supersonic-now-playing--title'.")

(defvar-local supersonic-now-playing--album nil
  "Album of the track the now-playing buffer is currently showing, or nil.
See `supersonic-now-playing--title'.")

(defvar-local supersonic-now-playing--art-id nil
  "Cover art id of the track the now-playing buffer is currently showing, or nil.
Set by `supersonic-now-playing--render'; read by
`supersonic-now-playing-animate-art-overlay' so it can redraw the art
with new text without needing the whole song alist again.")

(defvar-local supersonic-now-playing--field-index 0
  "Index into `supersonic-now-playing-cycle-fields' of the field currently shown.
Advanced by `supersonic-now-playing--animation-tick'; reset to 0
whenever `supersonic-now-playing--render' moves on to a new track, so
a track change always starts out on the title.")

(defvar-local supersonic-now-playing--scroll-offset 0
  "Pixels `supersonic-now-playing-animate-art-overlay-scroll' has scrolled so far.
Advanced by `supersonic-now-playing-scroll-step' on every animation
tick; reset to 0 whenever `supersonic-now-playing--render' moves on to
a new track, the same way `supersonic-now-playing--field-index' is, so
a track change always starts the crawl back at the beginning.")

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

(defvar-local supersonic-now-playing--position nil
  "Playback position in seconds as of the now-playing buffer's last update.
Written by `supersonic-now-playing--render' and by every
`supersonic-now-playing--tick', so that whatever needs the current
position can read it here rather than close over one captured earlier.
`supersonic-now-playing--show-waveform' is the reason it exists: a
cold-cache waveform arrives a transcode later than the render that
asked for it, and drawing that first seekbar against the position from
back then put the played/unplayed split seconds behind where playback
actually was until the next tick corrected it.")

(defvar-local supersonic-now-playing--waveform nil
  "(TRACK-ID . ENVELOPE) for the waveform seekbar currently shown, or nil.
ENVELOPE lets `supersonic-now-playing--tick' recolor the seekbar's
played/unplayed split every second without asking
`supersonic-waveform-ensure' again.")

(defvar-local supersonic-now-playing--waveform-bucket nil
  "Index of the first unplayed bucket in the seekbar as last drawn, or nil.
The seekbar is a bucketed image, so the position ticking on does not
change it at all until the played/unplayed boundary actually crosses
into the next bucket -- once every twelve seconds for a 300-bucket
seekbar over an hour-long podcast, against a tick a second.  Comparing
against this is how `supersonic-now-playing--recolor-waveform' skips
the redraws in between.")

(defvar-local supersonic-now-playing--waveform-requested nil
  "Non-nil once a waveform fetch has been kicked off for the current track.
Set whether or not that fetch went on to succeed.
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

(defun supersonic-now-playing--field-value (field)
  "Return the current buffer's cached value for FIELD (title/artist/album)."
  (pcase field
    ('title supersonic-now-playing--title)
    ('artist supersonic-now-playing--artist)
    ('album supersonic-now-playing--album)))

(defun supersonic-now-playing--current-field ()
  "Return (FIELD . VALUE) for whichever field is currently up to be shown.
FIELD is whatever `supersonic-now-playing--field-index' currently
points at in `supersonic-now-playing-cycle-fields'; VALUE falls back to
the track's title -- \"?\" failing that -- when the list is empty or
the field it points at has no value for the current track, e.g. an
index landing on `album' for a track with none.  What actually happens
with the result is up to `supersonic-now-playing-animation-function'."
  (let* ((fields supersonic-now-playing-cycle-fields)
         (field (and fields (nth (mod supersonic-now-playing--field-index (length fields)) fields))))
    (cons (or field 'title) (or (and field (supersonic-now-playing--field-value field)) supersonic-now-playing--title "?"))))

(defun supersonic-now-playing-animate-label (buff field)
  "Show FIELD's value as BUFF's text label next to the cover art.
FIELD is a (SYMBOL . VALUE) cons, as `supersonic-now-playing--current-field'
returns.  The default value of `supersonic-now-playing-animation-function',
and the only thing a rotated field did before that existed."
  (supersonic-now-playing--update-field buff 'label (propertize (cdr field) 'face 'bold)))

(defun supersonic-now-playing-animate-art-overlay (buff field)
  "Layer FIELD's value onto BUFF's cover art instead of the side label.
FIELD is a (SYMBOL . VALUE) cons, as `supersonic-now-playing--current-field'
returns.  Falls back to `supersonic-now-playing-animate-label' when
there is no cached art to layer text onto -- `supersonic-art-available-p'
is nil, or the file for the current track has not landed yet -- the
same case `supersonic-now-playing--art' draws nothing for."
  (with-current-buffer buff
    (if (and supersonic-now-playing--art-id
             (supersonic-art-available-p)
             (file-exists-p (supersonic-art-cache-file supersonic-now-playing--art-id supersonic-now-playing-art-size)))
        (supersonic-now-playing--update-field
         buff 'art
         (supersonic-art-overlay-propertize
          supersonic-now-playing--art-id supersonic-now-playing-art-size (cdr field)))
      (supersonic-now-playing-animate-label buff field))))

(defun supersonic-now-playing--scroll-text ()
  "Return `supersonic-now-playing-cycle-fields''s values joined into one line.
What `supersonic-now-playing-animate-art-overlay-scroll' scrolls across
the art, rather than any one field passed to it -- the whole point of
that function over `supersonic-now-playing-animate-art-overlay' is
showing every configured field together instead of switching between
them.  Fields with no value for the current track (e.g. `album' on a
track with none) are left out rather than shown as an empty stretch of
the line.  Falls back to the track's title -- \"?\" failing that -- if
none of them have a value, the same fallback
`supersonic-now-playing--current-field' uses for a single field."
  (let ((values (delq nil (mapcar #'supersonic-now-playing--field-value supersonic-now-playing-cycle-fields))))
    (if values
        (mapconcat #'identity values "   •   ")
      (or supersonic-now-playing--title "?"))))

(defun supersonic-now-playing-animate-art-overlay-scroll (buff field)
  "Scroll `supersonic-now-playing-cycle-fields' across BUFF's cover art.
FIELD is a (SYMBOL . VALUE) cons, as `supersonic-now-playing--current-field'
returns, used only for the label fallback below -- unlike
`supersonic-now-playing-animate-art-overlay', which draws whichever
single field FIELD names, this one always draws every configured field
together (see `supersonic-now-playing--scroll-text'), so nothing about
FIELD itself matters here and no field ever gets left out waiting for
its turn.  Falls back to `supersonic-now-playing-animate-label' under
the same conditions that one does: no cover art available, or the file
for the current track not cached yet."
  (with-current-buffer buff
    (if (and supersonic-now-playing--art-id
             (supersonic-art-available-p)
             (file-exists-p (supersonic-art-cache-file supersonic-now-playing--art-id supersonic-now-playing-art-size)))
        (progn
          (setq supersonic-now-playing--scroll-offset
                (+ supersonic-now-playing--scroll-offset supersonic-now-playing-scroll-step))
          (supersonic-now-playing--update-field
           buff 'art
           (supersonic-art-overlay-scroll-propertize
            supersonic-now-playing--art-id supersonic-now-playing-art-size
            (supersonic-now-playing--scroll-text) supersonic-now-playing--scroll-offset)))
      (supersonic-now-playing-animate-label buff field))))

(defun supersonic-now-playing--start-animation-timer ()
  "Start rotating `supersonic-now-playing-cycle-fields', unless already running."
  (when (and supersonic-now-playing-cycle-fields (not supersonic-now-playing--animation-timer))
    (setq supersonic-now-playing--animation-timer
          (run-at-time
           supersonic-now-playing-animation-interval supersonic-now-playing-animation-interval
           #'supersonic-now-playing--animation-tick))))

(defun supersonic-now-playing--stop-animation-timer ()
  "Stop rotating `supersonic-now-playing-cycle-fields'."
  (when supersonic-now-playing--animation-timer
    (cancel-timer supersonic-now-playing--animation-timer)
    (setq supersonic-now-playing--animation-timer nil)))

(defun supersonic-now-playing--animation-tick ()
  "Advance the animated field and render it.
Rendering is `supersonic-now-playing-animation-function''s job; this
just picks which field is next.  Stops itself once there is nothing
left playing to animate, and keeps quiet while the buffer is not on
display, the same way `supersonic-now-playing--tick' does for the
position -- worth even more here than there: unlike a position update,
`supersonic-now-playing-animate-art-overlay-scroll' redraws the cover
art's whole SVG image on every call, and at the short interval a
smooth scroll needs, doing that for a buffer nobody is looking at would
burn CPU on nothing.  Neither the field index nor the scroll offset
advances while skipped, so the animation picks back up from wherever it
left off instead of jumping ahead once the buffer is shown again."
  (let ((buff (supersonic-now-playing-buffer)))
    (cond
     ((or (not buff) (not (supersonic-playback-live-p)))
      (supersonic-now-playing--stop-animation-timer))
     ((not (get-buffer-window buff t)))
     (t
      (with-current-buffer buff
        (setq supersonic-now-playing--field-index (1+ supersonic-now-playing--field-index))
        (funcall supersonic-now-playing-animation-function buff (supersonic-now-playing--current-field)))))))

(defun supersonic-now-playing--tick ()
  "Update the playback position in the now-playing buffer.
Asks the active backend where it is rather than counting seconds
locally, so a seek or a pause in between two ticks can never leave the
position drifting -- `supersonic-now-playing-maybe-update-position'
exists only to show a seek sooner than the next tick would.  Stops
itself once there is nothing left to update, and keeps quiet while the
buffer is not on display.  Also picks up a waveform fetch
`supersonic-now-playing--maybe-fetch-waveform' skipped earlier for
exactly that reason, the first tick after the buffer becomes visible
again (see `supersonic-now-playing--waveform-requested')."
  (let ((buff (supersonic-now-playing-buffer)))
    (cond
     ((or (not buff) (not (supersonic-playback-live-p)))
      (supersonic-now-playing--stop-timer))
     ((not (get-buffer-window buff t)))
     (t
      (supersonic-now-playing--show-position buff)))))

(aio-defun
 supersonic-now-playing--show-position (buff)
 "Ask the active backend where it is and update everything in BUFF that follows.
The position line, the seekbar's played/unplayed split, and a waveform
fetch that was skipped while nobody was looking at BUFF."
 (let ((position (aio-await (supersonic-playback-status 'position))))
   (when (buffer-live-p buff)
     (with-current-buffer buff
       (setq supersonic-now-playing--position position))
     (supersonic-now-playing--update-field
      buff 'duration
      (supersonic-now-playing--position position (buffer-local-value 'supersonic-now-playing--duration buff)))
     (supersonic-now-playing--recolor-waveform buff position)
     (unless (buffer-local-value 'supersonic-now-playing--waveform-requested buff)
       (supersonic-now-playing--maybe-fetch-waveform
        buff (buffer-local-value 'supersonic-now-playing--track-id buff))))))

(defun supersonic-now-playing-maybe-update-position ()
  "Update the position shown in the now-playing buffer, if it is on display.
Hung off `supersonic-playback-position-change-hook', so that a seek
shows up as soon as it has taken effect rather than whenever the next
`supersonic-now-playing--tick' happens to come round -- a wait of up
to `supersonic-now-playing-interval' that made clicking the waveform
seekbar feel unresponsive.  Cheaper than a refresh: only the position
and the seekbar move when playback jumps, so none of the metadata is
looked up again."
  (let ((buff (supersonic-now-playing-buffer)))
    (when (and buff (get-buffer-window buff t) (supersonic-playback-live-p))
      (supersonic-now-playing--show-position buff))))

(defun supersonic-now-playing-maybe-refresh ()
  "Refresh the now-playing buffer from the active backend's state, if it is open.
Called at the same points as `supersonic-queue-maybe-refresh', plus
whenever the active backend reports that playback was paused or resumed."
  (let ((buff (supersonic-now-playing-buffer)))
    (when buff
      (supersonic-now-playing-fetch-and-render buff))))

;; Wired up from the outside rather than the backend calling these
;; directly, so the backends stay independent of this one's buffers --
;; see `supersonic-playback-track-change-hook'/
;; `supersonic-playback-state-change-hook'.
(add-hook 'supersonic-playback-track-change-hook #'supersonic-queue-maybe-refresh)
(add-hook 'supersonic-playback-track-change-hook #'supersonic-now-playing-maybe-refresh)
(add-hook 'supersonic-playback-state-change-hook #'supersonic-now-playing-maybe-refresh)
(add-hook 'supersonic-playback-position-change-hook #'supersonic-now-playing-maybe-update-position)

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
            (let ((inhibit-read-only t)
                  (stale (get-text-property (prop-match-beginning match) 'display)))
              (delete-region (prop-match-beginning match) (prop-match-end match))
              ;; Each seekbar redraw is a `create-image' over freshly
              ;; generated pixel data, so it gets its own entry in Emacs's
              ;; image cache -- and entries linger there for
              ;; `image-cache-eviction-delay' (300 seconds by default)
              ;; whether or not anything still displays them.  Unflushed,
              ;; a few minutes of playback keeps a few hundred decoded
              ;; 500x48 pixmaps alive for one seekbar.  Errors ignored
              ;; because `image-flush' insists on a real window-system
              ;; frame ("Window system frame should be used" anywhere
              ;; else), and a cache entry that outlives its display is
              ;; worth strictly less than the render it would abort.
              (when (eq (car-safe stale) 'image)
                (ignore-errors
                  (image-flush stale)))
              (goto-char (prop-match-beginning match))
              (insert (propertize value 'supersonic-now-playing-field field)))))))))

(defun supersonic-now-playing--progress-ratio (position duration)
  "Return POSITION/DURATION clamped to 0..1, or 0 if either is unavailable.
POSITION is coerced to a float first: mpv reports \"time-pos\" as one,
but a duration straight out of a Subsonic response is a whole number
of seconds, and two integers would divide down to a ratio of 0 for
everything short of the very last second of the track."
  (if (and position duration (> duration 0))
      (max 0.0 (min 1.0 (/ (float position) duration)))
    0.0))

(defun supersonic-now-playing--progress-bucket (envelope progress)
  "Return the index of ENVELOPE's first unplayed bucket at PROGRESS (0..1).
This is the only thing about the rendered seekbar that PROGRESS
changes, and what `supersonic-now-playing--waveform-bucket' records."
  (floor (* progress (length (car envelope)))))

(defun supersonic-now-playing--recolor-waveform (buff position)
  "Redraw BUFF's waveform seekbar with POSITION as the new played/unplayed split.
No-op unless a waveform is already showing for BUFF's current track --
there's nothing to recolor before `supersonic-waveform-ensure''s
callback has delivered the first envelope -- and no-op too while the
split still falls within the bucket it was last drawn in, which is
most ticks (see `supersonic-now-playing--progress-bucket')."
  (when (buffer-live-p buff)
    (with-current-buffer buff
      (when supersonic-now-playing--waveform
        (let* ((envelope (cdr supersonic-now-playing--waveform))
               (progress (supersonic-now-playing--progress-ratio position supersonic-now-playing--duration))
               (bucket (supersonic-now-playing--progress-bucket envelope progress)))
          (unless (eql bucket supersonic-now-playing--waveform-bucket)
            (setq supersonic-now-playing--waveform-bucket bucket)
            (supersonic-now-playing--update-field
             buff 'waveform (supersonic-waveform-propertize envelope progress))))))))

(defun supersonic-now-playing--insert-button (label command)
  "Insert a boxed button reading LABEL that runs COMMAND when activated.
Padded with a space on each side and given `supersonic-now-playing-button'
so it reads as its own pushable control -- the way ready-player boxes
its transport row -- rather than as underlined link text."
  (insert-text-button
   (format " %s " label)
   'face 'supersonic-now-playing-button
   'action (lambda (_button) (call-interactively command))
   'follow-link t))

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
    (when (and (supersonic-art-available-p)
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
      (let* ((inhibit-read-only t)
             (art (and song (supersonic-now-playing--art song)))
             (duration (and song (assoc-default "duration" song)))
             (size (and song (assoc-default "size" song)))
             ;; Re-rendering the track already on show is the common case,
             ;; not the exception: a pause, a resume, `g', and two or three
             ;; renders inside the first second of a fresh mpv start all
             ;; land here.  Whatever waveform state such a render finds is
             ;; still about the very track it is re-rendering, so it has to
             ;; survive -- blanking it unconditionally meant every one of
             ;; those looked like a first request for a waveform nobody had
             ;; ever asked for, and re-ran generation accordingly.
             (same-track (and track-id (equal track-id supersonic-now-playing--track-id))))
        (setq supersonic-now-playing--duration duration)
        (setq supersonic-now-playing--position position)
        (setq supersonic-now-playing--track-id track-id)
        (setq supersonic-now-playing--title (and song (assoc-default "title" song)))
        (setq supersonic-now-playing--artist (and song (assoc-default "artist" song)))
        (setq supersonic-now-playing--album (and song (assoc-default "album" song)))
        (setq supersonic-now-playing--art-id (and song (assoc-default "coverArt" song)))
        (unless same-track
          ;; A track change always starts the animated field back on the
          ;; title, whatever it last settled on for the track before it,
          ;; and any scrolling overlay back at the beginning of its crawl.
          (setq supersonic-now-playing--field-index 0)
          (setq supersonic-now-playing--scroll-offset 0)
          ;; Whatever waveform was cached here belonged to the previous
          ;; track (or there wasn't one); `supersonic-now-playing--maybe-fetch-waveform'
          ;; repopulates it for the new one once it's ready.
          (setq supersonic-now-playing--waveform nil)
          (setq supersonic-now-playing--waveform-bucket nil)
          (setq supersonic-now-playing--waveform-requested nil))
        (if song
            (supersonic-now-playing--start-timer)
          (supersonic-now-playing--stop-timer))
        (if (and song supersonic-now-playing-cycle-fields)
            (supersonic-now-playing--start-animation-timer)
          (supersonic-now-playing--stop-animation-timer))
        (erase-buffer)
        (if (not song)
            (insert "Nothing is playing.\n")
          (progn
            (when art
              (insert (propertize art 'supersonic-now-playing-field 'art) "  "))
            ;; Just the tagged placeholder here, the same way the waveform
            ;; slot below is -- `supersonic-now-playing-animation-function'
            ;; fills it in (or leaves it empty, if it targets the art
            ;; overlay instead) a few lines down, once the rest of the
            ;; buffer exists for it to search across.
            (insert (propertize " " 'supersonic-now-playing-field 'label))
            ;; No blank line before the waveform -- it sits right under
            ;; the label -- but one is still wanted before the buttons
            ;; when there is no waveform to close that gap instead.
            (insert (if (supersonic-waveform-available-p) "\n" "\n\n"))
            ;; Just the tagged placeholder here; whether there is an
            ;; image to put in it is settled at the end of this function,
            ;; via `supersonic-now-playing--show-waveform'.
            (when (supersonic-waveform-available-p)
              (insert (propertize " " 'supersonic-now-playing-field 'waveform) "\n"))
            (supersonic-now-playing--insert-button "|◀◀" #'supersonic-prev-track)
            (insert "  ")
            (supersonic-now-playing--insert-button
             (if paused
                 "▶"
               "⏸")
             #'supersonic-toggle-playing)
            (insert "  ")
            (supersonic-now-playing--insert-button "▶▶|" #'supersonic-skip-track)
            (insert "  ")
            (supersonic-now-playing--insert-button "◀◀" #'supersonic-seek-back)
            (insert "  ")
            (supersonic-now-playing--insert-button "▶▶" #'supersonic-seek-forward)
            (insert "\n\n")
            (supersonic-now-playing--insert-field "Title" (assoc-default "title" song))
            (supersonic-now-playing--insert-field "Artist" (assoc-default "artist" song))
            (supersonic-now-playing--insert-field "Album" (assoc-default "album" song))
            (supersonic-now-playing--insert-field "Duration" (supersonic-now-playing--position position duration)
                                                  'duration)
            (supersonic-now-playing--insert-field "Format" (supersonic-now-playing--format song))
            (supersonic-now-playing--insert-field "Size" (and size (format "%.2f MB" (/ size 1048576.0))))
            ;; Paint whatever `supersonic-now-playing--field-index' points at
            ;; right away rather than leaving the label (or the art overlay)
            ;; blank until the first animation tick, which is up to
            ;; `supersonic-now-playing-animation-interval' seconds away --
            ;; this runs whether or not cycling is even enabled, since a
            ;; pinned-to-the-title display still needs painting once.
            (funcall supersonic-now-playing-animation-function buff (supersonic-now-playing--current-field))))
        (goto-char (point-min))
        ;; `erase-buffer' above took the seekbar image with it.  If this
        ;; was a re-render of a track whose envelope is already in hand,
        ;; put it straight back instead of leaving a gap there until the
        ;; next tick or progress report fills it in again.
        (when supersonic-now-playing--waveform
          (supersonic-now-playing--show-waveform buff track-id (cdr supersonic-now-playing--waveform)))))))

(aio-defun
 supersonic-now-playing-fetch-and-render (buff)
 "Query the active backend for its current track and render it into BUFF.
Tolerates a failing metadata lookup the way `supersonic-queue-parse'
does: rather than blanking a view that refreshes on every track change,
it falls back to showing the bare track id.

Wrapped in `supersonic--with-async-error-handling' because no caller
awaits the promise this returns -- they all `ignore' it, this being a
render nothing waits on.  Anything signalled in here (a missing
authinfo entry, so no URL to build; a render bug) would otherwise
reject a promise nobody is listening to and disappear without a trace,
leaving nothing behind but a buffer that quietly stopped updating."
 (supersonic--with-async-error-handling
  buff "refresh the now-playing buffer"
  (if (supersonic-playback-live-p)
      (let ((track-id (aio-await (supersonic-playback-status 'track-id))))
        (if track-id
            (let* ((outcome
                    (aio-await
                     (aio-catch (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,track-id)))))))
                   (song
                    (if (eq (car outcome) :success)
                        (supersonic-recursive-assoc (cdr outcome) '("subsonic-response" "song"))
                      `(("title" . ,track-id)))))
              (when (and (supersonic-art-available-p) (assoc-default "coverArt" song))
                (aio-await
                 (aio-catch (supersonic--fetch-art (assoc-default "coverArt" song) supersonic-now-playing-art-size))))
              ;; Fired concurrently and asked for last, so both are as fresh as
              ;; possible: the art fetch above can take a while on a cold cache.
              (let* ((position-promise (supersonic-playback-status 'position))
                     (paused-promise (supersonic-playback-status 'paused))
                     (position (aio-await position-promise))
                     (paused (aio-await paused-promise)))
                (supersonic-now-playing--render buff song paused position track-id)
                (supersonic-now-playing--maybe-fetch-waveform buff track-id)))
          (supersonic-now-playing--render buff nil nil nil nil)))
    (supersonic-now-playing--render buff nil nil nil nil))))

(defun supersonic-now-playing--show-waveform (buff track-id envelope)
  "Patch BUFF's waveform field to display ENVELOPE for TRACK-ID, if still current.
Shared by `supersonic-waveform-ensure''s final callback, its progress
callback -- see `supersonic-now-playing--maybe-fetch-waveform', so the
seekbar fills in gradually as buckets finish analyzing instead of only
popping in once the whole track is done -- and
`supersonic-now-playing--render' putting an already-analyzed seekbar
back after an `erase-buffer'.

Takes the played/unplayed split from `supersonic-now-playing--position'
rather than from an argument: on a cold cache this runs a whole
transcode after the render that asked for it, by which point any
position that render could have passed along is stale."
  (when (and envelope (buffer-live-p buff))
    (with-current-buffer buff
      ;; The buffer may have moved on to a different track by the time a
      ;; full-track transcode finishes; discard a now-stale result instead
      ;; of showing another track's waveform under this one.
      (when (equal track-id supersonic-now-playing--track-id)
        (setq supersonic-now-playing--waveform (cons track-id envelope))
        (let ((progress
               (supersonic-now-playing--progress-ratio
                supersonic-now-playing--position supersonic-now-playing--duration)))
          (setq supersonic-now-playing--waveform-bucket (supersonic-now-playing--progress-bucket envelope progress))
          (supersonic-now-playing--update-field buff 'waveform (supersonic-waveform-propertize envelope progress)))))))

(defun supersonic-now-playing--maybe-fetch-waveform (buff track-id)
  "Kick off waveform generation for TRACK-ID and patch it into BUFF.
No-op unless `supersonic-waveform-available-p' and BUFF is actually on
display -- a full-track transcode is real CPU and network work, not
worth spending on a buffer nobody is looking at (see
`supersonic-now-playing--tick' for how a buffer that becomes visible
again still gets one).  Fires and forgets rather than being awaited by
the caller, so a cold-cache waveform never delays the rest of the
buffer from appearing.  The seekbar fills in progressively, bucket by
bucket, rather than only appearing once the whole track has been
analyzed -- see `supersonic-waveform-ensure''s PROGRESS-CALLBACK.

Safe to call repeatedly for the track already being generated, which
is what every re-render does: `supersonic-waveform-ensure' recognizes
that as the job it is already running and just re-points it (see
`supersonic-waveform--job')."
  (when (and (supersonic-waveform-available-p) track-id (buffer-live-p buff) (get-buffer-window buff t))
    (with-current-buffer buff
      (setq supersonic-now-playing--waveform-requested t))
    (supersonic-waveform-ensure
     track-id
     (lambda (envelope) (supersonic-now-playing--show-waveform buff track-id envelope))
     (lambda (envelope) (supersonic-now-playing--show-waveform buff track-id envelope)))))

(defun supersonic-now-playing-refresh ()
  "Refresh the now-playing buffer from the active backend's current state."
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
    map)
  "Keymap for `supersonic-now-playing-mode'.")

(define-derived-mode
 supersonic-now-playing-mode
 special-mode
 "Supersonic Now Playing"
 "Major mode for the buffer opened by `supersonic-show-now-playing'.")

;;;###autoload
(defun supersonic-show-now-playing ()
  "Open a buffer showing the track the active backend is currently on.
The buffer follows the active backend on its own -- track changes,
pausing and resuming are all reflected without a manual refresh, the
same way the play queue buffer keeps itself current."
  (interactive)
  (let ((buff (get-buffer-create supersonic-now-playing-buffer-name)))
    (with-current-buffer buff
      (unless (derived-mode-p 'supersonic-now-playing-mode)
        (supersonic-now-playing-mode)))
    (ignore (supersonic-now-playing-fetch-and-render buff))
    (pop-to-buffer-same-window buff)))

(defun supersonic--format-duration (format seconds)
  "Format SECONDS with `format-seconds' FORMAT, or \"\" if there is no duration.
The Subsonic API marks a song's/episode's \"duration\" optional and
really does leave it out -- a podcast episode that hasn't been
downloaded yet typically has none.  `format-seconds' signals a
`wrong-type-argument' on nil, and since these are formatted inside a
`mapcar' over a whole response, one such row used to take every row of
the list buffer down with it."
  (if seconds
      (format-seconds format seconds)
    ""))

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
      (supersonic-playback-start (list (car result)))))))

(defun supersonic-open-search-result ()
  "Open a view of the result from the result at point."
  (interactive)
  (supersonic-open-search-appropriate-result (tabulated-list-get-id)))

(defvar supersonic-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-search-result)
    map)
  "Keymap for `supersonic-search-mode'.")

;;;###autoload
(defun supersonic-search ()
  "List supersonic search results."
  (interactive)
  (let ((new-buff (get-buffer-create "*supersonic-search*")))
    (supersonic--init-list-buffer new-buff #'supersonic-search-mode "Searching...")
    (ignore (supersonic-search-refresh (read-string "Query: ") new-buff))
    (pop-to-buffer-same-window new-buff)))

(define-derived-mode
 supersonic-search-mode
 tabulated-list-mode
 "Supersonic search mode"
 "Major mode for the result list opened by `supersonic-search'.
Each row is an artist, album or song; opening one dispatches on which."
 ;;  type: artist|album|track
 (setq tabulated-list-format [("Type" 10 t) ("Name" 30 t)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))


;;;
;;; Tracks
;;;

(defun supersonic-get-tracklist-id (id)
  "Return the ids of the tracks from ID to the end of the current list.
Plays/enqueues \"this one and everything after it\", which is what both
`supersonic-play-tracks' and `supersonic-enqueue-tracks' mean by the
entry at point.  An ID that isn't in the list at all yields nil."
  (mapcar #'car (seq-drop-while (lambda (entry) (not (equal (car entry) id))) tabulated-list-entries)))

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
                 (supersonic--format-duration "%m:%.2s" duration)
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
  (supersonic-playback-start (supersonic-get-tracklist-id (tabulated-list-get-id))))

(defun supersonic-enqueue-tracks ()
  "Add all the tracks after the point in the list to the play queue."
  (interactive)
  (let ((ids (supersonic-get-tracklist-id (tabulated-list-get-id))))
    (supersonic-playback-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids))))

(defvar supersonic-tracks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-play-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-tracks)
    map)
  "Keymap for `supersonic-tracks-mode'.")

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
 "Major mode for the track list of one album, opened by `supersonic-tracks'."
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
    (supersonic-playback-enqueue ids)
    (message "Added %d track(s) to the queue" (length ids)))))

(defvar supersonic-album-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map)
  "Keymap for `supersonic-album-mode'.")

(defvar supersonic-album-type-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'supersonic-open-tracks)
    (define-key map (kbd "a") #'supersonic-enqueue-album)
    map)
  "Keymap for `supersonic-album-type-mode'.")

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
 "Major mode for a list of albums across artists (newest, random, ...)."
 (setq tabulated-list-format [("Albums" 30 t) ("Artists" 30 t) ("Art" 30 nil)])
 (setq tabulated-list-padding 2)
 (tabulated-list-init-header))

(define-derived-mode
 supersonic-album-mode
 tabulated-list-mode
 "Supersonic Albums"
 "Major mode for the album list of one artist, opened by `supersonic-albums'."
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
    map)
  "Keymap for `supersonic-artist-mode'.")

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
 "Major mode for the artist list opened by `supersonic-artists'."
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
   (supersonic-get-json (supersonic-build-url "/createPodcastChannel.view" `(("url" . ,(read-string "feed url: "))))))
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
    map)
  "Keymap for `supersonic-podcast-mode'.")

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
 "Major mode for the podcast channel list opened by `supersonic-podcasts'."
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
               (supersonic--format-duration "%h:%.2m:%.2s" (assoc-default "duration" episode))
               (assoc-default "status" episode))))
           episodes)))
    result))

(defun supersonic-play-podcast ()
  "Play a podcast episode at point."
  (interactive)
  (supersonic-playback-start (list (tabulated-list-get-id))))

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
    map)
  "Keymap for `supersonic-podcast-episodes-mode'.")

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
 "Major mode for one podcast channel's episodes.
Opened by `supersonic-podcast-episodes'."
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
   ("B" "Seek back" supersonic-seek-back :transient t)
   ("k" "Switch backend" supersonic-playback-switch-backend)]])

(defun supersonic-unload-function ()
  "Undo the `supersonic-playback.el' hook entries this file adds at load time.
Called by `unload-feature', which would otherwise leave those hooks
holding references to functions that no longer exist.  Returns nil so
`unload-feature' still goes on to remove the definitions itself."
  (remove-hook 'supersonic-playback-track-change-hook #'supersonic-queue-maybe-refresh)
  (remove-hook 'supersonic-playback-track-change-hook #'supersonic-now-playing-maybe-refresh)
  (remove-hook 'supersonic-playback-state-change-hook #'supersonic-now-playing-maybe-refresh)
  (remove-hook 'supersonic-playback-position-change-hook #'supersonic-now-playing-maybe-update-position)
  nil)

(provide 'supersonic)

;;; supersonic.el ends here
