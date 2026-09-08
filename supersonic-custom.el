;;; supersonic-custom.el --- User options for supersonic.el -*- lexical-binding: t; -*-

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

;; Every user option supersonic.el has, in one place, plus the
;; `supersonic' customization group they all belong to.
;;
;; They live here rather than each in the file that reads them because
;; several are read by more than one: `supersonic-cache-path' by both
;; the cover art and the waveform cache, `supersonic-mpv' by both the
;; playback backend and the waveform transcoder (which is otherwise
;; deliberately independent of that backend -- see
;; `supersonic-waveform.el').  Splitting them up would mean either
;; picking an arbitrary owner for each shared one or duplicating
;; declarations.
;;
;; This is also the file with no dependencies of its own, which every
;; other file in the package requires first.  That is the point: before
;; this file existed, the options were defined in `supersonic.el' ahead
;; of its own `require's, and the files that read them carried `defvar'
;; stubs to keep the byte-compiler quiet -- so requiring any one of them
;; on its own (a test, or a user who only wants the seekbar) left those
;; options genuinely void, with the stubs hiding it.

;;; Code:

(defgroup supersonic nil
  "Browse and play music from a Subsonic server."
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
  "Path to the mpv executable, or nil if there is none on variable `exec-path'."
  :type '(choice (const :tag "Not available" nil) (file :tag "Path to mpv"))
  :group 'supersonic)

(defcustom supersonic-default-volume 100
  "Default  volume for mpv to use."
  :type 'integer
  :group 'supersonic)

(defcustom supersonic-seek-step 10
  "Seconds `supersonic-seek-forward' and `supersonic-seek-back' jump by.
Either command also takes a numeric prefix argument, which overrides
this for that one seek."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-cache-path (expand-file-name "supersonic-cache" user-emacs-directory)
  "Path to store cached cover art and waveform peak/RMS envelopes.
Shared by both `supersonic-art-cache-file' and
`supersonic-waveform-cache-file', which prefix their file names
distinctly enough (\"art-\"/\"waveform-\") that the two never collide,
even though both are keyed on a Subsonic id that could otherwise
coincide (e.g. a track and its own cover art id)."
  :type 'directory
  :group 'supersonic)

(defcustom supersonic-enable-art nil
  "Enable displaying album art in supported frames.
Also a statement about the frame itself, since supersonic only draws
art it can draw -- see `supersonic-art-available-p'."
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

(defcustom supersonic-playback-backend 'mpv
  "Which playback backend plays what supersonic.el is asked to play.
The symbol a backend registered itself under via
`supersonic-playback-register-backend'; every play, enqueue and
transport command dispatches to it.  Only `mpv' is built in, and
selecting a backend whose file has not been loaded is reported when a
playback command is next used."
  :type '(choice (const :tag "Local mpv process" mpv) (symbol :tag "Other registered backend"))
  :group 'supersonic)

(defcustom supersonic-mpv-timeout 0.5
  "Seconds to wait when starting or killing the mpv process."
  :type 'number
  :group 'supersonic)

(defcustom supersonic-waveform-samplerate 3000
  "Sample rate in Hz mpv transcodes a track to for waveform analysis.
Every sample is walked individually in Lisp and every byte of the
transcode passes through a temp file and an Emacs buffer, so both
analysis time and peak memory scale linearly with this: an hour-long
podcast is 11 MB and 5.4 M iterations at 3000 Hz, four times that at
12000.  Only `supersonic-waveform-buckets' peak/RMS pairs come out the
far end either way, so the precision this buys is precision inside a
bucket -- a few hundred samples per bucket already pins a peak down
well.  Raising it is unlikely to be visible in the rendered seekbar;
lowering it far enough eventually flattens short transients.

The cache is keyed on this value (see `supersonic-waveform-cache-file'),
so changing it costs a re-analysis of anything already cached rather
than mixing envelopes measured at different rates."
  :type 'integer
  :group 'supersonic)

(provide 'supersonic-custom)
;;; supersonic-custom.el ends here
