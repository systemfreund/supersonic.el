;;; supersonic-mpris.el --- MPRIS (D-Bus) remote control for supersonic.el  -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
;; URL: https://github.com/systemfreund/supersonic.el
;; Version: 0.1.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "27.1") (supersonic "0.2.0") (aio "1.0"))

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

;; Optional MPRIS support for supersonic.el, so that desktop environments
;; and tools such as playerctl can see and control playback over D-Bus.
;;
;; This file is deliberately one-directional: supersonic.el has no
;; knowledge of it.  It observes and drives supersonic.el purely from the
;; outside, via `advice-add' on its public/internal entry points, so it
;; can be dropped in or removed without touching supersonic.el at all.
;;
;; Enable it explicitly, it is never loaded or activated as a side
;; effect of requiring `supersonic':
;;
;;   (require 'supersonic-mpris)
;;   (supersonic-mpris-mode 1)
;;
;; Scope: this only implements Play/Pause/PlayPause/Stop/Next/Previous
;; and Metadata.  Seek/SetPosition, Volume, LoopStatus, Shuffle and Rate
;; are intentionally not implemented.

;;; Code:

(require 'dbus)
(require 'supersonic)
(require 'aio)

;; fix byte-compiler complaints, as supersonic.el does for the same variable
(defvar url-http-end-of-headers)

(defgroup supersonic-mpris nil
  "MPRIS (D-Bus) remote control support for supersonic.el."
  :prefix "supersonic-mpris-"
  :group 'supersonic)

(defconst supersonic-mpris--bus-name "org.mpris.MediaPlayer2.supersonic"
  "The well-known D-Bus name we register on the session bus.")

(defconst supersonic-mpris--path "/org/mpris/MediaPlayer2"
  "The MPRIS object path, fixed by the spec.")

(defconst supersonic-mpris--root-interface "org.mpris.MediaPlayer2")

(defconst supersonic-mpris--player-interface "org.mpris.MediaPlayer2.Player")

(defconst supersonic-mpris--introspection-xml
  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
 \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
<node>
  <interface name=\"org.freedesktop.DBus.Introspectable\">
    <method name=\"Introspect\">
      <arg name=\"xml_data\" type=\"s\" direction=\"out\"/>
    </method>
  </interface>
  <interface name=\"org.freedesktop.DBus.Properties\">
    <method name=\"Get\">
      <arg name=\"interface_name\" type=\"s\" direction=\"in\"/>
      <arg name=\"property_name\" type=\"s\" direction=\"in\"/>
      <arg name=\"value\" type=\"v\" direction=\"out\"/>
    </method>
    <method name=\"GetAll\">
      <arg name=\"interface_name\" type=\"s\" direction=\"in\"/>
      <arg name=\"properties\" type=\"a{sv}\" direction=\"out\"/>
    </method>
    <method name=\"Set\">
      <arg name=\"interface_name\" type=\"s\" direction=\"in\"/>
      <arg name=\"property_name\" type=\"s\" direction=\"in\"/>
      <arg name=\"value\" type=\"v\" direction=\"in\"/>
    </method>
    <signal name=\"PropertiesChanged\">
      <arg name=\"interface_name\" type=\"s\"/>
      <arg name=\"changed_properties\" type=\"a{sv}\"/>
      <arg name=\"invalidated_properties\" type=\"as\"/>
    </signal>
  </interface>
  <interface name=\"org.mpris.MediaPlayer2\">
    <method name=\"Raise\"/>
    <method name=\"Quit\"/>
    <property name=\"CanQuit\" type=\"b\" access=\"read\"/>
    <property name=\"CanRaise\" type=\"b\" access=\"read\"/>
    <property name=\"HasTrackList\" type=\"b\" access=\"read\"/>
    <property name=\"Identity\" type=\"s\" access=\"read\"/>
    <property name=\"DesktopEntry\" type=\"s\" access=\"read\"/>
    <property name=\"SupportedUriSchemes\" type=\"as\" access=\"read\"/>
    <property name=\"SupportedMimeTypes\" type=\"as\" access=\"read\"/>
  </interface>
  <interface name=\"org.mpris.MediaPlayer2.Player\">
    <method name=\"Play\"/>
    <method name=\"Pause\"/>
    <method name=\"PlayPause\"/>
    <method name=\"Stop\"/>
    <method name=\"Next\"/>
    <method name=\"Previous\"/>
    <property name=\"PlaybackStatus\" type=\"s\" access=\"read\"/>
    <property name=\"Metadata\" type=\"a{sv}\" access=\"read\"/>
    <property name=\"CanGoNext\" type=\"b\" access=\"read\"/>
    <property name=\"CanGoPrevious\" type=\"b\" access=\"read\"/>
    <property name=\"CanPlay\" type=\"b\" access=\"read\"/>
    <property name=\"CanPause\" type=\"b\" access=\"read\"/>
    <property name=\"CanSeek\" type=\"b\" access=\"read\"/>
    <property name=\"CanControl\" type=\"b\" access=\"read\"/>
  </interface>
</node>"
  "Static introspection XML.

`dbus.el' does not answer Introspectable.Introspect on our behalf for
objects we register as a service, but several MPRIS clients (notably
playerctl) call it before trusting a player, so we answer it
ourselves.")

(defvar supersonic-mpris--registrations nil
  "D-Bus registration objects to tear down when the mode is disabled.")

(defvar supersonic-mpris--playback-status "Stopped"
  "Current MPRIS PlaybackStatus: \"Playing\", \"Paused\" or \"Stopped\".")

(defvar supersonic-mpris--track-index nil
  "1-based index (mpv's playlist_entry_id) of the current track, or nil.")

(defvar supersonic-mpris--pause-observed nil
  "Non-nil once `observe_property' has been registered on the running mpv.
`supersonic-mpv-start' (\"Replace\") may now run against an mpv instance
that is already alive rather than a freshly spawned one, so this guards
against re-registering the same observer id on every Replace, which
would otherwise fire duplicate PropertiesChanged signals per pause
toggle.  Reset by `supersonic-mpris--after-mpv-kill' whenever mpv actually
goes away.")

(defvar supersonic-mpris--track-song nil
  "Parsed \"song\" alist (as returned by getSong.view) for the current track.")

(defvar supersonic-mpris--art-file nil
  "Path to the cached art file for the current track, or nil.")

;;;
;;; Metadata dict construction
;;;

(defun supersonic-mpris--current-track-id ()
  "Return the supersonic id of the current track, or nil."
  (and supersonic-mpris--track-index
       (gethash supersonic-mpris--track-index supersonic--playlist)))

(defun supersonic-mpris--track-object-path (id)
  "Build a valid D-Bus object path for track ID."
  (if id
      (concat "/org/mpris/MediaPlayer2/Track/"
              (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" id))
    "/org/mpris/MediaPlayer2/TrackList/NoTrack"))

(defun supersonic-mpris--metadata ()
  "Build the MPRIS Metadata dict-entry list (\"a{sv}\") for the current track."
  (let* ((id (supersonic-mpris--current-track-id))
         (song supersonic-mpris--track-song)
         (title (and song (assoc-default "title" song)))
         (album (and song (assoc-default "album" song)))
         (artist (and song (assoc-default "artist" song)))
         (duration (and song (assoc-default "duration" song))))
    (cons
     :array
     (delq
      nil
      (list
       (list :dict-entry "mpris:trackid"
             (list :variant :object-path (supersonic-mpris--track-object-path id)))
       (when title
         (list :dict-entry "xesam:title" (list :variant title)))
       (when album
         (list :dict-entry "xesam:album" (list :variant album)))
       (when artist
         (list :dict-entry "xesam:artist" (list :variant (list :array artist))))
       (when duration
         (list :dict-entry "mpris:length" (list :variant :int64 (* duration 1000000))))
       (when supersonic-mpris--art-file
         (list :dict-entry "mpris:artUrl"
               (list :variant (concat "file://" supersonic-mpris--art-file)))))))))

(defun supersonic-mpris--set-player-property (property value)
  "Set PROPERTY on the Player interface to VALUE and notify listeners.
This simply re-registers the property; `dbus-register-property'
overwrites the previous value and, with EMITS-SIGNAL, takes care of
sending PropertiesChanged itself."
  (dbus-register-property
   :session supersonic-mpris--bus-name supersonic-mpris--path
   supersonic-mpris--player-interface property :read value t))

(defun supersonic-mpris--announce-metadata ()
  "Push the current Metadata dict out over D-Bus."
  (supersonic-mpris--set-player-property "Metadata" (supersonic-mpris--metadata)))

;;;
;;; Reacting to supersonic.el's mpv process, without supersonic.el knowing
;;;

(aio-defun supersonic-mpris--fetch-song (id)
  "Fetch and cache metadata + art for track ID, then re-announce Metadata."
  (condition-case err
      (let* ((data (aio-await (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,id))))))
             (song (supersonic-recursive-assoc data '("subsonic-response" "song"))))
        ;; Ignore replies for a track we have since moved on from.
        (when (equal id (supersonic-mpris--current-track-id))
          (setq supersonic-mpris--track-song song)
          (supersonic-mpris--announce-metadata)
          (when (and supersonic-enable-art song (assoc-default "coverArt" song))
            (supersonic-mpris--fetch-art id (assoc-default "coverArt" song)))))
    (error (message "supersonic-mpris: failed to fetch metadata for %s: %s" id err))))

(defun supersonic-mpris--fetch-art (id art-id)
  "Download cover art ART-ID for track ID into the shared art cache.
Re-announces Metadata once the art is available, unless ID is no
longer the current track."
  (let ((file (expand-file-name art-id supersonic-art-cache-path)))
    (if (file-exists-p file)
        (when (equal id (supersonic-mpris--current-track-id))
          (setq supersonic-mpris--art-file file)
          (supersonic-mpris--announce-metadata))
      (unless (file-exists-p supersonic-art-cache-path)
        (mkdir supersonic-art-cache-path))
      (url-retrieve
       (supersonic-build-url "/getCoverArt.view"
                            `(("id" . ,art-id) ("size" . ,(int-to-string supersonic-art-size))))
       (lambda (status)
         (when (and (not (plist-get status :error)) (equal id (supersonic-mpris--current-track-id)))
           (write-region (+ url-http-end-of-headers 1) (point-max) file nil 'no-message)
           (setq supersonic-mpris--art-file file)
           (supersonic-mpris--announce-metadata)))))))

(defun supersonic-mpris--set-track (index)
  "Record INDEX (mpv's 1-based playlist_entry_id) as the current track."
  (setq supersonic-mpris--track-index index)
  (setq supersonic-mpris--track-song nil)
  (setq supersonic-mpris--art-file nil)
  (supersonic-mpris--announce-metadata)
  (let ((id (supersonic-mpris--current-track-id)))
    (when id
      ;; Defer off the process filter so a slow HTTP request never blocks
      ;; the mpv IPC socket.
      (run-at-time 0 nil #'supersonic-mpris--fetch-song id))))

(defun supersonic-mpris--set-playback-status (status)
  "Record STATUS (\"Playing\", \"Paused\" or \"Stopped\") and announce it."
  (unless (equal status supersonic-mpris--playback-status)
    (setq supersonic-mpris--playback-status status)
    (supersonic-mpris--set-player-property "PlaybackStatus" status)))

(defun supersonic-mpris--socket-filter (_process output)
  "Watch mpv's own OUTPUT (advice on `supersonic--mpv-socket-filter') for events."
  (dolist (line (split-string output "\n" t))
    (let ((parsed (ignore-errors (json-read-from-string line))))
      (when parsed
        (let ((event (alist-get 'event parsed)))
          (cond
           ((member event '("start-file" "end-file"))
            (supersonic-mpris--set-track (alist-get 'playlist_entry_id parsed)))
           ((and (string-equal event "property-change")
                 (string-equal (alist-get 'name parsed) "pause"))
            (supersonic-mpris--set-playback-status
             (if (eq (alist-get 'data parsed) t) "Paused" "Playing")))))))))

(defun supersonic-mpris--after-mpv-start (&rest _)
  "Advice: after `supersonic-mpv-start', observe mpv's pause state."
  (setq supersonic-mpris--track-index nil)
  (unless supersonic-mpris--pause-observed
    (supersonic-mpv-command "observe_property" 1 "pause")
    (setq supersonic-mpris--pause-observed t))
  (supersonic-mpris--set-playback-status "Playing"))

(defun supersonic-mpris--around-mpv-enqueue (orig-fn &rest args)
  "Advice: run `supersonic-mpv-enqueue' via ORIG-FN with ARGS.
`supersonic-mpv-enqueue' can, like `supersonic-mpv-start', be the first
action that brings mpv up from a dead/idle state (if mpv was not
already running, its \"append-play\" load starts playback right away).
Only in that case do we need to react the same way we do after
`supersonic-mpv-start'; if mpv was already alive, playback was either
already running or intentionally paused, and enqueuing more tracks
must not disturb that."
  (let ((was-live (supersonic-mpv-live-p)))
    (prog1 (apply orig-fn args)
      (unless was-live
        (supersonic-mpris--after-mpv-start)))))

(defun supersonic-mpris--after-mpv-kill (&rest _)
  "Advice: after `supersonic-mpv-kill', reflect the stopped state."
  (setq supersonic-mpris--track-index nil)
  (setq supersonic-mpris--track-song nil)
  (setq supersonic-mpris--art-file nil)
  (setq supersonic-mpris--pause-observed nil)
  (supersonic-mpris--set-playback-status "Stopped")
  (supersonic-mpris--announce-metadata))

;;;
;;; org.mpris.MediaPlayer2 (root interface)
;;;

(defun supersonic-mpris--quit ()
  "Handle the MPRIS Quit method: stop mpv, never Emacs."
  (when (supersonic-mpv-live-p)
    (supersonic-mpv-kill)))

;;;
;;; org.mpris.MediaPlayer2.Player
;;;

(defun supersonic-mpris--play ()
  "Handle the MPRIS Play method."
  (if (supersonic-mpv-live-p)
      (supersonic-mpv-command "set_property" "pause" :false)
    (message "supersonic-mpris: nothing to play, start playback from Emacs first")))

(defun supersonic-mpris--pause ()
  "Handle the MPRIS Pause method."
  (when (supersonic-mpv-live-p)
    (supersonic-mpv-command "set_property" "pause" t)))

(defun supersonic-mpris--play-pause ()
  "Handle the MPRIS PlayPause method."
  (if (supersonic-mpv-live-p)
      (supersonic-toggle-playing)
    (message "supersonic-mpris: nothing to play, start playback from Emacs first")))

(defun supersonic-mpris--stop ()
  "Handle the MPRIS Stop method."
  (when (supersonic-mpv-live-p)
    (supersonic-mpv-kill)))

(defun supersonic-mpris--next ()
  "Handle the MPRIS Next method."
  (when (supersonic-mpv-live-p)
    (supersonic-skip-track)))

(defun supersonic-mpris--previous ()
  "Handle the MPRIS Previous method."
  (when (supersonic-mpv-live-p)
    (supersonic-prev-track)))

;;;
;;; Registration
;;;

(defun supersonic-mpris--register-method (interface method handler)
  "Register METHOD on INTERFACE, calling HANDLER, and track it for teardown.
All the methods we implement (Raise/Quit/Play/Pause/.../Previous) have no
D-Bus return value, so the wrapper discards HANDLER's own Lisp return value
and reports `:ignore', which `dbus-handle-event' requires for an empty
reply -- without it, HANDLER's return value would get sent back as a
bogus reply argument, tripping up strict clients such as playerctl."
  (push (dbus-register-method
         :session supersonic-mpris--bus-name supersonic-mpris--path
         interface method (lambda (&rest _args) (funcall handler) :ignore) t)
        supersonic-mpris--registrations))

(defun supersonic-mpris--register-fixed-property (interface property value)
  "Register PROPERTY on INTERFACE with a fixed, never-changing VALUE."
  (push (dbus-register-property
         :session supersonic-mpris--bus-name supersonic-mpris--path
         interface property :read value)
        supersonic-mpris--registrations))

(defun supersonic-mpris--register ()
  "Register the MPRIS D-Bus service and its interfaces."
  (unless (memq (dbus-register-service :session supersonic-mpris--bus-name :do-not-queue)
                '(:primary-owner :already-owner))
    (user-error "Could not acquire %s (already running elsewhere?)"
                supersonic-mpris--bus-name))
  (supersonic-mpris--register-method
   dbus-interface-introspectable "Introspect"
   (lambda () supersonic-mpris--introspection-xml))
  ;; Root interface: methods.
  (supersonic-mpris--register-method supersonic-mpris--root-interface "Raise" #'ignore)
  (supersonic-mpris--register-method supersonic-mpris--root-interface "Quit" #'supersonic-mpris--quit)
  ;; Root interface: properties, all fixed for the lifetime of the service.
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "CanQuit" t)
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "CanRaise" nil)
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "HasTrackList" nil)
  (supersonic-mpris--register-fixed-property
   supersonic-mpris--root-interface "Identity" "supersonic.el")
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "DesktopEntry" "")
  (supersonic-mpris--register-fixed-property
   supersonic-mpris--root-interface "SupportedUriSchemes" '(:array :signature "as"))
  (supersonic-mpris--register-fixed-property
   supersonic-mpris--root-interface "SupportedMimeTypes" '(:array :signature "as"))
  ;; Player interface: methods.
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Play" #'supersonic-mpris--play)
  (supersonic-mpris--register-method
   supersonic-mpris--player-interface "Pause" #'supersonic-mpris--pause)
  (supersonic-mpris--register-method
   supersonic-mpris--player-interface "PlayPause" #'supersonic-mpris--play-pause)
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Stop" #'supersonic-mpris--stop)
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Next" #'supersonic-mpris--next)
  (supersonic-mpris--register-method
   supersonic-mpris--player-interface "Previous" #'supersonic-mpris--previous)
  ;; Player interface: properties.  PlaybackStatus and Metadata are kept
  ;; current via `supersonic-mpris--set-player-property'; the Can* flags are
  ;; fixed for the reduced v1 scope (no seeking, no track list).
  (push (dbus-register-property
         :session supersonic-mpris--bus-name supersonic-mpris--path
         supersonic-mpris--player-interface "PlaybackStatus" :read
         supersonic-mpris--playback-status)
        supersonic-mpris--registrations)
  (push (dbus-register-property
         :session supersonic-mpris--bus-name supersonic-mpris--path
         supersonic-mpris--player-interface "Metadata" :read (supersonic-mpris--metadata))
        supersonic-mpris--registrations)
  (dolist (prop '("CanGoNext" "CanGoPrevious" "CanPlay" "CanPause" "CanControl"))
    (supersonic-mpris--register-fixed-property supersonic-mpris--player-interface prop t))
  (supersonic-mpris--register-fixed-property supersonic-mpris--player-interface "CanSeek" nil)
  ;; Drive the interface from supersonic.el's own mpv process, without
  ;; supersonic.el needing to know we exist.
  (advice-add 'supersonic--mpv-socket-filter :after #'supersonic-mpris--socket-filter)
  (advice-add 'supersonic-mpv-start :after #'supersonic-mpris--after-mpv-start)
  (advice-add 'supersonic-mpv-enqueue :around #'supersonic-mpris--around-mpv-enqueue)
  (advice-add 'supersonic-mpv-kill :after #'supersonic-mpris--after-mpv-kill))

(defun supersonic-mpris--unregister ()
  "Tear down the MPRIS D-Bus service and stop observing supersonic.el."
  (advice-remove 'supersonic--mpv-socket-filter #'supersonic-mpris--socket-filter)
  (advice-remove 'supersonic-mpv-start #'supersonic-mpris--after-mpv-start)
  (advice-remove 'supersonic-mpv-enqueue #'supersonic-mpris--around-mpv-enqueue)
  (advice-remove 'supersonic-mpv-kill #'supersonic-mpris--after-mpv-kill)
  (dolist (registration supersonic-mpris--registrations)
    (dbus-unregister-object registration))
  (setq supersonic-mpris--registrations nil)
  (dbus-unregister-service :session supersonic-mpris--bus-name)
  (setq supersonic-mpris--playback-status "Stopped")
  (setq supersonic-mpris--track-index nil)
  (setq supersonic-mpris--track-song nil)
  (setq supersonic-mpris--art-file nil))

;;;###autoload
(define-minor-mode supersonic-mpris-mode
  "Expose supersonic.el's mpv playback as an MPRIS player over D-Bus.

Once enabled, desktop environments and tools such as playerctl can see
supersonic.el under the name `org.mpris.MediaPlayer2.supersonic' on the
session bus, and use it to Play/Pause/Stop/Next/Previous and read
Metadata (title/artist/album/art/length).  Seeking, volume, shuffle and
loop control are intentionally out of scope.

This is a global mode with no association to any particular buffer."
  :global t
  :group 'supersonic-mpris
  (if supersonic-mpris-mode
      (condition-case err
          (supersonic-mpris--register)
        (error
         ;; Release whatever we managed to register (including the bus
         ;; name itself, if we got that far) before giving up.
         (supersonic-mpris--unregister)
         (setq supersonic-mpris-mode nil)
         (signal (car err) (cdr err))))
    (supersonic-mpris--unregister)))

(provide 'supersonic-mpris)

;;; supersonic-mpris.el ends here
