;;; subsonic-mpris.el --- MPRIS (D-Bus) remote control for subsonic.el  -*- lexical-binding: t; -*-

;; Author: Alex McGrath <amk@amk.ie>
;; URL: https://git.sr.ht/~amk/subsonic.el
;; Version: 0.1.0
;; Keywords: multimedia
;; Package-Requires: ((emacs "27.1") (subsonic "0.2.0"))

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

;; Optional MPRIS support for subsonic.el, so that desktop environments
;; and tools such as playerctl can see and control playback over D-Bus.
;;
;; This file is deliberately one-directional: subsonic.el has no
;; knowledge of it.  It observes and drives subsonic.el purely from the
;; outside, via `advice-add' on its public/internal entry points, so it
;; can be dropped in or removed without touching subsonic.el at all.
;;
;; Enable it explicitly, it is never loaded or activated as a side
;; effect of requiring `subsonic':
;;
;;   (require 'subsonic-mpris)
;;   (subsonic-mpris-mode 1)
;;
;; Scope: this only implements Play/Pause/PlayPause/Stop/Next/Previous
;; and Metadata.  Seek/SetPosition, Volume, LoopStatus, Shuffle and Rate
;; are intentionally not implemented.

;;; Code:

(require 'dbus)
(require 'subsonic)

;; fix byte-compiler complaints, as subsonic.el does for the same variable
(defvar url-http-end-of-headers)

(defgroup subsonic-mpris nil
  "MPRIS (D-Bus) remote control support for subsonic.el."
  :prefix "subsonic-mpris-"
  :group 'subsonic)

(defconst subsonic-mpris--bus-name "org.mpris.MediaPlayer2.subsonic"
  "The well-known D-Bus name we register on the session bus.")

(defconst subsonic-mpris--path "/org/mpris/MediaPlayer2"
  "The MPRIS object path, fixed by the spec.")

(defconst subsonic-mpris--root-interface "org.mpris.MediaPlayer2")

(defconst subsonic-mpris--player-interface "org.mpris.MediaPlayer2.Player")

(defconst subsonic-mpris--introspection-xml
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

(defvar subsonic-mpris--registrations nil
  "D-Bus registration objects to tear down when the mode is disabled.")

(defvar subsonic-mpris--playback-status "Stopped"
  "Current MPRIS PlaybackStatus: \"Playing\", \"Paused\" or \"Stopped\".")

(defvar subsonic-mpris--track-index nil
  "1-based index (mpv's playlist_entry_id) of the current track, or nil.")

(defvar subsonic-mpris--pause-observed nil
  "Non-nil once `observe_property' has been registered on the running mpv.
`subsonic-mpv-start' (\"Replace\") may now run against an mpv instance
that is already alive rather than a freshly spawned one, so this guards
against re-registering the same observer id on every Replace, which
would otherwise fire duplicate PropertiesChanged signals per pause
toggle.  Reset by `subsonic-mpris--after-mpv-kill' whenever mpv actually
goes away.")

(defvar subsonic-mpris--track-song nil
  "Parsed \"song\" alist (as returned by getSong.view) for the current track.")

(defvar subsonic-mpris--art-file nil
  "Path to the cached art file for the current track, or nil.")

;;;
;;; Metadata dict construction
;;;

(defun subsonic-mpris--current-track-id ()
  "Return the subsonic id of the current track, or nil."
  (and subsonic-mpris--track-index
       (gethash subsonic-mpris--track-index subsonic--playlist)))

(defun subsonic-mpris--track-object-path (id)
  "Build a valid D-Bus object path for track ID."
  (if id
      (concat "/org/mpris/MediaPlayer2/Track/"
              (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" id))
    "/org/mpris/MediaPlayer2/TrackList/NoTrack"))

(defun subsonic-mpris--metadata ()
  "Build the MPRIS Metadata dict-entry list (\"a{sv}\") for the current track."
  (let* ((id (subsonic-mpris--current-track-id))
         (song subsonic-mpris--track-song)
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
             (list :variant :object-path (subsonic-mpris--track-object-path id)))
       (when title
         (list :dict-entry "xesam:title" (list :variant title)))
       (when album
         (list :dict-entry "xesam:album" (list :variant album)))
       (when artist
         (list :dict-entry "xesam:artist" (list :variant (list :array artist))))
       (when duration
         (list :dict-entry "mpris:length" (list :variant :int64 (* duration 1000000))))
       (when subsonic-mpris--art-file
         (list :dict-entry "mpris:artUrl"
               (list :variant (concat "file://" subsonic-mpris--art-file)))))))))

(defun subsonic-mpris--set-player-property (property value)
  "Set PROPERTY on the Player interface to VALUE and notify listeners.
This simply re-registers the property; `dbus-register-property'
overwrites the previous value and, with EMITS-SIGNAL, takes care of
sending PropertiesChanged itself."
  (dbus-register-property
   :session subsonic-mpris--bus-name subsonic-mpris--path
   subsonic-mpris--player-interface property :read value t))

(defun subsonic-mpris--announce-metadata ()
  "Push the current Metadata dict out over D-Bus."
  (subsonic-mpris--set-player-property "Metadata" (subsonic-mpris--metadata)))

;;;
;;; Reacting to subsonic.el's mpv process, without subsonic.el knowing
;;;

(defun subsonic-mpris--fetch-song (id)
  "Fetch and cache metadata + art for track ID, then re-announce Metadata."
  (condition-case err
      (let* ((data (subsonic-get-json (subsonic-build-url "/getSong.view" `(("id" . ,id)))))
             (song (subsonic-recursive-assoc data '("subsonic-response" "song"))))
        ;; Ignore replies for a track we have since moved on from.
        (when (equal id (subsonic-mpris--current-track-id))
          (setq subsonic-mpris--track-song song)
          (subsonic-mpris--announce-metadata)
          (when (and subsonic-enable-art song (assoc-default "coverArt" song))
            (subsonic-mpris--fetch-art id (assoc-default "coverArt" song)))))
    (error (message "subsonic-mpris: failed to fetch metadata for %s: %s" id err))))

(defun subsonic-mpris--fetch-art (id art-id)
  "Download cover art ART-ID for track ID into the shared art cache.
Re-announces Metadata once the art is available, unless ID is no
longer the current track."
  (let ((file (expand-file-name art-id subsonic-art-cache-path)))
    (if (file-exists-p file)
        (when (equal id (subsonic-mpris--current-track-id))
          (setq subsonic-mpris--art-file file)
          (subsonic-mpris--announce-metadata))
      (unless (file-exists-p subsonic-art-cache-path)
        (mkdir subsonic-art-cache-path))
      (url-retrieve
       (subsonic-build-url "/getCoverArt.view"
                            `(("id" . ,art-id) ("size" . ,(int-to-string subsonic-art-size))))
       (lambda (status)
         (when (and (not (plist-get status :error)) (equal id (subsonic-mpris--current-track-id)))
           (write-region (+ url-http-end-of-headers 1) (point-max) file)
           (setq subsonic-mpris--art-file file)
           (subsonic-mpris--announce-metadata)))))))

(defun subsonic-mpris--set-track (index)
  "Record INDEX (mpv's 1-based playlist_entry_id) as the current track."
  (setq subsonic-mpris--track-index index)
  (setq subsonic-mpris--track-song nil)
  (setq subsonic-mpris--art-file nil)
  (subsonic-mpris--announce-metadata)
  (let ((id (subsonic-mpris--current-track-id)))
    (when id
      ;; Defer off the process filter so a slow HTTP request never blocks
      ;; the mpv IPC socket.
      (run-at-time 0 nil #'subsonic-mpris--fetch-song id))))

(defun subsonic-mpris--set-playback-status (status)
  "Record STATUS (\"Playing\", \"Paused\" or \"Stopped\") and announce it."
  (unless (equal status subsonic-mpris--playback-status)
    (setq subsonic-mpris--playback-status status)
    (subsonic-mpris--set-player-property "PlaybackStatus" status)))

(defun subsonic-mpris--socket-filter (_process output)
  "Watch mpv's own OUTPUT (advice on `subsonic--mpv-socket-filter') for events."
  (dolist (line (split-string output "\n" t))
    (let ((parsed (ignore-errors (json-read-from-string line))))
      (when parsed
        (let ((event (alist-get 'event parsed)))
          (cond
           ((member event '("start-file" "end-file"))
            (subsonic-mpris--set-track (alist-get 'playlist_entry_id parsed)))
           ((and (string-equal event "property-change")
                 (string-equal (alist-get 'name parsed) "pause"))
            (subsonic-mpris--set-playback-status
             (if (eq (alist-get 'data parsed) t) "Paused" "Playing")))))))))

(defun subsonic-mpris--after-mpv-start (&rest _)
  "Advice: after `subsonic-mpv-start', observe mpv's pause state."
  (setq subsonic-mpris--track-index nil)
  (unless subsonic-mpris--pause-observed
    (subsonic-mpv-command "observe_property" 1 "pause")
    (setq subsonic-mpris--pause-observed t))
  (subsonic-mpris--set-playback-status "Playing"))

(defun subsonic-mpris--around-mpv-enqueue (orig-fn &rest args)
  "Advice: run `subsonic-mpv-enqueue' via ORIG-FN with ARGS.
`subsonic-mpv-enqueue' can, like `subsonic-mpv-start', be the first
action that brings mpv up from a dead/idle state (if mpv was not
already running, its \"append-play\" load starts playback right away).
Only in that case do we need to react the same way we do after
`subsonic-mpv-start'; if mpv was already alive, playback was either
already running or intentionally paused, and enqueuing more tracks
must not disturb that."
  (let ((was-live (subsonic-mpv-live-p)))
    (prog1 (apply orig-fn args)
      (unless was-live
        (subsonic-mpris--after-mpv-start)))))

(defun subsonic-mpris--after-mpv-kill (&rest _)
  "Advice: after `subsonic-mpv-kill', reflect the stopped state."
  (setq subsonic-mpris--track-index nil)
  (setq subsonic-mpris--track-song nil)
  (setq subsonic-mpris--art-file nil)
  (setq subsonic-mpris--pause-observed nil)
  (subsonic-mpris--set-playback-status "Stopped")
  (subsonic-mpris--announce-metadata))

;;;
;;; org.mpris.MediaPlayer2 (root interface)
;;;

(defun subsonic-mpris--quit ()
  "Handle the MPRIS Quit method: stop mpv, never Emacs."
  (when (subsonic-mpv-live-p)
    (subsonic-mpv-kill)))

;;;
;;; org.mpris.MediaPlayer2.Player
;;;

(defun subsonic-mpris--play ()
  "Handle the MPRIS Play method."
  (if (subsonic-mpv-live-p)
      (subsonic-mpv-command "set_property" "pause" :false)
    (message "subsonic-mpris: nothing to play, start playback from Emacs first")))

(defun subsonic-mpris--pause ()
  "Handle the MPRIS Pause method."
  (when (subsonic-mpv-live-p)
    (subsonic-mpv-command "set_property" "pause" t)))

(defun subsonic-mpris--play-pause ()
  "Handle the MPRIS PlayPause method."
  (if (subsonic-mpv-live-p)
      (subsonic-toggle-playing)
    (message "subsonic-mpris: nothing to play, start playback from Emacs first")))

(defun subsonic-mpris--stop ()
  "Handle the MPRIS Stop method."
  (when (subsonic-mpv-live-p)
    (subsonic-mpv-kill)))

(defun subsonic-mpris--next ()
  "Handle the MPRIS Next method."
  (when (subsonic-mpv-live-p)
    (subsonic-skip-track)))

(defun subsonic-mpris--previous ()
  "Handle the MPRIS Previous method."
  (when (subsonic-mpv-live-p)
    (subsonic-prev-track)))

;;;
;;; Registration
;;;

(defun subsonic-mpris--register-method (interface method handler)
  "Register METHOD on INTERFACE, calling HANDLER, and track it for teardown.
All the methods we implement (Raise/Quit/Play/Pause/.../Previous) have no
D-Bus return value, so the wrapper discards HANDLER's own Lisp return value
and reports `:ignore', which `dbus-handle-event' requires for an empty
reply -- without it, HANDLER's return value would get sent back as a
bogus reply argument, tripping up strict clients such as playerctl."
  (push (dbus-register-method
         :session subsonic-mpris--bus-name subsonic-mpris--path
         interface method (lambda (&rest _args) (funcall handler) :ignore) t)
        subsonic-mpris--registrations))

(defun subsonic-mpris--register-fixed-property (interface property value)
  "Register PROPERTY on INTERFACE with a fixed, never-changing VALUE."
  (push (dbus-register-property
         :session subsonic-mpris--bus-name subsonic-mpris--path
         interface property :read value)
        subsonic-mpris--registrations))

(defun subsonic-mpris--register ()
  "Register the MPRIS D-Bus service and its interfaces."
  (unless (memq (dbus-register-service :session subsonic-mpris--bus-name :do-not-queue)
                '(:primary-owner :already-owner))
    (user-error "Could not acquire %s (already running elsewhere?)"
                subsonic-mpris--bus-name))
  (subsonic-mpris--register-method
   dbus-interface-introspectable "Introspect"
   (lambda () subsonic-mpris--introspection-xml))
  ;; Root interface: methods.
  (subsonic-mpris--register-method subsonic-mpris--root-interface "Raise" #'ignore)
  (subsonic-mpris--register-method subsonic-mpris--root-interface "Quit" #'subsonic-mpris--quit)
  ;; Root interface: properties, all fixed for the lifetime of the service.
  (subsonic-mpris--register-fixed-property subsonic-mpris--root-interface "CanQuit" t)
  (subsonic-mpris--register-fixed-property subsonic-mpris--root-interface "CanRaise" nil)
  (subsonic-mpris--register-fixed-property subsonic-mpris--root-interface "HasTrackList" nil)
  (subsonic-mpris--register-fixed-property
   subsonic-mpris--root-interface "Identity" "subsonic.el")
  (subsonic-mpris--register-fixed-property subsonic-mpris--root-interface "DesktopEntry" "")
  (subsonic-mpris--register-fixed-property
   subsonic-mpris--root-interface "SupportedUriSchemes" '(:array :signature "as"))
  (subsonic-mpris--register-fixed-property
   subsonic-mpris--root-interface "SupportedMimeTypes" '(:array :signature "as"))
  ;; Player interface: methods.
  (subsonic-mpris--register-method subsonic-mpris--player-interface "Play" #'subsonic-mpris--play)
  (subsonic-mpris--register-method
   subsonic-mpris--player-interface "Pause" #'subsonic-mpris--pause)
  (subsonic-mpris--register-method
   subsonic-mpris--player-interface "PlayPause" #'subsonic-mpris--play-pause)
  (subsonic-mpris--register-method subsonic-mpris--player-interface "Stop" #'subsonic-mpris--stop)
  (subsonic-mpris--register-method subsonic-mpris--player-interface "Next" #'subsonic-mpris--next)
  (subsonic-mpris--register-method
   subsonic-mpris--player-interface "Previous" #'subsonic-mpris--previous)
  ;; Player interface: properties.  PlaybackStatus and Metadata are kept
  ;; current via `subsonic-mpris--set-player-property'; the Can* flags are
  ;; fixed for the reduced v1 scope (no seeking, no track list).
  (push (dbus-register-property
         :session subsonic-mpris--bus-name subsonic-mpris--path
         subsonic-mpris--player-interface "PlaybackStatus" :read
         subsonic-mpris--playback-status)
        subsonic-mpris--registrations)
  (push (dbus-register-property
         :session subsonic-mpris--bus-name subsonic-mpris--path
         subsonic-mpris--player-interface "Metadata" :read (subsonic-mpris--metadata))
        subsonic-mpris--registrations)
  (dolist (prop '("CanGoNext" "CanGoPrevious" "CanPlay" "CanPause" "CanControl"))
    (subsonic-mpris--register-fixed-property subsonic-mpris--player-interface prop t))
  (subsonic-mpris--register-fixed-property subsonic-mpris--player-interface "CanSeek" nil)
  ;; Drive the interface from subsonic.el's own mpv process, without
  ;; subsonic.el needing to know we exist.
  (advice-add 'subsonic--mpv-socket-filter :after #'subsonic-mpris--socket-filter)
  (advice-add 'subsonic-mpv-start :after #'subsonic-mpris--after-mpv-start)
  (advice-add 'subsonic-mpv-enqueue :around #'subsonic-mpris--around-mpv-enqueue)
  (advice-add 'subsonic-mpv-kill :after #'subsonic-mpris--after-mpv-kill))

(defun subsonic-mpris--unregister ()
  "Tear down the MPRIS D-Bus service and stop observing subsonic.el."
  (advice-remove 'subsonic--mpv-socket-filter #'subsonic-mpris--socket-filter)
  (advice-remove 'subsonic-mpv-start #'subsonic-mpris--after-mpv-start)
  (advice-remove 'subsonic-mpv-enqueue #'subsonic-mpris--around-mpv-enqueue)
  (advice-remove 'subsonic-mpv-kill #'subsonic-mpris--after-mpv-kill)
  (dolist (registration subsonic-mpris--registrations)
    (dbus-unregister-object registration))
  (setq subsonic-mpris--registrations nil)
  (dbus-unregister-service :session subsonic-mpris--bus-name)
  (setq subsonic-mpris--playback-status "Stopped")
  (setq subsonic-mpris--track-index nil)
  (setq subsonic-mpris--track-song nil)
  (setq subsonic-mpris--art-file nil))

;;;###autoload
(define-minor-mode subsonic-mpris-mode
  "Expose subsonic.el's mpv playback as an MPRIS player over D-Bus.

Once enabled, desktop environments and tools such as playerctl can see
subsonic.el under the name `org.mpris.MediaPlayer2.subsonic' on the
session bus, and use it to Play/Pause/Stop/Next/Previous and read
Metadata (title/artist/album/art/length).  Seeking, volume, shuffle and
loop control are intentionally out of scope.

This is a global mode with no association to any particular buffer."
  :global t
  :group 'subsonic-mpris
  (if subsonic-mpris-mode
      (condition-case err
          (subsonic-mpris--register)
        (error
         ;; Release whatever we managed to register (including the bus
         ;; name itself, if we got that far) before giving up.
         (subsonic-mpris--unregister)
         (setq subsonic-mpris-mode nil)
         (signal (car err) (cdr err))))
    (subsonic-mpris--unregister)))

(provide 'subsonic-mpris)

;;; subsonic-mpris.el ends here
