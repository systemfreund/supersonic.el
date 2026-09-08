;;; supersonic-mpris.el --- MPRIS (D-Bus) remote control for supersonic.el  -*- lexical-binding: t; -*-

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

;; Optional MPRIS support for supersonic.el, so that desktop environments
;; and tools such as playerctl can see and control playback over D-Bus.
;;
;; This file is deliberately one-directional: supersonic.el has no
;; knowledge of it.  It observes and drives playback purely through the
;; backend-agnostic facade in `supersonic-playback.el' -- subscribing to
;; `supersonic-playback-track-change-hook'/`supersonic-playback-state-change-hook'
;; and pulling whatever changed back through `supersonic-playback-status',
;; and issuing control through `supersonic-playback-toggle-play'/`-next'/
;; `-prev'/`-stop' -- so it behaves the same regardless of which backend
;; is active, and neither side has to know the other exists.
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
(require 'aio)
(require 'supersonic-custom)
(require 'supersonic-api)
(require 'supersonic-playback)

(defgroup supersonic-mpris nil
  "MPRIS (D-Bus) remote control support for supersonic.el."
  :prefix "supersonic-mpris-"
  :group 'supersonic)

(defconst supersonic-mpris--bus-name "org.mpris.MediaPlayer2.supersonicel"
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

(defvar supersonic-mpris--track-id nil
  "Supersonic id of the current track, in the facade's own vocabulary, or nil.
Set from `supersonic-playback-status', never from a backend's private
notion of a track -- see `supersonic-mpris--sync'.")

(defvar supersonic-mpris--track-song nil
  "Parsed \"song\" alist (as returned by getSong.view) for the current track.")

;;;
;;; Metadata dict construction
;;;

(defun supersonic-mpris--track-object-path (id)
  "Build a valid D-Bus object path for track ID."
  (if id
      (concat "/org/mpris/MediaPlayer2/Track/" (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" id))
    "/org/mpris/MediaPlayer2/TrackList/NoTrack"))

(defun supersonic-mpris--metadata ()
  "Build the MPRIS Metadata dict-entry list (\"a{sv}\") for the current track."
  (let* ((song supersonic-mpris--track-song)
         (title (and song (assoc-default "title" song)))
         (album (and song (assoc-default "album" song)))
         (artist (and song (assoc-default "artist" song)))
         (duration (and song (assoc-default "duration" song))))
    (cons
     :array
     (delq
      nil
      (list
       (list
        :dict-entry
        "mpris:trackid"
        (list :variant :object-path (supersonic-mpris--track-object-path supersonic-mpris--track-id)))
       (when title
         (list :dict-entry "xesam:title" (list :variant title)))
       (when album
         (list :dict-entry "xesam:album" (list :variant album)))
       (when artist
         (list :dict-entry "xesam:artist" (list :variant (list :array artist))))
       (when duration
         (list :dict-entry "mpris:length" (list :variant :int64 (* duration 1000000)))))))))

(defun supersonic-mpris--set-player-property (property value)
  "Set PROPERTY on the Player interface to VALUE and notify listeners.
This simply re-registers the property; `dbus-register-property'
overwrites the previous value and, with EMITS-SIGNAL, takes care of
sending PropertiesChanged itself."
  (dbus-register-property
   :session supersonic-mpris--bus-name supersonic-mpris--path supersonic-mpris--player-interface property
   :read value t))

(defun supersonic-mpris--announce-metadata ()
  "Push the current Metadata dict out over D-Bus."
  (supersonic-mpris--set-player-property "Metadata" (supersonic-mpris--metadata)))

;;;
;;; Reacting to the playback facade, without either side knowing about the other
;;;

(aio-defun
 supersonic-mpris--fetch-song (id) "Fetch metadata for track ID, then re-announce Metadata."
 (condition-case err
     (let* ((data (aio-await (supersonic-get-json (supersonic-build-url "/getSong.view" `(("id" . ,id))))))
            (song (supersonic-recursive-assoc data '("subsonic-response" "song"))))
       ;; Ignore replies for a track we have since moved on from.
       (when (equal id supersonic-mpris--track-id)
         (setq supersonic-mpris--track-song song)
         (supersonic-mpris--announce-metadata)))
   (error
    (message "supersonic-mpris: failed to fetch metadata for %s: %s" id err))))

(defun supersonic-mpris--set-playback-status (status)
  "Record STATUS (\"Playing\", \"Paused\" or \"Stopped\") and announce it."
  (unless (equal status supersonic-mpris--playback-status)
    (setq supersonic-mpris--playback-status status)
    (supersonic-mpris--set-player-property "PlaybackStatus" status)))

(aio-defun
 supersonic-mpris--sync ()
 "Pull the active backend's current track and play state and announce them.
Hung off both `supersonic-playback-track-change-hook' and
`supersonic-playback-state-change-hook' -- neither carries a payload,
only the fact that something may be different now, so this simply asks
the facade what is true right now (via `supersonic-playback-live-p' and
`supersonic-playback-status') and pushes whatever changed out over
D-Bus.  One handler for both hooks rather than one apiece: a track
change can flip Playing/Paused just as well as a bare pause toggle can
\(mpv unpauses itself as it loads a fresh queue\), so either hook firing
has to be able to update either half of what MPRIS reports."
 (if (supersonic-playback-live-p)
     (let* ((id-promise (supersonic-playback-status 'track-id))
            (paused-promise (supersonic-playback-status 'paused))
            (id (aio-await id-promise))
            (paused (aio-await paused-promise)))
       (supersonic-mpris--set-playback-status
        (if paused
            "Paused"
          "Playing"))
       (unless (equal id supersonic-mpris--track-id)
         (setq supersonic-mpris--track-id id)
         (setq supersonic-mpris--track-song nil)
         (supersonic-mpris--announce-metadata)
         (when id
           ;; Defer off whatever called us -- for the mpv backend, its own
           ;; IPC process filter -- so a slow HTTP request never blocks it.
           (run-at-time 0 nil #'supersonic-mpris--fetch-song id))))
   (supersonic-mpris--set-playback-status "Stopped")
   (when supersonic-mpris--track-id
     (setq supersonic-mpris--track-id nil)
     (setq supersonic-mpris--track-song nil)
     (supersonic-mpris--announce-metadata))))

;;;
;;; org.mpris.MediaPlayer2 (root interface)
;;;

(defun supersonic-mpris--quit ()
  "Handle the MPRIS Quit method: stop playback, never Emacs."
  (when (supersonic-playback-live-p)
    (supersonic-playback-stop)))

;;;
;;; org.mpris.MediaPlayer2.Player
;;;

(aio-defun
 supersonic-mpris--play ()
 "Handle the MPRIS Play method.
Play always means \"resume\", never \"toggle\", but the facade only
exposes `supersonic-playback-toggle-play' -- so this asks whether
playback is actually paused first and toggles only then, rather than
toggling unconditionally and flipping already-playing audio into
paused."
 (if (supersonic-playback-live-p)
     (when (aio-await (supersonic-playback-status 'paused))
       (supersonic-playback-toggle-play))
   (message "supersonic-mpris: nothing to play, start playback from Emacs first")))

(aio-defun
 supersonic-mpris--pause () "Handle the MPRIS Pause method.  The mirror image of `supersonic-mpris--play'."
 (when (supersonic-playback-live-p)
   (unless (aio-await (supersonic-playback-status 'paused))
     (supersonic-playback-toggle-play))))

(defun supersonic-mpris--play-pause ()
  "Handle the MPRIS PlayPause method."
  (if (supersonic-playback-live-p)
      (supersonic-playback-toggle-play)
    (message "supersonic-mpris: nothing to play, start playback from Emacs first")))

(defun supersonic-mpris--stop ()
  "Handle the MPRIS Stop method.  See `supersonic-mpris--quit'."
  (when (supersonic-playback-live-p)
    (supersonic-playback-stop)))

(defun supersonic-mpris--next ()
  "Handle the MPRIS Next method."
  (when (supersonic-playback-live-p)
    (supersonic-playback-next)))

(defun supersonic-mpris--previous ()
  "Handle the MPRIS Previous method."
  (when (supersonic-playback-live-p)
    (supersonic-playback-prev)))

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
         :session supersonic-mpris--bus-name supersonic-mpris--path interface method
         (lambda (&rest _args)
           (funcall handler)
           :ignore)
         t)
        supersonic-mpris--registrations))

(defun supersonic-mpris--register-fixed-property (interface property value)
  "Register PROPERTY on INTERFACE with a fixed, never-changing VALUE.
Passes DONT-REGISTER-SERVICE so this does not itself put the service
name on the bus -- see `supersonic-mpris--register' for why that
matters."
  (push (dbus-register-property
         :session supersonic-mpris--bus-name supersonic-mpris--path interface property
         :read value nil t)
        supersonic-mpris--registrations))

(defun supersonic-mpris--register ()
  "Register the MPRIS D-Bus service and its interfaces.
Every method/property below is registered with DONT-REGISTER-SERVICE,
and the well-known service name is only requested as the very last
step, once the whole object is built.  Without that, each
`dbus-register-method'/`dbus-register-property' call implicitly
\(re-)requests the name itself -- a synchronous D-Bus round trip during
which Emacs services other pending D-Bus traffic, including an MPRIS
client reacting to the resulting NameOwnerChanged by introspecting us
immediately, while the interface is still half-built.  Clients that
don't retry on that (playerctld, notably) then get stuck treating us as
broken until they are restarted."
  (supersonic-mpris--register-method
   dbus-interface-introspectable "Introspect" (lambda () supersonic-mpris--introspection-xml))
  ;; Root interface: methods.
  (supersonic-mpris--register-method supersonic-mpris--root-interface "Raise" #'ignore)
  (supersonic-mpris--register-method supersonic-mpris--root-interface "Quit" #'supersonic-mpris--quit)
  ;; Root interface: properties, all fixed for the lifetime of the service.
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "CanQuit" t)
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "CanRaise" nil)
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "HasTrackList" nil)
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "Identity" "supersonic.el")
  (supersonic-mpris--register-fixed-property supersonic-mpris--root-interface "DesktopEntry" "")
  (supersonic-mpris--register-fixed-property
   supersonic-mpris--root-interface "SupportedUriSchemes" '(:array :signature "as"))
  (supersonic-mpris--register-fixed-property
   supersonic-mpris--root-interface "SupportedMimeTypes" '(:array :signature "as"))
  ;; Player interface: methods.
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Play" #'supersonic-mpris--play)
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Pause" #'supersonic-mpris--pause)
  (supersonic-mpris--register-method supersonic-mpris--player-interface "PlayPause" #'supersonic-mpris--play-pause)
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Stop" #'supersonic-mpris--stop)
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Next" #'supersonic-mpris--next)
  (supersonic-mpris--register-method supersonic-mpris--player-interface "Previous" #'supersonic-mpris--previous)
  ;; Player interface: properties.  PlaybackStatus and Metadata are kept
  ;; current via `supersonic-mpris--set-player-property'; the Can* flags are
  ;; fixed for the reduced v1 scope (no seeking, no track list).
  (push (dbus-register-property
         :session supersonic-mpris--bus-name supersonic-mpris--path supersonic-mpris--player-interface "PlaybackStatus"
         :read supersonic-mpris--playback-status nil t)
        supersonic-mpris--registrations)
  (push (dbus-register-property
         :session supersonic-mpris--bus-name supersonic-mpris--path supersonic-mpris--player-interface "Metadata"
         :read (supersonic-mpris--metadata) nil t)
        supersonic-mpris--registrations)
  (dolist (prop '("CanGoNext" "CanGoPrevious" "CanPlay" "CanPause" "CanControl"))
    (supersonic-mpris--register-fixed-property supersonic-mpris--player-interface prop t))
  (supersonic-mpris--register-fixed-property supersonic-mpris--player-interface "CanSeek" nil)
  ;; Drive the interface from the playback facade, without either side
  ;; needing to know we exist.
  (add-hook 'supersonic-playback-track-change-hook #'supersonic-mpris--sync)
  (add-hook 'supersonic-playback-state-change-hook #'supersonic-mpris--sync)
  ;; Only now, with every method/property handler wired up locally, put the
  ;; well-known name on the bus -- see the docstring above for why this has
  ;; to be last.
  (unless (memq
           (dbus-register-service :session supersonic-mpris--bus-name :do-not-queue) '(:primary-owner :already-owner))
    (user-error "Could not acquire %s (already running elsewhere?)" supersonic-mpris--bus-name)))

(defun supersonic-mpris--unregister ()
  "Tear down the MPRIS D-Bus service and stop observing the playback facade."
  (remove-hook 'supersonic-playback-track-change-hook #'supersonic-mpris--sync)
  (remove-hook 'supersonic-playback-state-change-hook #'supersonic-mpris--sync)
  (dolist (registration supersonic-mpris--registrations)
    (dbus-unregister-object registration))
  (setq supersonic-mpris--registrations nil)
  (dbus-unregister-service :session supersonic-mpris--bus-name)
  (setq supersonic-mpris--playback-status "Stopped")
  (setq supersonic-mpris--track-id nil)
  (setq supersonic-mpris--track-song nil))

;;;###autoload
(define-minor-mode supersonic-mpris-mode
  "Expose supersonic.el's playback as an MPRIS player over D-Bus.

Once enabled, desktop environments and tools such as playerctl can see
supersonic.el under the name `org.mpris.MediaPlayer2.supersonicel' on the
session bus, and use it to Play/Pause/Stop/Next/Previous and read
Metadata (title/artist/album/length).  Seeking, volume, shuffle and
loop control are intentionally out of scope.

This is a global mode with no association to any particular buffer."
  :global t
  :group
  'supersonic-mpris
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
