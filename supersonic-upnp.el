;;; supersonic-upnp.el --- UPnP AVTransport helpers for supersonic.el -*- lexical-binding: t; -*-

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

;; The `upnp' playback backend: plays to a UPnP MediaRenderer, such
;; as an AV receiver, network speaker or smart TV, by sending it stream
;; URLs over SOAP.  It works with any provider implementing
;; `stream-url'.  Select the renderer with
;; `supersonic-upnp-select-renderer'.
;;
;; Most renderers hold one track at a time, so the queue is kept here.
;; The renderer is polled, see `supersonic-poller.el', and the next
;; track is loaded when the current one ends -- so only while Emacs
;; runs.  Renderers that support it are given the next track in
;; advance, for gapless playback.
;;
;; The stream and cover art URLs, credentials included, are passed to
;; the renderer as they are.

;;; Code:
(require 'aio)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'xml)
(require 'supersonic-api)
(require 'supersonic-custom)
(require 'supersonic-provider)
(require 'supersonic-playback)
(require 'supersonic-poller)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defconst supersonic-upnp--content-types
  '(("mp3" . "audio/mpeg")
    ("flac" . "audio/flac")
    ("ogg" . "audio/ogg")
    ("opus" . "audio/opus")
    ("oga" . "audio/ogg")
    ("aac" . "audio/aac")
    ("m4a" . "audio/mp4")
    ("wav" . "audio/wav")
    ("raw" . "audio/octet-stream"))
  "Alist of stream format extension to MIME type.
\"raw\" is Subsonic's format for no transcoding.")

(defun supersonic-upnp--content-type (track &optional format)
  "Return the MIME type of a stream of TRACK requested in FORMAT.
FORMAT's `:format' takes precedence over TRACK's `:content-type'."
  (or (let ((requested (plist-get format :format)))
        (and requested (cdr (assoc requested supersonic-upnp--content-types))))
      (plist-get track :content-type)
      "application/octet-stream"))

(defconst supersonic-upnp--dlna-flags "01700000000000000000000000000000"
  "The `DLNA.ORG_FLAGS' of every stream: an ordinary HTTP stream.")

(defun supersonic-upnp--range-seekable-p (format)
  "Guess whether a stream requested in FORMAT serves byte ranges.
Servers usually do for untranscoded files only."
  (or (null format)
      (and (equal (plist-get format :format) "raw")
           (not (plist-get format :max-bit-rate)))))

(defun supersonic-upnp--protocol-info (track &optional format seekable)
  "Return the `protocolInfo' of a stream of TRACK requested in FORMAT.
SEEKABLE non-nil says the stream serves byte ranges.  Some TVs ignore
`Seek' on a stream not declared seekable."
  (format "http-get:*:%s:DLNA.ORG_OP=%s;DLNA.ORG_FLAGS=%s"
          (supersonic-upnp--content-type track format)
          (if seekable "01" "00")
          supersonic-upnp--dlna-flags))

(defun supersonic-upnp--ranges-served-p (code headers)
  "Return whether a range request answered with CODE and HEADERS was served.
Return t or nil, or `unknown' if the request failed."
  (cond
   ((eql code 206) t)
   ((and (integerp code) (<= 200 code 299))
    (let ((case-fold-search t))
      (and (string-match-p "^accept-ranges:[ \t]*bytes" headers) t)))
   (t 'unknown)))

(defun supersonic-upnp--send-probe (url)
  "Return a promise of the answer to a HEAD request for URL's first bytes."
  (let ((url-request-method "HEAD")
        (url-request-extra-headers '(("Range" . "bytes=0-1"))))
    (supersonic-url-retrieve url)))

(aio-defun supersonic-upnp--probe-ranges (url)
  "Return a promise of whether the stream at URL serves byte ranges.
Resolve to t, nil or `unknown'."
  (condition-case nil
      (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-upnp--send-probe url))))
        (unwind-protect
            (if (plist-get status :error)
                'unknown
              (with-current-buffer buffer
                (supersonic-upnp--ranges-served-p
                 url-http-response-status
                 (buffer-substring (point-min) (or url-http-end-of-headers (point-max))))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))))
    (error 'unknown)))

(aio-defun supersonic-upnp--seekable-p (url format)
  "Return a promise of whether the stream at URL, requested in FORMAT, is seekable.
Ask the server, or guess from FORMAT if that fails."
  (let ((served (aio-await (supersonic-upnp--probe-ranges url))))
    (if (eq served 'unknown)
        (supersonic-upnp--range-seekable-p format)
      served)))

(defun supersonic-upnp--didl-lite (url track &optional format art-url seekable)
  "Return the DIDL-Lite metadata of URL, a stream of TRACK.
ART-URL is the cover art URL, or nil.  FORMAT and SEEKABLE are as in
`supersonic-upnp--protocol-info'."
  (let ((title (or (plist-get track :title) (plist-get track :id) ""))
        (artist (plist-get track :artist))
        (album (plist-get track :album))
        (protocol-info (supersonic-upnp--protocol-info track format seekable)))
    (concat
     "<DIDL-Lite xmlns=\"urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/\""
     " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
     " xmlns:upnp=\"urn:schemas-upnp-org:metadata-1-0/upnp/\">"
     (format
      "<item id=\"%s\" parentID=\"-1\" restricted=\"1\">" (xml-escape-string (or (plist-get track :id) "")))
     (format "<dc:title>%s</dc:title>" (xml-escape-string title))
     (and artist (format "<upnp:artist>%s</upnp:artist>" (xml-escape-string artist)))
     (and album (format "<upnp:album>%s</upnp:album>" (xml-escape-string album)))
     (and art-url (format "<upnp:albumArtURI>%s</upnp:albumArtURI>" (xml-escape-string art-url)))
     "<upnp:class>object.item.audioItem.musicTrack</upnp:class>"
     (format
      "<res protocolInfo=\"%s\">%s</res>" (xml-escape-string protocol-info) (xml-escape-string url))
     "</item>"
     "</DIDL-Lite>")))

(defvar supersonic-upnp--warned-loopback-hosts nil
  "Loopback hosts already warned about.")

(defun supersonic-upnp--loopback-host-p (host)
  "Return non-nil if HOST is a loopback address."
  (or (member host '("localhost" "::1" "[::1]"))
      (string-suffix-p ".localhost" host)
      (string-match-p "\\`127\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" host)))

(defun supersonic-upnp--warn-if-loopback (url)
  "Warn once per host if URL points at a loopback address.
A renderer would look for the server on itself."
  (let ((host (url-host (url-generic-parse-url url))))
    (when (and host
               (supersonic-upnp--loopback-host-p host)
               (not (member host supersonic-upnp--warned-loopback-hosts)))
      (push host supersonic-upnp--warned-loopback-hosts)
      (display-warning
       'supersonic
       (format-message
        "Stream URL points at `%s', which a UPnP renderer resolves to itself; \
set the server address (`supersonic-host' for Subsonic) to one the renderer can reach"
        host)))))

(aio-defun supersonic-upnp--item (track-id)
  "Return a promise resolving to what to tell a renderer to play TRACK-ID.
That is a plist (:url URL :metadata DIDL :duration DURATION)."
  (let* ((format supersonic-upnp-stream-format)
         (track (aio-await (supersonic-provider-track track-id)))
         (url (aio-await (supersonic-provider-stream-url track-id format)))
         (_ (supersonic-upnp--warn-if-loopback url))
         (art (plist-get track :art))
         (art-url
          (and art
               (supersonic-provider-supports-p 'cover-art-url)
               (aio-await (supersonic-provider-cover-art-url art supersonic-upnp-art-size))))
         (seekable (aio-await (supersonic-upnp--seekable-p url format))))
    (list :url url
          :metadata (supersonic-upnp--didl-lite url track format art-url seekable)
          :duration (plist-get track :duration))))

;;;
;;; Renderer discovery and selection
;;;

(defconst supersonic-upnp--ssdp-address [239 255 255 250 1900]
  "The SSDP multicast address and port.")

(defconst supersonic-upnp--renderer-device-type "urn:schemas-upnp-org:device:MediaRenderer:"
  "The device type of a MediaRenderer, without the version.")

(defconst supersonic-upnp--av-transport-service-type "urn:schemas-upnp-org:service:AVTransport:"
  "The service type of AVTransport, without the version.")

(defun supersonic-upnp--m-search (timeout)
  "Return an SSDP search request for MediaRenderers answering within TIMEOUT."
  (concat
   "M-SEARCH * HTTP/1.1\r\n"
   (format "HOST: %s\r\n" (format-network-address supersonic-upnp--ssdp-address))
   "MAN: \"ssdp:discover\"\r\n"
   (format "MX: %d\r\n" (max 1 (min 5 (1- (ceiling timeout)))))
   "ST: " supersonic-upnp--renderer-device-type "1\r\n"
   "\r\n"))

(defun supersonic-upnp--ssdp-location (response)
  "Return the LOCATION header of the SSDP search RESPONSE, or nil."
  (let ((case-fold-search t))
    (and (string-match-p "\\`HTTP/1\\.[01] 200" response)
         (string-match "^location:\\(.*\\)$" response)
         (let ((location (string-trim (match-string 1 response))))
           (and (not (string-empty-p location)) location)))))

(defun supersonic-upnp--ssdp-search (timeout)
  "Return a promise of the description URLs an SSDP search finds in TIMEOUT."
  (let* ((promise (aio-promise))
         (locations nil)
         ;; A server socket, as answers come from each device's own address.
         (process
          (make-network-process
           :name "supersonic-upnp-ssdp"
           :type 'datagram
           :server t
           :family 'ipv4
           :host "0.0.0.0"
           :service 0
           :coding 'binary
           :noquery t
           :filter
           (lambda (_process response)
             (let ((location (supersonic-upnp--ssdp-location response)))
               (when (and location (not (member location locations)))
                 (push location locations)))))))
    (set-process-datagram-address process supersonic-upnp--ssdp-address)
    (process-send-string process (supersonic-upnp--m-search timeout))
    (run-at-time timeout nil
                 (lambda ()
                   (delete-process process)
                   (let ((found (reverse locations)))
                     (aio-resolve promise (lambda () found)))))
    promise))

(defun supersonic-upnp--node-text (node)
  "Return the trimmed text directly inside NODE, or nil if NODE is nil."
  (and node (string-trim (apply #'concat (seq-filter #'stringp (dom-children node))))))

(defun supersonic-upnp--child-text (node tag)
  "Return the trimmed text of NODE's child element TAG, or nil."
  (supersonic-upnp--node-text (dom-child-by-tag node tag)))

(defun supersonic-upnp--type-p (node tag type)
  "Return non-nil if NODE's child TAG names TYPE in any version."
  (string-prefix-p type (or (supersonic-upnp--child-text node tag) "")))

(defun supersonic-upnp--av-transport-control-url (device)
  "Return the AVTransport control URL of DEVICE, or nil."
  (seq-some
   (lambda (service)
     (and (supersonic-upnp--type-p service 'serviceType supersonic-upnp--av-transport-service-type)
          (supersonic-upnp--child-text service 'controlURL)))
   (dom-by-tag (dom-child-by-tag device 'serviceList) 'service)))

(defun supersonic-upnp--parse-description (xml location)
  "Return the renderer described by the device description XML at LOCATION.
That is a plist (:name NAME :udn UDN :location LOCATION :control-url
URL), or nil if XML describes no MediaRenderer with AVTransport."
  (let* ((root (with-temp-buffer
                 (insert xml)
                 (libxml-parse-xml-region (point-min) (point-max))))
         (base (or (and root (supersonic-upnp--child-text root 'URLBase)) location)))
    (and root
         (seq-some
          (lambda (device)
            (let ((control-url (supersonic-upnp--av-transport-control-url device)))
              (and control-url
                   (supersonic-upnp--type-p device 'deviceType supersonic-upnp--renderer-device-type)
                   (list :name (or (supersonic-upnp--child-text device 'friendlyName) location)
                         :udn (supersonic-upnp--child-text device 'UDN)
                         :location location
                         :control-url (url-expand-file-name control-url base)))))
          (dom-by-tag root 'device)))))

(aio-defun supersonic-upnp--describe (location)
  "Return a promise of the renderer described at LOCATION, or nil."
  (supersonic-upnp--parse-description (aio-await (supersonic-get-text location)) location))

(defun supersonic-upnp--describe-all (locations)
  "Return promises of the renderers described at LOCATIONS, wrapped by `aio-catch'."
  (let ((supersonic-request-timeout supersonic-upnp-discovery-timeout))
    (mapcar (lambda (location) (aio-catch (supersonic-upnp--describe location))) locations)))

(aio-defun supersonic-upnp--discover ()
  "Return a promise of the UPnP renderers on the local network."
  (let ((pending (supersonic-upnp--describe-all
                  (aio-await (supersonic-upnp--ssdp-search supersonic-upnp-discovery-timeout))))
        (renderers nil))
    (dolist (promise pending)
      (let* ((outcome (aio-await promise))
             (renderer (and (eq (car outcome) :success) (cdr outcome))))
        (when (and renderer
                   (not (and (plist-get renderer :udn)
                             (seq-find (lambda (known) (equal (plist-get known :udn) (plist-get renderer :udn)))
                                       renderers))))
          (push renderer renderers))))
    (nreverse renderers)))

(defun supersonic-upnp--renderer-label (renderer)
  "Return RENDERER's name and host, for selection."
  (format "%s (%s)"
          (plist-get renderer :name)
          (url-host (url-generic-parse-url (plist-get renderer :location)))))

;; Explicit, as the autoloads generator doesn't know `aio-defun'.
;;;###autoload (autoload 'supersonic-upnp-select-renderer "supersonic-upnp" nil t)
(aio-defun supersonic-upnp-select-renderer (&optional by-url)
  "Select the UPnP renderer to play to.
Search the network and offer the renderers found.  You can also enter
the URL of a renderer's device description instead; with prefix
argument BY-URL, only ask for the URL.  The choice applies to this
session; you are asked whether to save it."
  (interactive "P")
  (supersonic--with-async-error-handling
   nil "select a UPnP renderer"
   (let* ((renderers
           (unless by-url
             (message "Searching for UPnP renderers...")
             (aio-await (supersonic-upnp--discover))))
          (choices (mapcar (lambda (renderer) (cons (supersonic-upnp--renderer-label renderer) renderer)) renderers))
          (input
           (string-trim
            (completing-read (cond
                              (choices
                               "UPnP renderer (or description URL): ")
                              (by-url
                               "Description URL of the UPnP renderer: ")
                              (t
                               "No UPnP renderer found; description URL: "))
                             choices)))
          (renderer
           (or (cdr (assoc input choices))
               (if (string-match-p "\\`https?://" input)
                   (or (aio-await (supersonic-upnp--describe input))
                       (user-error "%s describes no renderer with AVTransport" input))
                 (user-error "\"%s\" is neither a renderer found nor a description URL" input)))))
     (let ((name (plist-get renderer :name)))
       (customize-set-variable
        'supersonic-upnp-renderer (list :location (plist-get renderer :location) :udn (plist-get renderer :udn) :name name))
       (supersonic-playback-offer-to-save 'supersonic-upnp-renderer "UPnP renderer" name)))))

;;;
;;; Talking to the renderer's AVTransport service
;;;

(defvar supersonic-upnp--control nil
  "The selected renderer's (LOCATION . CONTROL-URL), or nil.")

(aio-defun supersonic-upnp--control-url ()
  "Return a promise of the selected renderer's AVTransport control URL."
  (let ((location (plist-get supersonic-upnp-renderer :location)))
    (unless location
      (user-error "No UPnP renderer selected; select one with `M-x supersonic-upnp-select-renderer'"))
    (unless (equal location (car supersonic-upnp--control))
      (let ((renderer (aio-await (supersonic-upnp--describe location))))
        (unless renderer
          (error "%s no longer describes a UPnP renderer with AVTransport" location))
        (setq supersonic-upnp--control (cons location (plist-get renderer :control-url)))))
    (cdr supersonic-upnp--control)))

(defun supersonic-upnp--soap-envelope (action arguments)
  "Return the SOAP envelope invoking AVTransport ACTION with ARGUMENTS.
ARGUMENTS is an alist of names and string values, in order."
  (concat
   "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
   "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\""
   " s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
   "<s:Body>"
   (format "<u:%s xmlns:u=\"%s1\">" action supersonic-upnp--av-transport-service-type)
   "<InstanceID>0</InstanceID>"
   (mapconcat (lambda (argument)
                (format "<%s>%s</%s>" (car argument) (xml-escape-string (cdr argument)) (car argument)))
              arguments "")
   (format "</u:%s>" action)
   "</s:Body>"
   "</s:Envelope>"))

(defun supersonic-upnp--send (url action envelope)
  "Return a promise of the answer to POSTing ENVELOPE, for ACTION, to URL."
  (let ((url-request-method "POST")
        (url-request-data (encode-coding-string envelope 'utf-8))
        (url-request-extra-headers
         `(("Content-Type" . "text/xml; charset=\"utf-8\"")
           ("SOAPACTION" . ,(format "\"%s1#%s\"" supersonic-upnp--av-transport-service-type action)))))
    (supersonic-url-retrieve url)))

(aio-defun supersonic-upnp--post (url action envelope)
  "Return a promise of (STATUS . BODY) for POSTing ENVELOPE to URL.
ACTION is the action invoked.  An HTTP error is not signaled."
  (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-upnp--send url action envelope))))
    (unwind-protect
        (cons status
              (with-current-buffer buffer
                (and (boundp 'url-http-end-of-headers)
                     url-http-end-of-headers
                     (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8))))
      (kill-buffer buffer))))

(defun supersonic-upnp--parse-body (body)
  "Return the parsed XML of the SOAP response BODY, or nil."
  (and body
       (with-temp-buffer
         (insert body)
         (libxml-parse-xml-region (point-min) (point-max)))))

(defun supersonic-upnp--soap-result (action status body)
  "Return ACTION's response element in the SOAP response BODY.
STATUS is the status `url-retrieve' passed.  Signal an error with the
renderer's reason if it refused."
  (let* ((root (supersonic-upnp--parse-body body))
         (fault (and root (car (dom-by-tag root 'Fault)))))
    (cond
     (fault
      (error "The UPnP renderer refused %s: %s (error %s)"
             action
             (or (supersonic-upnp--node-text (car (dom-by-tag fault 'errorDescription)))
                 (supersonic-upnp--node-text (car (dom-by-tag fault 'faultstring)))
                 "no reason given")
             (or (supersonic-upnp--node-text (car (dom-by-tag fault 'errorCode))) "unknown")))
     ((plist-get status :error)
      (error "The UPnP renderer did not answer %s: %S" action (plist-get status :error)))
     ((and root (car (dom-by-tag root (intern (concat action "Response"))))))
     (t
      (error "The UPnP renderer gave no answer to %s" action)))))

(defun supersonic-upnp--fault-code (body)
  "Return the UPnP error code of the SOAP fault in BODY, or nil."
  (let* ((root (supersonic-upnp--parse-body body))
         (fault (and root (car (dom-by-tag root 'Fault))))
         (code (and fault (supersonic-upnp--node-text (car (dom-by-tag fault 'errorCode))))))
    (and code (string-match-p "\\`[0-9]+\\'" code) (string-to-number code))))

(aio-defun supersonic-upnp--invoke (action &optional arguments)
  "Return a promise of (STATUS . BODY) for AVTransport ACTION with ARGUMENTS."
  (let ((url (aio-await (supersonic-upnp--control-url))))
    (aio-await (supersonic-upnp--post url action (supersonic-upnp--soap-envelope action arguments)))))

(aio-defun supersonic-upnp--soap (action &optional arguments)
  "Return a promise of the response element of AVTransport ACTION with ARGUMENTS.
Signal an error if the renderer refuses."
  (pcase-let ((`(,status . ,body) (aio-await (supersonic-upnp--invoke action arguments))))
    (supersonic-upnp--soap-result action status body)))

(defun supersonic-upnp--parse-time (string)
  "Return the seconds of the UPnP time STRING, H:MM:SS, or nil."
  (and string
       (string-match "\\`\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\'" string)
       (+ (* 3600 (string-to-number (match-string 1 string)))
          (* 60 (string-to-number (match-string 2 string)))
          (string-to-number (match-string 3 string)))))

(defun supersonic-upnp--format-time (seconds)
  "Return SECONDS as UPnP time, H:MM:SS."
  (let ((seconds (max 0 (round seconds))))
    (format "%d:%02d:%02d" (/ seconds 3600) (% (/ seconds 60) 60) (% seconds 60))))

;;;
;;; The client-side queue and the polled snapshot
;;;

(defvar supersonic-upnp--queue nil
  "The track ids queued to play on the renderer.")

(defvar supersonic-upnp--index nil
  "Index in `supersonic-upnp--queue' of the current track, or nil.")

(defvar supersonic-upnp--track-duration nil
  "The provider's duration of the current track.")

(defvar supersonic-upnp--user-stopped nil
  "Non-nil if the user stopped playback since the current track was loaded.
Renderers report a track played to its end and a stopped one alike.")

(defvar supersonic-upnp--queue-generation 0
  "Counter incremented when `supersonic-upnp--queue' is replaced.")

(defvar supersonic-upnp--current-url nil
  "The URL the renderer was given for the current track, or nil.")

(defvar supersonic-upnp--next nil
  "Play the next queue entry, if there is one.")

(defvar supersonic-upnp--move-on-wait 2
  "Seconds to wait for the renderer to start the preloaded next track.")

(defvar supersonic-upnp--gapless nil
  "(LOCATION . SUPPORTED) for whether the renderer takes a next track, or nil.")

(defconst supersonic-upnp--unsupported-fault-codes '(401 602)
  "UPnP error codes meaning an action isn't supported.")

(defvar supersonic-upnp--preload-failures nil
  "(LOCATION . COUNT) of the renderer's refusals in a row of a next track.")

(defconst supersonic-upnp--preload-description "preload the next track on the UPnP renderer"
  "What a failed preload is reported as having failed to do.")

(defconst supersonic-upnp--max-preload-failures 3
  "Refusals in a row after which a renderer counts as taking no next track.")

(defconst supersonic-upnp--end-tolerance 3
  "Seconds before its end a track may stop and still count as played.")

(defconst supersonic-upnp--running-states '("PLAYING" "TRANSITIONING")
  "Transport states in which a renderer is playing or about to.")

(defconst supersonic-upnp--ended-states '("STOPPED" "NO_MEDIA_PRESENT")
  "Transport states of a renderer whose track played to its end.")

(defun supersonic-upnp--current-track ()
  "Return the id of the track the renderer was given, or nil."
  (and supersonic-upnp--index (nth supersonic-upnp--index supersonic-upnp--queue)))

(defun supersonic-upnp--current-p (index generation)
  "Return non-nil if INDEX in the queue of GENERATION is still current."
  (and (eql index supersonic-upnp--index)
       (eql generation supersonic-upnp--queue-generation)))

(defun supersonic-upnp--state (&optional snapshot)
  "Return the transport state in SNAPSHOT, by default the latest."
  (plist-get (or snapshot (supersonic-upnp--snapshot)) :state))

(defun supersonic-upnp--playing-p (&optional snapshot)
  "Return non-nil if SNAPSHOT, by default the latest, finds the renderer playing."
  (equal "PLAYING" (supersonic-upnp--state snapshot)))

(defun supersonic-upnp--paused-p (&optional snapshot)
  "Return non-nil unless SNAPSHOT, by default the latest, finds it running."
  (not (member (supersonic-upnp--state snapshot) supersonic-upnp--running-states)))

(defun supersonic-upnp--duration (&optional snapshot)
  "Return the duration of the current track, or nil.
Take it from SNAPSHOT, by default the latest, or from the provider."
  (or (plist-get (or snapshot (supersonic-upnp--snapshot)) :duration)
      supersonic-upnp--track-duration))

(defun supersonic-upnp--position ()
  "Return the renderer's estimated current position."
  (let ((snapshot (supersonic-upnp--snapshot)))
    (supersonic-poller-position snapshot (supersonic-upnp--playing-p snapshot) (supersonic-upnp--duration))))

(defun supersonic-upnp--max-poll-gap ()
  "Return the longest time between two polls that detect a track's end."
  (+ (* 2 supersonic-upnp-poll-interval) supersonic-request-timeout supersonic-upnp--end-tolerance))

(defun supersonic-upnp--track-ended-p (previous current)
  "Return non-nil if the track playing in snapshot PREVIOUS ended by CURRENT.
The track must have stopped near its end, without the user stopping
it, and CURRENT must follow PREVIOUS within
`supersonic-upnp--max-poll-gap', so that a renderer switched off
mid-track doesn't start the next one when it comes back."
  (and previous current
       (not supersonic-upnp--user-stopped)
       (plist-get current :index)
       (eql (plist-get previous :index) (plist-get current :index))
       (supersonic-upnp--playing-p previous)
       (member (supersonic-upnp--state current) supersonic-upnp--ended-states)
       (<= (- (plist-get current :polled-at) (plist-get previous :polled-at)) (supersonic-upnp--max-poll-gap))
       (let ((duration (supersonic-upnp--duration previous)))
         (or (null duration)
             (>= (+ (or (plist-get previous :position) 0)
                    (- (plist-get current :polled-at) (plist-get previous :polled-at)))
                 (- duration supersonic-upnp--end-tolerance))))))

(defun supersonic-upnp--summarize (snapshot)
  "Return the summary of SNAPSHOT that `supersonic-poller-announce' compares.
Only the pause state comes from SNAPSHOT."
  (list :track-id (supersonic-upnp--current-track)
        :paused (supersonic-upnp--paused-p snapshot)
        :entries (copy-sequence supersonic-upnp--queue)))

(aio-defun supersonic-upnp--fetch ()
  "Return a promise of a snapshot of the renderer's state."
  (let* ((index supersonic-upnp--index)
         (transport (aio-await (supersonic-upnp--soap "GetTransportInfo")))
         (position (aio-await (supersonic-upnp--soap "GetPositionInfo")))
         (duration (supersonic-upnp--parse-time (supersonic-upnp--child-text position 'TrackDuration))))
    (list :state (supersonic-upnp--child-text transport 'CurrentTransportState)
          :position (supersonic-upnp--parse-time (supersonic-upnp--child-text position 'RelTime))
          :duration (and duration (> duration 0) duration)
          :uri (supersonic-upnp--child-text position 'TrackURI)
          :index index
          :polled-at (float-time))))

(defun supersonic-upnp--moved-on-p (snapshot)
  "Return non-nil if SNAPSHOT finds the renderer on the preloaded next track."
  (let ((next supersonic-upnp--next))
    (and next
         supersonic-upnp--index
         (eql (plist-get snapshot :index) supersonic-upnp--index)
         (eql (plist-get next :index) (1+ supersonic-upnp--index))
         (eql (plist-get next :generation) supersonic-upnp--queue-generation)
         (equal (plist-get snapshot :uri) (plist-get next :url)))))

(defun supersonic-upnp--adopt-next (snapshot)
  "Make the preloaded next track current, and SNAPSHOT its snapshot."
  (let ((next supersonic-upnp--next))
    (setq supersonic-upnp--index (plist-get next :index))
    (setq supersonic-upnp--current-url (plist-get next :url))
    (setq supersonic-upnp--track-duration (plist-get next :duration))
    (setq supersonic-upnp--user-stopped nil)
    (setq supersonic-upnp--next nil)
    (setf (plist-get snapshot :index) supersonic-upnp--index)))

(defun supersonic-upnp--gapless-known-p ()
  "Return non-nil if the renderer is known to take a next track."
  (equal supersonic-upnp--gapless (cons (plist-get supersonic-upnp-renderer :location) t)))

(defun supersonic-upnp--lost-p (snapshot)
  "Return non-nil if SNAPSHOT finds a gapless renderer on an unknown URL."
  (and (supersonic-upnp--gapless-known-p)
       supersonic-upnp--index
       (eql (plist-get snapshot :index) supersonic-upnp--index)
       (supersonic-upnp--playing-p snapshot)
       (not (member (plist-get snapshot :uri)
                    (list supersonic-upnp--current-url (plist-get supersonic-upnp--next :url))))))

(defun supersonic-upnp--give-up-gapless ()
  "Stop preloading next tracks on the selected renderer.
Return a function clearing the next track it may hold."
  (setq supersonic-upnp--gapless (cons (plist-get supersonic-upnp-renderer :location) nil))
  (setq supersonic-upnp--next nil)
  #'supersonic-upnp--clear-next)

(defun supersonic-upnp--landed (previous current)
  "Return a function moving on after snapshot CURRENT followed PREVIOUS, or nil.
Adopt a preloaded next track the renderer moved on to, give up
preloading if the renderer plays an unknown URL, and load the next
track if the current one ended."
  (cond
   ((supersonic-upnp--moved-on-p current)
    (supersonic-upnp--adopt-next current)
    #'supersonic-upnp--preload-next)
   ((supersonic-upnp--lost-p current)
    (supersonic-upnp--give-up-gapless))
   ((supersonic-upnp--track-ended-p previous current)
    (let ((index (plist-get current :index))
          (generation supersonic-upnp--queue-generation))
      (if supersonic-upnp--next
          (lambda () (supersonic-upnp--advance-unless-moved-on index generation))
        (lambda () (supersonic-upnp--advance index generation)))))))

(defun supersonic-upnp--forget-control ()
  "Forget the control URL, in case the renderer's address changed."
  (setq supersonic-upnp--control nil))

(defun supersonic-upnp--make-poller ()
  "Return a poller for the renderer that has not polled yet."
  (supersonic-poller-create
   :backend 'upnp
   :interval 'supersonic-upnp-poll-interval
   :fetch #'supersonic-upnp--fetch
   :summarize #'supersonic-upnp--summarize
   :description "poll the UPnP renderer"
   :on-failure #'supersonic-upnp--forget-control
   :on-landed #'supersonic-upnp--landed))

(defvar supersonic-upnp--poller (supersonic-upnp--make-poller)
  "The poller of the renderer.
Its snapshot is a plist of `:state', the transport state;
`:position' and `:duration' in seconds, or nil; `:uri', the track URL
the renderer reports; `:index', `supersonic-upnp--index' when the
poll was sent; and `:polled-at'.")

(defun supersonic-upnp--snapshot ()
  "Return the renderer's latest snapshot, or nil."
  (supersonic-poller-snapshot supersonic-upnp--poller))

(defun supersonic-upnp--poll ()
  "Return a promise of polling the renderer."
  (supersonic-poller-poll supersonic-upnp--poller))

(defun supersonic-upnp-live-p ()
  "Return non-nil if the last poll of the renderer succeeded."
  (supersonic-poller-live supersonic-upnp--poller))

(aio-defun supersonic-upnp--ensure-polled ()
  "Poll the renderer unless a poll already found it on the current track."
  (unless (and (supersonic-upnp--snapshot)
               (eql (plist-get (supersonic-upnp--snapshot) :index) supersonic-upnp--index))
    (aio-await (supersonic-upnp--poll))))

(aio-defun supersonic-upnp--play ()
  "Tell the renderer to play."
  (aio-await (supersonic-upnp--soap "Play" '(("Speed" . "1")))))

(defconst supersonic-upnp--transition-not-available 701
  "UPnP error code of an action the transport state doesn't allow.")

(aio-defun supersonic-upnp--load (item)
  "Give the renderer ITEM, from `supersonic-upnp--item', to play.
If the renderer refuses in its current state, as the LG TV does while
paused, stop it and try again."
  (let ((arguments `(("CurrentURI" . ,(plist-get item :url)) ("CurrentURIMetaData" . ,(plist-get item :metadata)))))
    (pcase-let ((`(,status . ,body) (aio-await (supersonic-upnp--invoke "SetAVTransportURI" arguments))))
      (when (eql (supersonic-upnp--fault-code body) supersonic-upnp--transition-not-available)
        (setq supersonic-upnp--user-stopped t)
        (aio-await (supersonic-upnp--soap "Stop"))
        (pcase-setq `(,status . ,body) (aio-await (supersonic-upnp--invoke "SetAVTransportURI" arguments))))
      (supersonic-upnp--soap-result "SetAVTransportURI" status body))))

(defun supersonic-upnp--gapless-p ()
  "Return non-nil unless the renderer is known not to take a next track."
  (let ((location (plist-get supersonic-upnp-renderer :location)))
    (or (not (equal location (car supersonic-upnp--gapless)))
        (cdr supersonic-upnp--gapless))))

(aio-defun supersonic-upnp--set-next (url metadata)
  "Tell the renderer to play URL, described by METADATA, next.
Resolve to non-nil if it accepted, nil if it doesn't support that.
Signal other errors."
  (let ((location (plist-get supersonic-upnp-renderer :location)))
    (pcase-let ((`(,status . ,body)
                 (aio-await
                  (supersonic-upnp--invoke
                   "SetNextAVTransportURI" `(("NextURI" . ,url) ("NextURIMetaData" . ,metadata))))))
      (if (memql (supersonic-upnp--fault-code body) supersonic-upnp--unsupported-fault-codes)
          (progn (setq supersonic-upnp--gapless (cons location nil)) nil)
        (condition-case err
            (supersonic-upnp--soap-result "SetNextAVTransportURI" status body)
          (error
           (let ((failures (1+ (if (equal location (car supersonic-upnp--preload-failures))
                                   (cdr supersonic-upnp--preload-failures)
                                 0))))
             (setq supersonic-upnp--preload-failures (cons location failures))
             (when (>= failures supersonic-upnp--max-preload-failures)
               (setq supersonic-upnp--gapless (cons location nil))))
           (signal (car err) (cdr err))))
        (setq supersonic-upnp--preload-failures nil)
        (setq supersonic-upnp--gapless (cons location t))))))

(aio-defun supersonic-upnp--clear-next ()
  "Clear the renderer's next track, ignoring errors."
  (condition-case nil
      (aio-await (supersonic-upnp--soap "SetNextAVTransportURI" '(("NextURI" . "") ("NextURIMetaData" . ""))))
    (error nil)))

(aio-defun supersonic-upnp--preload-next ()
  "Give the renderer the next queue entry, for gapless playback.
Without a next entry, clear the renderer's next track.  Errors are
ignored, unless `debug-on-error' is non-nil."
  (let ((index supersonic-upnp--index)
        (generation supersonic-upnp--queue-generation))
    (condition-case err
        (when (and index (supersonic-upnp--gapless-p))
          (if (< (1+ index) (length supersonic-upnp--queue))
              (let ((item (aio-await (supersonic-upnp--item (nth (1+ index) supersonic-upnp--queue)))))
                (when (and (supersonic-upnp--current-p index generation)
                           (aio-await (supersonic-upnp--set-next (plist-get item :url) (plist-get item :metadata)))
                           (supersonic-upnp--current-p index generation))
                  (setq supersonic-upnp--next
                        (list :index (1+ index) :generation generation
                              :url (plist-get item :url) :duration (plist-get item :duration)))))
            (when (supersonic-upnp--gapless-known-p)
              (aio-await (supersonic-upnp--clear-next)))))
      (error
       (when debug-on-error
         (supersonic--report-async-error supersonic-upnp--preload-description err))
       (when (supersonic-upnp--gapless-known-p)
         (aio-await (supersonic-upnp--clear-next)))))))

(aio-defun supersonic-upnp--play-index (index)
  "Load the queue entry at INDEX on the renderer and play it.
INDEX only becomes current once the renderer accepted it.  Then
preload the entry after it."
  (let ((item (aio-await (supersonic-upnp--item (nth index supersonic-upnp--queue)))))
    (aio-await (supersonic-upnp--load item))
    (setq supersonic-upnp--next nil)
    (aio-await (supersonic-upnp--play))
    (setq supersonic-upnp--index index)
    (setq supersonic-upnp--current-url (plist-get item :url))
    (setq supersonic-upnp--track-duration (plist-get item :duration))
    (setq supersonic-upnp--user-stopped nil)
    (supersonic-poller-replace-snapshot
     supersonic-upnp--poller
     (list :state "TRANSITIONING" :position 0 :duration nil :index index :polled-at (float-time))))
  (aio-await (supersonic-upnp--poll))
  (aio-await (supersonic-upnp--preload-next)))

(aio-defun supersonic-upnp--advance (index generation)
  "Play the entry after INDEX, which just ended, or stop at the queue's end.
Do nothing unless INDEX in the queue of GENERATION is still current."
  (supersonic--with-async-error-handling
   nil "play the next track on the UPnP renderer"
   (when (supersonic-upnp--current-p index generation)
     (if (< (1+ index) (length supersonic-upnp--queue))
         (aio-await (supersonic-upnp--play-index (1+ index)))
       (setq supersonic-upnp--index nil)
       (supersonic-poller-announce supersonic-upnp--poller)))))

(aio-defun supersonic-upnp--advance-unless-moved-on (index generation)
  "Move on from INDEX in the queue of GENERATION, unless the renderer did.
Wait `supersonic-upnp--move-on-wait' seconds for the renderer to start
the preloaded next track first; loading it again would cause a gap."
  (aio-await (aio-sleep supersonic-upnp--move-on-wait))
  (aio-await (supersonic-upnp--poll))
  (aio-await (supersonic-upnp--advance index generation)))

;;;
;;; The facade's operations
;;;

;; Nobody awaits these, so each reports its own errors.

(aio-defun supersonic-upnp-status (key)
  "Return a promise resolving to the renderer's current KEY.
The value comes from the latest poll."
  (pcase key
    ('track-id (supersonic-upnp--current-track))
    ('position (supersonic-upnp--position))
    ('paused (supersonic-upnp--paused-p))))

(aio-defun supersonic-upnp-queue ()
  "Return a promise resolving to the queue."
  (seq-map-indexed
   (lambda (id index) (list :track-id id :current (eql index supersonic-upnp--index)))
   supersonic-upnp--queue))

(aio-defun supersonic-upnp--start (ids)
  "Replace the queue with IDS and play the first.
If the renderer refuses, keep the old queue."
  (supersonic--with-async-error-handling
   nil "start UPnP playback"
   (let ((queue supersonic-upnp--queue)
         (generation supersonic-upnp--queue-generation))
     (setq supersonic-upnp--queue (copy-sequence ids))
     (setq supersonic-upnp--queue-generation (1+ generation))
     (condition-case err
         (if ids
             (aio-await (supersonic-upnp--play-index 0))
           (setq supersonic-upnp--index nil)
           (supersonic-poller-announce supersonic-upnp--poller))
       (error
        (setq supersonic-upnp--queue queue)
        (setq supersonic-upnp--queue-generation generation)
        (signal (car err) (cdr err)))))))

(defun supersonic-upnp-start (ids)
  "Replace the queue with IDS and start playing them on the renderer."
  (ignore (supersonic-upnp--start ids)))

(aio-defun supersonic-upnp--enqueue (ids)
  "Append IDS to the queue.
Start playing them if nothing was left to play."
  (supersonic--with-async-error-handling
   nil "enqueue on the UPnP renderer"
   (let ((length (length supersonic-upnp--queue)))
     (setq supersonic-upnp--queue (append supersonic-upnp--queue ids))
     (supersonic-poller-announce supersonic-upnp--poller)
     (when ids
       (cond
        ((null supersonic-upnp--index)
         (aio-await (supersonic-upnp--play-index length)))
        ((eql (1+ supersonic-upnp--index) length)
         (aio-await (supersonic-upnp--preload-next))))))))

(defun supersonic-upnp-enqueue (ids)
  "Append IDS to the queue, playing them if nothing is left to play."
  (ignore (supersonic-upnp--enqueue ids)))

(aio-defun supersonic-upnp--toggle-play ()
  "Pause the renderer if it is playing, else play the current track.
Load the track again if the renderer has lost it."
  (supersonic--with-async-error-handling
   nil "toggle UPnP playback"
   (when supersonic-upnp--index
     (aio-await (supersonic-upnp--ensure-polled))
     (cond
      ((member (supersonic-upnp--state) supersonic-upnp--running-states)
       (aio-await (supersonic-upnp--soap "Pause"))
       (aio-await (supersonic-upnp--poll)))
      ((member (supersonic-upnp--state) '("PAUSED_PLAYBACK" "STOPPED"))
       (setq supersonic-upnp--user-stopped nil)
       (aio-await (supersonic-upnp--play))
       (aio-await (supersonic-upnp--poll)))
      (t
       (aio-await (supersonic-upnp--play-index supersonic-upnp--index)))))))

(defun supersonic-upnp-toggle-play ()
  "Toggle between playing and paused on the renderer."
  (ignore (supersonic-upnp--toggle-play)))

(aio-defun supersonic-upnp--next ()
  "Play the queue's next entry, if there is one."
  (supersonic--with-async-error-handling
   nil "skip to the next track on the UPnP renderer"
   (when (and supersonic-upnp--index (< (1+ supersonic-upnp--index) (length supersonic-upnp--queue)))
     (aio-await (supersonic-upnp--play-index (1+ supersonic-upnp--index))))))

(defun supersonic-upnp-next ()
  "Skip to the next track in the queue."
  (ignore (supersonic-upnp--next)))

(aio-defun supersonic-upnp--prev ()
  "Play the previous queue entry, if there is one."
  (supersonic--with-async-error-handling
   nil "go back to the previous track on the UPnP renderer"
   (let ((index (if supersonic-upnp--index
                    (1- supersonic-upnp--index)
                  (1- (length supersonic-upnp--queue)))))
     (when (>= index 0)
       (aio-await (supersonic-upnp--play-index index))))))

(defun supersonic-upnp-prev ()
  "Go back to the previous track in the queue."
  (ignore (supersonic-upnp--prev)))

(aio-defun supersonic-upnp--stop ()
  "Stop the renderer, if it plays something from here."
  (supersonic--with-async-error-handling
   nil "stop the UPnP renderer"
   (when supersonic-upnp--index
     (setq supersonic-upnp--user-stopped t)
     (aio-await (supersonic-upnp--soap "Stop"))
     (aio-await (supersonic-upnp--poll)))))

(defun supersonic-upnp-stop ()
  "Stop playback on the renderer."
  (ignore (supersonic-upnp--stop)))

(aio-defun supersonic-upnp--seek-to (seconds)
  "Seek the renderer to SECONDS into the current track."
  (let* ((duration (supersonic-upnp--duration))
         (seconds (max 0 (if duration (min seconds duration) seconds))))
    (aio-await
     (supersonic-upnp--soap "Seek" `(("Unit" . "REL_TIME") ("Target" . ,(supersonic-upnp--format-time seconds))))))
  (aio-await (supersonic-poller-finish-seek supersonic-upnp--poller)))

(aio-defun supersonic-upnp--seek (offset)
  "Seek OFFSET seconds from the renderer's current position."
  (supersonic--with-async-error-handling
   nil "seek on the UPnP renderer"
   (when supersonic-upnp--index
     (aio-await (supersonic-upnp--ensure-polled))
     (aio-await (supersonic-upnp--seek-to (+ (or (supersonic-upnp--position) 0) offset))))))

(defun supersonic-upnp-seek (offset)
  "Seek OFFSET seconds relative to the current position on the renderer."
  (ignore (supersonic-upnp--seek offset)))

(aio-defun supersonic-upnp--seek-fraction (fraction)
  "Seek the renderer to FRACTION, between 0.0 and 1.0, of the current track."
  (supersonic--with-async-error-handling
   nil "seek on the UPnP renderer"
   (when supersonic-upnp--index
     (aio-await (supersonic-upnp--ensure-polled))
     (let ((duration (supersonic-upnp--duration)))
       (unless duration
         (error "Neither the UPnP renderer nor the provider knows the current track's duration"))
       (aio-await (supersonic-upnp--seek-to (* fraction duration)))))))

(defun supersonic-upnp-seek-fraction (fraction)
  "Seek the renderer to FRACTION, between 0.0 and 1.0, of the current track."
  (ignore (supersonic-upnp--seek-fraction fraction)))

(supersonic-poller-register 'supersonic-upnp--poller)

(supersonic-playback-register-backend
 'upnp
 '((start . supersonic-upnp-start)
   (enqueue . supersonic-upnp-enqueue)
   (toggle-play . supersonic-upnp-toggle-play)
   (next . supersonic-upnp-next)
   (prev . supersonic-upnp-prev)
   (stop . supersonic-upnp-stop)
   (seek . supersonic-upnp-seek)
   (seek-fraction . supersonic-upnp-seek-fraction)
   (live-p . supersonic-upnp-live-p)
   (status . supersonic-upnp-status)
   (queue . supersonic-upnp-queue))
 :requires '(stream-url))

(provide 'supersonic-upnp)
;;; supersonic-upnp.el ends here
