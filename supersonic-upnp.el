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

;; The first slice of the planned `upnp' playback backend (#63): what
;; a UPnP MediaRenderer's `AVTransport:1' service needs to be told
;; *what* it is about to play, ahead of #65 building the SOAP client
;; that actually sends `SetAVTransportURI'.
;;
;; A renderer receives one plain URL and a DIDL-Lite XML fragment
;; describing it -- title, artist, album, cover art, and a `res' element whose
;; `protocolInfo' names the exact MIME type of what that URL serves.
;; Getting `protocolInfo' wrong (claiming MP3 while the URL actually
;; serves FLAC, say) is a common way for cheap renderers to simply
;; refuse to play, or to play noise -- so it has to reflect what will
;; really come back, not just an assumption.
;;
;; That is where `supersonic-provider-stream-url''s optional FORMAT
;; hint comes in: when a caller asked the active provider to transcode
;; to a given format, `protocolInfo' has to say so instead of trusting
;; the track's own, pre-transcoding `:content-type'.  Which format to
;; ask for is `supersonic-upnp-stream-format';
;; `supersonic-upnp--item' resolves a track id to the URL and metadata
;; for it in one go.  The renderer connection itself belongs to the
;; backend in #65.
;;
;; Both the stream URL and the cover art URL carry whatever
;; credentials the provider puts in them -- for Subsonic, the
;; non-expiring token-auth triple -- and are handed to the renderer
;; as they are.  That is deliberate; the README says what this exposes.
;;
;; What a renderer cannot do is reach a server only this machine can:
;; a URL on `localhost' sends it looking for the server on itself, and
;; it answers `SetAVTransportURI' with a bare "Resource not found".
;; `supersonic-upnp--item' warns about that case instead.
;;
;; Which renderer to play to is `supersonic-upnp-renderer', picked with
;; `supersonic-upnp-select-renderer' (#64).  That command finds
;; renderers with an SSDP search: an `M-SEARCH' sent to the UPnP
;; multicast group, answered by unicast from each device with the URL
;; of its XML device description.  Only a device whose description
;; names it a MediaRenderer with an `AVTransport' service is offered;
;; anything else that happens to answer (a lighting bridge, say) is
;; left out, as is a device whose description cannot be fetched at all.
;; Where multicast does not get through, the description URL can be
;; entered by hand instead.
;;
;; This file has no `supersonic-playback-register-backend' call yet
;; and nothing in the package requires it: until #65 turns it into a
;; backend, it has to be loaded explicitly to select a renderer.

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
  "Map of a stream FORMAT's `:format' extension to its MIME content type.
Deliberately separate from `:content-type' in the facade's own track
vocabulary: that key describes the track's original encoding, which a
requested FORMAT overrides by having the provider transcode to
something else entirely.  \"raw\" is Subsonic's own `stream.view'
value for \"skip transcoding\", which says nothing about the actual
encoding either -- it maps to a generic type since the real one is
whatever the track already was.")

(defun supersonic-upnp--content-type (track &optional format)
  "Return the content type a stream of TRACK, requested with FORMAT, has.
TRACK is a track plist as `supersonic-provider-track' resolves to.
FORMAT is the plist `supersonic-provider-stream-url' takes, or nil.

When FORMAT names a `:format', that is what the provider actually
transcodes to -- overriding whatever TRACK's own `:content-type' says,
since after transcoding that is no longer the truth.  Without a
FORMAT, TRACK's `:content-type' is what was actually asked for, so it
is used as given.  Falls back to a generic type when neither is known,
rather than guessing wrong and asserting it with confidence."
  (or (let ((requested (plist-get format :format)))
        (and requested (cdr (assoc requested supersonic-upnp--content-types))))
      (plist-get track :content-type)
      "application/octet-stream"))

(defun supersonic-upnp--protocol-info (track &optional format)
  "Return the DIDL `res' element's `protocolInfo' for TRACK and FORMAT.
See `supersonic-upnp--content-type' for how the content type itself is
worked out.  The rest of the four colon-separated fields UPnP defines
\(protocol, network, content type, additional info\) are the wildcard
DLNA clients are expected to accept from an HTTP source with nothing
more specific to say."
  (format "http-get:*:%s:*" (supersonic-upnp--content-type track format)))

(defun supersonic-upnp--didl-lite (url track &optional format art-url)
  "Return a DIDL-Lite XML document describing URL, a stream of TRACK.
ART-URL, if given, becomes the item's `albumArtURI'.
FORMAT is as `supersonic-provider-stream-url' takes it, and decides
`protocolInfo' via `supersonic-upnp--protocol-info' -- see there for
why it must reflect what was actually requested rather than TRACK's
own encoding.

This is what `SetAVTransportURI' and `SetNextAVTransportURI' pass as
`CurrentURIMetaData'/`NextURIMetaData' (#65), so a renderer with a
display can show the track's title, artist and album while it plays.
Values are escaped with `xml-escape-string', since a title or artist
containing \\='&\\=', \\='<\\=' or a quote would otherwise end the
element early or unbalance the document; TRACK's `:id' becomes the
DIDL item's own `id' attribute for the same reason ids elsewhere are
opaque strings -- a renderer never has reason to parse it, only to
hand it back verbatim in whatever it reports playing."
  (let ((title (or (plist-get track :title) (plist-get track :id) ""))
        (artist (plist-get track :artist))
        (album (plist-get track :album))
        (protocol-info (supersonic-upnp--protocol-info track format)))
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
  "Loopback hosts `supersonic-upnp--warn-if-loopback' has warned about.
Kept so that the warning comes once per host, not with every track.")

(defun supersonic-upnp--loopback-host-p (host)
  "Return non-nil if HOST names this machine itself.
That is `localhost' or a name under it, any 127.0.0.0/8 address, or
the IPv6 loopback address, bracketed as `url-host' returns it or not."
  (or (member host '("localhost" "::1" "[::1]"))
      (string-suffix-p ".localhost" host)
      (string-match-p "\\`127\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" host)))

(defun supersonic-upnp--warn-if-loopback (url)
  "Warn, once per host, if URL points at this machine's loopback address.
A renderer resolves such a URL to itself, never reaching the server."
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

(aio-defun
 supersonic-upnp--item (track-id)
 "Return a promise resolving to what a renderer is told to play TRACK-ID.
That is a plist (:url URL :metadata DIDL): the active provider's
stream URL, asked for in `supersonic-upnp-stream-format', and the
DIDL-Lite document describing it, for `SetAVTransportURI' and
`SetNextAVTransportURI' to pass on as they are.

The track's cover art goes along as `albumArtURI' whenever the track
has any and the provider can name a URL for it; otherwise the
document simply has none, since a renderer plays fine without.

A stream URL on a loopback address is warned about, see
`supersonic-upnp--warn-if-loopback', but still returned: the
renderer may run on this machine itself."
 (let* ((format supersonic-upnp-stream-format)
        (track (aio-await (supersonic-provider-track track-id)))
        (url (aio-await (supersonic-provider-stream-url track-id format)))
        (_ (supersonic-upnp--warn-if-loopback url))
        (art (plist-get track :art))
        (art-url
         (and art
              (supersonic-provider-supports-p 'cover-art-url)
              (aio-await (supersonic-provider-cover-art-url art supersonic-upnp-art-size)))))
   (list :url url :metadata (supersonic-upnp--didl-lite url track format art-url))))

;;;
;;; Renderer discovery and selection

(defconst supersonic-upnp--ssdp-address [239 255 255 250 1900]
  "The SSDP multicast group and port, as `set-process-datagram-address' takes it.")

(defconst supersonic-upnp--renderer-device-type "urn:schemas-upnp-org:device:MediaRenderer:1"
  "The device type an SSDP search asks for.")

(defun supersonic-upnp--m-search (timeout)
  "Return an SSDP `M-SEARCH' request for MediaRenderers, for a TIMEOUT search.
Its `MX' header is how many seconds a device may wait at random before
answering, so that not all of them answer at once.  One second less
than TIMEOUT leaves the slowest a second to arrive; the specification
allows one to five."
  (concat
   "M-SEARCH * HTTP/1.1\r\n"
   "HOST: 239.255.255.250:1900\r\n"
   "MAN: \"ssdp:discover\"\r\n"
   (format "MX: %d\r\n" (max 1 (min 5 (1- (ceiling timeout)))))
   "ST: " supersonic-upnp--renderer-device-type "\r\n"
   "\r\n"))

(defun supersonic-upnp--ssdp-location (response)
  "Return the `LOCATION' header of SSDP search RESPONSE, or nil.
That is the URL of the answering device's description.  Header names
are matched regardless of case, since devices differ in how they
spell them.  A RESPONSE that is not a `200 OK' has none."
  (let ((case-fold-search t))
    (and (string-match-p "\\`HTTP/1\\.[01] 200" response)
         (string-match "^location:\\(.*\\)$" response)
         (let ((location (string-trim (match-string 1 response))))
           (and (not (string-empty-p location)) location)))))

(defun supersonic-upnp--ssdp-search (timeout)
  "Return a promise of the description URLs an SSDP search finds.
Sends one `M-SEARCH' to the multicast group and collects the
`LOCATION' of every answer that arrives within TIMEOUT seconds, each
URL once, however often its device answered.

The socket is a datagram server, not a client connected to the
multicast group: a connected UDP socket only receives from the address
it is connected to, but answers come by unicast from each device's
own address."
  (let* ((promise (aio-promise))
         (locations nil)
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

(defun supersonic-upnp--child-text (node tag)
  "Return the trimmed text of NODE's child element TAG, or nil.
Collected by hand: `dom-text' and `dom-texts' are obsolete as of Emacs
31.1, but their replacement `dom-inner-text' does not exist in 28.1."
  (let ((child (dom-child-by-tag node tag)))
    (and child (string-trim (apply #'concat (seq-filter #'stringp (dom-children child)))))))

(defun supersonic-upnp--av-transport-control-url (device)
  "Return the relative or absolute control URL of DEVICE's `AVTransport'.
DEVICE is a `device' element of a device description; nil if it has
no such service."
  (seq-some
   (lambda (service)
     (and (string-prefix-p "urn:schemas-upnp-org:service:AVTransport:"
                           (or (supersonic-upnp--child-text service 'serviceType) ""))
          (supersonic-upnp--child-text service 'controlURL)))
   (dom-by-tag (dom-child-by-tag device 'serviceList) 'service)))

(defun supersonic-upnp--parse-description (xml location)
  "Return the renderer the device description XML, from LOCATION, describes.
That is a plist (:name NAME :udn UDN :location LOCATION :control-url
URL), where URL is the `AVTransport' service's control URL made
absolute.  Relative URLs in a description are relative to its
`URLBase', if it has one, or else to LOCATION itself.

A description may describe more than one device, the root one
embedding others.  The first that is a MediaRenderer, of any version,
with an `AVTransport' service is the renderer; with none, or for XML
that does not parse, the value is nil."
  (let* ((root (with-temp-buffer
                 (insert xml)
                 (libxml-parse-xml-region (point-min) (point-max))))
         (base (or (and root (supersonic-upnp--child-text root 'URLBase)) location)))
    (and root
         (seq-some
          (lambda (device)
            (let ((control-url (supersonic-upnp--av-transport-control-url device)))
              (and control-url
                   (string-prefix-p "urn:schemas-upnp-org:device:MediaRenderer:"
                                    (or (supersonic-upnp--child-text device 'deviceType) ""))
                   (list :name (or (supersonic-upnp--child-text device 'friendlyName) location)
                         :udn (supersonic-upnp--child-text device 'UDN)
                         :location location
                         :control-url (url-expand-file-name control-url base)))))
          (dom-by-tag root 'device)))))

;; fix byte-compiler complaints
(defvar url-http-end-of-headers)

(aio-defun
 supersonic-upnp--describe (location)
 "Return a promise of the renderer whose device description is at LOCATION.
See `supersonic-upnp--parse-description' for what it resolves to,
including nil for a device that is not a renderer.  The promise
rejects if the description cannot be fetched."
 (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-url-retrieve location))))
   (unwind-protect
       (progn
         (when (plist-get status :error)
           (error "Failed to fetch %s: %S" location (plist-get status :error)))
         (with-current-buffer buffer
           ;; Device descriptions are UTF-8 by the UPnP specification.
           (supersonic-upnp--parse-description
            (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)
            location)))
     (kill-buffer buffer))))

(defun supersonic-upnp--describe-all (locations)
  "Start fetching the device descriptions at LOCATIONS, all at once.
Return a list of promises, one per location, as `aio-catch' wraps
them.  Each fetch gives up after `supersonic-upnp-discovery-timeout'
rather than `supersonic-request-timeout', which is about a server
that is known to be there."
  (let ((supersonic-request-timeout supersonic-upnp-discovery-timeout))
    (mapcar (lambda (location) (aio-catch (supersonic-upnp--describe location))) locations)))

(aio-defun
 supersonic-upnp--discover ()
 "Return a promise of the UPnP renderers on the local network.
A list of renderers as `supersonic-upnp--parse-description' returns
them, in the order they answered the SSDP search, each device once.

A device is left out, rather than failing the whole search, if its
description cannot be fetched or does not describe a renderer that
can be played to -- whatever else answers a search is not ours to
judge."
 (let ((pending (supersonic-upnp--describe-all
                 (aio-await (supersonic-upnp--ssdp-search supersonic-upnp-discovery-timeout))))
       (renderers nil))
   (dolist (promise pending)
     (let* ((outcome (aio-await promise))
            (renderer (and (eq (car outcome) :success) (cdr outcome))))
       (when (and renderer
                  (not (seq-find (lambda (known) (equal (plist-get known :udn) (plist-get renderer :udn)))
                                 renderers)))
         (push renderer renderers))))
   (nreverse renderers)))

(defun supersonic-upnp--renderer-label (renderer)
  "Return how RENDERER is offered for selection.
Its friendly name, followed by its host, since two renderers of the
same model often share a name."
  (format "%s (%s)"
          (plist-get renderer :name)
          (url-host (url-generic-parse-url (plist-get renderer :location)))))

;;;###autoload
(aio-defun
 supersonic-upnp-select-renderer (&optional by-url)
 "Select the UPnP renderer to play to, and save it for future sessions.
Searches the local network for renderers and offers them by name.
Instead of picking one, the URL of a renderer's device description can
be entered, for a renderer whose answers do not reach Emacs -- where
multicast does not get through to it, say.  With prefix argument
BY-URL, no search is made and only the URL is asked for.

The choice is saved as `supersonic-upnp-renderer' via
`customize-save-variable'."
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
                      (user-error "No UPnP renderer with AVTransport is described at %s" input))
                (user-error "Neither a UPnP renderer found nor a description URL: %s" input)))))
    (customize-save-variable
     'supersonic-upnp-renderer
     (list :location (plist-get renderer :location) :udn (plist-get renderer :udn) :name (plist-get renderer :name)))
    (message "Selected UPnP renderer %s" (plist-get renderer :name)))))

(provide 'supersonic-upnp)
;;; supersonic-upnp.el ends here
