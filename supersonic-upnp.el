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

;; The `upnp' playback backend (#63): plays to a UPnP MediaRenderer --
;; an AV receiver, a network speaker, a smart TV -- by telling its
;; `AVTransport:1' service, over SOAP, which URL to fetch and play.
;; Registers itself with the facade in `supersonic-playback.el' as it
;; is loaded, the way `supersonic-jukebox.el' does, for any provider
;; that can name a stream URL.
;;
;; Most renderers hold one track at a time, so the queue is kept here
;; (#65), as provider track ids and the index of the one the renderer
;; was given.  A renderer reports nothing on its own unless subscribed
;; to, which would take an HTTP server in Emacs; instead, while `upnp'
;; is the active backend, it is polled every
;; `supersonic-upnp-poll-interval' seconds for its transport state and
;; position.  The facade's hooks run, and scrobbles go out, as polls
;; find things changed, and a poll that finds the current track played
;; to its end loads the next one -- see `supersonic-upnp--track-ended-p'
;; for how that is told apart from a track stopped early.  So the queue
;; only moves on while Emacs runs.
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
;; Nothing in the package requires this file: like the jukebox, the
;; backend is opt-in.  `supersonic-upnp-select-renderer' is autoloaded,
;; though.

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

(defconst supersonic-upnp--dlna-flags "01700000000000000000000000000000"
  "The `DLNA.ORG_FLAGS' every stream is described with.
Streaming transfer mode, background transfer mode and connection
stall allowed, in DLNA 1.5 terms: an ordinary HTTP stream, nothing a
renderer needs to treat specially.")

(defun supersonic-upnp--range-seekable-p (format)
  "Return non-nil if a stream requested with FORMAT likely serves HTTP ranges.
Only a guess, for when `supersonic-upnp--probe-ranges' cannot find
out: a server usually serves the file as it is stored with byte
ranges, and a transcoded stream without, since its length is not known
before it has been transcoded.  So only a stream requested without a
FORMAT hint, or with Subsonic's \"raw\" format and no bit rate to keep
under, is guessed to be seekable."
  (or (null format)
      (and (equal (plist-get format :format) "raw")
           (not (plist-get format :max-bit-rate)))))

(defun supersonic-upnp--protocol-info (track &optional format seekable)
  "Return the DIDL `res' element's `protocolInfo' for TRACK and FORMAT.
See `supersonic-upnp--content-type' for how the content type itself is
worked out.  Of the four colon-separated fields UPnP defines
\(protocol, network, content type, additional info\), the last says
whether the stream can be seeked within, as DLNA's `DLNA.ORG_OP':
`01' for byte ranges if SEEKABLE is non-nil, and `00' for not at all.
It matters: LG and Samsung TVs answer `Seek' on a stream that does not
say `01' with success, and then ignore it."
  (format "http-get:*:%s:DLNA.ORG_OP=%s;DLNA.ORG_FLAGS=%s"
          (supersonic-upnp--content-type track format)
          (if seekable "01" "00")
          supersonic-upnp--dlna-flags))

(defun supersonic-upnp--ranges-served-p (code headers)
  "Return what a probe answered with CODE and HEADERS says of byte ranges.
CODE is the HTTP status code of a `HEAD' request asking for a byte
range, HEADERS the response's header block.  t if the server served
the range -- 206 -- or says it serves ranges with `Accept-Ranges:
bytes'; nil for any other success, since then it does not; and
`unknown' for a failure, which says nothing either way."
  (cond
   ((eql code 206) t)
   ((and (integerp code) (<= 200 code 299))
    (let ((case-fold-search t))
      (and (string-match-p "^accept-ranges:[ \t]*bytes" headers) t)))
   (t 'unknown)))

;; fix byte-compiler complaints
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defun supersonic-upnp--send-probe (url)
  "Send URL a `HEAD' request for its first two bytes.
Return the promise `supersonic-url-retrieve' does.  `HEAD', because
the answer to a range request that is not served is the whole stream
-- a transcoded track, all of it."
  (let ((url-request-method "HEAD")
        (url-request-extra-headers '(("Range" . "bytes=0-1"))))
    (supersonic-url-retrieve url)))

(aio-defun
 supersonic-upnp--probe-ranges (url)
 "Return a promise of whether the stream at URL serves byte ranges.
Resolves to t, nil or `unknown', as `supersonic-upnp--ranges-served-p'
decides from the answer; to `unknown', too, if there is no answer.
Whether a server serves ranges depends on the server, and on whether
it transcodes -- Navidrome serves the stored file with ranges and a
transcoded stream with `Accept-Ranges: none' -- so it is asked rather
than assumed."
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

(aio-defun
 supersonic-upnp--seekable-p (url format)
 "Return a promise of whether the stream at URL, asked for in FORMAT, is seekable.
Probed with `supersonic-upnp--probe-ranges'; where the probe cannot
tell, guessed from FORMAT with `supersonic-upnp--range-seekable-p'."
 (let ((served (aio-await (supersonic-upnp--probe-ranges url))))
   (if (eq served 'unknown)
       (supersonic-upnp--range-seekable-p format)
     served)))

(defun supersonic-upnp--didl-lite (url track &optional format art-url seekable)
  "Return a DIDL-Lite XML document describing URL, a stream of TRACK.
ART-URL, if given, becomes the item's `albumArtURI'.
FORMAT is as `supersonic-provider-stream-url' takes it, and decides
`protocolInfo' via `supersonic-upnp--protocol-info', together with
SEEKABLE -- see there for why it must reflect what was actually
requested rather than TRACK's own encoding.

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

The server is asked whether the stream can be seeked within, see
`supersonic-upnp--seekable-p', so that `protocolInfo' tells the
renderer.

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
              (aio-await (supersonic-provider-cover-art-url art supersonic-upnp-art-size))))
        (seekable (aio-await (supersonic-upnp--seekable-p url format))))
   (list :url url :metadata (supersonic-upnp--didl-lite url track format art-url seekable))))

;;;
;;; Renderer discovery and selection
;;;

(defconst supersonic-upnp--ssdp-address [239 255 255 250 1900]
  "The SSDP multicast group and port, as `set-process-datagram-address' takes it.")

(defconst supersonic-upnp--renderer-device-type "urn:schemas-upnp-org:device:MediaRenderer:"
  "The device type of a MediaRenderer, without its version.
An SSDP search asks for version 1, which every later version also
answers to; a device description may name any version.")

(defconst supersonic-upnp--av-transport-service-type "urn:schemas-upnp-org:service:AVTransport:"
  "The service type of `AVTransport', without its version.")

(defun supersonic-upnp--m-search (timeout)
  "Return an SSDP `M-SEARCH' request for MediaRenderers, for a TIMEOUT search.
Its `MX' header is how many seconds a device may wait at random before
answering, so that not all of them answer at once.  One second less
than TIMEOUT leaves the slowest a second to arrive; the specification
allows one to five."
  (concat
   "M-SEARCH * HTTP/1.1\r\n"
   (format "HOST: %s\r\n" (format-network-address supersonic-upnp--ssdp-address))
   "MAN: \"ssdp:discover\"\r\n"
   (format "MX: %d\r\n" (max 1 (min 5 (1- (ceiling timeout)))))
   "ST: " supersonic-upnp--renderer-device-type "1\r\n"
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

(defun supersonic-upnp--text (node)
  "Return the trimmed text directly inside NODE, or nil if NODE is nil.
Collected by hand: `dom-text' and `dom-texts' are obsolete as of Emacs
31.1, but their replacement `dom-inner-text' does not exist in 28.1."
  (and node (string-trim (apply #'concat (seq-filter #'stringp (dom-children node))))))

(defun supersonic-upnp--child-text (node tag)
  "Return the trimmed text of NODE's child element TAG, or nil."
  (supersonic-upnp--text (dom-child-by-tag node tag)))

(defun supersonic-upnp--type-p (node tag type)
  "Return non-nil if NODE's child element TAG names TYPE, of any version.
TYPE is a device or service type up to and including its last colon."
  (string-prefix-p type (or (supersonic-upnp--child-text node tag) "")))

(defun supersonic-upnp--av-transport-control-url (device)
  "Return the relative or absolute control URL of DEVICE's `AVTransport'.
DEVICE is a `device' element of a device description; nil if it has
no such service."
  (seq-some
   (lambda (service)
     (and (supersonic-upnp--type-p service 'serviceType supersonic-upnp--av-transport-service-type)
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
                   (supersonic-upnp--type-p device 'deviceType supersonic-upnp--renderer-device-type)
                   (list :name (or (supersonic-upnp--child-text device 'friendlyName) location)
                         :udn (supersonic-upnp--child-text device 'UDN)
                         :location location
                         :control-url (url-expand-file-name control-url base)))))
          (dom-by-tag root 'device)))))

(aio-defun
 supersonic-upnp--describe (location)
 "Return a promise of the renderer whose device description is at LOCATION.
See `supersonic-upnp--parse-description' for what it resolves to,
including nil for a device that is not a renderer.  The promise
rejects if the description cannot be fetched."
 (supersonic-upnp--parse-description (aio-await (supersonic-get-text location)) location))

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
                  (not (and (plist-get renderer :udn)
                            (seq-find (lambda (known) (equal (plist-get known :udn) (plist-get renderer :udn)))
                                      renderers))))
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
 "Select the UPnP renderer to play to.
Searches the local network for renderers and offers them by name.
Instead of picking one, the URL of a renderer's device description can
be entered, for a renderer whose answers do not reach Emacs -- where
multicast does not get through to it, say.  With prefix argument
BY-URL, no search is made and only the URL is asked for.

The choice becomes `supersonic-upnp-renderer' for this session, and
is saved for future ones only if confirmed: saving writes to
`custom-file', or to the init file without one, which not everyone
wants touched -- their configuration may set the renderer itself."
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
      (if (y-or-n-p (format "Selected UPnP renderer %s; save it for future sessions? " name))
          (progn
            (customize-save-variable 'supersonic-upnp-renderer supersonic-upnp-renderer)
            (message "Saved UPnP renderer %s" name))
        (message "Selected UPnP renderer %s for this session" name))))))

;;;
;;; Talking to the renderer's AVTransport service
;;;

(defvar supersonic-upnp--control nil
  "The selected renderer's (LOCATION . CONTROL-URL), as last looked up.
LOCATION is the `:location' of `supersonic-upnp-renderer' it was
looked up for, so selecting another renderer invalidates it.  Cleared
when a poll fails, too, in case the renderer moved to another address
and its description now names another control URL.")

(aio-defun
 supersonic-upnp--control-url ()
 "Return a promise of the selected renderer's `AVTransport' control URL.
Read from its device description at `supersonic-upnp-renderer''s
`:location', once per renderer -- see `supersonic-upnp--control'.
Signals a `user-error' if no renderer is selected."
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
  "Return the SOAP envelope invoking `AVTransport' ACTION with ARGUMENTS.
ARGUMENTS is an alist of argument names and string values, in the
order the action's specification lists them; `InstanceID' 0, which
every action takes first, is added in front.  Values are escaped, the
DIDL-Lite metadata `SetAVTransportURI' carries as much as any other."
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
  "POST ENVELOPE, invoking ACTION, to control URL.
Return the promise `supersonic-url-retrieve' does.  A function of its
own because `url-retrieve' reads the request from dynamic variables,
and binding those across an `aio-await' is not something an
`aio-defun' can be trusted with."
  (let ((url-request-method "POST")
        (url-request-data (encode-coding-string envelope 'utf-8))
        (url-request-extra-headers
         `(("Content-Type" . "text/xml; charset=\"utf-8\"")
           ("SOAPACTION" . ,(format "\"%s1#%s\"" supersonic-upnp--av-transport-service-type action)))))
    (supersonic-url-retrieve url)))

(aio-defun
 supersonic-upnp--post (url action envelope)
 "Return a promise of what control URL answers ACTION's ENVELOPE with.
That is (STATUS . BODY): STATUS as `url-retrieve' reports it, BODY the
response body as text, or nil if there was none.  An HTTP error is
not an error here: a renderer refusing an action answers with status
500 and a SOAP fault in BODY that says why -- see
`supersonic-upnp--soap-result'."
 (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-upnp--send url action envelope))))
   (unwind-protect
       (cons status
             (with-current-buffer buffer
               (and (boundp 'url-http-end-of-headers)
                    url-http-end-of-headers
                    (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8))))
     (kill-buffer buffer))))

(defun supersonic-upnp--soap-result (action status body)
  "Return the response element of ACTION from SOAP response BODY.
STATUS is what `url-retrieve' reported.  A SOAP fault in BODY is
signalled as an `error' carrying the renderer's own UPnP error
description and code, rather than as the XML it came in; so is a
request that got no SOAP answer at all."
  (let* ((root (and body
                    (with-temp-buffer
                      (insert body)
                      (libxml-parse-xml-region (point-min) (point-max)))))
         (fault (and root (car (dom-by-tag root 'Fault)))))
    (cond
     (fault
      (error "The UPnP renderer refused %s: %s (error %s)"
             action
             (or (supersonic-upnp--text (car (dom-by-tag fault 'errorDescription)))
                 (supersonic-upnp--text (car (dom-by-tag fault 'faultstring)))
                 "no reason given")
             (or (supersonic-upnp--text (car (dom-by-tag fault 'errorCode))) "unknown")))
     ((plist-get status :error)
      (error "The UPnP renderer did not answer %s: %S" action (plist-get status :error)))
     ((and root (car (dom-by-tag root (intern (concat action "Response"))))))
     (t
      (error "The UPnP renderer gave no answer to %s" action)))))

(aio-defun
 supersonic-upnp--soap (action &optional arguments)
 "Invoke `AVTransport' ACTION with ARGUMENTS on the selected renderer.
Return a promise of the action's response element, from which its
output arguments are read with `supersonic-upnp--child-text'.  See
`supersonic-upnp--soap-envelope' for ARGUMENTS, and
`supersonic-upnp--soap-result' for how failures are signalled."
 (let ((url (aio-await (supersonic-upnp--control-url))))
   (pcase-let ((`(,status . ,body)
                (aio-await (supersonic-upnp--post url action (supersonic-upnp--soap-envelope action arguments)))))
     (supersonic-upnp--soap-result action status body))))

(defun supersonic-upnp--parse-time (string)
  "Return the seconds UPnP time STRING stands for.
STRING is H+:MM:SS, optionally with a fraction of a second.  The
value is nil for anything else, such as the `NOT_IMPLEMENTED'
renderers answer for a value they do not track."
  (and string
       (string-match "\\`\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\'" string)
       (+ (* 3600 (string-to-number (match-string 1 string)))
          (* 60 (string-to-number (match-string 2 string)))
          (string-to-number (match-string 3 string)))))

(defun supersonic-upnp--format-time (seconds)
  "Return SECONDS, rounded and never negative, as UPnP time H:MM:SS."
  (let ((seconds (max 0 (round seconds))))
    (format "%d:%02d:%02d" (/ seconds 3600) (% (/ seconds 60) 60) (% seconds 60))))

;;;
;;; The client-side queue and the polled snapshot
;;;

(defvar supersonic-upnp--queue nil
  "The provider track ids queued to play on the renderer, in order.
Held here rather than on the renderer, since most renderers hold one
track at a time.")

(defvar supersonic-upnp--index nil
  "Index into `supersonic-upnp--queue' of the track the renderer was given.
nil before anything was started, and once the queue has been played to
its end.")

(defvar supersonic-upnp--stopped nil
  "Non-nil if the user stopped playback since the current track was loaded.
A renderer reports a track that played to its end and one that was
stopped alike, as `STOPPED'; only the first may move on to the next.")

(defvar supersonic-upnp--snapshot nil
  "What the latest poll of the renderer found, or nil before it.
A plist: `:state', the renderer's transport state, such as \"PLAYING\"
or \"PAUSED_PLAYBACK\"; `:position' and `:duration', in seconds, or nil
where the renderer did not say; `:index', the value of
`supersonic-upnp--index' when the poll was sent, so that a poll that
was overtaken by a track change is not taken for the new track's; and
`:polled-at', the `float-time' it was taken at.

Reset to nil whenever a track is loaded, so that the state the
previous track left the renderer in is never compared with the new
track's -- see `supersonic-upnp--track-ended-p'.")

(defvar supersonic-upnp--announced nil
  "What the facade's hooks were last run for: (TRACK-ID PAUSED ENTRIES).
See `supersonic-upnp--announce'.")

(defvar supersonic-upnp--live nil
  "Non-nil if the last poll of the renderer got an answer.")

(defvar supersonic-upnp--poll-failing nil
  "Non-nil once a poll has failed without a later poll succeeding since.
So that an outage is reported once, rather than once per poll.")

(defvar supersonic-upnp--poll-in-flight nil
  "Promise resolved once the poll in flight is done, or nil if none is.
See `supersonic-upnp--poll'.")

(defvar supersonic-upnp--timer nil
  "Timer polling the renderer, running only while `upnp' is the active backend.")

(defconst supersonic-upnp--end-tolerance 3
  "Seconds before its duration a track may stop and still count as ended.
Positions a renderer reports, and the times polls arrive at, are only
so exact.")

(defun supersonic-upnp-live-p ()
  "Return non-nil if the last poll of the renderer succeeded."
  supersonic-upnp--live)

(defun supersonic-upnp--current-track ()
  "Return the track id the renderer was given to play, or nil."
  (and supersonic-upnp--index (nth supersonic-upnp--index supersonic-upnp--queue)))

(defun supersonic-upnp--playing-p (&optional snapshot)
  "Return non-nil if SNAPSHOT, by default the latest, finds the renderer playing."
  (equal "PLAYING" (plist-get (or snapshot supersonic-upnp--snapshot) :state)))

(defun supersonic-upnp--position (&optional now)
  "Return the renderer's playback position at NOW, by default the current time.
Counted on from the last poll's position by the time elapsed since,
while playing, so that it advances by the second between polls the
way the now-playing buffer expects; never past the track's duration."
  (let* ((snapshot supersonic-upnp--snapshot)
         (position (plist-get snapshot :position))
         (duration (plist-get snapshot :duration)))
    (when position
      (let ((position (if (supersonic-upnp--playing-p snapshot)
                          (+ position (- (or now (float-time)) (plist-get snapshot :polled-at)))
                        position)))
        (if duration
            (min position duration)
          position)))))

(defun supersonic-upnp--track-ended-p (previous current)
  "Return non-nil if the track playing in snapshot PREVIOUS ended by CURRENT.
That is: both are of the same track; PREVIOUS found it playing;
CURRENT finds the renderer `STOPPED' or with no media, and not because
the user stopped it; and by CURRENT the track would have played to
within `supersonic-upnp--end-tolerance' of its duration.  Without that
last condition, a track stopped on the renderer's own remote would be
taken for one played to the end.  A track whose duration the renderer
does not report counts as ended whenever it stops."
  (and previous current
       (not supersonic-upnp--stopped)
       (plist-get current :index)
       (eql (plist-get previous :index) (plist-get current :index))
       (supersonic-upnp--playing-p previous)
       (member (plist-get current :state) '("STOPPED" "NO_MEDIA_PRESENT"))
       (let ((duration (plist-get previous :duration)))
         (or (null duration)
             (>= (+ (or (plist-get previous :position) 0)
                    (- (plist-get current :polled-at) (plist-get previous :polled-at)))
                 (- duration supersonic-upnp--end-tolerance))))))

(defun supersonic-upnp--scrobble-track-change (previous-track current-track)
  "Scrobble PREVIOUS-TRACK as played and CURRENT-TRACK as playing now.
Either may be nil.  The same as `supersonic-jukebox.el' does, since
this backend, too, only learns of a track change by polling.
`supersonic-provider-scrobble' itself checks whether to scrobble."
  (when previous-track
    (supersonic-provider-scrobble previous-track))
  (when current-track
    (supersonic-provider-scrobble current-track t)))

(defun supersonic-upnp--announce (&optional went-live)
  "Run the facade's hooks for whatever changed since they last ran.
Compares the current track, whether the renderer is paused and the
queue's entries with `supersonic-upnp--announced': a different track
runs the track-change hook, and scrobbles; otherwise different entries
run the queue-change hook, and a different pause state the
state-change hook.  WENT-LIVE non-nil runs the track-change hook
regardless, without scrobbling, since a renderer that answers again
after an outage is news to whoever showed it gone."
  (let ((previous supersonic-upnp--announced)
        (current (list (supersonic-upnp--current-track)
                       (not (supersonic-upnp--playing-p))
                       (copy-sequence supersonic-upnp--queue))))
    (setq supersonic-upnp--announced current)
    (cond
     ((not (equal (car previous) (car current)))
      (supersonic-upnp--scrobble-track-change (car previous) (car current))
      (run-hooks 'supersonic-playback-track-change-hook))
     (went-live
      (run-hooks 'supersonic-playback-track-change-hook))
     (t
      (unless (equal (nth 2 previous) (nth 2 current))
        (run-hooks 'supersonic-playback-queue-change-hook))
      (unless (eq (nth 1 previous) (nth 1 current))
        (run-hooks 'supersonic-playback-state-change-hook))))))

(aio-defun
 supersonic-upnp--poll ()
 "Ask the renderer for its transport state and position, and act on it.
Caches the answer as `supersonic-upnp--snapshot', marks the backend
live, and runs the facade's hooks for whatever changed.  When the
answer shows that the current track played to its end -- see
`supersonic-upnp--track-ended-p' -- the next queued track is loaded.

A failure marks the backend not live and is reported once per outage,
the way `supersonic-jukebox--poll' does, never as a backtrace.  Never
runs alongside another poll: with one in flight, this waits for it and
then polls itself."
 (while supersonic-upnp--poll-in-flight
   (aio-await supersonic-upnp--poll-in-flight))
 (let ((done (aio-promise))
       (ended nil))
   (setq supersonic-upnp--poll-in-flight done)
   (unwind-protect
       (let ((previous supersonic-upnp--snapshot)
             (index supersonic-upnp--index)
             (was-live supersonic-upnp--live))
         (condition-case err
             (let* ((transport (aio-await (supersonic-upnp--soap "GetTransportInfo")))
                    (position (aio-await (supersonic-upnp--soap "GetPositionInfo")))
                    (duration (supersonic-upnp--parse-time (supersonic-upnp--child-text position 'TrackDuration)))
                    (current
                     (list :state (supersonic-upnp--child-text transport 'CurrentTransportState)
                           :position (supersonic-upnp--parse-time (supersonic-upnp--child-text position 'RelTime))
                           :duration (and duration (> duration 0) duration)
                           :index index
                           :polled-at (float-time))))
               (setq supersonic-upnp--snapshot current)
               (setq supersonic-upnp--live t)
               (setq supersonic-upnp--poll-failing nil)
               (setq ended (supersonic-upnp--track-ended-p previous current))
               (supersonic-upnp--announce (not was-live)))
           (error
            (setq supersonic-upnp--live nil)
            (setq supersonic-upnp--control nil)
            (unless supersonic-upnp--poll-failing
              (setq supersonic-upnp--poll-failing t)
              (supersonic--report-async-error "poll the UPnP renderer" err)
              (run-hooks 'supersonic-playback-track-change-hook)))))
     (setq supersonic-upnp--poll-in-flight nil)
     (aio-resolve done #'ignore))
   ;; Only now that this poll is no longer in flight: loading the next
   ;; track polls in turn.
   (when ended
     (aio-await (supersonic-upnp--advance (plist-get supersonic-upnp--snapshot :index))))))

(defun supersonic-upnp--poll-tick ()
  "Poll the renderer, unless a poll is still waiting for its answer.
What `supersonic-upnp--timer' runs; see `supersonic-jukebox--poll-tick'
for why it skips rather than waits."
  (unless supersonic-upnp--poll-in-flight
    (ignore (supersonic-upnp--poll))))

(aio-defun
 supersonic-upnp--play-index (index)
 "Load the queue's entry at INDEX on the renderer and play it.
The hooks run for the new track at once, before the renderer has even
been told, so that what shows it does not lag a poll behind."
 (setq supersonic-upnp--index index)
 (setq supersonic-upnp--snapshot nil)
 (setq supersonic-upnp--stopped nil)
 (supersonic-upnp--announce)
 (let ((item (aio-await (supersonic-upnp--item (nth index supersonic-upnp--queue)))))
   (aio-await
    (supersonic-upnp--soap
     "SetAVTransportURI" `(("CurrentURI" . ,(plist-get item :url)) ("CurrentURIMetaData" . ,(plist-get item :metadata)))))
   (aio-await (supersonic-upnp--soap "Play" '(("Speed" . "1")))))
 (aio-await (supersonic-upnp--poll)))

(aio-defun
 supersonic-upnp--advance (index)
 "Move on from the queue's entry at INDEX, which just played to its end.
Plays the entry after it, or, past the end of the queue, leaves
nothing current.  Does nothing if the user moved to another entry in
the meantime."
 (supersonic--with-async-error-handling
  nil "play the next track on the UPnP renderer"
  (when (eql index supersonic-upnp--index)
    (if (< (1+ index) (length supersonic-upnp--queue))
        (aio-await (supersonic-upnp--play-index (1+ index)))
      (setq supersonic-upnp--index nil)
      (supersonic-upnp--announce)))))

;;;
;;; The facade's operations
;;;
;;; As in `supersonic-jukebox.el', each operation the facade fires and
;;; forgets is a plain function around an `aio-defun' that reports its
;;; own failures, which would otherwise reject a promise nobody awaits.
;;;

(aio-defun
 supersonic-upnp-status (key)
 "Resolve to the renderer's current KEY, from the latest poll.
Never asks the renderer itself; see `supersonic-upnp--poll'."
 (pcase key
   ('track-id (supersonic-upnp--current-track))
   ('position (supersonic-upnp--position))
   ('paused (not (supersonic-upnp--playing-p)))))

(aio-defun
 supersonic-upnp-queue ()
 "Resolve to the client-side queue, marking the entry the renderer was given."
 (seq-map-indexed
  (lambda (id index) (list :track-id id :current (eql index supersonic-upnp--index)))
  supersonic-upnp--queue))

(aio-defun
 supersonic-upnp--start (ids) "Replace the queue with IDS and play the first."
 (supersonic--with-async-error-handling
  nil "start UPnP playback"
  (setq supersonic-upnp--queue (copy-sequence ids))
  (if ids
      (aio-await (supersonic-upnp--play-index 0))
    (setq supersonic-upnp--index nil)
    (supersonic-upnp--announce))))

(defun supersonic-upnp-start (ids)
  "Replace the queue with IDS and start playing them on the renderer."
  (ignore (supersonic-upnp--start ids)))

(aio-defun
 supersonic-upnp--enqueue (ids)
 "Append IDS to the queue, playing the first if nothing is left to play.
Nothing is left to play before anything was started and once the
queue has been played to its end.  A queue merely stopped or paused
partway through is left that way, the same as the jukebox does."
 (supersonic--with-async-error-handling
  nil "enqueue on the UPnP renderer"
  (let ((length (length supersonic-upnp--queue)))
    (setq supersonic-upnp--queue (append supersonic-upnp--queue ids))
    (if (and ids (null supersonic-upnp--index))
        (aio-await (supersonic-upnp--play-index length))
      (supersonic-upnp--announce)))))

(defun supersonic-upnp-enqueue (ids)
  "Append IDS to the queue, playing them if nothing is left to play."
  (ignore (supersonic-upnp--enqueue ids)))

(aio-defun
 supersonic-upnp--toggle-play ()
 "Pause the renderer if it is playing, or else play the current track.
Decided from the latest poll.  A renderer that has no media any more
-- because another application used it in the meantime, say -- gets
the current track loaded again rather than told to play nothing."
 (supersonic--with-async-error-handling
  nil "toggle UPnP playback"
  (cond
   ((supersonic-upnp--playing-p)
    (aio-await (supersonic-upnp--soap "Pause"))
    (aio-await (supersonic-upnp--poll)))
   ((null supersonic-upnp--index))
   ((member (plist-get supersonic-upnp--snapshot :state) '("PAUSED_PLAYBACK" "STOPPED"))
    (setq supersonic-upnp--stopped nil)
    (aio-await (supersonic-upnp--soap "Play" '(("Speed" . "1"))))
    (aio-await (supersonic-upnp--poll)))
   (t
    (aio-await (supersonic-upnp--play-index supersonic-upnp--index))))))

(defun supersonic-upnp-toggle-play ()
  "Toggle between playing and paused on the renderer."
  (ignore (supersonic-upnp--toggle-play)))

(aio-defun
 supersonic-upnp--next () "Play the queue's next entry, if there is one."
 (supersonic--with-async-error-handling
  nil "skip to the next track on the UPnP renderer"
  (when (and supersonic-upnp--index (< (1+ supersonic-upnp--index) (length supersonic-upnp--queue)))
    (aio-await (supersonic-upnp--play-index (1+ supersonic-upnp--index))))))

(defun supersonic-upnp-next ()
  "Skip to the next track in the queue."
  (ignore (supersonic-upnp--next)))

(aio-defun
 supersonic-upnp--prev () "Play the queue's previous entry, if there is one."
 (supersonic--with-async-error-handling
  nil "go back to the previous track on the UPnP renderer"
  (when (and supersonic-upnp--index (> supersonic-upnp--index 0))
    (aio-await (supersonic-upnp--play-index (1- supersonic-upnp--index))))))

(defun supersonic-upnp-prev ()
  "Go back to the previous track in the queue."
  (ignore (supersonic-upnp--prev)))

(aio-defun
 supersonic-upnp--stop ()
 "Stop the renderer, if it was given anything to play from here.
Otherwise it is left alone: switching away from the `upnp' backend
stops it, and that must not stop whatever another application has the
renderer playing.  The queue and its current entry stay, so that
playing again starts that entry over."
 (supersonic--with-async-error-handling
  nil "stop the UPnP renderer"
  (when supersonic-upnp--index
    (setq supersonic-upnp--stopped t)
    (aio-await (supersonic-upnp--soap "Stop"))
    (aio-await (supersonic-upnp--poll)))))

(defun supersonic-upnp-stop ()
  "Stop playback on the renderer."
  (ignore (supersonic-upnp--stop)))

(aio-defun
 supersonic-upnp--seek-to (seconds)
 "Seek the renderer to SECONDS into the current track."
 (aio-await
  (supersonic-upnp--soap "Seek" `(("Unit" . "REL_TIME") ("Target" . ,(supersonic-upnp--format-time seconds)))))
 (aio-await (supersonic-upnp--poll))
 (run-hooks 'supersonic-playback-position-change-hook))

(aio-defun
 supersonic-upnp--seek (offset)
 "Seek OFFSET seconds from the renderer's current position.
`Seek' takes an absolute position, so OFFSET is added to the one
counted on from the latest poll -- see `supersonic-upnp--position'."
 (supersonic--with-async-error-handling
  nil "seek on the UPnP renderer"
  (aio-await (supersonic-upnp--seek-to (+ (or (supersonic-upnp--position) 0) offset)))))

(defun supersonic-upnp-seek (offset)
  "Seek OFFSET seconds relative to the current position on the renderer."
  (ignore (supersonic-upnp--seek offset)))

(aio-defun
 supersonic-upnp--seek-fraction (fraction)
 "Seek to FRACTION (0.0 to 1.0) of the way through the current track.
Multiplied by the duration the renderer reported; one that reports
none cannot be seeked this way, which is said rather than guessed at."
 (supersonic--with-async-error-handling
  nil "seek on the UPnP renderer"
  (let ((duration (plist-get supersonic-upnp--snapshot :duration)))
    (unless duration
      (error "The UPnP renderer reports no duration for the current track"))
    (aio-await (supersonic-upnp--seek-to (* fraction duration))))))

(defun supersonic-upnp-seek-fraction (fraction)
  "Seek to FRACTION (0.0 to 1.0) of the way through the renderer's track."
  (ignore (supersonic-upnp--seek-fraction fraction)))

;;;
;;; Polling only while `upnp' is the active backend
;;;

(defun supersonic-upnp--start-polling ()
  "Start polling the renderer, unless already polling, and poll once now."
  (unless supersonic-upnp--timer
    (setq supersonic-upnp--timer
          (run-at-time supersonic-upnp-poll-interval supersonic-upnp-poll-interval #'supersonic-upnp--poll-tick))
    (supersonic-upnp--poll-tick)))

(defun supersonic-upnp--stop-polling ()
  "Stop polling the renderer and forget what the last poll found.
The queue stays, for when `upnp' is selected again."
  (when supersonic-upnp--timer
    (cancel-timer supersonic-upnp--timer)
    (setq supersonic-upnp--timer nil))
  (setq supersonic-upnp--snapshot nil)
  (setq supersonic-upnp--live nil)
  (setq supersonic-upnp--poll-failing nil)
  (setq supersonic-upnp--announced nil))

(defun supersonic-upnp--watch-backend (_symbol new-value _operation _where)
  "Poll the renderer exactly while `supersonic-playback-backend' is `upnp'.
NEW-VALUE is the backend about to be selected.  Watches
`supersonic-playback-backend' the way `supersonic-jukebox--watch-backend'
does."
  (if (eq new-value 'upnp)
      (supersonic-upnp--start-polling)
    (supersonic-upnp--stop-polling)))

;; `remove-variable-watcher' first so that re-evaluating this file
;; cannot stack a second watcher.
(remove-variable-watcher 'supersonic-playback-backend #'supersonic-upnp--watch-backend)
(add-variable-watcher 'supersonic-playback-backend #'supersonic-upnp--watch-backend)
(supersonic-upnp--watch-backend 'supersonic-playback-backend supersonic-playback-backend 'set nil)

;; A renderer fetches what it plays from a URL, so the backend plays
;; for any provider that can name one.
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
