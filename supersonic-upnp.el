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
;; describing it -- title, artist, album, and a `res' element whose
;; `protocolInfo' names the exact MIME type of what that URL serves.
;; Getting `protocolInfo' wrong (claiming MP3 while the URL actually
;; serves FLAC, say) is a common way for cheap renderers to simply
;; refuse to play, or to play noise -- so it has to reflect what will
;; really come back, not just an assumption.
;;
;; That is where `supersonic-provider-stream-url''s optional FORMAT
;; hint comes in: when a caller asked the active provider to transcode
;; to a given format, `protocolInfo' has to say so instead of trusting
;; the track's own, pre-transcoding `:content-type'.  This file knows
;; nothing about which format to ask for, or when -- that policy, and
;; the renderer connection itself, belongs to the backend in #65.
;;
;; This file has no `supersonic-playback-register-backend' call yet
;; and nothing in the package requires it: it is only ever pulled in
;; by `supersonic-tests.el' until #65 gives it a caller.

;;; Code:
(require 'xml)
(require 'supersonic-custom)

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

(defun supersonic-upnp--didl-lite (url track &optional format)
  "Return a DIDL-Lite XML document describing URL, a stream of TRACK.
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
hand it back verbatim in whatever it reports playing.

Cover art (`albumArtURI') is deliberately left out of this first
slice: it would mean deciding whether the same URL a renderer already
gets for the stream -- and everything that implies about who a
credentialed art URL is handed to -- is acceptable to also hand it for
art, and that decision belongs with the backend that actually casts,
not with this XML-shaping helper."
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
     "<upnp:class>object.item.audioItem.musicTrack</upnp:class>"
     (format
      "<res protocolInfo=\"%s\">%s</res>" (xml-escape-string protocol-info) (xml-escape-string url))
     "</item>"
     "</DIDL-Lite>")))

(provide 'supersonic-upnp)
;;; supersonic-upnp.el ends here
