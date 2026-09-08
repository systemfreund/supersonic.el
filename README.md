# supersonic.el

[![CI](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml/badge.svg)](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml)
<!-- [![MELPA](https://melpa.org/packages/supersonic-badge.svg)](https://melpa.org/#/supersonic) -->

This is a subsonic client for emacs using [mpv](https://mpv.io/) for music playing. It
works with any server implementing the Subsonic API, such as
[Navidrome](https://www.navidrome.org/), [Airsonic](https://airsonic.github.io/),
[Gonic](https://github.com/sentriz/gonic) or [Ampache](https://ampache.org/).

<img width="2560" height="1440" alt="image" src="https://github.com/user-attachments/assets/5083c2f6-61de-43ee-b403-a0a3e5cb29d5" />
<img width="859" height="983" alt="image" src="https://github.com/user-attachments/assets/11cc18f2-5581-472a-b648-c2089b54770d" />

Based on [subsonic.el](https://git.sr.ht/~amk/subsonic.el)

## Usage

~The package is available on melpa as `supersonic`~ (not available, yet. Please clone the repository instead.)

Example use-package config:

```elisp
(use-package supersonic
  :load-path "~/src/supersonic.el" ;; clone the repository here
  :commands supersonic
  :bind (("C-c m" . supersonic))
  :custom
  (supersonic-host "https://mysubsonicserver:4355") ;; For authentication see section below
  (supersonic-scrobble-plays t)
  (supersonic-enable-art t)
  (supersonic-enable-waveform t))
```

`supersonic-host` may be given without a scheme, in which case
`https://` is assumed. In case you are running a subsonic server
without HTTPS, prefix it with `http://` instead.

Use the `supersonic` command to open a transient with commonly used
commands available.

For a list of available configuration options check `customize-group supersonic`

## Play queue

In the tracks and albums buffers, `RET` replaces the current play
queue and starts playing immediately, while `a` adds to the play
queue instead, so that e.g. two albums can be queued up to play back
to back:

- Tracks buffer: `RET`/`a` act on the current track and every track
  after it in the buffer.
- Albums buffer: `RET` opens the album's track list; `a` adds the
  whole album to the play queue directly, without opening it.

## Now playing

`N` in the `supersonic` transient (`supersonic-show-now-playing`) opens
a buffer for the track mpv is currently on: cover art, title, artist,
album, format, duration and size, plus clickable playback controls.

Keys in that buffer: `SPC` play/pause, `n`/`p` next/previous track,
`f`/`b` seek, `g` refresh manually.

Enabling `supersonic-enable-waveform` adds a clickable waveform seekbar
below the transport buttons, click anywhere on it to seek there. 
`supersonic-waveform-buckets` sets both its resolution and how
finely the track is analyzed; `supersonic-waveform-width`/
`-height` set its display size. `supersonic-waveform-samplerate` sets
how much audio detail the analysis looks at, and so what it costs in
time and memory. Envelopes are cached on disk under
`supersonic-cache-path`, keyed on the bucket count and the sample rate,
so changing either costs a one-off re-analysis rather than mixing
measurements.

## Cover art

Cover art needs `supersonic-enable-art` to be enabled, and a graphical 
frame to draw it in. Its size is set 
per view: `supersonic-list-art-size`  for the album and podcast lists, 
`supersonic-now-playing-art-size` here. Each is both the display height 
in pixels and the size requested from the server, and the cache keeps 
one file per size, so raising either one costs a single re-download.

## MPRIS

`supersonic-mpris.el` exposes supersonic.el's playback as an MPRIS
player on the D-Bus session bus, so desktop environments and tools such
as `playerctl` can see and control it. It is not loaded or activated
automatically, and supersonic.el has no dependency on it. Enable it
explicitly:

```
(use-package supersonic
  :config
  (require 'supersonic-mpris)
  (supersonic-mpris-mode t))
```

## Authentication

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents

    machine SUBSONIC_URL login USERNAME password PASSWORD

`SUBSONIC_URL` is the URL of your server, e.g.
`https://mysubsonicserver:4355` or `http://localhost:1234`.

Make sure the `machine` field in your authinfo entry matches `supersonic-host` 
exactly, scheme included.

### KeePassXC via Secret Service

If you'd rather keep the credentials in KeePassXC than in an
authinfo file, enable *Secret Service Integration* under
`Tools -> Settings -> Secret Service Integration` and unlock the
database. Then [tell](https://www.gnu.org/software/emacs/manual/html_node/auth/Secret-Service-API.html#Secret-Service-API-1) Emacs to also search that collection

`auth-source`'s Secret Service backend only matches on an entry's
custom *Attributes*, not on its regular URL/username fields, so add
these on the `Advanced` tab of the entry:

- `host` set to the same value as `supersonic-host` (with scheme, e.g.
  `https://coolsupersonic.example.com`)
- `user` set to your subsonic username

The entry's regular password field is used as the secret.
