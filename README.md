# supersonic.el

[![CI](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml/badge.svg)](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml)
<!-- [![MELPA](https://melpa.org/packages/supersonic-badge.svg)](https://melpa.org/#/supersonic) -->

This is a subsonic client for emacs using [mpv](https://mpv.io/) for music playing. It
works with any server implementing the Subsonic API, such as
[Navidrome](https://www.navidrome.org/), [Airsonic](https://airsonic.github.io/),
[Gonic](https://github.com/sentriz/gonic) or [Ampache](https://ampache.org/).

<img width="1445" height="1041" alt="image" src="https://github.com/user-attachments/assets/095571c2-0c8c-4c38-9fa8-d1ae1b5c10ec" />
<img width="1924" height="1081" alt="image" src="https://github.com/user-attachments/assets/e7e698ca-6b1f-4505-8452-1118b6a66762" />
<img width="1920" height="1042" alt="image" src="https://github.com/user-attachments/assets/0bfad485-afe9-4ab8-972a-5ce8d26925ce" />

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
  (supersonic-enable-scrobbling t)
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
next to the cover art, above the label and the transport buttons,
click anywhere on it to seek there.
`supersonic-waveform-width`/`-height` set its display size. 

The label below the cover art shows the track's title by default.
Setting `supersonic-now-playing-cycle-fields` to a list of `title`,
`artist` and/or `album` makes it rotate through them instead, every
`supersonic-now-playing-cycle-interval` seconds (10 by default).

That rotation can also be layered onto the cover art itself, in place
of the side label, by adding `supersonic-now-playing-animate-art-overlay`
to `supersonic-now-playing-animation-functions` (the default,
`supersonic-now-playing-animate-label`, is what draws the side label
instead). `supersonic-now-playing-animate-art-overlay-scroll` is a
third option: instead of switching between fields, it joins all of
them into one line and scrolls it across the art. To show more than
one of these at once (e.g. the side label and the art overlay
together), just list more than one.

With both `supersonic-enable-art` and `supersonic-enable-waveform` on,
turning on `supersonic-now-playing-waveform-in-overlay` layers the
waveform seekbar onto the cover art as well, in a lane above the text,
instead of showing it next to the cover art on its own.

The Title/Artist/Album/Duration/Format/Size rows below the transport
buttons come from `supersonic-now-playing-render-functions`, a list
with one function per row, in the order they should appear. Drop the
entries you don't want -- setting it to `nil` hides all of them,
leaving just the cover art, the label and the buttons -- or reorder it,
or add a function of your own for a row it doesn't already have.

By default the now-playing buffer opens in the selected window, replacing
whatever was there. To keep it pinned in its own window instead, e.g. to
browse albums or tracks alongside it as in the screenshot above, put it
in a side window via `display-buffer-alist`:

```elisp
(add-to-list 'display-buffer-alist
             `(,supersonic-now-playing-buffer-name
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.3)))
```

`N`/`supersonic-show-now-playing` then always opens that buffer on the
right, leaving whichever list buffer you had open (artist albums, search
results, ...) in place on the left.

## Cover art

Cover art needs `supersonic-enable-art` to be enabled, and a graphical 
frame to draw it in. Its size is set per view: `supersonic-list-art-size` 
for the album and podcast lists, `supersonic-now-playing-art-size` here. 

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

## Jukebox playback

`supersonic-jukebox.el` plays back through the Subsonic server's own
remote jukebox instead of a local mpv process, so tracks play out of 
the server's own speakers. It is not loaded or activated automatically, 
and supersonic.el has no dependency on it. Enable it explicitly and 
select it as the active backend:

```
(use-package supersonic
  :custom
  (supersonic-playback-backend 'jukebox)
  :config
  (require 'supersonic-jukebox))
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
