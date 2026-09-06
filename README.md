# subsonic.el

[![MELPA](https://melpa.org/packages/subsonic-badge.svg)](https://melpa.org/#/subsonic)

This is a subsonic client for emacs using mpv for music playing.

## Setup

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents

    machine SUBSONIC_URL login USERNAME password PASSWORD

The `subsonic-host` must be set to the same value as SUBSONIC_URL in
your init file, example below.

## Usage

The package is available on melpa as `subsonic`

Example use-package config:

```
(use-package subsonic
  :commands subsonic
  :bind (("C-c m" . subsonic))
  :custom
  (subsonic-host "coolsubsonic.example.com")
  (subsonic-enable-art t))
```

In case you are running subsonic server without HTTPS ( HTTP only), add following line to the use-package :custom block 
```
(subsonic-ssl nil)
```

Use the `subsonic` command to open a transient with commonly used
commands available.

For a list of available configuration options check `customize-group subsonic`

## Play queue

In the tracks and albums buffers, `RET` replaces the current play
queue and starts playing immediately, while `a` adds to the play
queue instead, so that e.g. two albums can be queued up to play back
to back:

- Tracks buffer: `RET`/`a` act on the current track and every track
  after it in the buffer.
- Albums buffer: `RET` opens the album's track list; `a` adds the
  whole album to the play queue directly, without opening it.

## MPRIS

`subsonic-mpris.el` exposes subsonic.el's mpv playback as an MPRIS
player on the D-Bus session bus, so desktop environments and tools such
as `playerctl` can see and control it. It is not loaded or activated
automatically, and subsonic.el has no dependency on it. Enable it
explicitly:

```
(use-package subsonic
  :config
  (require 'subsonic-mpris)
  (subsonic-mpris-mode t))
```

Currently in scope: Play/Pause/PlayPause/Stop/Next/Previous and
Metadata (title/artist/album/art/length). Seeking, volume, shuffle and
loop control are not implemented.

## Info

This uses some code from docker.el for examples of transient and
tabulated-list-mode as well as the mpv logic from mpv.el

This has only been tested with gonic however it should function with
other servers

## Contributing/Issues

For quick questions, I'm `amk` on libera.chat, you can find me in #emacs

Please send any patches or share any issues you may have on the mailing list here:
https://lists.sr.ht/~amk/public-inbox

or alternatively if you prefer a pull-request style flow :
https://codeberg.org/amk/subsonic.el


## Screenshots

![album list view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/artist.png)
![podcasts view](https://git.sr.ht/~amk/subsonic.el/blob/master/images/podcasts.png)
