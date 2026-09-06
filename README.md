# supersonic.el

[![CI](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml/badge.svg)](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml)

*NO MELPA PACKAGE, YET*
<!-- [![MELPA](https://melpa.org/packages/supersonic-badge.svg)](https://melpa.org/#/supersonic) -->

This is a subsonic client for emacs using mpv for music playing.

Based on [subsonic.el](https://git.sr.ht/~amk/subsonic.el)

## Setup

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents

    machine SUBSONIC_URL login USERNAME password PASSWORD

The `supersonic-host` must be set to the same value as SUBSONIC_URL in
your init file, example below.

## Usage

~The package is available on melpa as `supersonic`~ (TODO)

Example use-package config:

```
(use-package supersonic
  :load-path "~/src/supersonic.el" ;; clone the repository here
  :commands supersonic
  :bind (("C-c m" . supersonic))
  :custom
  (supersonic-host "coolsupersonic.example.com")
  (supersonic-enable-art t)
  (supersonic-scrobble-plays t))
```

In case you are running subsonic server without HTTPS (HTTP only), add following line to the use-package :custom block 
```
(supersonic-ssl nil)
```

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

## MPRIS

`supersonic-mpris.el` exposes supersonic.el's mpv playback as an MPRIS
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

Currently in scope: Play/Pause/PlayPause/Stop/Next/Previous and
Metadata (title/artist/album/art/length). Seeking, volume, shuffle and
loop control are not implemented.

## Screenshots

<img width="1775" height="1094" alt="image" src="https://github.com/user-attachments/assets/7fa1726e-6399-46e3-966f-037e3c61d640" />

