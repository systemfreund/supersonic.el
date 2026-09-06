# supersonic.el

[![CI](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml/badge.svg)](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml)
<!-- [![MELPA](https://melpa.org/packages/supersonic-badge.svg)](https://melpa.org/#/supersonic) -->

This is a subsonic client for emacs using [mpv](https://mpv.io/) for music playing. It
works with any server implementing the Subsonic API, such as
[Navidrome](https://www.navidrome.org/), [Airsonic](https://airsonic.github.io/),
[Gonic](https://github.com/sentriz/gonic) or [Ampache](https://ampache.org/).

<img width="1775" height="1094" alt="image" src="https://github.com/user-attachments/assets/7fa1726e-6399-46e3-966f-037e3c61d640" />

Based on [subsonic.el](https://git.sr.ht/~amk/subsonic.el)

## Usage

~The package is available on melpa as `supersonic`~ (not available, yet. Please clone the repository instead.)

Example use-package config:

```
(use-package supersonic
  :load-path "~/src/supersonic.el" ;; clone the repository here
  :commands supersonic
  :bind (("C-c m" . supersonic))
  :custom
  (supersonic-host "https://coolsupersonic.example.com")
  (supersonic-enable-art t)
  (supersonic-scrobble-plays t))
```

`supersonic-host` may be given without a scheme, in which case
`https://` is assumed. In case you are running a subsonic server
without HTTPS (HTTP only), prefix it with `http://` instead -- and
make sure the `machine` field in your authinfo entry matches
`supersonic-host` exactly, scheme included.

Use the `supersonic` command to open a transient with commonly used
commands available.

For a list of available configuration options check `customize-group supersonic`


## Authentication

Add a `~/.authinfo.gpg` or `~/.authinfo` file with the following contents

    machine SUBSONIC_URL login USERNAME password PASSWORD

`SUBSONIC_URL` is the URL of your server, e.g.
`https://coolsupersonic.example.com` or
`http://coolsupersonic.example.com:4533`.

The `supersonic-host` in your init file must be set to the same
value as `SUBSONIC_URL`, example below.

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


