# Emacs integration: use it, on purpose

This repo is developed live, on stream. Viewers watch the Emacs frame, not the
terminal. That changes the default: prefer doing things *through* the running
Emacs session over doing them invisibly in the sandbox, whenever both are
reasonably possible.

## What's available

An `emacs` MCP server is registered (`claude mcp list` shows it) and talks to
the user's actual, already-running Emacs session over a Unix socket
(`~/.emacs.d/emacs-mcp-server.sock`, backed by
`~/src/emacs-mcp-server`). It currently exposes:

- **`eval-elisp`** — run arbitrary Elisp in that session. Use it to open/visit
  files, move point, switch buffers, `eval-defun`, `revert-buffer`, run
  `M-x ert`/project tests, etc.
- **`get-diagnostics`** — pull flycheck/flymake diagnostics from project
  buffers instead of re-deriving them from a separate lint run.

## Default: prefer the Emacs tools when editing this repo

- Before editing a file, `find-file`/`switch-to-buffer` it in the live session
  so the viewer sees the buffer that's about to change, then make the edit
  (still fine to use `Edit`/`Write` for the actual content — the visitability
  win is having the *right buffer already open and visible* when the diff
  lands, not routing every keystroke through Elisp).
- After an external edit (`Edit`/`Write`/shell), `revert-buffer` the
  corresponding Emacs buffer so what's on screen matches disk — don't leave
  the audience looking at stale content.
- Run tests/checks through the Emacs session (`M-x ert-run-tests-interactively`,
  project compile, etc.) rather than a background shell command, so results
  render in a visible buffer instead of a tool-result the stream can't see.
  Use `get-diagnostics` to surface flycheck/flymake state instead of shelling
  out to a linter.
- Narrating a change by moving point to the relevant defun and evaluating it
  (`eval-defun`) is preferred over a silent full-buffer reload when the point
  is to show *that specific change* taking effect.

This is a deliberate trade for visibility, not a claim that the Emacs path is
faster — do it because the audience is watching that frame, not despite it.

## Keep `eval-elisp` calls simple, one step at a time

Prefer flat, single-purpose expressions over nested ones that chain several
actions into one `eval-elisp` call. A viewer following along can track
`(find-file "supersonic-mpv.el")` followed by `(goto-char (point-min))`
followed by `(search-forward "defun supersonic-mpv-play")` — three legible
steps, each with an obvious effect. They can't track the equivalent single
`(with-current-buffer (find-file-noselect "supersonic-mpv.el") (goto-char
(point-min)) (search-forward "defun supersonic-mpv-play") (eval-defun nil))`
call the same way: it's one opaque tool invocation, then one result, with the
intermediate state never shown.

So: split what would naturally be one nested `let`/`progn`/`with-current-buffer`
expression into a short sequence of separate `eval-elisp` calls instead —
even at the cost of a few extra tool round-trips. Each call should do one
thing (open a file, move point, evaluate one form, switch a buffer) so the
before/after is easy to narrate and easy for a viewer to predict. Reach for a
single combined expression only when the steps genuinely can't be observed
independently (e.g. a value has to be threaded through that isn't worth
displaying on its own).

`eval-elisp` is destructive/open-world by design (see the emacs-mcp-server
README's Security section) — the usual judgment about destructive tool calls
still applies. Don't reach for it to touch files or state outside this repo's
concern just because it's available.
