;;; supersonic-api.el --- Subsonic HTTP/auth/JSON layer for supersonic.el -*- lexical-binding: t; -*-

;; Author: systemfreund <github@o9z.de>
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

;; Subsonic HTTP/auth/JSON plumbing for supersonic.el: building
;; authenticated request URLs, fetching and decoding JSON responses, and
;; the generic async-error-handling helpers the various list buffers
;; wrap their refreshes in.  No knowledge of mpv or of any particular
;; buffer/UI here.

;;; Code:
(require 'json)
(require 'url)
(require 'aio)

;; fix byte-compiler complaints
(defvar supersonic-host)

(defun supersonic-auth ()
  "Return the auth-source entry for the current `supersonic-host'.
Calls `auth-source-search' fresh every time rather than memoizing the
result ourselves -- `auth-source-search' already caches internally
(see `auth-source-do-cache'), but that cache is invalidated by
`auth-source-forget-all-cached' and expires on its own, so deferring
to it means both a `supersonic-host' change and a corrected
authinfo entry (after forgetting the cache) take effect on the next
request instead of being frozen in for the rest of the Emacs
session."
  (car (auth-source-search :host supersonic-host)))

(defun supersonic-alist->query (al)
  "Convert an alist -- AL to a set of url query parameters."
  (if al
      (concat "?" (mapconcat (lambda (q) (concat (car q) "=" (cdr q))) al "&"))
    ""))

;; fix byte-compiler complaints
(defvar url-http-end-of-headers)

(aio-defun
 supersonic-get-json (url) "Return a promise resolving to the parsed json response from URL."
 (pcase-let ((`(,status . ,buffer) (aio-await (aio-url-retrieve url))))
   (unwind-protect
       (progn
         (when (plist-get status :error)
           (error "Failed to fetch %s: %S" url (plist-get status :error)))
         (with-current-buffer buffer
           (let*
               ((json-array-type 'list)
                (json-key-type 'string)
                (data
                 (condition-case nil
                     (json-read-from-string
                      ;; The Subsonic API always returns UTF-8 JSON (per RFC 8259); `aio-url-retrieve' doesn't reliably
                      ;; decode the body for us across Emacs versions, so decode explicitly.  Safe even if it's already
                      ;; decoded: `decode-coding-string' is a no-op on text that isn't raw undecoded bytes.
                      (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8))
                   (json-readtable-error
                    (error "Failed to read json")))))
             (supersonic--signal-if-failed data)
             data)))
     (kill-buffer buffer))))

(defun supersonic--signal-if-failed (data)
  "Signal an `error' if the parsed Subsonic response DATA reports failure.
The Subsonic API reports application-level failures (e.g. a wrong
username/password from auth-source) inside a 200 OK response body,
as a \"subsonic-response\" with status \"failed\" and an \"error\"
object, rather than via the HTTP status -- so without this check
`supersonic-get-json' would return that body as if it were a normal,
empty result instead of raising anything the user can see."
  (let ((response (supersonic-recursive-assoc data '("subsonic-response"))))
    (when (equal (assoc-default "status" response) "failed")
      (let ((err (assoc-default "error" response)))
        (user-error "%s" (or (assoc-default "message" err) "Subsonic request failed"))))))

(defun supersonic-recursive-assoc (data keys)
  "Recursively assoc DATA from a list of KEYS."
  (if keys
      (supersonic-recursive-assoc (assoc-default (car keys) data) (cdr keys))
    data))

(defun supersonic--random-salt ()
  "Generate a random alphanumeric salt for Subsonic token authentication.
12 hex characters, well above the API's 6-character minimum."
  (mapconcat (lambda (_) (format "%x" (random 16))) (make-list 12 nil) ""))

(defun supersonic--auth-query ()
  "Build the \"u\"/\"t\"/\"s\" token-auth query parameters for one request.
Uses Subsonic's token authentication (t = md5(password + salt), s = a
fresh salt per request) instead of sending the plaintext password, so
it never ends up in a URL -- which, depending on how that URL is used
elsewhere (e.g. handed to curl as an argument), could otherwise be
visible to any local user via `ps' or in a subprocess's argv."
  (let* ((auth (supersonic-auth))
         (password (funcall (plist-get auth :secret)))
         (salt (supersonic--random-salt)))
    `(("u" . ,(plist-get auth :user)) ("t" . ,(md5 (concat password salt))) ("s" . ,salt))))

(defun supersonic-build-url (endpoint extra-query)
  "Build a valid supersonic url for a given ENDPOINT.
EXTRA-QUERY is used for any extra query parameters"
  (let ((auth (supersonic-auth)))
    (if auth
        (let ((host (plist-get auth :host)))
          (concat
           (unless (string-match-p "\\`https?://" host)
             "https://")
           host "/rest" endpoint
           (supersonic-alist->query
            (append (supersonic--auth-query) `(("c" . "ElSonic") ("v" . "1.16.0") ("f" . "json")) extra-query))))
      (user-error
       "Failed to load .authinfo, please provide auth configuration for
supersonic, and ensure supersonic-host is set correctly"))))

(defun supersonic--report-async-error (description err)
  "Tell the user that DESCRIPTION failed with ERR via the echo area.
DESCRIPTION is a short present-tense phrase, e.g. \"fetch tracks\"."
  (message "[Supersonic] Failed to %s: %s" description (error-message-string err)))

(defun supersonic--handle-async-error (buffer description err)
  "Report that DESCRIPTION failed with ERR, both in BUFFER and the echo area.
BUFFER is the tabulated-list buffer whose refresh failed; its contents
are replaced with the error and configuration hints.  The same failure
is also echoed via `supersonic--report-async-error'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Error: Failed to %s: %s\n\n" description (error-message-string err)))
        (insert "Configuration hint:\n")
        (insert "  - Check that supersonic-host is configured correctly\n")
        (insert "  - Ensure the scheme (http:// or https://) matches your server\n")
        (insert "  - Verify .authinfo has the correct host (must match supersonic-host exactly)\n"))))
  (supersonic--report-async-error description err))

(defmacro supersonic--with-async-error-handling (buff description &rest body)
  "Run BODY, reporting any error via DESCRIPTION instead of propagating it.
BUFF, if non-nil, is a tabulated-list buffer whose contents are replaced
with the error and configuration hints, in addition to an echo-area
message; if BUFF is nil, only the echo-area message is shown.
DESCRIPTION is a short present-tense phrase, e.g. \"fetch tracks\",
combined into \"Failed to DESCRIPTION: ERR\".

Wraps BODY in a `condition-case'.  Safe to use inside an `aio-defun':
generator.el fully macroexpands a function body -- including calls to
this macro -- before transforming it, and the `condition-case' this
expands to is itself transform-aware."
  (declare (indent 2))
  `(condition-case err
       (progn
         ,@body)
     (error
      (if ,buff
          (supersonic--handle-async-error ,buff ,description err)
        (supersonic--report-async-error ,description err)))))

(defun supersonic--init-list-buffer (buff mode-fn placeholder)
  "Ready BUFF as a fresh tabulated-list buffer while an async refresh runs.
Turns on MODE-FN (a derived tabulated-list mode) and shows PLACEHOLDER
text (e.g. \"Loading tracks...\") until the refresh that follows
replaces it with real entries."
  (with-current-buffer buff
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert placeholder "\n")
    (setq buffer-read-only t)
    (funcall mode-fn)))

(provide 'supersonic-api)
;;; supersonic-api.el ends here
