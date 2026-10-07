;;; supersonic-api.el --- Subsonic HTTP/auth/JSON layer for supersonic.el -*- lexical-binding: t; -*-

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

;; Subsonic HTTP/auth/JSON plumbing for supersonic.el: building
;; authenticated request URLs and fetching and decoding JSON responses.
;; No knowledge of mpv or of any particular buffer/UI here.  Browsing
;; reaches it only through the Subsonic provider in
;; `supersonic-subsonic.el'; see `supersonic-provider.el'.

;;; Code:
(require 'json)
(require 'url)
(require 'aio)
(require 'supersonic-custom)

(defun supersonic-auth ()
  "Return the auth-source entry for the current `supersonic-host'.
Calls `auth-source-search' fresh every time rather than memoizing the
result ourselves -- `auth-source-search' already caches internally
\(see `auth-source-do-cache'), but that cache is invalidated by
`auth-source-forget-all-cached' and expires on its own, so deferring
to it means both a `supersonic-host' change and a corrected
authinfo entry (after forgetting the cache) take effect on the next
request instead of being frozen in for the rest of the Emacs
session."
  (car (auth-source-search :host supersonic-host)))

(defun supersonic-alist->query (al)
  "Convert AL, an alist of string keys to string values, to a query string.
Returns \"\" for an empty AL, so `supersonic-build-url' never appends a
dangling \"?\".

Values are percent-encoded here rather than at each call site.  Subsonic
ids are opaque server-generated strings and some servers really do emit
ones containing characters that are reserved in a query (\"&\", \"+\",
\"=\"), which would otherwise silently split one parameter into two --
and a search query or podcast feed url has no chance of being clean.
Keys are left alone: every one of them is a literal spelled out in this
package.  A nil value still encodes as the empty string, the same as
the plain `concat' this used to do."
  (if al
      (concat "?" (mapconcat (lambda (q) (concat (car q) "=" (url-hexify-string (or (cdr q) "")))) al "&"))
    ""))

;; fix byte-compiler complaints
(defvar url-http-end-of-headers)

(defun supersonic-url-retrieve (url)
  "Wrap `url-retrieve' of URL in a promise of (STATUS . BUFFER).
Like `aio-url-retrieve', except that it gives up once the server has
not answered within `supersonic-request-timeout': the promise then
rejects, and the connection and BUFFER are gone already.  Otherwise
BUFFER is the response buffer, which the caller must kill.

`aio-url-retrieve' can't do this from the outside, since it never
hands out the buffer `url-retrieve' returns -- and closing the
connection is the point: a request that merely stopped being waited
for would still hold a file descriptor open (#70)."
  (let* ((promise (aio-promise))
         (timer nil)
         (buffer
          (condition-case err
              (url-retrieve url
                            (lambda (status)
                              (when timer
                                (cancel-timer timer))
                              (let ((value (cons status (current-buffer))))
                                (aio-resolve promise (lambda () value)))))
            (error (aio-resolve promise (lambda () (signal (car err) (cdr err))))
                   nil))))
    ;; `url-retrieve' calls back before returning for some URLs (and
    ;; connection errors), in which case there is nothing left to time.
    (when (and buffer (not (aio-result promise)))
      (setq timer (run-at-time supersonic-request-timeout nil
                               #'supersonic--abort-request promise buffer url supersonic-request-timeout)))
    promise))

(defun supersonic--abort-request (promise buffer url timeout)
  "Give up on the request for URL behind PROMISE, unanswered after TIMEOUT.
Close BUFFER's connection, kill BUFFER and reject PROMISE.  The
sentinel goes first so that `url-http' doesn't call back about the
closed connection as well, the same as `url-queue-kill-job'."
  (unless (aio-result promise)
    (when (buffer-live-p buffer)
      (let (process)
        (while (setq process (get-buffer-process buffer))
          (set-process-sentinel process #'ignore)
          (delete-process process)))
      (kill-buffer buffer))
    ;; Without the query string: it carries the auth token and salt.
    (let ((endpoint (car (split-string url "?"))))
      (aio-resolve promise
                   (lambda ()
                     (error "No answer from %s within %s seconds" endpoint timeout))))))

(aio-defun
 supersonic-get-json (url) "Return a promise resolving to the parsed json response from URL."
 (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-url-retrieve url))))
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
                      ;; The Subsonic API always returns UTF-8 JSON (per RFC 8259); `url-retrieve' doesn't reliably
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

(defun supersonic-get-id-as-string (data)
  "Return DATA's \"id\" field as a string, converting from a number if necessary."
  (let ((id (assoc-default "id" data)))
    (if (numberp id)
        (number-to-string id)
      id)))

(provide 'supersonic-api)
;;; supersonic-api.el ends here
