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
(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'aio)
(require 'supersonic-custom)

(defun supersonic-auth-source-credentials (host)
  "Return the auth-source entry for HOST, a `supersonic-host'.
The default `supersonic-auth-function': the entry is a plist with the
user name under :user and the password under :secret, as that option
asks for.  Only an entry with a password is returned.

Calls `auth-source-search' fresh every time rather than memoizing the
result ourselves -- `auth-source-search' already caches internally
\(see `auth-source-do-cache'), but that cache is invalidated by
`auth-source-forget-all-cached' and expires on its own, so deferring
to it means both a `supersonic-host' change and a corrected
authinfo entry (after forgetting the cache) take effect on the next
request instead of being frozen in for the rest of the Emacs
session."
  (car (auth-source-search :host host :require '(:secret))))

(defun supersonic-auth (&optional host)
  "Return the credentials `supersonic-auth-function' has for HOST.
HOST defaults to `supersonic-host'.  Returns nil without asking while
it is unset: a lookup for no host would match any entry."
  (let ((host (or host supersonic-host "")))
    (unless (string-empty-p host)
      (funcall supersonic-auth-function host))))

(defvar supersonic--credentials-cache (make-hash-table :test #'equal)
  "What the functions `supersonic-cached' returned have kept.
Keyed by (ID . SERVER), ID telling those functions apart; each value
is (EXPIRES . RESULT), EXPIRES a `float-time'.")

(defun supersonic--cached-result (result)
  "Return RESULT with its secret resolved and copied, or nil if it has none.
RESULT is what a `supersonic-auth-function' or a
`supersonic-music-assistant-token-function' returns: a plist with the
secret under :secret, of which only :user and :secret are kept, or
the token itself.  Either may be a function returning the string --
which may be a list, as interpreted closures are before Emacs 30, so
a plist is told by the keyword it starts with.  A copy, as the string
handed out may be one its source wipes later, as
`password-cache-remove' does on expiry."
  (let ((resolve (lambda (secret)
                   (let ((secret (if (functionp secret) (funcall secret) secret)))
                     (and secret (copy-sequence secret))))))
    (if (keywordp (car-safe result))
        (let ((secret (funcall resolve (plist-get result :secret))))
          (and secret (list :user (plist-get result :user) :secret secret)))
      (funcall resolve result))))

;;;###autoload
(defun supersonic-cached (function)
  "Return a function calling FUNCTION and keeping its result per server.
For `supersonic-auth-function' and
`supersonic-music-assistant-token-function', when FUNCTION is slow --
it decrypts a file, say -- as both are called often.  FUNCTION is
called with the server as they are; what it returns is kept for
`supersonic-credentials-cache-expiry' seconds, with a secret handed
out as a function resolved first.  Finding nothing is not kept.
A server rejecting the credentials drops what was kept for it;
`supersonic-forget-credentials' drops everything, e.g. after a
password change.  So does wrapping a function anew, as when
re-evaluating the configuration, once what the old wrapper kept
expires."
  (let ((id (make-symbol "supersonic-cached")))
    (lambda (server)
      (let* ((key (cons id server))
             (entry (gethash key supersonic--credentials-cache)))
        (if (and entry (< (float-time) (car entry)))
            (cdr entry)
          (let ((result (supersonic--cached-result (funcall function server))))
            (remhash key supersonic--credentials-cache)
            (supersonic--drop-expired-credentials)
            (when result
              (puthash key (cons (+ (float-time) supersonic-credentials-cache-expiry) result)
                       supersonic--credentials-cache))
            result))))))

(defun supersonic--drop-expired-credentials ()
  "Drop whatever `supersonic-cached' kept that has expired.
Otherwise it would only go once the very same key is asked for again,
which for a server no longer used, or a function since wrapped anew,
is never."
  (let ((now (float-time)))
    (maphash (lambda (key entry)
               (unless (< now (car entry))
                 (remhash key supersonic--credentials-cache)))
             supersonic--credentials-cache)))

;;;###autoload
(defun supersonic-forget-credentials ()
  "Forget the credentials kept by functions made with `supersonic-cached'.
They are asked for again on the next request."
  (interactive)
  (clrhash supersonic--credentials-cache)
  nil)

(defun supersonic--forget-rejected-credentials (server)
  "Forget what `supersonic-cached' kept for SERVER, which just rejected it.
SERVER is as the credential functions are called with it.  Return a
note to add to the error saying so, or nil if nothing was kept for
SERVER -- trying again then changes nothing, so it is not suggested."
  (let ((forgotten nil))
    (maphash (lambda (key _entry)
               (when (equal (cdr key) server)
                 (remhash key supersonic--credentials-cache)
                 (setq forgotten t)))
             supersonic--credentials-cache)
    (and forgotten "; the cached credentials were forgotten, try again")))

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
 supersonic-get-text (url)
 "Return a promise resolving to the body of the response from URL, as text.
The body is decoded as UTF-8, which every caller expects: the Subsonic
API always returns UTF-8 JSON (per RFC 8259), and a UPnP device
description is UTF-8 by its specification.  `url-retrieve' doesn't
reliably decode the body for us across Emacs versions, so it is
decoded explicitly -- safe even if it already is, since
`decode-coding-string' is a no-op on text that isn't raw undecoded
bytes.  The promise rejects if the request failed."
 (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-url-retrieve url))))
   (unwind-protect
       (progn
         (when (plist-get status :error)
           (error "Failed to fetch %s: %S" url (plist-get status :error)))
         (with-current-buffer buffer
           (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)))
     (kill-buffer buffer))))

(aio-defun
 supersonic-get-json (url) "Return a promise resolving to the parsed json response from URL."
 (let* ((body (aio-await (supersonic-get-text url)))
        (json-array-type 'list)
        (json-key-type 'string)
        (data
         (condition-case nil
             (json-read-from-string body)
           (json-readtable-error
            (error "Failed to read json")))))
   (supersonic--signal-if-failed data)
   data))

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
        (user-error "%s%s"
                    (or (assoc-default "message" err) "Subsonic request failed")
                    ;; 40: wrong username or password.
                    (or (and (eql (assoc-default "code" err) 40)
                             (supersonic--forget-rejected-credentials supersonic-host))
                        ""))))))

(defun supersonic-recursive-assoc (data keys)
  "Recursively assoc DATA from a list of KEYS."
  (if keys
      (supersonic-recursive-assoc (assoc-default (car keys) data) (cdr keys))
    data))

(defun supersonic--random-salt ()
  "Generate a random alphanumeric salt for Subsonic token authentication.
12 hex characters, well above the API's 6-character minimum."
  (mapconcat (lambda (_) (format "%x" (random 16))) (make-list 12 nil) ""))

(defun supersonic--auth-query (user password)
  "Build the \"u\"/\"t\"/\"s\" token-auth query parameters for one request.
USER and PASSWORD are the Subsonic credentials.  Uses Subsonic's
token authentication (t = md5(password + salt), s = a fresh salt per
request) instead of sending the plaintext password, so it never ends
up in a URL -- which, depending on how that URL is used elsewhere
\(e.g. handed to curl as an argument), could otherwise be visible to
any local user via `ps' or in a subprocess's argv."
  (let ((salt (supersonic--random-salt)))
    `(("u" . ,user) ("t" . ,(md5 (concat password salt))) ("s" . ,salt))))

(defun supersonic-build-url (endpoint extra-query)
  "Build a valid supersonic url for a given ENDPOINT.
EXTRA-QUERY is used for any extra query parameters.  The url is on
`supersonic-host'; the credentials come from `supersonic-auth', asked
once per url."
  (let ((host (or supersonic-host "")))
    (when (string-empty-p host)
      (user-error "Set `supersonic-host' to your Subsonic server, e.g. https://host:4533"))
    (let* ((auth (supersonic-auth host))
           (secret (plist-get auth :secret))
           (password (if (functionp secret) (funcall secret) secret)))
      (unless password
        (user-error "No Subsonic password for %s: add an authinfo entry with it as the machine, or set `supersonic-auth-function'"
                    host))
      (concat
       (unless (string-match-p "\\`https?://" host)
         "https://")
       host "/rest" endpoint
       (supersonic-alist->query
        (append (supersonic--auth-query (plist-get auth :user) password)
                `(("c" . "ElSonic") ("v" . "1.16.0") ("f" . "json"))
                extra-query))))))

(defun supersonic-get-id-as-string (data)
  "Return DATA's \"id\" field as a string, converting from a number if necessary."
  (let ((id (assoc-default "id" data)))
    (if (numberp id)
        (number-to-string id)
      id)))

(provide 'supersonic-api)
;;; supersonic-api.el ends here
