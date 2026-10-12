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
(require 'supersonic-credentials)

(defcustom supersonic-auth-function #'supersonic-auth-source-credentials
  "Function returning the credentials for the Subsonic server.
Called with the server, `supersonic-host', it returns a plist
\(:user USER :secret SECRET), where SECRET is the password or a
function returning it, or nil if it has none.  The default looks the
server up in auth-source.

For a password kept where auth-source cannot find it by host, such
as a pass entry named for something else:

  (setq supersonic-auth-function
        (lambda (_host)
          (list :user \"USERNAME\"
                :secret (auth-source-pass-get \='secret \"my/entry\"))))

It is called for every url built -- each track queued, each cover
shown -- so a slow lookup, as decrypting that entry is, should be
wrapped in `supersonic-cached':

  (setq supersonic-auth-function
        (supersonic-cached
         (lambda (_host)
           (list :user \"USERNAME\"
                 :secret (auth-source-pass-get \='secret \"my/entry\")))))"
  :type '(choice (function-item supersonic-auth-source-credentials)
                 (function :tag "Other function"))
  :group 'supersonic)

(defun supersonic-auth-source-credentials (host)
  "Return the auth-source entry for HOST, a `supersonic-host'.
The default `supersonic-auth-function': the entry is a plist with the
user name under :user and the password under :secret, as that option
asks for.  Only an entry with a password is returned.

Not memoized: `auth-source-search' caches on its own, and once that
cache is forgotten a corrected entry takes effect."
  (car (auth-source-search :host host :require '(:secret))))

(defun supersonic-auth (&optional host)
  "Return the credentials `supersonic-auth-function' has for HOST.
HOST defaults to `supersonic-host'.  Returns nil without asking while
it is unset: a lookup for no host would match any entry."
  (let ((host (or host supersonic-host "")))
    (unless (string-empty-p host)
      (funcall supersonic-auth-function host))))

(defun supersonic-alist->query (al)
  "Convert AL, an alist of string keys to string values, to a query string.
Returns \"\" for an empty AL, so `supersonic-build-url' never appends a
dangling \"?\".

Values are percent-encoded, since ids, search queries and feed urls
can contain characters reserved in a query; keys are literals and are
left alone.  A nil value encodes as the empty string."
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

Closing the connection is the point: a request that is merely no
longer waited for still holds a file descriptor open (#70)."
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
The body is decoded as UTF-8, as both Subsonic JSON and UPnP device
descriptions are; `url-retrieve' doesn't reliably do that across Emacs
versions.  The promise rejects if the request failed."
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
Subsonic reports failures such as wrong credentials inside a 200 OK
response, with status \"failed\" and an \"error\" object."
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
USER and PASSWORD are the Subsonic credentials.  Token authentication
\(t = md5(password + salt), with a fresh salt s) keeps the plaintext
password out of URLs, which can end up in a subprocess's argv."
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
