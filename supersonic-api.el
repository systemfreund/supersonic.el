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

;; HTTP layer of supersonic.el: authenticated Subsonic request URLs,
;; JSON responses, and the credential cache `supersonic-cached'.

;;; Code:
(require 'auth-source)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'aio)
(require 'supersonic-custom)

(defvar supersonic--credentials-cache (make-hash-table :test #'equal)
  "Results cached by functions made with `supersonic-cached'.
Keys are (ID . SERVER), values (EXPIRES . RESULT).")

(defun supersonic--cached-result (result)
  "Return RESULT with its secret resolved and copied, or nil if it has none.
RESULT is a credentials plist with a :secret, or a token.  A secret
that is a function is called.  It is copied because its source may
wipe the original later."
  (let ((resolve (lambda (secret)
                   (let ((secret (if (functionp secret) (funcall secret) secret)))
                     (and secret (copy-sequence secret))))))
    (if (keywordp (car-safe result))
        (let ((secret (funcall resolve (plist-get result :secret))))
          (and secret (list :user (plist-get result :user) :secret secret)))
      (funcall resolve result))))

;;;###autoload
(defun supersonic-cached (function)
  "Return a function that calls FUNCTION and caches its result per server.
Use it to wrap a slow `supersonic-auth-function' or
`supersonic-music-assistant-token-function'.  A result is kept for
`supersonic-credentials-cache-expiry' seconds; nil is not cached.
Credentials a server rejects are dropped, and
`supersonic-forget-credentials' drops all of them."
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
  "Remove expired entries from `supersonic--credentials-cache'."
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
  "Forget the credentials cached for SERVER, which rejected them.
Return a note to append to the error message, or nil if nothing was
cached for SERVER."
  (let ((forgotten nil))
    (maphash (lambda (key _entry)
               (when (equal (cdr key) server)
                 (remhash key supersonic--credentials-cache)
                 (setq forgotten t)))
             supersonic--credentials-cache)
    (and forgotten "; the cached credentials were forgotten, try again")))

(defcustom supersonic-auth-function #'supersonic-auth-source-credentials
  "Function returning the credentials for the Subsonic server.
It is called with `supersonic-host' and returns a plist
\(:user USER :secret SECRET), SECRET being the password or a function
returning it, or nil.  The default looks the host up in auth-source.

For a password stored elsewhere, e.g. in a pass entry:

  (setq supersonic-auth-function
        (lambda (_host)
          (list :user \"USERNAME\"
                :secret (auth-source-pass-get \='secret \"my/entry\"))))

It is called for every request, so wrap a slow lookup like this one
in `supersonic-cached':

  (setq supersonic-auth-function
        (supersonic-cached
         (lambda (_host)
           (list :user \"USERNAME\"
                 :secret (auth-source-pass-get \='secret \"my/entry\")))))"
  :type '(choice (function-item supersonic-auth-source-credentials)
                 (function :tag "Other function"))
  :group 'supersonic)

(defun supersonic-auth-source-credentials (host)
  "Return the auth-source entry with a password for HOST.
This is the default `supersonic-auth-function'."
  (car (auth-source-search :host host :require '(:secret))))

(defun supersonic-auth (&optional host)
  "Return the credentials `supersonic-auth-function' has for HOST.
HOST defaults to `supersonic-host'.  Return nil if it is empty."
  (let ((host (or host supersonic-host "")))
    (unless (string-empty-p host)
      (funcall supersonic-auth-function host))))

(defun supersonic-alist->query (al)
  "Return the query string for AL, an alist of string keys and values.
Values are percent-encoded, nil becoming empty.  Return \"\" for an
empty AL."
  (if al
      (concat "?" (mapconcat (lambda (q) (concat (car q) "=" (url-hexify-string (or (cdr q) "")))) al "&"))
    ""))

(defvar url-http-end-of-headers)

(defun supersonic-url-retrieve (url)
  "Return a promise of (STATUS . BUFFER) for URL, like `aio-url-retrieve'.
If the server does not answer within `supersonic-request-timeout'
seconds, close the connection and reject the promise.  Otherwise the
caller must kill BUFFER."
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
  "Abort the request for URL, unanswered after TIMEOUT seconds.
Close BUFFER's connection, kill BUFFER and reject PROMISE."
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

(aio-defun supersonic-get-text (url)
  "Return a promise of the body of the response from URL, as UTF-8 text.
The promise rejects if the request fails."
  (pcase-let ((`(,status . ,buffer) (aio-await (supersonic-url-retrieve url))))
    (unwind-protect
        (progn
          (when (plist-get status :error)
            (error "Failed to fetch %s: %S" url (plist-get status :error)))
          (with-current-buffer buffer
            (decode-coding-string (buffer-substring (1+ url-http-end-of-headers) (point-max)) 'utf-8)))
      (kill-buffer buffer))))

(aio-defun supersonic-get-json (url)
  "Return a promise resolving to the parsed json response from URL."
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
  "Signal an error if the Subsonic response DATA reports a failure.
Subsonic reports failures such as wrong credentials in a 200 response."
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
  "Return a random 12-character hex salt for token authentication."
  (mapconcat (lambda (_) (format "%x" (random 16))) (make-list 12 nil) ""))

(defun supersonic--auth-query (user password)
  "Return the query parameters authenticating USER with PASSWORD.
Token authentication keeps the password itself out of the URL."
  (let ((salt (supersonic--random-salt)))
    `(("u" . ,user) ("t" . ,(md5 (concat password salt))) ("s" . ,salt))))

(defun supersonic-build-url (endpoint extra-query)
  "Return the URL of the Subsonic API ENDPOINT on `supersonic-host'.
EXTRA-QUERY is an alist of further query parameters.  The credentials
come from `supersonic-auth'."
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
