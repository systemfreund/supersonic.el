;;; supersonic-credentials.el --- Credential caching for supersonic.el -*- lexical-binding: t; -*-

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

;; `supersonic-cached', for wrapping a slow `supersonic-auth-function'
;; or `supersonic-music-assistant-token-function', and the cache behind
;; it.
;;
;; This file requires nothing else of the package, and nothing of it
;; may come to.  `supersonic-cached' is called from the user's
;; configuration, possibly in a value `custom-set-variables' (or
;; `use-package''s :custom) saved -- and such a value is only evaluated
;; once its `defcustom' runs, which is while `supersonic-custom.el' or
;; another file of the package is still loading.  The autoload this
;; file is then loaded through must not lead back to that file: it
;; used to live in `supersonic-api.el', which requires
;; `supersonic-custom', so the saved value signalled "Recursive
;; `require' for feature `supersonic-custom'".

;;; Code:

(defcustom supersonic-credentials-cache-expiry 3600
  "Seconds a function made by `supersonic-cached' keeps what it found.
Read when something is kept, so a change applies to what is kept from
then on, not to what already is.  `supersonic-forget-credentials'
drops everything kept before it expires."
  :type 'number
  :group 'supersonic)

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

(provide 'supersonic-credentials)
;;; supersonic-credentials.el ends here
