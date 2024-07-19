;;; gt-httpx.el --- Http Client Components -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Author: lorniu <lorniu@gmail.com>
;; Package-Requires: ((emacs "28.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Http client components used by the framework. With unified interface to
;; compact different http backends.
;;
;;  - Support both sync/async request
;;  - Support streaming request
;;  - Support retry for timeout
;;  - Support config proxies for backends
;;  - Support file upload/download (TODO)
;;

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'eieio)

(declare-function gt-log "ext:gt-core")
(declare-function gt-single--eieio-childp "ext:gt-core")

(unless (fboundp 'gt-single) ; suppress compiler error
  (defclass gt-single () () :abstract t))

(defun gt-format-params (data)
  "Format DATA to k=v style query string.
DATA should be list of (key . value)."
  (if (or (null data) (stringp data)) data
    (mapconcat (lambda (arg)
                 (format "%s=%s"
                         (url-hexify-string (format "%s" (car arg)))
                         (url-hexify-string (format "%s" (or (cdr arg) 1)))))
               (delq nil data) "&")))


;;; Http Client

(defclass gt-http-client (gt-single)
  ((user-agent :initarg :user-agent :initform nil :type (or string null)))
  "Used to send http request."
  :abstract t)

(defvar gt-http-client-max-retry 3)

(defvar gt-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36")

(defvar-local gt-http-client-stream-abort-flag nil
  "Non-nil means to ignore following stream progress in callback of http filter.")

(cl-defgeneric gt-request (http-client &rest _args &key url method headers data filter done fail sync retry &allow-other-keys)
  "Send HTTP request using the given HTTP-CLIENT.

Optional keyword arguments:
  - URL: The URL to send the request to.
  - METHOD: Request method. If nil guess by data.
  - HEADERS: Additional headers to include in the request.
  - DATA: The data to include in the request.
  - FILTER: A function to be called every time when some data returned.
  - DONE: A function to be called when the request succeeds.
  - FAIL: A function to be called when the request fails.
  - RETRY: how many times it can retry for timeout
  - SYNC: non-nil means request synchronized

If request async, return the process behind the request."
  (:method :around ((client gt-http-client) &rest args &key url method headers data filter done fail sync retry)
           ;; normalize and validate
           (if (and (null filter) (null done)) (setq sync t args `(:sync t ,@args)))
           (cl-assert (and url (or (and sync (not filter)) (and (not sync) (or filter done)))))
           (if (null method) (setq args `(:method ,(if data 'post 'get) ,@args)))
           (if (listp data) (setq data (gt-format-params data) args `(:data ,data ,@args)))
           ;; log
           (gt-log (eieio-object-class client)
             (format "> %s\n> %s" client url)
             (if headers (format "> HEADER: %s" headers))
             (if data (format "> DATA:   %s" data)))
           ;; sync
           (if sync (apply #'cl-call-next-method client args)
             ;; async
             (let* ((tag (eieio-object-class client))
                    (failfn (lambda (status)
                              ;; retry for timeout
                              (unless retry (setq retry gt-http-client-max-retry))
                              (if (and (string-match-p "Operation timeout" (format "%s" status)) (cl-plusp retry))
                                  (progn (gt-log tag (format "request timeout, retrying (remains %d times)..." retry))
                                         (apply #'gt-request client `(:retry ,(1- retry) ,@args)))
                                ;; failed finally
                                (gt-log tag (format "Request FAIL: (%s) %s" url status))
                                (if fail (funcall fail status)
                                  (signal (car status) (cdr status))))))
                    (filterfn (when filter
                                (lambda ()
                                  ;; abort action and error case
                                  (unless gt-http-client-stream-abort-flag
                                    (condition-case err
                                        (funcall filter)
                                      (error
                                       (setq gt-http-client-stream-abort-flag t)
                                       (gt-log tag (format "Error in filter: (%s) %s" url err))
                                       (funcall failfn err)))))))
                    (donefn (lambda (raw)
                              (gt-log tag (format "✓ %s" url))
                              (when done (funcall done raw)))))
               (apply #'cl-call-next-method client `(:fail ,failfn :filter ,filterfn :done ,donefn ,@args))))))


;; http client implemented using `url.el'

(defclass gt-url-http-client (gt-http-client)
  ((proxy-services
    :initarg :proxies
    :initform nil
    :type (or list null)
    :documentation "Proxy services passed to `url.el', see `url-proxy-services' for details."))
  :documentation "Http Client implemented using `url.el'.")

(defvar url-http-end-of-headers)

(defvar url-http-transfer-encoding)

(defvar gt-url-extra-filter nil)

(defun gt-url-http-extra-filter (beg end len)
  (when (and gt-url-extra-filter (bound-and-true-p url-http-end-of-headers)
             (if (equal url-http-transfer-encoding "chunked") (= beg end) ; when delete
               (= len 0))) ; when insert
    (save-excursion
      (save-restriction
        (narrow-to-region url-http-end-of-headers (point-max))
        (funcall gt-url-extra-filter)))))

(cl-defmethod gt-request ((client gt-url-http-client) &key url method headers data filter done fail sync retry)
  (ignore retry)
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-user-agent (or (oref client user-agent) gt-user-agent))
         (url-proxy-services (or (oref client proxy-services) url-proxy-services))
         (url-request-extra-headers headers)
         (url-request-method (upcase (format "%s" method)))
         (url-request-data data)
         (url-mime-encoding-string "identity"))
    ;; sync
    (if sync
        (condition-case err
            (with-current-buffer (url-retrieve-synchronously url nil t)
              (unwind-protect
                  (let ((s (buffer-substring-no-properties url-http-end-of-headers (point-max))))
                    (if done (funcall done s) s))
                (kill-buffer (current-buffer))))
          (error (if fail (funcall fail err) (signal 'user-error (cdr err)))))
      ;; async
      (let ((buf (url-retrieve url
                               (lambda (status)
                                 (let ((cb (current-buffer)))
                                   (set-buffer-multibyte t)
                                   (remove-hook 'after-change-functions #'gt-url-http-extra-filter t)
                                   (unwind-protect
                                       (if-let (err (or (cdr-safe (plist-get status :error))
                                                        (when (or (null url-http-end-of-headers) (= 1 (point-max)))
                                                          "Nothing responsed from server")))
                                           (if fail (funcall fail err)
                                             (signal 'user-error err))
                                         (when done
                                           (funcall done (buffer-substring-no-properties url-http-end-of-headers (point-max)))))
                                     (kill-buffer cb))))
                               nil t)))
        (when (and filter (buffer-live-p buf))
          (with-current-buffer buf
            (setq-local gt-url-extra-filter filter)
            (add-hook 'after-change-functions #'gt-url-http-extra-filter nil t)))
        (get-buffer-process buf)))))


;;; request with curl implemented via package `plz.el'

;; you should install `plz' before use this

(defclass gt-plz-http-client (gt-http-client)
  ((extra-args
    :initarg :args
    :type list
    :documentation "Extra arguments passed to curl programe."))
  :documentation "Http Client implemented using `plz.el'.")

(defvar plz-curl-program)
(defvar plz-curl-default-args)
(defvar plz-http-end-of-headers-regexp)

(declare-function plz "ext:plz.el" t t)
(declare-function plz-error-message "ext:plz.el" t t)
(declare-function plz-error-curl-error "ext:plz.el" t t)
(declare-function plz-error-response "ext:plz.el" t t)
(declare-function plz-response-status "ext:plz.el" t t)
(declare-function plz-response-body "ext:plz.el" t t)

(defvar gt-plz-initialize-error-message
  "\n\nTry to install curl and specify the program like this to solve the problem:\n
  (setq plz-curl-program \"c:/msys64/usr/bin/curl.exe\")\n
Or switch http client to `gt-url-http-client' instead:\n
  (setq gt-default-http-client (gt-url-http-client))")

(cl-defmethod gt-request :before ((_ gt-plz-http-client) &rest _)
  (unless (and (require 'plz nil t) (executable-find plz-curl-program))
    (error "You should have `plz.el' and `curl' installed before using `gt-plz-http-client'")))

(cl-defmethod gt-request ((client gt-plz-http-client) &key url method headers data filter done fail sync retry)
  (ignore retry)
  (let ((plz-curl-default-args
         (if (slot-boundp client 'extra-args)
             (append (oref client extra-args) plz-curl-default-args)
           plz-curl-default-args))
        (headers (cons `("User-Agent" . ,(or (oref client user-agent) gt-user-agent)) headers)))
    ;; sync
    (if sync
        (condition-case err
            (let ((r (plz method url :headers headers :body data :as 'string)))
              (if done (funcall done r) r))
          (error (if fail (funcall fail err) (signal 'user-error (cdr err)))))
      ;; async
      (plz method url :headers headers :body data :as 'string
        :filter (when filter
                  (lambda (proc string)
                    (with-current-buffer (process-buffer proc)
                      (save-excursion
                        (goto-char (point-max))
                        (insert string)
                        (goto-char (point-min))
                        (when (re-search-forward plz-http-end-of-headers-regexp nil t)
                          (save-restriction
                            (narrow-to-region (point) (point-max))
                            (funcall filter)))))))
        :then (lambda (raw)
                (when done (funcall done raw)))
        :else (lambda (err)
                (let ((ret ;; try to compat with error object of url.el, see `url-retrieve' for details
                       (or (plz-error-message err)
                           (when-let (r (plz-error-curl-error err))
                             (list 'curl-error
                                   (concat (format "%s" (or (cdr r) (car r)))
                                           (pcase (car r)
                                             (2 (when (memq system-type '(cygwin windows-nt ms-dos))
                                                  gt-plz-initialize-error-message))))))
                           (when-let (r (plz-error-response err))
                             (list 'http (plz-response-status r) (plz-response-body r))))))
                  (if fail (funcall fail ret)
                    (signal 'user-error ret))))))))

(provide 'gt-httpx)

;;; gt-httpx.el ends here
