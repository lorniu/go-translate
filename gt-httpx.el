;;; gt-httpx.el --- Http Client -*- lexical-binding: t -*-

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
;;  - Support file upload/download
;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'url)

(if (fboundp 'gt-log)
    (declare-function gt-log "ext:gt-core")
  (defalias 'gt-log 'ignore))

(if (fboundp 'gt-single)
    (declare-function gt-single--eieio-childp "ext:gt-core")
  (defclass gt-single () () :abstract t))

(defun gt-http-binary-p (content-type)
  "Check if current CONTENT-TYPE is binary."
  (if (null content-type) nil
    (cl-destructuring-bind (mime sub) (string-split content-type "/" nil "[ \n\r\t]")
      (not (or (equal mime "text")
               (and (equal mime "application")
                    (string-match-p "json\\|xml\\|php" sub)))))))

(defun gt-format-params (alist)
  "Format ALIST to k=v style query string."
  (mapconcat (lambda (arg)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car arg)))
                       (url-hexify-string (format "%s" (or (cdr arg) 1)))))
             (delq nil alist) "&"))

(defvar gt-multipart-boundary "gt-boundary-O0o0O69Oo")

(defun gt-format-formdata (alist)
  "Generate multipart/formdata string from ALIST."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (cl-loop for (key . value) in alist for i from 1
             for filep = nil for contentype = nil
             do (setq key (format "%s" key))
             do (if (consp value) ; ((afile "~/aaa.jpg" "image/jpeg"))
                    (setq contentype (or (cadr value) "application/octet-stream")
                          value (format "%s" (car value)) filep t)
                  (setq value (format "%s" value)))
             for newline = "\r\n"
             do (insert "--" gt-multipart-boundary newline)
             if filep do (let ((fn (url-encode-url (url-file-nondirectory value))))
                           (insert "Content-Disposition: form-data; name=\"" key "\" filename=\"" fn "\"" newline)
                           (insert "Content-Type: " contentype newline newline)
                           (insert-file-contents-literally value)
                           (goto-char (point-max)))
             else do (insert "Content-Disposition: form-data; name=\"" key "\"" newline newline value)
             if (< i (length alist)) do (insert newline)
             else do (insert newline "--" gt-multipart-boundary "--"))
    (buffer-substring-no-properties (point-min) (point-max))))


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

Keyword arguments:
  - URL: The URL to send the request to.
  - METHOD: Request method, symbol like \\='post. If nil guess by data.
  - HEADERS: Additional headers to include in the request. Alist.
  - DATA: The data to include in the request. If this is a string, it will be
          sent directly as request body. If this is a list and every element
          is (key . value) then this will be joined to a string like a=1&b=2 and
          then be sent. If this is a list and some element is (key filename)
          format, then the list will be normalized as multipart formdata string
          and be sent.
  - FILTER: A function to be called every time when some data returned.
  - DONE: A function to be called when the request succeeds.
  - FAIL: A function to be called when the request fails.
  - RETRY: How many times it can retry for timeout. Number.
  - SYNC: Non-nil means request synchronized. Boolean.

If request async, return the process behind the request."
  (:method :around ((client gt-http-client) &rest args &key url method _headers data filter done fail sync retry)
           ;; normalize and validate
           (if (and (null filter) (null done)) (setq sync t args `(:sync t ,@args)))
           (cl-assert (and url (or (and sync (not filter)) (and (not sync) (or filter done)))))
           (if (null method) (setq args `(:method ,(if data 'post 'get) ,@args)))
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

(defvar url-http-content-type)
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
         (formdatap (and (consp data)
                         (or (string-match-p "multipart/formdata"
                                             (or (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case) ""))
                             (cl-some (lambda (x) (consp (cdr x))) data))))
         (url-request-data (funcall (if (atom data) #'identity ; string
                                      (if formdatap #'gt-format-formdata #'gt-format-params)) ; alist
                                    data))
         (url-request-extra-headers (progn
                                      (when formdatap
                                        (setf (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case)
                                              (concat "multipart/form-data; boundary=" gt-multipart-boundary)))
                                      headers))
         (url-request-method (string-to-unibyte (upcase (format "%s" method))))
         (url-mime-encoding-string "identity")
         (get-resp-content (lambda ()
                             (set-buffer-multibyte (not (gt-http-binary-p url-http-content-type)))
                             (buffer-substring-no-properties (min (1+ url-http-end-of-headers) (point-max)) (point-max)))))
    ;; log
    (gt-log (eieio-object-class client)
      (format "> %s\n> %s" client url)
      (if url-request-extra-headers (format "> HEADER: %S" url-request-extra-headers))
      (if url-request-data (format "> DATA:   %s" url-request-data)))
    ;; sync
    (if sync
        (condition-case err
            (let ((buf (url-retrieve-synchronously url nil t)))
              (unwind-protect
                  (with-current-buffer buf
                    (let ((s (funcall get-resp-content))) (if done (funcall done s) s)))
                (ignore-errors (kill-buffer buf))))
          (error (if fail (funcall fail err) (signal 'user-error (cdr err)))))
      ;; async
      (let ((buf (url-retrieve url
                               (lambda (status)
                                 (let ((cb (current-buffer)))
                                   (remove-hook 'after-change-functions #'gt-url-http-extra-filter t)
                                   (unwind-protect
                                       (if-let (err (or (cdr-safe (plist-get status :error))
                                                        (when (or (null url-http-end-of-headers) (= 1 (point-max)))
                                                          (list 'empty-response "Nothing responsed from server"))))
                                           (if fail (funcall fail err) (signal 'user-error err))
                                         (if done (funcall done (funcall get-resp-content))))
                                     (kill-buffer cb))))
                               nil t)))
        (when (and filter (buffer-live-p buf))
          (with-current-buffer buf
            (setq-local gt-url-extra-filter filter)
            (add-hook 'after-change-functions #'gt-url-http-extra-filter nil t)))
        (get-buffer-process buf)))))


;;; request with `curl' by package `plz.el'

;; you should install `plz.el' first before use this

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
(declare-function plz--narrow-to-body "ext:plz.el" t t)

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
  (let* ((plz-curl-default-args (if (slot-boundp client 'extra-args)
                                    (append (oref client extra-args) plz-curl-default-args)
                                  plz-curl-default-args))
         (formdatap (and (consp data)
                         (or (string-match-p "multipart/formdata"
                                             (or (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case) ""))
                             (cl-some (lambda (x) (consp (cdr x))) data))))
         (data (funcall (if (atom data) #'identity ; string
                          (if formdatap #'gt-format-formdata #'gt-format-params)) ; alist
                        data))
         (string-or-binary (lambda () ; decode according content-type. there is no builtin way to do this in plz
                             (widen)
                             (let* ((content-type (mail-fetch-field "content-type"))
                                    (binaryp (gt-http-binary-p content-type)))
                               (set-buffer-multibyte (not binaryp))
                               (goto-char (point-min))
                               (plz--narrow-to-body)
                               (unless binaryp (decode-coding-region (point-min) (point-max) 'utf-8))
                               (buffer-string)))))
    ;; headers
    (when formdatap
      (setf (alist-get "Content-Type" headers nil nil #'string-equal-ignore-case)
            (concat "multipart/form-data; boundary=" gt-multipart-boundary)))
    (unless (alist-get "User-Agent" headers nil nil #'string-equal-ignore-case)
      (push `("User-Agent" . ,(or (oref client user-agent) gt-user-agent)) headers))
    ;; log
    (gt-log (eieio-object-class client)
      (format "> %s\n> %s" client url)
      (if headers (format "> HEADER: %s" headers))
      (if data (format "> DATA:   %s" data))
      (if plz-curl-default-args (format "> EXTRA: %s" plz-curl-default-args)))
    ;; sync
    (if sync
        (condition-case err
            (let ((r (plz method url
                       :headers headers
                       :body data
                       :body-type (if formdatap 'binary 'text)
                       :decode nil
                       :as string-or-binary
                       :then 'sync)))
              (if done (funcall done r) r))
          (error (if fail (funcall fail err)
                   (signal 'user-error (cdr err)))))
      ;; async
      (plz method url
        :headers headers
        :body data
        :body-type (if formdatap 'binary 'text)
        :decode nil
        :as string-or-binary
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
