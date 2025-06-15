;;; gt-engine-deepl.el --- Translate Engine for DeepL -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Author: lorniu <lorniu@gmail.com>

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

;; https://www.deepl.com

;;; Code:

(require 'gt-core)

(defgroup go-translate-deepl nil
  "Configs for DeepL engine."
  :group 'go-translate)


;;; Components

(defclass gt-deepl-parser (gt-parser) ())

(defclass gt-deepl-engine (gt-api-engine)
  ((tag       :initform 'DeepL)
   (host      :initform "https://api.deepl.com")
   (host-free :initform "https://api-free.deepl.com")
   (path      :initform "/v2/translate")
   (parse     :initform (gt-deepl-parser))
   (key       :initform 'auth-key) ; machine api.deepl.com login auth-key password ***
   (pro       :initform nil :initarg :pro :documentation "If PRO version.")))


;;; Engine

(defcustom gt-deepl-extra-params '(("split_sentences"     . "1")
                                   ("preserve_formatting" . "1"))
  "Extra translation params send to DeepL server."
  :type '(alist :key-type (string :tag "Key")
                :value-type (string :tag "Value"))
  :group 'go-translate-deepl)

(defvar gt-deepl-langs-mapping '((en . "EN")
                                 (zh . "ZH")
                                 (de . "DE") ; German
                                 (fr . "FR") ; French
                                 (it . "IT") ; Italian
                                 (ja . "JA") ; Japanese
                                 (es . "ES") ; Spanish
                                 (nl . "NL") ; Dutch
                                 (pl . "PL") ; Polish
                                 (pt . "PT") ; Portuguese (all Portuguese varieties mixed)
                                 (ru . "RU") ; Russian
                                 ))

(defun gt-deepl-lang (lang)
  (if-let* ((mapping (assoc lang gt-deepl-langs-mapping)))
      (cdr mapping)
    (user-error "Language %s is not supported by DeepL.
Supported list: %s" lang (mapconcat #'car gt-deepl-langs-mapping ", "))))

(cl-defmethod gt-resolve-key ((engine gt-deepl-engine))
  (gt-with-slots-for-key (key) engine
    (gt-lookup-password
     :user (if key (format "%s" key) "auth-key")
     :host "api.deepl.com")))

(cl-defmethod gt-execute ((engine gt-deepl-engine) task)
  (with-slots (text src tgt) task
    (with-slots (host host-free path pro rate-limit) engine
      (let ((url (concat (if pro host host-free) path))
            (headers `(www-url-u8 (auth "DeepL-Auth-Key" ,(gt-resolve-key engine))))
            (data `(("target_lang" . ,(gt-deepl-lang tgt))
                    ,(if-let* ((src (gt-deepl-lang src))) `("source_lang" . ,src))
                    ,@gt-deepl-extra-params))
            (fail (lambda (err)
                    (signal (car err)
                            (pcase (car-safe (cdr-safe err))
                              (403 '("[403] Authorization failed. Please supply a valid auth_key parameter"))
                              (413 '("[413] The request size exceeds the limit"))
                              (414 '("[414] Request-URI Too Long"))
                              (429 '("[429] Too many requests. Please wait and resend your request"))
                              (456 '("[456] Quota exceeded. The character limit has been reached"))
                              (_ (cdr err)))))))
        (gt-dolist-concurrency (item text rate-limit)
          (gt-request url
            :cache (if pdd-active-cacher `(t deepl ,src ,tgt ,item))
            :headers headers
            :data `(("text" . ,item) ,@data)
            :fail fail))))))


;;; Parser

(cl-defmethod gt-parse ((_ gt-deepl-parser) task)
  (cl-loop for item in (oref task res)
           for str = (decode-coding-string (mapconcat #'cdadr (cdar item) "\n") 'utf-8)
           collect (string-trim str) into lst
           finally (oset task res lst)))

(provide 'gt-engine-deepl)

;;; gt-engine-deepl.el ends here
