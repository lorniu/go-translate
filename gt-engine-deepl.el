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

(require 'gt-extension)

(defgroup go-translate-deepl nil
  "Configs for DeepL engine."
  :group 'go-translate)


;;; Components

(defclass gt-deepl-parser (gt-parser) ())

(defclass gt-deepl-engine (gt-engine)
  ((tag       :initform 'DeepL)
   (host      :initform "https://api.deepl.com")
   (host-free :initform "https://api-free.deepl.com")
   (path      :initform "/v2/translate")
   (parse     :initform (gt-deepl-parser))

   (pro       :initform nil
              :initarg :pro
              :documentation "Set t when use PRO version.")

   (key       :initform 'auth-key
              :initarg :key
              :documentation "The auth-key of DeepL.
You can also put it into .authinfo file as:
  machine api.deepl.com login auth-key password ***")))


;;; Utils

(defcustom gt-deepl-fill-enable t
  "Controller whether try to improve the input and output.
Default behavior is removing excess linebreaks in input for better
translation effect, and filling the output for better reading experience.
You can override the behaviors by :around the method `gt-deepl-fill-input'
or `gt-deepl-fill-output'."
  :type 'boolean
  :group 'go-translate-deepl)

(cl-defmethod gt-deepl-fill-input (text)
  "Improve the input TEXT for better translation effect.
Mainly remove excess linebreaks. I want to skip unfill on comments and codes,
but don't know how to implement easily. To make it better later, maybe."
  (if gt-deepl-fill-enable
      (with-temp-buffer
        (insert text)
        (let ((fill-column (* 2 (point-max))))
          (fill-region (point-min) (point-max)))
        (buffer-string))
    text))

(cl-defmethod gt-deepl-fill-output (text)
  "Improve the output TEXT for better reading experience.
Mainly fill the text to suitable length."
  (if gt-deepl-fill-enable
      (with-temp-buffer
        (insert text)
        (fill-region (point-min) (point-max))
        (buffer-string))
    text))


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

(defun gt-deepl-get-lang (lang)
  (if-let (mapping (assoc lang gt-deepl-langs-mapping))
      (cdr mapping)
    (user-error "Language %s is not supported by DeepL.
Supported list: %s" lang (mapconcat #'car gt-deepl-langs-mapping ", "))))

(cl-defmethod gt-ensure-key ((engine gt-deepl-engine))
  (with-slots (key) engine
    (unless (stringp key)
      (if-let (auth-key (gt-lookup-password
                         :user (if key (format "%s" key) "auth-key")
                         :host "api.deepl.com"))
          (setf key auth-key)
        (user-error "You should provide a auth-key for gt-deepl-engine")))))

(cl-defmethod gt-translate ((engine gt-deepl-engine) task next)
  (gt-ensure-key engine)
  (with-slots (text src tgt res) task
    (with-slots (host host-free path pro key) engine
      (gt-request :url (concat (if pro host host-free) path)
                  :headers `(("Content-Type"    . "application/x-www-form-urlencoded;charset=UTF-8")
                             ("Authorization"   . ,(concat "DeepL-Auth-Key " key)))
                  :data    `(("text"            . ,(gt-deepl-fill-input text))
                             ("target_lang"     . ,(gt-deepl-get-lang tgt))
                             ,(if-let (src (gt-deepl-get-lang src)) `("source_lang" . ,src))
                             ,@gt-deepl-extra-params)
                  :done (lambda (raw)
                          (setf res raw)
                          (funcall next task))
                  :fail (lambda (err)
                          (gt-fail task (pcase (car-safe (cdr-safe err))
                                          (403 "[403] Authorization failed. Please supply a valid auth_key parameter")
                                          (413 "[413] The request size exceeds the limit")
                                          (414 "[414] Request-URI Too Long")
                                          (429 "[429] Too many requests. Please wait and resend your request")
                                          (456 "[456] Quota exceeded. The character limit has been reached")
                                          (_ err))))))))


;;; Parser

(cl-defmethod gt-parse ((_ gt-deepl-parser) task)
  (with-slots (res meta) task
    (let* ((json (json-read-from-string res))
           (str (decode-coding-string (mapconcat #'cdadr (cdar json) "\n") 'utf-8))
           (hook (lambda (rs) (mapcar #'gt-deepl-fill-output rs))))
      (setf res str meta (plist-put meta :res-hook hook)))))

(provide 'gt-engine-deepl)

;;; gt-engine-deepl.el ends here
