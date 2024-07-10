;;; gt-engine-libre.el --- Engine for LibreTranslate -*- lexical-binding: t -*-

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

;; https://github.com/LibreTranslate/LibreTranslate
;; https://libretranslate.com/

;;; Code:

(require 'gt-extension)

(defgroup go-translate-libre nil
  "Configs for LibreTranslate engine."
  :group 'go-translate)

(defcustom gt-libre-host "https://translate.disroot.org/"
  "API host of LibreTranslate.

The official host is: https://libretranslate.com/, you can config this
to use third-party or your local service."
  :type 'string
  :group 'go-translate-libre)


;;; Components

(defclass gt-libre-parser (gt-parser) ())

(defclass gt-libre-engine (gt-engine)
  ((tag       :initform 'LibreTranslate)
   (host      :initform nil :initarg :host)
   (path      :initform "/translate")
   (parse     :initform (gt-libre-parser))
   (delimiter :initform "21597"
              ;; Any way to force specific segment not be changed after translate?
              ;; I don't know! Just set this a random number, to make it work...sometimes.
              ;; So this maybe failed in translating multiple parts text.
              )
   (cache     :initform nil)
   (key       :initform 'api-key
              :initarg :key
              :documentation "The api-key of LibreTranslate.
You can also put it into .authinfo file as:
  machine [HOST like libretranslate.com] login api-key password [***]")))


;;; Translate

(cl-defmethod gt-ensure-key ((engine gt-libre-engine))
  (with-slots (host key) engine
    (unless (stringp key)
      (setf key
            (gt-lookup-password
             :user (if key (format "%s" key) "api-key")
             :host (url-host (url-generic-parse-url (or host gt-libre-host))))))))

(cl-defmethod gt-translate ((engine gt-libre-engine) task next)
  (gt-ensure-key engine)
  (with-slots (text src tgt res) task
    (with-slots (host path key) engine
      (gt-request :url (concat (or host gt-libre-host) path)
                  :data `(("q"      . ,text)
                          ("source" . ,src)
                          ("target" . ,tgt)
                          ("format" . "text")
                          ("alternatives" . 1)
                          ,(if key `("api_key" . ,key)))
                  :done (lambda (raw)
                          (setf res raw)
                          (funcall next task))
                  :fail (lambda (err)
                          (gt-fail task err))))))

(cl-defmethod gt-parse ((_ gt-libre-parser) task)
  (with-slots (res) task
    (let ((json (json-read-from-string (decode-coding-string res 'utf-8))))
      (setf res (cdr (assoc 'translatedText json))))))

(provide 'gt-engine-libre)

;;; gt-engine-libre.el ends here
