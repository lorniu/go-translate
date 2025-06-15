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

(require 'gt-core)

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

(defclass gt-libre-engine (gt-api-engine)
  ((tag         :initform 'LibreTranslate)
   (host        :initform nil :initarg :host)
   (path        :initform "/translate")
   (key         :initform 'api-key :initarg :key) ; machine [HOST like libretranslate.com] login api-key password [***]
   (parse       :initform (gt-libre-parser))))


;;; Translate

(cl-defmethod gt-resolve-key ((engine gt-libre-engine))
  (gt-with-slots-for-key (host key) engine
    (gt-lookup-password
     :user (if key (format "%s" key) "api-key")
     :host (url-host (url-generic-parse-url (or host gt-libre-host))))
    t))

(cl-defmethod gt-execute ((engine gt-libre-engine) task)
  (with-slots (text src tgt) task
    (with-slots (host path rate-limit) engine
      (let* ((key (gt-resolve-key engine))
             (url (concat (or host gt-libre-host) path))
             (data `(("source" . ,src) ("target" . ,tgt)
                     ("format" . "text") ("alternatives" . 1)
                     ,(if (stringp key) `("api_key" . ,key)))))
        (gt-dolist-concurrency (item text rate-limit)
          (gt-request url
            :cache (if pdd-active-cacher `(t libre ,src ,tgt ,item))
            :data `(("q" . ,item) ,@data)))))))

(cl-defmethod gt-parse ((_ gt-libre-parser) task)
  (cl-loop for item in (oref task res)
           collect (cdr (assoc 'translatedText item)) into lst
           finally (oset task res lst)))

(provide 'gt-engine-libre)

;;; gt-engine-libre.el ends here
