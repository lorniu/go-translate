;;; gt-engine-youdao.el --- YouDao Dict -*- lexical-binding: t -*-

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

;; 有道词典 (dict.youdao.com), A chinese translation website.
;;
;; This engine is only for chinese user.
;; > 此引擎仅供中文翻译使用
;;
;; Simply implement using EWW to do the parsing job for now. I am too lazy to parse and extract the html results.
;; > 懒得去解析、组装 html，所以暂时直接借用 eww 的解析结果
;; > 权作应急使用，*也许* 以后会增加更完善的 parser 方案

;;; Code:

(require 'gt-extension)

(defclass gt-youdao-dict-eww-parser (gt-parser)
  ((tag     :initform "EWW")))

(defclass gt-youdao-dict-engine (gt-web-engine)
  ((tag     :initform '有道词典)
   (url     :initform "https://dict.youdao.com/result?word=%s&lang=%s")
   (parse   :initform (gt-youdao-dict-eww-parser))))

(cl-defmethod gt-translate ((engine gt-youdao-dict-engine) task next)
  (with-slots (text src tgt res meta) task
    (let* ((lang (cond ((string-equal src "zh") tgt)
                       ((string-equal tgt "zh") src)
                       (t (user-error "只支持中文跟其他语言之间的翻译"))))
           (url (format (oref engine url) (url-hexify-string text) (url-hexify-string lang))))
      (setf meta (list :url url))
      (gt-request :url url
                  :done (lambda (raw) (setf res raw) (funcall next task)) ; html format response
                  :fail (lambda (err) (gt-fail task err))))))

(cl-defmethod gt-parse ((_ gt-youdao-dict-eww-parser) task)
  (with-slots (res meta) task
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (with-temp-buffer
          (require 'eww)
          (insert "Content-type: text/html; charset=utf-8\n\n" res)
          (eww-render nil (plist-get meta :url) nil buf))
        (setf res (buffer-string))))))

(provide 'gt-engine-youdao)

;;; gt-engine-youdao.el ends here
