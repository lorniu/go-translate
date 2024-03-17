;;; gts-engine-youdao.el --- YouDao Dict -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

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

(require 'gts-implements)

(defclass gts-youdao-dict-eww-parser (gts-parser)
  ((tag     :initform "EWW")))

(defclass gts-youdao-dict-engine (gts-engine)
  ((tag     :initform "有道词典")
   (url     :initform "https://dict.youdao.com/result?word=%s&lang=%s")
   (parser  :initform (gts-youdao-dict-eww-parser))))

(cl-defmethod gts-translate ((engine gts-youdao-dict-engine) task next)
  (with-slots (text sl tl meta) task
    (let* ((lang (cond ((string-equal sl "zh") tl)
                       ((string-equal tl "zh") sl)
                       (t (user-error "只支持中文跟其他语言之间的翻译"))))
           (url (format (oref engine url) (url-hexify-string text) (url-hexify-string lang))))
      (setf meta (list :url url))
      (gts-request :url url
                   :done (lambda () (funcall next task)) ; html format response
                   :fail (lambda (err) (gts-fail task err))))))

(cl-defmethod gts-parse ((_ gts-youdao-dict-eww-parser) task)
  (let ((meta (oref task meta))
        (resp (buffer-string)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (with-temp-buffer
          (require 'eww)
          (insert "Content-type: text/html; charset=utf-8\n\n" resp)
          (eww-render nil (plist-get meta :url) nil buf))
        (cl-values (buffer-string) meta)))))

(provide 'gts-engine-youdao)

;;; gts-engine-youdao.el ends here
