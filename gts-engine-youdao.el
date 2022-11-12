;;; gts-engine-youdao.el --- YouDao Dict -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; 有道词典 (dict.youdao.com), A chinese translation website.
;;
;; This engine is only for chinese user.
;; > 此引擎仅供中文翻译使用
;;
;; Simply implement using eww for the time being, no parser provided. I am too lazy to fetch html directly and extract the results.
;; > 懒得去解析、组装 html，所以暂时直接借用 eww 的结果
;; > 这个引擎当前只作备份使用，*也许* 以后会进行完善

;;; Code:

(require 'gts-implements)

(defclass gts-youdao-dict-eww-engine (gts-engine)
  ((tag     :initform "有道词典")
   (url     :initform "https://dict.youdao.com/result?word=%s&lang=%s")
   (parser  :initform (gts-youdao-dict-eww-parser))))

(cl-defmethod gts-translate ((engine gts-youdao-dict-eww-engine) task rendercb)
  (with-slots (text from to) task
    (with-slots (url parser) engine
      (let* ((lang (cond ((string-equal from "zh") to)
                         ((string-equal to "zh") from)
                         (t (user-error "只支持中文跟其他语言之间的翻译"))))
             (url (format url (url-hexify-string text) (url-hexify-string lang))))
        (with-temp-buffer
          (let ((buf (current-buffer)))
            (with-current-buffer (url-retrieve-synchronously url)
              (require 'eww)
              (eww-render nil url nil buf))
            (gts-update-raw task (buffer-string))
            (gts-parse parser task)
            (funcall rendercb)))))))


(provide 'gts-engine-youdao)

;;; gts-engine-youdao.el ends here
