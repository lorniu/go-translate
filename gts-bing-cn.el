;;; gts-bing-cn.el --- Microsoft Translate -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; site: https://cn.bing.cn/translator

;;; Code:

(require 'gts-implements)

(defclass gts-bing-cn-parser (gts-parser) ())

(defclass gts-bing-cn-engine (gts-engine)
  ((tag      :initform "Bing")
   (base-url :initform "https://cn.bing.com")
   (sub-url  :initform "/ttranslatev3")

   (ig       :initform nil :initarg :ig)
   (key      :initform nil :initarg :key)
   (token    :initform nil :initarg :token)

   (parser   :initform (gts-bing-cn-parser))))


;;; Engine

(defvar gts-bing-cn-extra-langs-mapping '(("zh" . "zh-Hans")))

(cl-defmethod gts-get-lang ((_ gts-bing-cn-engine) lang)
  (or (cdr-safe (assoc lang gts-bing-cn-extra-langs-mapping)) lang))

(cl-defmethod gts-with-token ((o gts-bing-cn-engine) callback)
  (with-slots (token key ig base-url) o
    (if (and token key ig)
        (funcall callback)
      (gts-do-request (concat base-url "/translator")
                      :done
                      (lambda ()
                        (condition-case err
                            (let (key token ig)
                              (goto-char (point-min))
                              (re-search-forward "\"ig\":\"\\([^\"]+\\).*params_RichTranslateHelper = \\[\\([0-9]+\\),\"\\([^\"]+\\)")
                              (setq ig (match-string 1) key (match-string 2) token (match-string 3))
                              (oset o ig ig)
                              (oset o key key)
                              (oset o token token)
                              (gts-do-log "bing" (format "key: %s\ntoken: %s\nig: %s" key token ig))
                              (funcall callback))
                          (error (error "Error occurred when request with bing token (%s, %s)" o err))))
                      :fail
                      (lambda (status)
                        (error (format "ERR: %s" status)))))))

(cl-defmethod gts-translate ((engine gts-bing-cn-engine) &optional text from to rendercb)
  (with-slots (base-url sub-url token key ig parser) engine
    (gts-with-token
     engine
     (lambda ()
       (gts-do-request (format "%s%s?isVertical=1&IG=%s&IID=translator.5022.1" base-url sub-url ig)
                       :headers `(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8"))
                       :data `(("fromLang" . ,from)
                               ("to"       . ,(gts-get-lang engine to))
                               ("text"     . ,(gts-get-lang engine text))
                               ("key"      . ,key)
                               ("token"    . ,token))
                       :done (lambda ()
                               (let ((result (gts-parse parser text (buffer-string))))
                                 (funcall rendercb result)))
                       :fail (lambda (status)
                               (funcall rendercb status)))))))


;;; Parser

(cl-defmethod gts-parse ((_ gts-bing-cn-parser) _text resp)
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert resp)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (delete-region (point-min) (point))
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    (gts-do-log 'bing-result (buffer-string))
    (let* ((json (json-read))
           (result (cdr (assoc 'text
                               (aref
                                (cdr (assoc 'translations (aref json 0)))
                                0)))))
      result)))


(provide 'gts-bing-cn)

;;; gts-bing-cn.el ends here
