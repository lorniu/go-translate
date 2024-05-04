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

;; 有道词典 (https://dict.youdao.com), A Chinese translation website.
;;
;; This engine is only for Chinese user.
;;
;; > 此引擎仅供中文翻译使用
;;
;; There are two engine implements here, one is `gt-youdao-dict-engine' and another is `gt-youdao-suggest-engine'.
;;
;; > 这里为有道实现了两个引擎：
;; > 1. `gt-youdao-dict-engine'，基于网页翻译接口，具备基本的翻译能力
;; > 2. `gt-youdao-suggest-engine', 用于展示输入单词的同义词及相关词汇
;; >
;; > 另外，有道还提供 API 接口，罢了，要收费的。

;;; Code:

(require 'gt-extension)


;;; 有道词典

(defclass gt-youdao-dict-parser (gt-parser) ())

(defclass gt-youdao-dict-engine (gt-engine)
  ((tag     :initform '有道词典)
   (url     :initform "https://dict.youdao.com/result?lang=%s&word=%s")
   (parse   :initform (gt-youdao-dict-parser))))

(cl-defmethod gt-translate ((engine gt-youdao-dict-engine) task next)
  (with-slots (text src tgt res meta translator) task
    (let* ((lang (cond ((equal src 'zh) tgt)
                       ((equal tgt 'zh) src)
                       (t (user-error "只支持中文跟其他语言之间的翻译"))))
           (url (if (cdr (oref translator text))
                    (user-error "不支持分段翻译")
                  (format (oref engine url)
                          (url-hexify-string (format "%s" lang))
                          (url-hexify-string text)))))
      (gt-request :url url
                  :done (lambda (raw)
                          (setf res (gt-parse-html-dom raw) meta url)
                          (funcall next task))
                  :fail (lambda (err) (gt-fail task err))))))

(defun gt-youdao-dict--extract (dom)
  "从 DOM 中解析结果。"
  (let* ((phonetic (cl-loop for p in (dom-by-class dom "per-phone")
                            collect (cons (nth 2 (nth 2 p)) (nth 2 (nth 3 p)))))
         (node (dom-by-id dom "catalogue_author"))
         (exp-ce (cl-loop for n in (dom-by-class node "word-exp-ce")
                          for s = (car (dom-by-class n "point"))
                          for r = (car (dom-by-class n "word-exp_tran"))
                          collect (cons (nth 2 s) (nth 2 r))))
         (exp-nw (unless exp-ce
                   (cl-loop for n in (dom-by-class node "trans-content")
                            collect (nth 2 n))))
         (exp-ec (unless (or exp-nw exp-ce)
                   (cl-loop for n in (dom-by-class node "word-exp")
                            collect (cons (nth 2 (nth 2 n)) (nth 2 (nth 3 n))))))
         (exam-type (mapcar #'caddr (dom-by-class node "exam_type-value")))
         (word-wfs (cl-loop for n in (dom-by-class node "word-wfs-cell-less")
                            collect (cons (nth 2 (nth 2 (nth 2 n))) (nth 2 (nth 3 n))))))
    (list :exp-nw exp-nw :exp-ce exp-ce :exp-ec exp-ec :phonetic phonetic
          :exam-type exam-type :word-wfs word-wfs)))

(defun gt-youdao-dict--filter (result)
  "对 RESULT 进行某些美化。"
  (string-replace "】" "]" (string-replace "【" "[" result)))

(defun gt-youdao-dict--tts-url (word &optional lang)
  (format "http://dict.youdao.com/dictvoice?audio=%s&type=%s"
          (url-hexify-string word)
          (if (member lang '("英" 1 "1")) 1 0)))

(cl-defmethod gt-parse ((_ gt-youdao-dict-parser) task)
  (with-slots (res meta) task
    (gt-plist-let ((gt-youdao-dict--extract res))
      (with-temp-buffer
        ;; 音标
        (when .phonetic
          (insert (substring-no-properties (oref task text)) "  "
                  (propertize (if .exp-ce
                                  (propertize (caar .phonetic) 'face 'gt-youdao-dict-phonetic-face)
                                (mapconcat (lambda (p)
                                             (concat (propertize (car p) 'face 'gt-youdao-dict-label-face) " "
                                                     (propertize (cdr p)
                                                                 'face 'gt-youdao-dict-phonetic-face
                                                                 'mouse-face 'bold
                                                                 'gt-tts-url (gt-youdao-dict--tts-url (oref task text) (car p))
                                                                 'keymap (gt-simple-keymap [mouse-1] #'gt-do-speak))))
                                           .phonetic "  "))
                              'display '(height 0.7))
                  "\n\n"))
        ;; 释义
        (insert (cond
                 (.exp-nw (mapconcat #'identity .exp-nw "\n"))
                 (.exp-ce (mapconcat (lambda (exp)
                                       (concat (propertize (car exp) 'face 'gt-youdao-dict-entry-face) "\n\n  " (cdr exp)))
                                     .exp-ce "\n\n"))
                 (.exp-ec (mapconcat (lambda (exp)
                                       (if (cdr exp)
                                           (concat (propertize (car exp) 'face 'gt-youdao-dict-entry-face) " " (cdr exp))
                                         (gt-youdao-dict--filter (car exp))))
                                     .exp-ec "\n\n"))
                 (t (user-error "也许没有翻译结果?")))
                "\n\n")
        ;; 相关
        (cl-loop for wf in .word-wfs
                 for i from 1
                 do (insert (propertize (car wf) 'face 'gt-youdao-dict-label-face) "  " (cdr wf))
                 do (insert (if (= (length .word-wfs) i) "\n\n" (if (> (current-column) fill-column) "\n" "\t"))))
        ;; 考试
        (when-let (et (and .exam-type (mapconcat #'identity .exam-type " / ")))
          (insert (propertize et 'face 'gt-youdao-dict-phonetic-face 'display '(height 0.7))))
        ;; 返回
        (setf res (if (or .exp-ce .exp-ec)
                      (propertize (buffer-string) 'gt-url meta)
                    (buffer-string)))))))


;;; 有道同义词

(defclass gt-youdao-suggest-parser (gt-parser) ())

(defclass gt-youdao-suggest-engine (gt-engine)
  ((tag       :initform '有道同义词)
   (url       :initform "https://dict.youdao.com/suggest?q=%s&num=%s&doctype=json")
   (limit     :initform 9 :initarg :limit)
   (delimiter :initform nil)
   (parse     :initform (gt-youdao-suggest-parser))))

(cl-defmethod gt-translate ((engine gt-youdao-suggest-engine) task next)
  (with-slots (text res meta) task
    (when (cdr text)
      (user-error "不支持一次翻译多个单词或句子"))
    (let ((url (format (oref engine url) (url-hexify-string (car text)) (oref engine limit))))
      (gt-request :url url
                  :done (lambda (raw) (setf res raw meta url) (funcall next task))
                  :fail (lambda (err) (gt-fail task err))))))

(cl-defmethod gt-parse ((_ gt-youdao-suggest-parser) task)
  (with-slots (res meta) task
    (let ((json (json-read-from-string res)))
      (unless (= (alist-get 'code (alist-get 'result json)) 200)
        (user-error (alist-get 'msg (alist-get 'result json))))
      (let ((lst (cl-loop
                  for item across (alist-get 'entries (alist-get 'data json))
                  for i from 1
                  for ent = (propertize (alist-get 'entry item) 'face 'gt-youdao-suggest-entry-face)
                  for exp = (if-let (ex (alist-get 'explain item)) (propertize (concat "   " ex) 'wrap-prefix "   "))
                  collect (concat (if (> i 1) (gt-line-height-separator 18)) ent
                                  (if exp (concat "\n" (gt-line-height-separator 8) exp))))))
        (with-temp-buffer
          (insert (string-join lst "\n"))
          (goto-char (point-min))
          (while (re-search-forward (format "\\(%s\\)\\(\\. \\|．\\)"
                                            (mapconcat (lambda (v) (format "%s" v)) gt-word-classes "\\|"))
                                    nil t)
            (put-text-property (match-beginning 0) (match-end 0) 'face 'gt-youdao-suggest-cixing-face))
          (setf res (propertize (buffer-string) 'gt-url meta)))))))

(provide 'gt-engine-youdao)

;;; gt-engine-youdao.el ends here
