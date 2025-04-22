;;; gt-tests.el --- Tests -*- lexical-binding: t -*-

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

;; Unit Tests

;;; Code:

(require 'ert)
(require 'go-translate)

;;; Tests

(defvar gt-test-text-1
  "

This buffer is for text that is not saved, and for Lisp evaluation.

 To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

")

(defvar gt-test-text-2 "你好，朋友 欢迎你")

(ert-deftest test--gt-word-p ()
  (should (gt-word-p 'zh "你好"))
  (should (gt-word-p 'zh "  你好 "))
  (should-not (gt-word-p 'zh "你好 吧"))
  (should-not (gt-word-p 'zh "你好，吧"))
  (should (gt-word-p 'en "hello"))
  (should-not (gt-word-p 'en "hello,world"))
  (should-not (gt-word-p 'en "hello world")))

(ert-deftest test--gt-pick-items-by-thing ()
  (should (null (gt-pick-items-by-thing nil nil)))
  (should (equal (gt-pick-items-by-thing gt-test-text-1 nil) gt-test-text-1))
  (should (equal (gt-pick-items-by-thing gt-test-text-1 'word)
                 '("This" "buffer" "is" "for" "text" "that" "is" "not" "saved" "and"
                   "for" "Lisp" "evaluation" "To" "create" "a" "file" "visit" "it" "with" "C" "x" "C" "f"
                   "and" "enter" "text" "in" "its" "buffer")))
  (should (equal (gt-pick-items-by-thing gt-test-text-1 'word (lambda (w) (string-prefix-p "e" w)))
                 '("evaluation" "enter")))
  (should (equal (gt-pick-items-by-thing gt-test-text-1 'paragraph)
                 '("This buffer is for text that is not saved, and for Lisp evaluation.\n"
                   " To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.\n")))
  (should (equal (gt-pick-items-by-thing gt-test-text-2 'word)
                 '("你好" "朋友" "欢迎你"))))

(ert-deftest test--gt-available-langs ()
  (should-error (gt-available-langs nil nil))
  (should-error (gt-available-langs '(en) nil))
  (let ((gt-target-history '((en ja) (zh ja) (en zh))))
    (let ((gt-polyglot-p nil)
          (gt-ignore-target-history-p t))
      (should (equal (gt-available-langs '(en zh ja) nil)
                     '((en zh) (en ja))))
      (should (equal (gt-available-langs '(en zh ja) gt-test-text-1)
                     '((en zh) (en ja))))
      (should (equal (gt-available-langs '(en zh ja) gt-test-text-2)
                     '((zh en) (zh ja))))
      (should (equal (gt-available-langs
                      '(en zh ja) (gt-pick-items-by-thing gt-test-text-1 'word))
                     '((en zh) (en ja)))))
    (let ((gt-polyglot-p t)
          (gt-ignore-target-history-p t))
      (should (equal (gt-available-langs '(en zh ja) gt-test-text-1)
                     '((en zh ja))))
      (should (equal (gt-available-langs '(en zh ja) gt-test-text-2)
                     '((zh en ja)))))
    (let ((gt-polyglot-p nil)
          (gt-ignore-target-history-p nil))
      (should (equal (gt-available-langs '(en zh ja) nil)
                     '((en ja) (en zh))))
      (should (equal (gt-available-langs '(en zh ja) gt-test-text-1)
                     '((en ja) (en zh))))
      (should (equal (gt-available-langs '(en zh ja) gt-test-text-2)
                     '((zh ja) (zh en)))))))

(ert-deftest test--gt-valid-literally ()
  (should (gt-valid-literally 'word '("hello") 'en 'fr))
  (should-not (gt-valid-literally 'word '("hello world") 'en 'fr))
  (should-not (gt-valid-literally 'word '("hello" "world") 'en 'fr))
  (should (gt-valid-literally 'not-word '("hello" "world") 'en 'fr))
  (should (gt-valid-literally 'parts '("hello" "world") 'en 'fr))
  (should (gt-valid-literally 'not-parts '("hello") 'en 'fr))
  (should (gt-valid-literally 'src:en '("hello") 'en 'fr))
  (should (gt-valid-literally 'tgt:fr '("hello") 'en 'fr))
  (should (gt-valid-literally 'not-tgt:en '("hello") 'en 'fr))
  (should (gt-valid-literally '(and not-tgt:en (or word not-parts)) '("hello") 'en 'fr)))

(ert-deftest test--gt-plist-let ()
  (should (gt-plist-let '(:b 2 :c 3)
            (equal (+ .b .c) 5)))
  (should (gt-plist-let `(:b 2 :c ,(max 2 3))
            (equal (+ .b .c) 5)))
  (should (gt-plist-let ((a 1) '(:b 2 :c 3))
            (equal (+ a .b .c) 6)))
  (should (gt-plist-let ((a 1) `(:b 2 :c ,(max 2 3)))
            (equal (+ a .b .c) 6)))
  (should (let ((a `(:b 2 :c ,(max 2 3))))
            (gt-plist-let a (equal (+ .b .c) 5)))))

(ert-deftest test--gt-translation-life-cycle ()
  (let ((translator (gt-translator
                     :taker (gt-taker
                             :langs '(en zh)
                             :text 'buffer
                             :pick 'word
                             :pick-pred (lambda (w) (string-prefix-p "w" w))
                             :then #'ignore)
                     :engines (list
                               (gt-google-engine
                                :parse (gt-google-summary-parser)
                                :if 'word
                                :cache nil
                                :then #'ignore)
                               (gt-deepl-engine
                                :if 'not-word
                                :cache nil))
                     :render (gt-insert-render
                              :type 'after
                              :then (lambda (ts)
                                      (with-slots (bounds state bag) ts
                                        (with-current-buffer (car bounds)
                                          (setf state 9 bag (buffer-string)))))))))
    (with-temp-buffer
      (insert "hello world")
      (gt-start translator)
      (while (< (oref translator state) 4) (sit-for 0.2))
      (should (string-match-p "^hello world[ \n]+世界$" (oref translator bag))))
    (with-temp-buffer
      (with-slots (bag state) translator
        (insert "love and peace")
        (gt-set-taker translator (gt-taker :langs '(en zh) :text 'buffer))
        (gt-start translator)
        (while (< state 4) (sit-for 0.2))
        (should (string-match-p "^love and peace[ \n]+爱与和平$" bag))))))

(defun gt-test-chat-session-1 ()
  (let* ((name "abc")
         (items (list (gt-chat-session-item :role 'system :content "you are a assist")
                      (gt-chat-session-item :role 'user :content "what is your name")
                      (gt-chat-session-item :role 'assistant :content "I am chatgpt...")
                      (gt-chat-session-item :role 'user :content "close your eyes")))
         (session (gt-chat-file-session
                   :name name
                   :llm 'openai
                   :top (oref (car items) id)
                   :items (nreverse items))))
    (gt-chat-session-save session)
    (setq session (gt-chat-session-read 'gt-chat-file-session name))
    (with-slots (items) session
      (push (gt-chat-session-item :role 'user :content "open your mouth") items))
    (gt-chat-session-save session)))

;;(gt-chat-new-session "woooo")
;;(gt-chat-del-session "woooo")
;;(gt-chat-get-session "woooo")
;;(oref gt-chat-current-session top)
;;(insert (gt-chat-stringify (car (oref gt-chat-current-session items))))
;;(insert (gt-chat-stringify gt-chat-current-session))
;;(gt-chat-sessions)
;;(setq tools (append tools (mapcar #'string-trim (string-split (match-string 1 content) ","))) content (substring content (match-end 0)))

(provide 'gt-tests)

;;; gt-tests.el ends here
