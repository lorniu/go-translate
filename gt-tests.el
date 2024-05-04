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

(ert-deftest test--gt-pick-items-from-text ()
  (should (null (gt-pick-items-from-text nil nil)))
  (should (equal (gt-pick-items-from-text gt-test-text-1 nil) gt-test-text-1))
  (should (equal (gt-pick-items-from-text gt-test-text-1 'word)
                 '("This" "buffer" "is" "for" "text" "that" "is" "not" "saved" "and"
                   "for" "Lisp" "evaluation" "To" "create" "a" "file" "visit" "it" "with" "C" "x" "C" "f"
                   "and" "enter" "text" "in" "its" "buffer")))
  (should (equal (gt-pick-items-from-text gt-test-text-1 'word (lambda (w) (string-prefix-p "e" w)))
                 '("evaluation" "enter")))
  (should (equal (gt-pick-items-from-text gt-test-text-1 'paragraph)
                 '("This buffer is for text that is not saved, and for Lisp evaluation."
                   " To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.")))
  (should (equal (gt-pick-items-from-text gt-test-text-2 'word)
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
                      '(en zh ja) (gt-pick-items-from-text gt-test-text-1 'word))
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

(ert-deftest test--gt-current-cacher ()
  (let ((gt-default-cacher (gt-memory-cacher)))
    (cl-multiple-value-bind (c d)
        (gt-current-cacher (gt-bing-engine :cache nil) nil)
      (should (and (null c) (null d))))
    (cl-multiple-value-bind (c d)
        (gt-current-cacher (gt-bing-engine :cache t) nil)
      (should (and (eq c gt-default-cacher) (null d))))
    (cl-multiple-value-bind (c d)
        (gt-current-cacher (gt-bing-engine :cache 'word) nil)
      (should (and (eq c gt-default-cacher) (eq d 'word))))
    (cl-multiple-value-bind (c d)
        (gt-current-cacher (gt-bing-engine :cache (gt-memory-cacher :if 'xxx)) nil)
      (should (and (eq (oref c if) 'xxx) (null d))))))

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
  (let* ((res nil)
         (translator (gt-translator :taker (gt-taker
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
                                                     (with-current-buffer (car (oref ts bounds))
                                                       (setq res (buffer-string))))))))
    (with-temp-buffer
      (insert "hello world")
      (gt-start translator)
      (while (< (oref translator state) 3) (sit-for 0.2))
      (should (equal "hello world\n世界" res)))
    (with-temp-buffer
      (insert "love and peace")
      (gt-set-taker translator (gt-taker :langs '(en zh) :text 'buffer))
      (gt-start translator)
      (while (< (oref translator state) 3) (sit-for 0.2))
      (should (equal "love and peace\n爱与和平" res)))))

(provide 'gt-tests)

;;; gt-tests.el ends here
