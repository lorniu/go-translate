;;; gt-engine-osxdict.el --- OSX-Dictionary -*- lexical-binding: t -*-

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

;; Engine for osx-dictionary (macOS only).
;;
;; osx-dictionary, a program written by itchyny, is Dictionary.app interface for macOS.
;; I know it from https://github.com/xuchunyang/osx-dictionary.el. This engine is working
;; with it, and can be used offline for macOS user.
;;
;; Make sure program `osx-dictionary' is on your PATH, or config it with `gt-osxdict-program'.
;;
;;   Source: https://github.com/itchyny/dictionary.vim/tree/master/autoload/dictionary.m
;;   Build:  clang -framework CoreServices -framework Foundation dictionary.m -o osx-dictionary
;;
;;; Code:

(require 'gt-core)

(defclass gt-osxdict-parser (gt-parser) ())

(defclass gt-osxdict-engine (gt-engine)
  ((tag :initform 'osx-dictionary)
   (program :initarg :program :initform nil :documentation "Path of osx-dictionary")
   (parse :initform (gt-osxdict-parser))))

(defvar gt-osxdict-program "osx-dictionary"
  "Executable command or full path of osx-dictionary.")

(cl-defmethod gt-execute ((engine gt-osxdict-engine) task)
  (let ((program (or (oref engine program) gt-osxdict-program))
        (text (oref task text)))
    (unless (executable-find program)
      (user-error (format "[gt-osxdict-engine] Make sure executable `%s' is available'.\n
 Source: https://github.com/itchyny/dictionary.vim/tree/master/autoload/dictionary.m
 Build:  clang -framework CoreServices -framework Foundation dictionary.m -o %s
" program program)))
    (when (cdr text)
      (user-error "[gt-osxdict-engine] Multiple parts translation is not supported"))
    (pdd-exec program text
      :done (lambda (str) (unless (string-empty-p str) str)))))

(cl-defmethod gt-parse ((_ gt-osxdict-parser) task)
  (with-slots (res) task
    (with-temp-buffer
      (insert res)
      ;; This is written casually, should be improved.
      ;; https://github.com/itchyny/dictionary.vim/blob/master/syntax/dictionary.vim
      (cl-loop for (regexp . face) in
               '(("|.+|"          . font-lock-comment-face)
                 ("^ *▸.*"        . font-lock-string-face)
                 ("^ *[A-Z]\\..*" . bold-italic))
               do (goto-char (point-min))
               do (while (re-search-forward regexp nil t)
                    (put-text-property (match-beginning 0) (match-end 0) 'face face)))
      (setf res (buffer-string)))))

(provide 'gt-engine-osxdict)

;;; gt-engine-osxdict.el ends here
