;;; gt-engine-stardict.el --- StarDict -*- lexical-binding: t -*-

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

;; Engine for sdcv (StarDict Console Version).
;;
;; sdcv is an offline translate software. It can be used when no network.
;;
;; Make sure sdcv is installed on your system:
;;
;;   sudo pacman -S sdcv
;;
;; Then download dictionary data to ~/.stardict/dic or /usr/share/stardict/dic:
;;
;;   # For example from http://download.huzheng.org:
;;
;;   mkdir -p ~/.stardict/dic
;;   cd ~/.stardict/dic
;;   wget http://download.huzheng.org/zh_CN/stardict-langdao-ce-gb-2.4.2.tar.bz2
;;   wget http://download.huzheng.org/zh_CN/stardict-langdao-ce-gb-2.4.2.tar.bz2
;;   tar xvf stardict-langdao-ec-gb-2.4.2.tar.bz2
;;   tar xvf stardict-langdao-ce-gb-2.4.2.tar.bz2
;;   sdcv -l
;;
;; Also, you can specify the dictionary and data-dir in `gt-stardict-engine' definition:
;;
;;   (gt-stardict-engine :dict "a-dictionary" :dir "~/.stardict/dic" :dir-only t)
;;
;;; Code:

(require 'gt-extension)

(defclass gt-stardict-parser (gt-parser) ())

(defclass gt-stardict-engine (gt-engine)
  ((tag :initform 'StarDict)
   (dict
    :initarg :dict
    :initform nil
    :type (or string null)
    :documentation "Search only this dictionary if not nil.")
   (dir
    :initarg :dir
    :initform nil
    :type (or string null)
    :documentation "Use this dir as data directory if not nil.")
   (dir-only
    :initarg :dir-only
    :initform nil
    :type boolean
    :documentation "Only use dictionaries in `dir'.")
   (exact
    :initarg :exact
    :initform nil
    :type boolean
    :documentation "Exact search, that is disable fuzzy-search.")
   (cache :initform nil)
   (parse :initform (gt-stardict-parser))))

(defvar gt-stardict-program "sdcv"
  "Executable command of sdcv (StarDict Console Version).")

(defconst gt-stardict-args '("--non-interactive" "--json-output" "-0" "-1"))

(defvar gt-stardict-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'gt-stardict-switch-dict)
    (define-key map (kbd "C-c C-c") #'gt-stardict-switch-dict)
    map)
  "The keymap used by StarDict.")

(defun gt-stardict-build-args (engine)
  (with-slots (dict dir dir-only exact) engine
    (let ((args gt-stardict-args))
      (if dict (setq args `("--use-dict" ,dict ,@args)))
      (if dir (setq args `("--data-dir" ,dir ,@args)))
      (if dir-only (setq args `("--only-data-dir" ,@args)))
      (if exact (setq args `("--exact-search" ,@args)))
      (gt-log 'gt-stardict (format "[sdcv arguments]: %s" args))
      args)))

(defun gt-stardict-list-dicts (engine)
  (with-temp-buffer
    (apply #'call-process gt-stardict-program nil t nil
           (cons "--list-dicts" (gt-stardict-build-args engine)))
    (goto-char (point-min))
    (cl-loop for d across (json-read)
             collect (cons (alist-get 'name d) (alist-get 'wordcount d)))))

(defun gt-stardict-switch-dict ()
  (interactive)
  (let* ((task (get-char-property (point) 'gt-task))
         (engine (and task (oref task engine)))
         (dicts (and engine (gt-stardict-list-dicts engine))))
    (when (= (length dicts) 0) (user-error "No dict found, nothing can do"))
    (let ((dict (completing-read "Dictionary to use: "
                                 (gt-make-completion-table (cons 'ALL dicts))
                                 nil t nil nil (oref engine dict))))
      (oset engine dict (unless (string= dict "ALL") dict))
      (oset gt-buffer-render-translator keep t)
      (gt-start gt-buffer-render-translator))))

(cl-defgeneric gt-stardict-pretty-definition (dictionary definition)
  "Try to pretty the DEFINITION part of DICTIONARY for output."
  (:method ((_ (eql '朗道)) definition)
           (with-temp-buffer
             (insert definition)
             ;; phonetic
             (goto-char (point-min))
             (while (re-search-forward "^\\*\\(\\[.+\\]\\)" nil t)
               (replace-match (propertize (match-string 1) 'face 'gt-stardict-phonetic-face)))
             ;; related
             (goto-char (point-min))
             (while (re-search-forward "^相关词组:" nil t)
               (replace-match (concat "\n" (propertize (match-string 0) 'face 'bold))))
             ;; word class
             (goto-char (point-min))
             (while (re-search-forward
                     (format "^\\(%s\\)\\. " (mapconcat (lambda (v) (format "%s" v)) gt-word-classes "\\|")) nil t)
               (put-text-property (match-beginning 0) (match-end 0) 'face 'gt-stardict-word-class-face))
             (buffer-string)))
  (cond ((string-match-p "^朗道.+字典" (format "%s" dictionary))
         (gt-stardict-pretty-definition '朗道 definition))
        (t definition)))

(cl-defmethod gt-translate ((engine gt-stardict-engine) task next)
  (unless (executable-find gt-stardict-program)
    (user-error "You should install `sdcv' first before use `gt-stardict-engine'"))
  (with-slots (text res) task
    (with-temp-buffer
      (apply #'call-process gt-stardict-program nil t nil
             (append (gt-stardict-build-args engine) (list text)))
      (setf res (buffer-string))
      (funcall next task))))

(cl-defmethod gt-parse ((_ gt-stardict-parser) task)
  (with-slots (res) task
    (let ((json (condition-case _ (json-read-from-string res)
                  (error (user-error res))))
          (props `(pointer hand help-echo "Click to switch dictionary" keymap ,gt-stardict-map)))
      (when (= (length json) 0)
        (user-error (apply #'propertize "No translation result found, sorry :(" props)))
      (with-temp-buffer
        (cl-loop for d in (cl-remove-duplicates (mapcar #'cdar json) :test #'string=)
                 do (insert (apply #'propertize d 'display '(height 0.6) 'face 'gt-stardict-dict-face props) "\n\n")
                 collect (cl-loop for r across (cl-remove-if-not
                                                (lambda (item) (string= (cdar item) d)) json)
                                  for i from 1
                                  for word = (propertize (alist-get 'word r)
                                                         'face 'gt-stardict-word-face
                                                         'display '(raise 0.6))
                                  for definition = (propertize (gt-stardict-pretty-definition (intern d) (alist-get 'definition r))
                                                               'display '(raise 0.3)
                                                               'line-prefix "  "
                                                               'wrap-prefix "  ")
                                  collect (concat (if (= i 0) (gt-line-height-separator 10)) word definition) into r1
                                  finally (return (string-join r1 "\n")))
                 into rs finally (insert (string-join rs "\n\n")))
        (setf res (buffer-string))))))

(provide 'gt-engine-stardict)

;;; gt-engine-stardict.el ends here
