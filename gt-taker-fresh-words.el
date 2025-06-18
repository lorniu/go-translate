;;; gt-taker-fresh-words.el --- Pick fresh words -*- lexical-binding: t -*-

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

;; Pick only fresh words

;;; Code:

(require 'gt-core)

(defvar gt-fresh-word-class 'gt-fresh-word-with-file)

(defvar gt-ripe-words-file (locate-user-emacs-file "gt-known-words.txt"))

(defmacro gt-with-ripe-words-file (&rest form)
  `(let* ((case-fold-search t)
          (bufname " gt-known-words")
          (buf (or (get-buffer bufname)
                   (progn
                     (unless (file-exists-p gt-ripe-words-file)
                       (write-region (point-min) (point-min) gt-ripe-words-file))
                     (find-file-noselect gt-ripe-words-file)))))
     (with-current-buffer buf
       (unless (equal (buffer-name) bufname)
         (rename-buffer bufname))
       (goto-char (point-min))
       (prog1 (progn ,@form) (basic-save-buffer)))))

(cl-defgeneric gt-word-fresh-p (word)
  (:method ((_ (eql 'gt-fresh-word-with-file)) word)
           (gt-with-ripe-words-file
            (not (re-search-forward (concat "^" word "$") nil t))))
  (gt-word-fresh-p gt-fresh-word-class word))

(cl-defgeneric gt-fresh-word (&rest words)
  (:method ((_ (eql 'gt-fresh-word-with-file)) &rest words)
           (gt-with-ripe-words-file
            (if (equal words (list t)) ; clear ripes
                (erase-buffer)
              (dolist (word words) ; remove from ripes
                (goto-char (point-min))
                (while (re-search-forward (concat "^" word "$") nil t)
                  (delete-region (match-beginning 0) (match-end 0))
                  (if (looking-at "\n") (delete-char 1)))))))
  (apply #'gt-fresh-word gt-fresh-word-class words))

(cl-defgeneric gt-ripen-word (&rest words)
  (:method ((_ (eql 'gt-fresh-word-with-file)) &rest words)
           (gt-with-ripe-words-file ; add to ripes
            (dolist (word words)
              (goto-char (point-min))
              (unless (re-search-forward (concat "^" word "$") nil t)
                (insert (downcase word) "\n")))))
  (apply #'gt-ripen-word gt-fresh-word-class words))

(defvar gt-fresh-words-last nil)

(defun gt-record-words-as-known ()
  "Record the words as known."
  (interactive)
  (let* ((si (string-join
              (sort (delete-dups
                     (mapcar #'downcase
                             (or gt-fresh-words-last
                                 (ensure-list (thing-at-point 'word))))))
              " "))
         (ss (read-string "Words to record as known: " si))
         (ws (split-string ss)))
    (apply #'gt-ripen-word ws)))

(defun gt-record-words-as-unknown ()
  "Record the words as unknown."
  (interactive)
  (let* ((ss (read-string "Words to record as unknown: " (thing-at-point 'word)))
         (ws (split-string ss)))
    (apply #'gt-fresh-word ws)))

(cl-defmethod gt-pick ((_ (eql 'fresh-word)) translator)
  (with-slots (text bounds taker) translator
    (setq gt-fresh-words-last nil)
    (let* ((car (if (cdr bounds) (cl-subseq bounds 0 2) (car text)))
           (pred (lambda (word)
                   (and (if-let* ((p (oref taker pick-pred))) (funcall p word) t)
                        (> (string-bytes word) 2)
                        (not (string-match-p "^[0-9]+$" word))
                        (gt-word-fresh-p word)
                        (push word gt-fresh-words-last)))))
      (gt-pick-items-by-thing car 'word pred))))

(add-to-list 'gt-taker-pick-things 'fresh-word t)

(provide 'gt-taker-fresh-words)

;;; gt-taker-fresh-words.el ends here
