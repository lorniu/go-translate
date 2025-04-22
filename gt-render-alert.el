;;; gt-render-alert.el --- Render with `alert.el' -*- lexical-binding: t -*-

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

;; https://github.com/jwiegley/alert

;;; Code:

(require 'gt-core)

(defclass gt-alert-render (gt-render) ()
  :documentation "Output results as system notification.
It depends on the `alert' package.")

(defvar gt-alert-render-args '(:timeout 10))

(declare-function alert "ext:alert.el" t t)

(cl-defmethod gt-init ((_ gt-alert-render) _)
  (unless (require 'alert nil t)
    (user-error "To use `gt-alert-render', you should install and load package `alert' first")))

(cl-defmethod gt-output ((render gt-alert-render) (translator gt-translator))
  (when (= (oref translator state) 3)
    (let ((ret (gt-extract-data render translator)) lst)
      ;; format
      (dolist (tr ret)
        (let ((prefix (if (cdr ret) (plist-get tr :prefix)))
              (result (string-join (ensure-list (plist-get tr :result)) "\n")))
          (push (concat prefix result) lst)))
      ;; output
      (message "")
      (apply #'alert (string-join (nreverse lst) "\n") :title "*Go-Translate*" gt-alert-render-args))))

(provide 'gt-render-alert)

;;; gt-render-alert.el ends here
