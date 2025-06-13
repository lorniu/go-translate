;;; gt-render-kill-ring.el --- Put result to kill ring -*- lexical-binding: t -*-

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

;; Put the result to kill ring.

;;; Code:

(require 'gt-core)

(defclass gt-kill-ring-render (gt-render) ()
  :documentation "Used to save the translate result into kill ring.")

(cl-defmethod gt-output ((render gt-kill-ring-render) (translator gt-translator))
  (deactivate-mark)
  (when (= (oref translator state) 3)
    (let ((ret (gt-extract-data render translator)))
      (when-let* ((err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret)))
        (kill-new "")
        (error "%s" (plist-get err :result)))
      (kill-new (mapconcat (lambda (r) (string-join (plist-get r :result) "\n")) ret "\n\n"))
      (message "Result already in the kill ring."))))

(provide 'gt-render-kill-ring)

;;; gt-render-kill-ring.el ends here
