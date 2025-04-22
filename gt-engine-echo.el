;;; gt-engine-echo.el --- Echo Translate -*- lexical-binding: t -*-

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

;; For simple transform, and for test.

;;; Code:

(require 'gt-core)

(defclass gt-echo-engine (gt-engine)
  ((tag  :initform 'Echo)
   (do   :initform nil :initarg :do)))

(cl-defmethod initialize-instance :after ((engine gt-echo-engine) &rest _)
  (with-slots (delimit) engine
    (setf delimit nil)))

(cl-defmethod gt-execute ((engine gt-echo-engine) task)
  (cl-labels ((process-step (op data)
                (pcase op
                  ('clean (mapcar #'substring-no-properties data))
                  ('br (mapcar #'gt-prop-for-bionic-reading data))
                  ((pred functionp) (funcall op data))
                  ((pred consp) (cl-loop with d = data
                                         for sub-op in op
                                         do (setq d (process-step sub-op d))
                                         finally return d))
                  (_ data))))
    (process-step (oref engine do) (oref task text))))



(defun gt-prop-for-bionic-reading (text)
  (with-temp-buffer
    (save-excursion (insert text))
    (skip-chars-forward " \t\n\r\f")
    (while (forward-word)
      (cl-destructuring-bind (beg . end) (bounds-of-thing-at-point 'word)
        (setq end (+ beg (/ (- end beg) 2)))
        (put-text-property beg end 'font-lock-face 'gt-bionic-reading-face)))
    (buffer-string)))

(provide 'gt-engine-echo)

;;; gt-engine-echo.el ends here
