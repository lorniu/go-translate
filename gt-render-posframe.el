;;; gt-render-posframe.el --- Render with childframe via `posframe.el' -*- lexical-binding: t -*-

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

;; https://github.com/tumashu/posframe

;;; Code:

(require 'gt-faces)
(require 'gt-render-buffer)

;; implements via package Posframe, you should install it before use this


;;; Popup Mode

(defclass gt-posframe-pop-render (gt-buffer-render)
  ((width        :initarg :width        :initform nil)
   (height       :initarg :height       :initform nil)
   (forecolor    :initarg :forecolor    :initform nil)
   (backcolor    :initarg :backcolor    :initform nil)
   (padding      :initarg :padding      :initform 12)
   (frame-params :initarg :frame-params :initform nil :type list
                 :documentation "Other parameters passed to `posframe-show'. Have higher priority."))
  "Pop up a childframe to show the result.
The frame will disappear when do do anything but focus in it.
Manually close the frame with `q'.")

(defvar gt-posframe-pop-render-buffer " *GT-Pop-Posframe*")
(defvar gt-posframe-pop-render-init-hook nil)
(defvar gt-posframe-pop-render-output-hook nil)
(defvar gt-posframe-pop-render-timeout 30)
(defvar gt-posframe-pop-render-poshandler nil)

(declare-function posframe-show "ext:posframe.el" t t)
(declare-function posframe-delete "ext:posframe.el" t t)
(declare-function posframe-hide "ext:posframe.el" t t)
(declare-function posframe-refresh "ext:posframe.el" t t)
(declare-function posframe-poshandler-frame-top-right-corner "ext:posframe.el" t t)

(defun gt-posframe-render-auto-close-handler ()
  "Close the pop-up posframe window."
  (interactive)
  (unless (or (and gt-current-command
                   (member this-command (list gt-current-command #'exit-minibuffer)))
              (and gt-posframe-pop-render-buffer
                   (string= (buffer-name) gt-posframe-pop-render-buffer)))
    (ignore-errors (posframe-delete gt-posframe-pop-render-buffer))
    (remove-hook 'post-command-hook #'gt-posframe-render-auto-close-handler)))

(cl-defmethod gt-init :before ((_ gt-posframe-pop-render) _)
  (unless (require 'posframe nil t)
    (user-error "To use `gt-posframe-render', you should install and load package `posframe' first")))

(cl-defmethod gt-init ((render gt-posframe-pop-render) translator)
  (with-slots (width height forecolor backcolor padding frame-params) render
    (let ((inhibit-read-only t)
          (gt-buffer-render-init-hook gt-posframe-pop-render-init-hook)
          (buf (get-buffer-create gt-posframe-pop-render-buffer))
          (rweight (cond ((functionp width) (funcall width translator))
                         ((numberp width) width)
                         (t (if fill-column (- fill-column 5) 50))))
          (rheight (cond ((functionp height) (funcall height translator))
                         ((numberp height) height)
                         (t 0))))
      ;; height
      (when (< rheight 1)
        (let* ((h 1) (factor 1) (ts (ensure-list (oref translator text))))
          (with-temp-buffer
            (insert (string-join ts "\n"))
            (goto-char (point-min))
            (while (re-search-forward "\n+" nil t) (cl-incf h)))
          (setq factor (* factor (length ts)))
          (setq factor (* factor (max 1 (length (ensure-list (oref translator engines))))))
          (setq factor (* factor (max 1 (length (ensure-list (cdr (oref translator target)))))))
          (setq rheight (* factor (+ 4 h)))))
      ;; render
      (oset render dislike-source t)
      (gt-buffer-render-init buf render translator)
      ;; create
      (unless (get-buffer-window (get-buffer buf))
        (apply #'posframe-show buf
               (append frame-params
                       (list
                        :timeout gt-posframe-pop-render-timeout
                        :width rweight
                        :height rheight
                        :max-height (/ (window-height) 2)
                        :foreground-color (or forecolor gt-pop-posframe-forecolor)
                        :background-color (or backcolor gt-pop-posframe-backcolor)
                        :internal-border-width padding
                        :internal-border-color (or backcolor gt-pop-posframe-backcolor)
                        :accept-focus t
                        :position (point)
                        :poshandler gt-posframe-pop-render-poshandler))))
      (posframe-refresh buf)
      ;; setup
      (with-current-buffer buf (gt-buffer-render-key ("q" "Close") (posframe-delete buf)))
      (add-hook 'post-command-hook #'gt-posframe-render-auto-close-handler))))

(cl-defmethod gt-output ((render gt-posframe-pop-render) (translator gt-translator))
  (when-let* ((buf (get-buffer gt-posframe-pop-render-buffer)))
    (let ((gt-buffer-render-output-hook gt-posframe-pop-render-output-hook))
      (gt-buffer-render-output buf render translator)
      (posframe-refresh buf))))


;;; Pin Mode

(defclass gt-posframe-pin-render (gt-posframe-pop-render)
  ((width        :initarg :width        :initform 60)
   (height       :initarg :height       :initform 20)
   (padding      :initarg :padding      :initform 8)
   (bd-width     :initarg :bd-width     :initform 1)
   (bd-color     :initarg :bd-color     :initform nil)
   (backcolor    :initarg :backcolor    :initform nil)
   (fri-color    :initarg :fringe-color :initform nil)
   (position     :initarg :position     :initform nil)
   (frame-params :initarg :frame-params :initform nil :type list
                 :documentation "Other parameters passed to `posframe-show'. Have higher priority."))
  "Pin the childframe in a fixed position to display the translate result.
The childframe will not close, until you kill it with `q'.
Other operations in the childframe buffer, just like in 'gt-buffer-render'.")

(defvar gt-posframe-pin-render-buffer " *GT-Pin-Posframe*")
(defvar gt-posframe-pin-render-frame nil)
(defvar gt-posframe-pin-render-init-hook nil)
(defvar gt-posframe-pin-render-output-hook nil)
(defvar gt-posframe-pin-render-poshandler #'posframe-poshandler-frame-top-right-corner)

(cl-defmethod gt-init ((render gt-posframe-pin-render) translator)
  (with-slots (width height min-width min-height bd-width forecolor backcolor bd-color fri-color padding position frame-params init) render
    (if (and (get-buffer gt-posframe-pin-render-buffer) gt-posframe-pin-render-frame)
        (make-frame-visible gt-posframe-pin-render-frame)
      (setq gt-posframe-pin-render-frame
            (let ((inhibit-read-only t))
              (apply #'posframe-show gt-posframe-pin-render-buffer
                     (append frame-params
                             (list
                              :string "\nLoading..."
                              :width width
                              :height height
                              :min-width width
                              :min-height height
                              :foreground-color (or forecolor gt-pin-posframe-forecolor)
                              :background-color (or backcolor gt-pin-posframe-backcolor)
                              :internal-border-width bd-width
                              :border-color (or bd-color gt-pin-posframe-bdcolor)
                              :left-fringe padding
                              :right-fringe padding
                              :refresh nil
                              :accept-focus t
                              :respect-header-line t
                              :position position
                              :poshandler (unless position gt-posframe-pin-render-poshandler))))))
      (set-frame-parameter gt-posframe-pin-render-frame 'drag-internal-border t)
      (set-frame-parameter gt-posframe-pin-render-frame 'drag-with-header-line t)
      (when-let* ((color (or fri-color gt-pin-posframe-fringe-color)))
        (set-face-background 'fringe color  gt-posframe-pin-render-frame)))
    ;; render
    (let ((gt-buffer-render-init-hook gt-posframe-pin-render-init-hook))
      (gt-buffer-render-init gt-posframe-pin-render-buffer render translator))
    ;; setup
    (with-current-buffer gt-posframe-pin-render-buffer
      (gt-buffer-render-key ("q" "Close") (posframe-hide gt-posframe-pin-render-buffer))
      (if (functionp init) (pdd-funcall init (list translator))))))

(cl-defmethod gt-output ((render gt-posframe-pin-render) (translator gt-translator))
  (let ((gt-buffer-render-output-hook gt-posframe-pin-render-output-hook))
    (gt-buffer-render-output gt-posframe-pin-render-buffer render translator)))

(provide 'gt-render-posframe)

;;; gt-render-posframe.el ends here
