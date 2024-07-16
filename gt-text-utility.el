;;; gt-text-utility.el --- Text utilities base on the translator -*- lexical-binding: t -*-

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

;; Integrate md5, sha1, base64, qrcode and others into this translation framework.
;;
;; Shows that this framework is more than just translation purposes.
;;
;; Use like this:
;;
;;   \\=(gt-start (gt-text-utility :render (gt-buffer-render)))
;;
;; To avoid be prompted, specify the targets like this:
;;
;;   \\=(gt-start (gt-text-utility :taker (gt-taker :langs '(md5 sha1)) :render (gt-insert-render)))

;;; Code:

(require 'gt-extension)

(defvar gt-text-utilities `(base64 rot13 qrcode speak ,@(secure-hash-algorithms))
  "List of available targets for `gt-text-utility-engine'.")

(defclass gt-text-utility (gt-translator) ())

(defclass gt-text-utility-engine (gt-engine)
  ((tag :initform 'Text-Utility)
   (cache :initform nil)
   (delimiter :initform nil)))

(cl-defmethod gt-reset :after ((translator gt-text-utility) &rest _)
  (with-slots (_engines) translator
    (unless _engines (setf _engines (gt-text-utility-engine)))
    (setf _engines (ensure-list _engines))
    (unless (and (not (cdr _engines)) (cl-typep (car _engines) 'gt-text-utility-engine))
      (user-error "%s should use only one gt-text-utility-engine as engine" (gt-desc translator)))))

(cl-defmethod gt-target ((taker gt-taker) (_ gt-text-utility) &rest _)
  (let ((tgts (if (slot-boundp taker 'langs)
                  (ensure-list (oref taker langs))
                (mapcar #'intern
                        (completing-read-multiple
                         "Utilities (can choose multiple): " gt-text-utilities
                         nil t nil 'gt-text-utility-hist)))))
    (unless tgts (user-error "No targets found"))
    (cons nil (cl-delete-duplicates tgts))))

(cl-defmethod gt-translate ((_ gt-text-utility-engine) task _)
  "Translate and render the result directly, skip parse and other steps."
  (with-slots (text tgt res translator) task
    (setf res (mapcar (lambda (c) (gt-text-util tgt (encode-coding-string c 'utf-8))) text))
    (gt-update-state translator)
    (gt-output (oref translator render) translator)))

(cl-defmethod gt-keybinds ((_buffer gt-buffer-render) (_ gt-text-utility))
  (gt-buffer-render-key ("t" "Cycle Next")  #'gt-buffer-render--cycle-next)
  (gt-buffer-render-key ("g" "Refresh")     #'gt-buffer-render--refresh)
  (gt-buffer-render-key ("q" "Quit")        #'kill-buffer-and-window)
  (gt-buffer-render-key ("C-x C-q")         #'gt-buffer-render--toggle-readonly)
  (gt-buffer-render-key ("?")               #'gt-buffer-render--show-tips))



(cl-defgeneric gt-text-util (target text)
  "Dispatch to different TEXT utility methods for different TARGET."
  (:method ((_ (eql 'base64)) text) (base64-encode-string text t))
  (:method ((_ (eql 'rot13)) text) (rot13 text))
  (:method ((_ (eql 'qrcode)) text) (gt-qrcode text))
  (:method ((_ (eql 'speak)) text) (gt-text-speak-button text))
  (if (member target (secure-hash-algorithms))
      (secure-hash target text)
    (user-error "Unimplemented")))

(defun gt-text-speak-button (str)
  (propertize str
              'display " click to speak... "
              'pointer 'hand
              'keymap (gt-simple-keymap [mouse-1] (lambda () (interactive) (gt-speak 'interact str t)))
              'face '(:box t)
              'mouse-face '(:reverse-video t :inherit font-lock-warning-face)))

(declare-function qrencode "ext:qrencode.el" t t)

;;;###autoload
(defun gt-qrcode (str)
  "Genrate QR Code for STR."
  (interactive (list (read-string "String to generate QRCode: ")))
  (let ((code (if (executable-find "qrencode")
                  (with-temp-buffer
                    (let ((coding-system-for-read 'raw-text))
                      (set-buffer-multibyte nil)
                      (insert str)
                      (shell-command-on-region (point-min) (point-max) "qrencode -t PNG -o -" (current-buffer))
                      (if (string-match-p "PNG" (buffer-substring 1 (min (point-max) 8)))
                          (propertize " " 'display `(image :type png :data ,(buffer-string) :width 200))
                        (user-error (buffer-string)))))
                (if (require 'qrencode nil t)
                    (qrencode str)
                  (user-error "Install program `qrencode' or package `qrencode.el' first to generate QR Code")))))
    (if (called-interactively-p 'any) (message "%s" code) code)))

(provide 'gt-text-utility)

;;; gt-text-utility.el ends here
