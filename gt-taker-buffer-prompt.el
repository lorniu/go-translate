;;; gt-taker-buffer-prompt.el --- Prompt with new buffer -*- lexical-binding: t -*-

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

;; Prompt with new buffer

;;; Code:

(require 'gt-core)

(defcustom gt-buffer-prompt-window-config
  '((display-buffer-reuse-window display-buffer-below-selected))
  "Window configuration of taker's buffer prompt window."
  :type 'sexp
  :group 'go-translate)

(defvar gt-buffer-prompt-name "*gt-taker*")

(defvar gt-buffer-prompt-map (make-sparse-keymap))

(defvar gt-buffer-prompt-after-init-function nil
  "Function with no argument, executed after prompt buffer init.
Can do extra setup works for the buffer through this function.")

(declare-function gt-set-render "ext:go-translate")
(declare-function gt-set-engines "ext:go-translate")
(declare-function gt-translator-info "ext:go-translate")

(cl-defmethod gt-prompt ((taker gt-taker) translator (_ (eql 'buffer)))
  "Prompt the TAKER's result using a new buffer for TRANSLATOR.

Only works when taker's prompt slot is config as `buffer:

  :taker (gt-picker :prompt `buffer)

Edit the text in buffer and confirm with `C-c C-c', you can also change
target, engines and render in the buffer for the following translation."
  (with-slots (text target render engines _render _engines) translator
    (when (cdr text)
      (user-error "Multiple text cannot be prompted"))
    (cl-labels ((prop (s &optional fn not-key)
                  (let (args)
                    (unless not-key (setq args `(,@args face font-lock-keyword-face)))
                    (if fn (setq args `(,@args local-map (keymap (mode-line keymap (mouse-1 . ,fn)))
                                               mouse-face font-lock-warning-face)))
                    (apply #'propertize (format "%s" s) args)))
                (set-head-line ()
                  (setq header-line-format
                        (concat " " (cl-loop for (key . value) in `(("C-c C-c" . "to apply")
                                                                    ("C-c C-k" . "to cancel"))
                                             concat (format "%s %s " (prop key) value))
                                " " (propertize "Translate taking..." 'face 'font-lock-warning-face))))
                (set-mode-line ()
                  (setq mode-line-format
                        (let ((ms (concat "C-c C-n: Next Target\nC-c C-p: Prev Target\n\n"
                                          "C-c C-e: Set Engines\nC-c C-r: Set Render")))
                          (mapcar (lambda (item) (when item (propertize item 'help-echo ms)))
                                  (cl-destructuring-bind (_ eg rd)
                                      (ignore-errors (gt-translator-info translator))
                                    (list (prop (concat
                                                 (if-let* ((src (car target))) (concat "[" (prop src) "] → "))
                                                 "[" (mapconcat (lambda (s) (prop s)) (cdr target) ", ") "]")
                                                #'cycle-next-target t)
                                          (if eg (concat "  Engines: " (prop eg #'set-engines)))
                                          (if rd (concat "  Render: " (prop rd #'set-render)))))))))
                (cycle-next-target (&optional backwardp)
                  (interactive)
                  (setf target
                        (gt-target taker (make-instance
                                          (eieio-object-class translator)
                                          :text (list (buffer-string)))
                                   (if backwardp 'prev 'next)))
                  (set-mode-line))
                (cycle-prev-target ()
                  (interactive)
                  (cycle-next-target t))
                (set-engines ()
                  (interactive)
                  (gt-set-engines translator)
                  (set-mode-line))
                (set-render ()
                  (interactive)
                  (gt-set-render translator)
                  (set-mode-line))
                (set-local-keys ()
                  (local-set-key (kbd "C-c C-n") #'cycle-next-target)
                  (local-set-key (kbd "C-c C-p") #'cycle-prev-target)
                  (local-set-key (kbd "C-c C-e") #'set-engines)
                  (local-set-key (kbd "C-c C-r") #'set-render)))
      (let* ((newtext (gt-read-from-buffer
                       :buffer gt-buffer-prompt-name
                       :initial-contents (or (car text) "")
                       :catch 'gt-buffer-prompt
                       :window-config gt-buffer-prompt-window-config
                       :keymap gt-buffer-prompt-map
                       (set-head-line)
                       (set-mode-line)
                       (set-local-keys)
                       (when gt-buffer-prompt-after-init-function
                         (funcall gt-buffer-prompt-after-init-function)))))
        (when (null newtext)
          (user-error ""))
        (when (string-blank-p newtext)
          (user-error "Text should not be null, abort"))
        (setf text (ensure-list newtext))))))

(provide 'gt-taker-buffer-prompt)

;;; gt-taker-buffer-prompt.el ends here
