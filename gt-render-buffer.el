;;; gt-render-buffer.el --- Render in a dedicated buffer -*- lexical-binding: t -*-

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

;; Popup a buffer to display the results.

;;; Code:

(require 'gt-core)
(require 'gt-faces)

(defclass gt-buffer-render (gt-render)
  ((buffer-name     :initarg :buffer-name     :initform nil)
   (window-config   :initarg :window-config   :initform nil)
   (split-threshold :initarg :split-threshold :initform nil))
  "Popup a new buffer to display the translation result.")

(defcustom gt-buffer-render-follow-p nil
  "Pop to the result buffer instead of just displaying."
  :type 'boolean
  :group 'go-translate)

(defcustom gt-buffer-render-split-width-threshold 80
  "Threshold width of window horizontal split for Buffer-Render."
  :group 'go-translate
  :type '(choice
          (const :tag "Disable" nil)
          (integer :tag "Threshold")))

(defcustom gt-buffer-render-window-config
  '((display-buffer-reuse-window display-buffer-in-direction)
    (direction . right))
  "Window configuration of buffer window of Buffer-Render.

Notice, this can be overrided by `window-config' slot of render instance."
  :type 'sexp
  :group 'go-translate)

(defcustom gt-buffer-render-init-hook nil
  "Hook run after buffer initialization in Buffer-Render."
  :type 'hook
  :group 'go-translate)

(defcustom gt-buffer-render-output-hook nil
  "Hook run after output finished in Buffer-Render."
  :type 'hook
  :group 'go-translate)

(defvar gt-buffer-render-buffer-name "*gt-result*")

(defvar gt-buffer-render-evil-leading-key "," "Leading key for keybinds in evil mode.")

(defvar-local gt-buffer-render-translator nil)
(defvar-local gt-buffer-render-markers nil)
(defvar-local gt-buffer-render-keybinding-messages nil)
(defvar-local gt-buffer-render-local-map nil)

(declare-function evil-define-key* "ext:evil-core.el" t t)

(cl-defmacro gt-buffer-render-key ((key &optional tag test) &rest form)
  (declare (indent 1))
  `(when ,(or test `,test t)
     (let ((fn ,(if (member (car-safe (car form)) '(function lambda))
                    `,(car form)
                  `(lambda () (interactive) ,@form))))
       (if (bound-and-true-p evil-mode)
           (evil-define-key* 'normal gt-buffer-render-local-map
                             (kbd (concat gt-buffer-render-evil-leading-key
                                          (if gt-buffer-render-evil-leading-key " ") ,key))
                             fn)
         (define-key gt-buffer-render-local-map (kbd ,key) fn))
       (setq gt-buffer-render-keybinding-messages
             (cl-remove ,key gt-buffer-render-keybinding-messages :key #'car :test #'string=))
       (when ,tag
         (push (cons ,key ,tag) gt-buffer-render-keybinding-messages)))))

(defun gt-buffer-render--refresh ()
  (interactive)
  (oset gt-buffer-render-translator keep t)
  (gt-start gt-buffer-render-translator))

(defun gt-buffer-render--cycle-next (&optional ignore-rules)
  (interactive "P")
  (with-slots (target taker keep) gt-buffer-render-translator
    (if (and (slot-boundp taker 'langs) (gt-functionp (oref taker langs)))
        (user-error "Current taker not support cycle next")
      (let* ((curr gt-last-target)
             (gt-skip-lang-rules-p ignore-rules)
             (gt-ignore-target-history-p t)
             (next (gt-target taker gt-buffer-render-translator 'next)))
        (unless (equal next curr)
          (setf target next keep t)
          (gt-start gt-buffer-render-translator))))))

(defun gt-buffer-render--toggle-polyglot ()
  (interactive)
  (setq gt-polyglot-p (not gt-polyglot-p))
  (with-slots (target keep) gt-buffer-render-translator
    (setf target nil keep t)
    (gt-start gt-buffer-render-translator)))

(defun gt-buffer-render--browser ()
  (interactive)
  (if-let* ((url (get-char-property (point) 'gt-url)))
      (progn (browse-url url)
             (message "Opening %s... Done!" url))
    (message "No url found on current result.")))

(defun gt-buffer-render--clear-cache ()
  (interactive)
  (pdd-cacher-clear gt-cache-store t)
  (message "Clear all caches done."))

(defun gt-buffer-render--keyboard-quit ()
  (interactive)
  (unwind-protect
      (gt-interrupt-speak-process)
    (keyboard-quit)))

(defun gt-buffer-render--show-tips ()
  (interactive)
  (if gt-buffer-render-keybinding-messages
      (message (mapconcat
                (lambda (kd) (concat (propertize (car kd) 'face 'font-lock-keyword-face) ": " (cdr kd)))
                (reverse gt-buffer-render-keybinding-messages) " "))
    (message "No any help tips found")))

(defun gt-buffer-render--toggle-readonly ()
  (interactive)
  (read-only-mode -1)
  (use-local-map nil)
  (local-set-key (kbd "C-x C-q")
                 (lambda ()
                   (interactive)
                   (read-only-mode 1)
                   (use-local-map gt-buffer-render-local-map))))

(defun gt-buffer-render--unfold-source-text ()
  (interactive)
  (mapc (lambda (ov) (delete-overlay ov)) (overlays-at (point))))

(defvar gt-buffer-render-dislike-source-text nil)

(defvar gt-buffer-render-source-text-limit 140
  "Fold some of the source text if it's too long.
This can be a number as visible length or a function with source text
as argument that return a length.")

(defvar gt-buffer-render-source-text-fold-bound-function
  #'gt-buffer-render-source-text-fold-bound)

(defun gt-buffer-render-source-text-fold-bound ()
  (when (and (numberp gt-buffer-render-source-text-limit)
             (> (point-max) gt-buffer-render-source-text-limit))
    (goto-char (min (point-max) (+ (point-min) gt-buffer-render-source-text-limit)))
    (move-to-column fill-column)
    (skip-chars-backward " \t\n")
    (when-let* ((bd (bounds-of-thing-at-point 'word)))
      (goto-char (cdr bd)))
    (when (> (- (point-max) 20) (point))
      (cons (point) (point-max)))))

(defun gt-buffer-insert-source-text (text &optional fold)
  "Propertize and insert the source TEXT into render buffer.
If FOLD non nil, only make part of the text visible."
  (let* ((text (string-trim text "\n+"))
         (beg (point))
         (end (progn (insert (substring-no-properties text)) (point))))
    (insert "\n\n")
    (save-excursion
      (put-text-property beg end 'face 'gt-buffer-render-source-face)
      (put-text-property beg (+ end 2) 'gt-source-text (substring-no-properties text))
      (when fold
        (when-let* ((bd (save-restriction
                          (narrow-to-region beg end)
                          (funcall gt-buffer-render-source-text-fold-bound-function)))
                    (ov (make-overlay beg end)))
          (overlay-put ov 'display
                       (concat
                        (propertize (buffer-substring beg (car bd)) 'face 'gt-buffer-render-source-face)
                        " ..."))
          (overlay-put ov 'keymap (gt-simple-keymap
                                   [return] #'gt-buffer-render--unfold-source-text
                                   [mouse-1] #'gt-buffer-render--unfold-source-text))
          (overlay-put ov 'pointer 'hand)
          (overlay-put ov 'help-echo "Click to unfold the text")))
      (cons beg end))))

(defun gt-buffer-render-init (buffer render translator)
  "Init BUFFER for RENDER of TRANSLATOR."
  (with-slots (text tasks engines) translator
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (entries (gt-extract-data render translator)))
        ;; setup
        (deactivate-mark)
        (font-lock-mode 1)
        (setq-local cursor-type 'hbar)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local gt-buffer-render-translator translator)
        (mapc (lambda (m) (set-marker m nil nil)) gt-buffer-render-markers)
        (setq-local gt-buffer-render-markers nil)
        (erase-buffer)
        ;; headline
        (let ((engines (cl-delete-duplicates (mapcar (lambda (task) (oref task engine)) tasks))))
          (gt-header render translator (unless (cdr engines) (gt-tag (car engines)))))
        ;; content
        (newline)
        (save-excursion
          (unless (or gt-buffer-render-dislike-source-text (cdr text)) ;; single part, single task
            (pcase-let ((`(,beg . ,end) (gt-buffer-insert-source-text (car text))))
              (put-text-property beg end 'gt-task (car tasks))))
          (cl-loop for c in text for i from 0
                   if (cdr text) do (gt-buffer-insert-source-text c) ; multi parts
                   do (cl-loop for entry in entries for j from 0
                               for stream = (plist-get entry :stream)
                               for res = (propertize "Loading..."
                                                     'face 'gt-buffer-render-loading-face
                                                     'gt-result (if stream 'stream t))
                               for (prefix task) = (list (plist-get entry :prefix) (plist-get entry :task))
                               for output = (propertize (concat prefix res "\n\n") 'gt-task task 'gt-part i)
                               do (let ((engine (oref task engine)))
                                    (if (and (oref engine stream) (= i 0)) ; record result bound for stream
                                        (let ((beg (point)) end)
                                          (insert output)
                                          (setq beg (set-marker (make-marker) (+ beg (length prefix))))
                                          (setq end (set-marker (make-marker) (save-excursion (skip-chars-backward " \t\n") (point))))
                                          (push beg gt-buffer-render-markers)
                                          (push end gt-buffer-render-markers)
                                          (oset task markers (cons beg end)))
                                      (insert output)))))))
      ;; keybinds
      (setq gt-buffer-render-local-map (make-sparse-keymap))
      (use-local-map gt-buffer-render-local-map)
      (gt-keybinds render translator)
      ;; state
      (read-only-mode 1)
      (if-let* ((w (get-buffer-window nil t))) (set-window-point w  (point)))
      ;; execute the hook if exists
      (run-hooks 'gt-buffer-render-init-hook))))

(defun gt-buffer-render-output (buffer render translator)
  "Output TRANSLATOR's result to BUFFER for RENDER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (entries (gt-extract-data render translator)) bds prop)
      (with-slots (text tasks) translator
        (save-excursion
          ;; refresh source text
          (goto-char (point-min))
          (unless (cdr text)
            (when-let* ((prop (text-property-search-forward 'gt-source-text)))
              (delete-region (prop-match-beginning prop) (prop-match-end prop))
              (when (or (cdr entries) (not (get-pos-property 1 'gt-mark (car (ensure-list (plist-get (car entries) :result))))))
                (let ((bd (gt-buffer-insert-source-text (car text) t)))
                  (unless (cdr tasks)
                    (put-text-property (car bd) (cdr bd) 'gt-task (car tasks)))))))
          ;; collect positions
          (goto-char (point-min))
          (while (setq prop (text-property-search-forward 'gt-result))
            (push (cons (set-marker (make-marker) (prop-match-beginning prop))
                        (set-marker (make-marker) (prop-match-end prop)))
                  bds))
          (setq bds (nreverse bds))
          ;; output results in bounds
          (cl-loop for _ in text
                   for i from 0
                   do (cl-loop for entry in entries
                               for (beg . end) = (pop bds)
                               for (res state task) = (list (plist-get entry :result) (plist-get entry :state) (plist-get entry :task))
                               do (goto-char beg)
                               do (when (and (cl-plusp state) (null (get-char-property beg 'gt-done))
                                             (or (not (eq (get-char-property (point) 'gt-result) 'stream)) (= state 1)))
                                    (delete-region beg end)
                                    (insert (propertize (if (consp res) (nth i res) res)
                                                        'gt-result t 'gt-done t 'gt-task task 'gt-part i))))))))
    ;; update states
    (set-buffer-modified-p nil)
    ;; execute the hook if exists
    (run-hooks 'gt-buffer-render-output-hook)))

(cl-defmethod gt-header ((_ gt-buffer-render) translator &optional tag)
  "Set head line format for Buffer Render of TRANSLATOR.
TAG is extra message show in the middle if not nil."
  (with-slots (target) translator
    (let ((line (append
                 '(" ")
                 (when-let* ((src (car target)))
                   (list
                    "[" (propertize (format "%s" src) 'face 'gt-buffer-render-header-lang-face) "]"
                    (if tag (concat " ― " (propertize (format "%s" tag) 'face 'gt-buffer-render-header-desc-face)) "")
                    " → "))
                 (list
                  "["
                  (mapconcat (lambda (s) (propertize (format "%s" s) 'face 'gt-buffer-render-header-lang-face)) (cdr target) ", ")
                  "]"
                  (when (and gt-buffer-render-keybinding-messages (not (bound-and-true-p evil-mode)))
                    (concat "    (" (propertize "?" 'face 'font-lock-type-face) " for tips)"))))))
      (setq header-line-format line))))

(cl-defmethod gt-keybinds ((_ gt-buffer-render) _translator)
  "Define keybinds for `gt-buffer-render-local-map'."
  (gt-buffer-render-key ("t" "Cycle Next")        #'gt-buffer-render--cycle-next)
  (gt-buffer-render-key ("T" "Toggle Polyglot")   #'gt-buffer-render--toggle-polyglot)
  (gt-buffer-render-key ("y" "Speak")             (lambda () (interactive) (let (gt-tts-last-engine) (gt-speak))))
  (gt-buffer-render-key ("O" "Browse")            #'gt-buffer-render--browser)
  (gt-buffer-render-key ("c" "Clear caches")      #'gt-buffer-render--clear-cache)
  (gt-buffer-render-key ("g" "Refresh")           #'gt-buffer-render--refresh)
  (gt-buffer-render-key ("n")                     #'next-line)
  (gt-buffer-render-key ("p")                     #'previous-line)
  (gt-buffer-render-key ("h")                     #'backward-char)
  (gt-buffer-render-key ("j")                     #'next-line)
  (gt-buffer-render-key ("k")                     #'previous-line)
  (gt-buffer-render-key ("l")                     #'forward-char)
  (gt-buffer-render-key ("q" "Quit")              #'kill-buffer-and-window)
  (gt-buffer-render-key ("C-g")                   #'gt-buffer-render--keyboard-quit)
  (gt-buffer-render-key ("C-x C-q")               #'gt-buffer-render--toggle-readonly)
  (gt-buffer-render-key ("?")                     #'gt-buffer-render--show-tips))

(cl-defmethod gt-extract-data :around ((render gt-buffer-render) translator)
  "Decorate prefix and result for buffer render."
  (cl-loop with mpp = (cdr (oref translator text))
           with entries = (cl-call-next-method render translator)
           for entry in entries
           for (prefix result state) = (gt-plist-let entry (list .prefix (format "%s" .result) .state))
           if (and prefix (or (not (slot-boundp render 'prefix)) (eq (oref render prefix) t)))
           do (plist-put entry :prefix
                         (concat (propertize (concat prefix (unless mpp "\n"))
                                             'face (if mpp 'gt-buffer-render-inline-prefix-face 'gt-buffer-render-block-prefix-face))
                                 "\n"))
           if (= 1 state) do (plist-put entry :result (propertize result 'face 'gt-buffer-render-error-face))
           collect entry))

(cl-defmethod gt-init ((render gt-buffer-render) translator)
  (with-slots (buffer-name split-threshold window-config) render
    (let ((buf (get-buffer-create (or buffer-name gt-buffer-render-buffer-name)))
          (split-width-threshold (or split-threshold gt-buffer-render-split-width-threshold split-width-threshold)))
      (gt-buffer-render-init buf render translator)
      (display-buffer buf (or window-config gt-buffer-render-window-config))
      (redisplay t))))

(cl-defmethod gt-output ((render gt-buffer-render) (translator gt-translator))
  (when-let* ((buf (get-buffer (or (oref render buffer-name) gt-buffer-render-buffer-name))))
    (gt-buffer-render-output buf render translator)
    (when (= (oref translator state) 3)
      (if gt-buffer-render-follow-p
          (pop-to-buffer buf)
        (display-buffer buf)))))

(cl-defmethod gt-output :after ((_ gt-buffer-render) (translator gt-translator))
  (when (= (oref translator state) 3) (message "")))

(provide 'gt-render-buffer)

;;; gt-render-buffer.el ends here
