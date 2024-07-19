;;; gt-extension.el --- Extension components -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Author: lorniu <lorniu@gmail.com>
;; Package-Requires: ((emacs "28.1"))

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

;; Some extension components

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'gt-core)
(require 'gt-faces)


;;; [Render] buffer render

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
  (if-let (url (get-char-property (point) 'gt-url))
      (progn (browse-url url)
             (message "Opening %s... Done!" url))
    (message "No url found on current result.")))

(defun gt-buffer-render--delete-cache ()
  (interactive)
  (when-let ((task (get-pos-property (point) 'gt-task))
             (key (gt-cache-key task (or (get-pos-property (point) 'gt-part) 0))))
    (gt-cache-set gt-default-cacher key nil)
    (message "Delete %s from cacher" key)))

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
  (mapc (lambda (ov)
          (delete-overlay ov))
        (overlays-at (point))))

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
    (when-let (bd (bounds-of-thing-at-point 'word))
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
      (put-text-property beg (+ end 2) 'gt-source-text t)
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
  (with-current-buffer buffer
    (with-slots (text tasks engines) translator
      ;; setup
      (deactivate-mark)
      (visual-line-mode -1)
      (font-lock-mode 1)
      (setq-local cursor-type 'hbar)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local gt-buffer-render-translator translator)
      ;; headline
      (let ((engines (cl-delete-duplicates (mapcar (lambda (task) (oref task engine)) tasks))))
        (gt-header render translator (unless (cdr engines) (oref (car engines) tag))))
      ;; content
      (let ((inhibit-read-only t)
            (ret (gt-extract render translator)))
        (erase-buffer)
        (newline)
        (save-excursion
          (unless (or gt-buffer-render-dislike-source-text (cdr text)) ;; single part, single task
            (pcase-let ((`(,beg . ,end) (gt-buffer-insert-source-text (car text))))
              (put-text-property beg end 'gt-task (car tasks))))
          (cl-loop for c in text
                   for i from 0
                   if (cdr text) do (gt-buffer-insert-source-text c) ; multi parts
                   do (cl-loop for tr in ret
                               for stream = (plist-get tr :stream)
                               for res = (propertize "Loading..."
                                                     'face 'gt-buffer-render-loading-face
                                                     'gt-result (if stream 'stream t))
                               for (prefix task) = (list (plist-get tr :prefix) (plist-get tr :task))
                               for output = (propertize (concat prefix res "\n\n") 'gt-task task 'gt-part i)
                               do (let ((engine (oref task engine)))
                                    (if (and (gt-stream-p engine) (= i 0)) ; record result bound for stream
                                        (let ((beg (point)) end)
                                          (insert output)
                                          (setq beg (set-marker (make-marker) (+ beg (length prefix))))
                                          (setq end (set-marker (make-marker) (save-excursion (skip-chars-backward " \t\n") (point))))
                                          (oset task markers (cons beg end)))
                                      (insert output)))))))
      ;; keybinds
      (setq gt-buffer-render-local-map (make-sparse-keymap))
      (use-local-map gt-buffer-render-local-map)
      (gt-keybinds render translator)
      ;; state
      (read-only-mode 1)
      (if-let (w (get-buffer-window nil t)) (set-window-point w  (point)))
      ;; execute the hook if exists
      (run-hooks 'gt-buffer-render-init-hook))))

(defun gt-buffer-render-output (buffer render translator)
  "Output TRANSLATOR's result to BUFFER for RENDER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (ret (gt-extract render translator)) bds prop)
      (with-slots (text tasks) translator
        (save-excursion
          ;; refresh source text
          (goto-char (point-min))
          (unless (cdr text)
            (when-let (prop (text-property-search-forward 'gt-source-text))
              (delete-region (prop-match-beginning prop) (prop-match-end prop))
              (when (or (cdr ret) (not (get-pos-property 1 'gt-mark (car (ensure-list (plist-get (car ret) :result))))))
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
                   do (cl-loop for tr in ret
                               for (beg . end) = (pop bds)
                               for (res state task) = (list (plist-get tr :result) (plist-get tr :state) (plist-get tr :task))
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
                 (when-let (src (car target))
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
  (gt-buffer-render-key ("y" "TTS")               (lambda () (interactive) (let (gt-tts-last-engine) (gt-do-speak))))
  (gt-buffer-render-key ("O" "Browser")           #'gt-buffer-render--browser)
  (gt-buffer-render-key ("c" "Del Cache")         #'gt-buffer-render--delete-cache)
  (gt-buffer-render-key ("C")                     #'gt-purge-cache)
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

(cl-defmethod gt-extract :around ((render gt-buffer-render) translator)
  "Decorate prefix and result for buffer render."
  (cl-loop with mpp = (cdr (oref translator text))
           with ret = (cl-call-next-method render translator)
           for tr in ret
           for (prefix result state) = (gt-plist-let tr (list .prefix (format "%s" .result) .state))
           if (and prefix (or (not (slot-boundp render 'prefix)) (eq (oref render prefix) t)))
           do (plist-put tr :prefix
                         (concat (propertize (concat prefix (unless mpp "\n"))
                                             'face (if mpp 'gt-buffer-render-inline-prefix-face 'gt-buffer-render-block-prefix-face))
                                 "\n"))
           if (= 1 state) do (plist-put tr :result (propertize result 'face 'gt-buffer-render-error-face))
           collect tr))

(cl-defmethod gt-init ((render gt-buffer-render) translator)
  (with-slots (buffer-name split-threshold window-config) render
    (let ((buf (get-buffer-create (or buffer-name gt-buffer-render-buffer-name)))
          (split-width-threshold (or split-threshold gt-buffer-render-split-width-threshold split-width-threshold)))
      (gt-buffer-render-init buf render translator)
      (display-buffer buf (or window-config gt-buffer-render-window-config)))))

(cl-defmethod gt-output ((render gt-buffer-render) translator)
  (when-let (buf (get-buffer (or (oref render buffer-name) gt-buffer-render-buffer-name)))
    (gt-buffer-render-output buf render translator)
    (when (= (oref translator state) 3)
      (if gt-buffer-render-follow-p
          (pop-to-buffer buf)
        (display-buffer buf)))))

(cl-defmethod gt-output :after ((_ gt-buffer-render) translator)
  (when (= (oref translator state) 3) (message "")))


;;; [Render] Child-Frame Render (Popup Mode)
;; implements via package Posframe, you should install it before use this

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
          (gt-buffer-render-dislike-source-text t)
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
      (with-current-buffer buf
        (gt-buffer-render-key ("q" "Close") (posframe-delete buf)))
      (add-hook 'post-command-hook #'gt-posframe-render-auto-close-handler))))

(cl-defmethod gt-output ((render gt-posframe-pop-render) translator)
  (when-let (buf (get-buffer gt-posframe-pop-render-buffer))
    (let ((gt-buffer-render-output-hook gt-posframe-pop-render-output-hook))
      (gt-buffer-render-output buf render translator)
      (posframe-refresh buf))))


;;; [Render] Child-Frame Render (Pin Mode)

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
  (if (and (get-buffer gt-posframe-pin-render-buffer) gt-posframe-pin-render-frame)
      (make-frame-visible gt-posframe-pin-render-frame)
    (with-slots (width height min-width min-height bd-width forecolor backcolor bd-color padding position frame-params) render
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
                              :poshandler (unless position gt-posframe-pin-render-poshandler)))))))
    (set-frame-parameter gt-posframe-pin-render-frame 'drag-internal-border t)
    (set-frame-parameter gt-posframe-pin-render-frame 'drag-with-header-line t)
    (when-let (color (or (oref render fri-color) gt-pin-posframe-fringe-color))
      (set-face-background 'fringe color  gt-posframe-pin-render-frame)))
  ;; render
  (let ((gt-buffer-render-init-hook gt-posframe-pin-render-init-hook))
    (gt-buffer-render-init gt-posframe-pin-render-buffer render translator))
  ;; setup
  (with-current-buffer gt-posframe-pin-render-buffer
    (gt-buffer-render-key ("q" "Close") (posframe-hide gt-posframe-pin-render-buffer))))

(cl-defmethod gt-output ((render gt-posframe-pin-render) translator)
  (let ((gt-buffer-render-output-hook gt-posframe-pin-render-output-hook))
    (gt-buffer-render-output gt-posframe-pin-render-buffer render translator)))


;;; [Render] kill-ring render

(defclass gt-kill-ring-render (gt-render) ()
  :documentation "Used to save the translate result into kill ring.")

(cl-defmethod gt-output ((render gt-kill-ring-render) translator)
  (deactivate-mark)
  (when (= (oref translator state) 3)
    (let ((ret (gt-extract render translator)))
      (when-let (err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret))
        (kill-new "")
        (error "%s" (plist-get err :result)))
      (kill-new (mapconcat (lambda (r) (string-join (plist-get r :result) "\n")) ret "\n\n"))
      (message "Result already in the kill ring."))))


;;; [Render] insert render

(defclass gt-insert-render (gt-render)
  ((type
    :initarg :type
    :initform 'after
    :type (or (member after replace) boolean)
    :documentation "How to insert the result.")
   (rfmt
    :initarg :rfmt
    :initform nil
    :documentation "Used to format the result string for insertion.
See `gt-insert-render-format' for details.")
   (sface
    :initarg :sface
    :initform nil
    :documentation "The propertize face of the source text after translation.
If this is nil then do nothing, if this is a face or a function return a face,
just propertize the source text with the face.")
   (rface
    :initarg :rface
    :initform nil
    :documentation "Result face.")))

(defcustom gt-insert-render-type 'after
  "Where to insert the result in Insert-Render.

If this is `replace', insert the result by taking place the source text,
otherwise, insert after the source text.

The value can be overrided by `type' slot of render."
  :type '(choice (const :tag "Repace" replace)
                 (other :tag "Insert after" after))
  :group 'go-translate)

(defcustom gt-insert-render-output-hook nil
  "Hook run after output finished in Insert-Render.
With current translator as the only argument."
  :type 'hook
  :group 'go-translate)

(defun gt-insert-render-format (render src res)
  "Format RES for insert RENDER.

SRC is the source text, RES is list extracted from translate task.

Join them to a string, format or pretty it, at last return it as the result that
used to insert.

If slot `rfmt' is a string contains `%s', format every part of results with
function `format' and join them.

   (gt-insert-render :rfmt \" <%s>\" :rface `font-lock-warning-face)

If `rfmt' is a function with solo argument, apply the function on every part of
results and join them. If with two arguments, pass source text as the first
argument. If four arguments, then chain the formatting task to the function.

   (gt-insert-render :rfmt (lambda (w) (format \" [%s]\"))
                     :rface `font-lock-warning-face)

   (gt-insert-render :rfmt (lambda (s w)
                             (if (length< s 3)
                               (format \"\\n- %s\" w)
                              (propertize w `face `font-lock-warning-face))))

Otherwise, join the results use the default logic."
  (with-slots (type rfmt rface) render
    (cond
     ((stringp rfmt)
      (mapconcat (lambda (r) (gt-face-lazy (format rfmt r) (gt-ensure-plain rface r))) res "\n"))
     ((functionp rfmt)
      (let ((n (cdr (func-arity rfmt))))
        (if (<= n 2)
            (mapconcat (lambda (r)
                         (let ((ret (or (if (= n 2) (funcall rfmt src r) (funcall rfmt r)) r)))
                           (gt-face-lazy ret (gt-ensure-plain rface r))))
                       res "\n")
          (funcall rfmt render src res))))
     (t (let* ((before (when (eq type 'after)
                         (if (string-blank-p src) ""
                           (if (gt-word-p nil src) " "
                             (if (or (looking-at "\n\n")
                                     (save-excursion (skip-chars-forward " \n\t") (eobp)))
                                 "\n\n"
                               "\n")))))
               (facefn (lambda (r) (gt-face-lazy r (gt-ensure-plain rface r))))
               (separator (if (= (length before) 0) "\n\n" before)))
          (concat before (mapconcat facefn res separator)))))))

(cl-defmethod gt-init ((render gt-insert-render) translator)
  (with-slots (text bounds tasks) translator
    (unless (buffer-live-p (car bounds))
      (error "Source buffer is unavailable, abort"))
    (when (with-current-buffer (car bounds) buffer-read-only)
      (error "Source buffer is readonly, can not insert"))
    ;; only simple translate (single part, single target) allowing streaming output
    ;; take bounds as output markers for it
    (when (and (not (cdr text))
               (not (cdr tasks))
               (gt-stream-p (oref (car tasks) engine)))
      (with-slots (markers engine) (car tasks)
        (let ((type (oref render type))
              (beg (if-let (p (caadr bounds))
                       (save-excursion
                         (goto-char p)
                         (skip-chars-forward "\n" (cdadr bounds))
                         (point))
                     (point)))
              (end (if-let (p (cdadr bounds))
                       (save-excursion
                         (goto-char p)
                         (skip-chars-backward " \t\n" (caadr bounds))
                         (point))
                     (point))))
          (setf markers (cons (set-marker (make-marker) (if (eq type 'after) end beg))
                              (set-marker (make-marker) end))))))))

(cl-defmethod gt-output ((render gt-insert-render) translator)
  (with-slots (bounds state) translator
    (when (= 3 state)
      (let ((ret (gt-extract render translator)))
        (when-let (err (cl-find-if (lambda (tr) (<= (plist-get tr :state) 1)) ret))
          (user-error "Error in translation, %s" (plist-get err :result)))
        (with-current-buffer (car bounds)
          (with-slots (rfmt sface) render
            (cl-loop with start = (point-marker)
                     with bds = (mapcar (lambda (bd)
                                          (cons (save-excursion
                                                  (goto-char (car bd))
                                                  (skip-chars-forward "\n")
                                                  (point-marker))
                                                (save-excursion
                                                  (goto-char (cdr bd))
                                                  (skip-chars-backward " \t\n")
                                                  (point-marker))))
                                        (cdr bounds))
                     with type = (let ((type (or (oref render type) gt-insert-render-type))
                                       (types '(after replace)))
                                   (if (member type types) type
                                     (intern (completing-read "Insert text as: " types nil t))))
                     with hash = (when (and (eq type 'replace) (not (buffer-modified-p)))
                                   (buffer-hash))
                     with len = (length (plist-get (car ret) :result))
                     for i from 0 below len
                     for (beg . end) = (if bds (nth i bds)
                                         (if (use-region-p)
                                             (car (region-bounds))
                                           (cons (point) (point))))
                     for src = (if bds (buffer-substring beg end) "")
                     for res = (mapcar (lambda (tr) (nth i (plist-get tr :result))) ret)
                     do (goto-char end)
                     for fres = (gt-insert-render-format render src res)
                     do (let (p)
                          (if (eq type 'replace)
                              (delete-region beg end)
                            (when-let (face (and (eq type 'after) (gt-ensure-plain sface src)))
                              (delete-region beg end)
                              (insert (propertize src 'face face))))
                          (setq p (point))
                          (insert (propertize fres 'type 'gt-insert-result))
                          (save-excursion
                            (goto-char p)
                            (skip-chars-forward " \t\n")
                            (push-mark)))
                     finally (progn
                               (if (> len 1) (goto-char start))
                               (when (and hash (equal hash (buffer-hash)))
                                 (set-buffer-modified-p nil))))
            (deactivate-mark)
            (run-hook-with-args 'gt-insert-render-output-hook translator)
            (message "ok.")))))))

(cl-defmethod gt-next-for-stream ((render gt-insert-render) task)
  (with-slots (markers res) task
    (let* ((beg (car markers)) (end (cdr markers))
           (bounds (oref (oref task translator) bounds))
           (insert-text (with-current-buffer (car bounds)
                          (save-excursion
                            (goto-char end)
                            (gt-insert-render-format
                             render
                             (if (and (eq (oref render type) 'after) (caadr bounds))
                                 (buffer-substring (caadr bounds) (cdadr bounds))
                               "")
                             (list (string-join (ensure-list res) "\n")))))))
      (gt-insert-text-at-marker insert-text beg end))))

(cl-defmethod gt-desc ((render gt-insert-render))
  (format "gt-insert-render/%s" (oref render type)))


;;; [Render] Render with Overlay

(defclass gt-overlay-render (gt-render)
  ((type
    :initarg :type
    :initform 'after
    :type (or (member after replace help-echo before) boolean)
    :documentation "How to display the result.")
   (rfmt
    :initarg :rfmt
    :initform nil
    :documentation
    "Used to format the translation result to fit the overlay display.
See `gt-overlay-render-format' for details.")
   (sface
    :initarg :sface
    :initform 'gt-overlay-source-face
    :documentation "The propertize face of the source text after translation.
If this is nil then do nothing, if this is a face or a function return a face,
just propertize the source text with the face.")
   (rface
    :initarg :rface
    :initform 'gt-overlay-result-face
    :documentation "Result face.")
   (rdisp
    :initarg :rdisp
    :initform nil
    :documentation "Same as rface but used in `display' property.")
   (pface
    :initarg :pface
    :initform 'gt-overlay-prefix-face
    :documentation "Prefix face.")
   (pdisp
    :initarg :pdisp
    :initform nil
    :documentation "Same as pface but used in `display' property.")))

(defcustom gt-overlay-render-type 'after
  "How to display result in Overlay-Render.

If this is `help-echo', display with help echo, if this is `replace', display
by covering the source text, otherwise, display before or after the source text.

The value can be overrided by `type' slot of render."
  :type '(choice (const :tag "Repace" replace)
                 (const :tag "Help Echo" help-echo)
                 (const :tag "After" after)
                 (other :tag "Before" before))
  :group 'go-translate)

(defvar gt-overlay-render-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") #'gt-delete-render-overlays)
    (define-key map (kbd "<mouse-3>") #'gt-overlay-render-save-to-kill-ring)
    map)
  "Keymap used in overlay render.")

(defun gt-overlay-render-get-overlays (beg &optional end)
  "Return overlays made by Overlay-Render in the region from BEG to END.
If END is nil, return the overlays at BEG."
  (cl-remove-if-not (lambda (ov) (overlay-get ov 'gt))
                    (if end
                        (overlays-in beg end)
                      (overlays-at beg))))

(defun gt-delete-render-overlays (beg &optional end)
  "Delete overlays made by Overlay-Render in the region from BEG to END.
If called interactively, delete overlays around point or in region. With
`current-prefix-arg' non nil, delete all overlays in the buffer."
  (interactive (cond (current-prefix-arg (list (point-min) (point-max)))
                     ((use-region-p) (list (region-beginning) (region-end)))
                     (t (list
                         (if (and (> (point) 1) ; for overlay before point
                                  (not (gt-overlay-render-get-overlays (point)))
                                  (gt-overlay-render-get-overlays (1- (point))))
                             (1- (point))
                           (point))
                         nil))))
  (mapc #'delete-overlay (gt-overlay-render-get-overlays beg end)))

(defun gt-overlay-render-save-to-kill-ring ()
  "Copy translate content at point to kill ring for Overlay-Render."
  (interactive)
  (if-let* ((ov (car (gt-overlay-render-get-overlays (point))))
            (rs (overlay-get ov 'gt)))
      (progn (kill-new (string-trim rs))
             (message "Result already in the kill ring."))
    (user-error "No translate overlay found at point")))

(defun gt-overlay-render-format (render src res prefix)
  "Format RES for overlay RENDER.

SRC is the source text, RES and PREFIX are list extracted from translate task.

Join them to a string, format or pretty it, at last return it as the result that
used to display.

If slot `rfmt' is a string contains `%s', format every part of results with
function `format' and join them.

   (gt-overlay-render :rfmt \" <%s>\" :rface `font-lock-warning-face)

If `rfmt' is a function with solo argument, apply the function on every part of
results and join them. If with two arguments, pass source text as the first
argument. If four arguments, then chain the formatting task to the function.

   (gt-overlay-render :rfmt (lambda (w) (format \" [%s]\"))
                      :rface `font-lock-warning-face)

   (gt-overlay-render :rfmt (lambda (s w)
                              (if (length< s 3)
                                (format \"\\n- %s\")
                               (propertize w `face `font-lock-warning-face))))

Otherwise, join the results use the default logic."
  (with-slots (type rfmt rface rdisp pface pdisp) render
    (cond
     ((stringp rfmt)
      (mapconcat (lambda (r) (gt-face-lazy (format rfmt r)
                                           (gt-ensure-plain rface r)
                                           (gt-ensure-plain rdisp r)))
                 res "\n"))
     ((functionp rfmt)
      (let ((n (cdr (func-arity rfmt))))
        (if (<= n 2)
            (mapconcat (lambda (r)
                         (let ((ret (or (if (= n 2) (funcall rfmt src r) (funcall rfmt r)) r)))
                           (gt-face-lazy ret (gt-ensure-plain rface r) (gt-ensure-plain rdisp r))))
                       res "\n")
          (funcall rfmt render src res prefix))))
     (t (cl-loop with before = (when (eq type 'after)
                                 (if (gt-word-p nil src) " "
                                   (if (or (looking-at "\n\n")
                                           (save-excursion (skip-chars-forward " \n\t") (eobp)))
                                       "\n\n"
                                     "\n")))
                 with after = (when (eq type 'before) (if (gt-word-p nil src) " " "\n"))
                 for r in res for p in prefix
                 for pr = (gt-face-lazy r (gt-ensure-plain rface r) (gt-ensure-plain rdisp r))
                 for pp = (if p (concat " " (gt-face-lazy p (gt-ensure-plain pface p) (gt-ensure-plain pdisp p))))
                 collect (concat pr pp) into ret
                 finally (return (concat before (string-join ret (or before "\n\n")) after)))))))

(cl-defmethod gt-init ((render gt-overlay-render) translator)
  (with-slots (bounds state) translator
    (unless (cdr bounds)
      (error "%s only works for buffer bounds, abort" (gt-desc render)))
    (unless (buffer-live-p (car bounds))
      (error "Source buffer is unavailable, abort"))))

(cl-defmethod gt-output ((render gt-overlay-render) translator)
  (with-slots (bounds state) translator
    (when (= 3 state)
      (let ((ret (gt-extract render translator)))
        (when-let (err (cl-find-if (lambda (tr) (<= (plist-get tr :state) 1)) ret))
          (user-error "Error in translation, %s" (plist-get err :result)))
        (with-current-buffer (car bounds)
          (cl-loop with start = (point-marker)
                   with type = (let ((type (or (oref render type) gt-overlay-render-type))
                                     (types '(help-echo replace after before)))
                                 (if (memq type types) type
                                   (intern (completing-read "Display with overlay as: " types nil t))))
                   with hooks = `((lambda (o &rest _) (delete-overlay o)))
                   for (beg . end) in (cdr bounds) for i from 0
                   do (save-excursion
                        (goto-char beg)
                        (skip-chars-forward "\n")
                        (setq beg (point))
                        (goto-char end)
                        (skip-chars-backward " \t\n")
                        (setq end (point)))
                   do (gt-delete-render-overlays beg end)
                   for src = (buffer-substring beg end)
                   do (save-excursion
                        (goto-char end)
                        (let* ((ov (make-overlay beg (point) nil t t))
                               (fres (gt-overlay-render-format
                                      render src
                                      (mapcar (lambda (tr) (nth i (plist-get tr :result))) ret)
                                      (mapcar (lambda (tr) (plist-get tr :prefix)) ret)))
                               (sface (oref render sface))
                               (sface (unless (eq type 'replace) (gt-ensure-plain sface src))))
                          (pcase type
                            ('help-echo (overlay-put ov 'help-echo fres))
                            ('after (overlay-put ov 'after-string fres))
                            ('before (overlay-put ov 'before-string fres)))
                          (overlay-put ov 'gt fres)
                          (overlay-put ov 'evaporate t)
                          (overlay-put ov 'pointer 'arrow)
                          (overlay-put ov 'insert-in-front-hooks hooks)
                          (overlay-put ov 'insert-behind-hooks hooks)
                          (overlay-put ov 'modification-hooks hooks)
                          (overlay-put ov 'keymap gt-overlay-render-map)
                          (if (eq type 'replace)
                              (progn (overlay-put ov 'display fres)
                                     (overlay-put ov 'help-echo src))
                            (if sface (overlay-put ov 'face sface)))))
                   finally (if (cddr bounds) (goto-char start)))
          (deactivate-mark)
          (message "ok."))))))

(cl-defmethod gt-desc ((render gt-overlay-render))
  (format "gt-overlay-render/%s" (oref render type)))


;;; [Render] Alert Render

(defclass gt-alert-render (gt-render) ()
  :documentation "Output results as system notification.
It depends on the `alert' package.")

(defvar gt-alert-render-args '(:timeout 10))

(declare-function alert "ext:alert.el" t t)

(cl-defmethod gt-init ((_ gt-alert-render) _)
  (unless (require 'alert nil t)
    (user-error "To use `gt-alert-render', you should install and load package `alert' first")))

(cl-defmethod gt-output ((render gt-alert-render) translator)
  (when (= (oref translator state) 3)
    (let ((ret (gt-extract render translator)) lst)
      ;; format
      (dolist (tr ret)
        (let ((prefix (if (cdr ret) (plist-get tr :prefix)))
              (result (string-join (ensure-list (plist-get tr :result)) "\n")))
          (push (concat prefix result) lst)))
      ;; output
      (message "")
      (apply #'alert (string-join (nreverse lst) "\n") :title "*Go-Translate*" gt-alert-render-args))))


;;; [Taker] Prompt with new buffer

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
                                                 (if-let (src (car target)) (concat "[" (prop src) "] → "))
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


;;; [Taker] Pick only fresh words

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

;; (gt-taker :pick 'fresh-word)

(cl-defmethod gt-pick ((_ (eql 'fresh-word)) translator)
  (with-slots (text bounds taker) translator
    (setq gt-fresh-words-last nil)
    (let* ((car (if (cdr bounds) (cl-subseq bounds 0 2) (car text)))
           (pred (lambda (word)
                   (and (if-let (p (oref taker pick-pred)) (funcall p word) t)
                        (> (string-bytes word) 2)
                        (not (string-match-p "^[0-9]+$" word))
                        (gt-word-fresh-p word)
                        (push word gt-fresh-words-last)))))
      (gt-pick-items-by-thing car 'word pred))))


;;; [Taker] pdf-view-mode

(declare-function pdf-view-active-region-p "ext:pdf-view.el" t t)
(declare-function pdf-view-active-region-text "ext:pdf-view.el" t t)

(cl-defmethod gt-text-at-point (_thing (_ (eql 'pdf-view-mode)))
  (if (pdf-view-active-region-p)
      (pdf-view-active-region-text)
    (user-error "You should make a selection before translate")))

(provide 'gt-extension)

;;; gt-extension.el ends here
