;;; gt-chat-openai.el --- Client of ChatGPT -*- lexical-binding: t -*-

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

;; Full client of ChatGPT.
;;
;;   https://platform.openai.com/docs/guides/function-calling
;;
;;
;;  (setq gt-default-translator (gt-chat-openai :taker (gt-taker :prompt 'buffer) :render (gt-buffer-render)))
;;

;;; Code:

(require 'gt-extension)
(require 'gt-engine-chatgpt)
(require 'gt-chat-session)

(declare-function gt-translator-info "ext:go-translate")
(declare-function gt-set-render "ext:go-translate")

(defvar gt-chat-current-session nil)

(defvar gt-chat-system-prompts
  '("You are a helpful assistant. You can help me by answering my questions. You can also ask me questions."
    "You are a writer"
    "You are a coder"
    "You are a singer"))

(defvar-local gt-chat-buffer-translator nil)

(defclass gt-chat-openai (gt-translator) ())

(defclass gt-chat-openai-engine-parser (gt-parser) ())

(defclass gt-chat-openai-engine (gt-chatgpt-engine)
  ((tag         :initform 'ChatGPT-1)
   (host        :initarg :host :initform nil)
   (model       :initarg :model :initform nil)
   (temperature :initarg :temperature :initform nil)
   (cache       :initform nil)
   (delimiter   :initform nil)
   (parse       :initform (gt-chat-openai-engine-parser))
   (key         :initarg :key :initform 'apikey
                :documentation "The apikey of ChatGPT.
Can also put into .authinfo file as:
  machine api.openai.com login apikey password ***")))

(cl-defmethod gt-reset :after ((translator gt-chat-openai) &rest _)
  (with-slots (_engines) translator
    (unless _engines (setf _engines (gt-chat-openai-engine)))
    (setf _engines (ensure-list _engines))
    (unless (and (not (cdr _engines)) (cl-typep (car _engines) 'gt-chat-openai-engine))
      (user-error "%s should use only one gt-chat-openai-engine as engine" (eieio-object-class translator)))))

(cl-defmethod gt-translate ((engine gt-chat-openai-engine) task next)
  (with-slots (text res translator) task
    (setf text (string-join text "\n"))
    (if-let* ((messages (gt-chat-parse-messages text))
              (items (plist-get messages :items))
              (last (car (last items)))
              (role (car last))
              (content (cdr last)))
        (cond
         ((eq role 'assistant)
          (oset translator text (list (gt-chat-stringify-session (butlast items))))
          (setf res (substring-no-properties content))
          (funcall next task))
         ((eq role 'user)
          (gt-chat-openai-chat engine
                               (gt-chat-generate-reqeust-messages text)
                               (lambda (raw)
                                 (let* ((json (json-read-from-string raw))
                                        (str (alist-get 'content (alist-get 'message (let-alist json (aref .choices 0)))))
                                        (gt-buffer-render-output-hook (lambda () (toggle-truncate-lines -1))))
                                   (gt-chat-sync-buffer-session gt-chat-current-session `((assistant . ,str)))
                                   (setf res (substring-no-properties str))
                                   (funcall next task)))
                               (lambda (err) (gt-fail task err))))
         (t (user-error "Wrong request content"))))))

(cl-defmethod gt-parse ((_ gt-chat-openai-engine-parser) _task))

(cl-defmethod gt-chat-openai-chat ((engine gt-chat-openai-engine) messages done fail)
  (gt-ensure-key engine)
  (with-slots (host key model temperature) engine
    (gt-request :url (concat (or host gt-chatgpt-host) "/v1/chat/completions")
                :headers `(("Content-Type" . "application/json")
                           ("Authorization" . ,(encode-coding-string (concat "Bearer " key) 'utf-8)))
                :data (encode-coding-string
                       (json-encode
                        `((model . ,(or model gt-chatgpt-model))
                          (temperature . ,(or temperature gt-chatgpt-temperature))
                          (messages . ,(if (vectorp messages) messages (vconcat messages)))))
                       'utf-8)
                :done done
                :fail fail)))



(defun gt-chat-ensure-directory ()
  (unless (file-directory-p gt-chat-session-location)
    (make-directory gt-chat-session-location t)))

(defun gt-chat-sessions ()
  (gt-chat-ensure-directory)
  (cl-loop for name in (mapcar #'car
                               (sort
                                (directory-files-and-attributes
                                 gt-chat-session-location nil "^[^.]" t)
                                (lambda (x y) (time-less-p (nth 6 y) (nth 6 x)))))
           for path = (expand-file-name name gt-chat-session-location)
           collect (cons name path)))

;;(gt-chat-new-session "woooo")
;;(gt-chat-del-session "woooo")
;;(gt-chat-get-session "woooo")
;;(insert (gt-chat-stringify gt-chat-current-session))
;;(oref gt-chat-current-session index)
;; (insert (gt-chat-stringify (car (oref gt-chat-current-session items))))
;;(insert (gt-chat-stringify gt-chat-current-session))
;; (gt-chat-items gt-chat-current-session)
;; (oset gt-chat-current-session index 1)

(defvar gt-chat-session-type 'gt-chat-file-session)

(defun gt-chat-get-session (name)
  (let ((path (expand-file-name name gt-chat-session-location)))
    (unless (file-exists-p path)
      (user-error "Current session `%s' not existed" name))
    (setq gt-chat-current-session
          (gt-chat-session-read gt-chat-session-type path))))

(defun gt-chat-new-session (name &optional system-prompt)
  (let ((path (expand-file-name name gt-chat-session-location)))
    (when (file-exists-p path)
      (user-error "Current session `%s' already existed" name))
    (gt-chat-ensure-directory)
    (setq gt-chat-current-session
          (make-instance gt-chat-session-type
                         :llm 'openai
                         :name name
                         :items (list
                                 (gt-chat-session-item
                                  :type 'system
                                  :content (or system-prompt (car gt-chat-system-prompts))))))
    (gt-chat-session-save gt-chat-current-session)))

(defun gt-chat-del-session (name)
  (let ((path (expand-file-name name gt-chat-session-location)))
    (unless (file-exists-p path)
      (user-error "Current session `%s' not existed" name))
    (when (equal name (oref gt-chat-current-session name))
      (setq gt-chat-current-session nil))
    (delete-file path)))

(defvar-local gt-chat-buffer-session nil)

(defun gt-chat-sync-buffer-session ()
  (let ((news (gt-chat-parse-buffer)))
    (with-slots (items index) gt-chat-buffer-session
      (setf items (append news (items-left))) ; TODO
      (setf index (oref (last news) id)))
    (gt-chat-session-save gt-chat-buffer-session)))

(defun gt-chat-load-session (session &optional input)
  (with-slots (items) session
    (unless (eq 'user (oref (car items) role))
      (push (gt-chat-session-item :role 'user :content input) items))
    (insert (gt-chat-stringify session))
    (gt-chat--customize-buffer)))

(defun gt-chat-delete-current-message ()
  (interactive)
  (let* ((inhibit-read-only t)
         (current (gt-chat-bound-of-current-message))
         (last (unless (= (point-min) (car current))
                 (save-excursion
                   (goto-char (car current))
                   (backward-char)
                   (gt-chat-bound-of-current-message)))))
    (unless (and (string-prefix-p (buffer-substring (car current) (cadr current)) "<U> ")
                 (not (save-excursion (forward-char) (re-search-forward "^<[AUSTR]> " nil t)))
                 (not (string-prefix-p (buffer-substring (car last) (cadr last)) "<U> ")))
      (kill-region (car current) (caddr current)))))

(defvar gt-chat-buffer-message-logo-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'outline-cycle)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "C-k") #'gt-chat-delete-current-message)
    map))

(defun gt-chat--customize-buffer ()
  (setq header-line-format
        `((:propertize ,(format " Chat with %s, Submit with C-c C-c, Cancel with C-c C-k"
                                (if-let (k (car (where-is-internal #'gt-chat-send-current)))
                                    (key-description k) 'gt-chat-send-current))
                       face gt-chat-buffer-head-line-face)))
  (setq mode-line-format
        (cl-flet ((props (fn)
                    `( 'face 'font-lock-keyword-face
                       'local-map '(keymap (mode-line keymap (mouse-1 . ,fn)))
                       'mouse-face 'font-lock-warning-face )))
          `(" Session: "
            (:eval (propertize
                    (let ((session gt-chat-buffer-session))
                      (with-temp-buffer
                        (save-excursion (insert (oref session name)))
                        (concat (thing-at-point 'sentence))))
                    ,@(props #'gt-chat--switch-session)))
            "  Model: "
            (:eval (with-slots (model temperature) (car (oref gt-chat-buffer-translator engines))
                     (propertize (format "%s/%s"
                                         (or model gt-chatgpt-model)
                                         (or temperature gt-chatgpt-temperature))
                                 ,@(props #'gt-chat--config-engine))))
            "  System prompt: "
            ,(apply #'propertize
                    (let ((p (gt-chat--current-system-prompt)))
                      (message ">>> 111. %s" (time-to-seconds))
                      (with-temp-buffer
                        (save-excursion (insert p))
                        (concat (thing-at-point 'sentence) "..")))
                    (props #'gt-chat--switch-system-prompt))
            "  Render: "
            (:eval (propertize
                    (nth 2 (gt-translator-info gt-chat-buffer-translator))
                    ,@(props #'gt-chat--set-current-render))))))
  (setq-local outline-regexp "^<[US]> ")
  (setq-local line-move-visual t)
  (outline-minor-mode 1))

(defun gt-chat--refresh-point ()
  (goto-char (point-max))
  (when (re-search-backward "^<[AU]> " nil t) (search-forward " "))
  (recenter-top-bottom 2))

(defun gt-chat-generate-reqeust-messages (string)
  (let* ((items (gt-chat-parse-messages string))
         (lst (cl-subseq (plist-get items :items) (plist-get items :index)))
         messages tools)
    (cl-loop for (role . content) in lst
             when (string-match "^ *\\[\\(.+?\\)\\] +" content)
             do (setq tools (append tools (mapcar #'string-trim (string-split (match-string 1 content) ",")))
                      content (substring content (match-end 0)))
             do (push `((role . ,role) (content . ,content)) messages))
    (list (nreverse messages) tools)))

(defun gt-chat-font-lock-string-with-markdown (str)
  (with-temp-buffer
    (let ((ori markdown-fontify-code-blocks-natively))
      (unwind-protect
          (progn
            (setq markdown-fontify-code-blocks-natively t)
            (markdown-mode)
            (insert str)
            (font-lock-ensure)
            (buffer-string))
        (setq markdown-fontify-code-blocks-natively ori)))))



(defvar gt-chat-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'gt-chat--switch-session)
    (define-key map (kbd "C-c C-p") #'gt-chat--switch-system-prompt)
    (define-key map (kbd "C-c C-r") #'gt-chat--set-current-render)
    (define-key map (kbd "C-c C-u") #'gt-chat--previous-message)
    (define-key map (kbd "M-m") #'gt-chat--back-to-indentation)
    (define-key map (kbd "C-c TAB") #'outline-cycle-buffer)
    (define-key map (kbd "C-c <return>") #'gt-chat-send-current)
    map))

(defvar gt-chat-buffer-name "*gt-chat-taker*")

(defvar gt-chat-buffer-window-config gt-buffer-prompt-window-config)

(defun gt-chat--previous-message ()
  (interactive)
  (if (= 1 (line-number-at-pos))
      (if (= (plist-get gt-chat-buffer-session :index) 0)
          (message "Already at the most top position")
        (let ((inhibit-read-only t) (offset (- (point-max) (point))))
          (message "Loading...")
          (gt-chat-sync-buffer-session gt-chat-buffer-session (buffer-string) 0)
          (erase-buffer)
          (gt-chat-load-session gt-chat-buffer-session)
          (goto-char (- (point-max) offset))))
    (when (looking-back "^<[AUSTR]> ")
      (beginning-of-line)
      (backward-char))
    (if (re-search-backward "^<[AUSTR]> " nil t)
        (goto-char (match-end 0))
      (beginning-of-line))))

(defun gt-chat--back-to-indentation ()
  (interactive)
  (end-of-line)
  (if (re-search-backward "^<[AUSTR]> " (line-beginning-position) t)
      (goto-char (match-end 0))
    (back-to-indentation)))

(defun gt-chat--set-current-render ()
  (interactive)
  (gt-set-render gt-chat-buffer-translator)
  (force-mode-line-update))

(defvar gt-chat-engine-models
  '("gpt-3.5-turbo"
    "gpt-4o"))

(defun gt-chat--config-engine ()
  (interactive)
  (let ((engine (car (oref gt-chat-buffer-translator engines))))
    (unless (typep engine 'gt-chatgpt-engine)
      (user-error "Invalid engine found"))
    (with-slots (model temperature) engine
      (let ((nm (completing-read "Model to use: " gt-chat-engine-models nil t
                                 nil nil (or model gt-chatgpt-model)))
            (nt (read-number "Temperature as: " (or temperature gt-chatgpt-temperature))))
        (setf model nm temperature nt)
        (force-mode-line-update)))))

(defun gt-chat--current-system-prompt ()
  (save-excursion
    (goto-char (point-max))
    (when-let (bd (and (re-search-backward "^<[0-9.]+:system> " nil t)
                       (gt-chat-bound-of-current-message)))
      (string-trim (buffer-substring-no-properties (cadr bd) (caddr bd))))))

(defvar gt-chat-system-prompt-history nil)

(defun gt-chat--switch-system-prompt ()
  (interactive)
  (let ((inhibit-read-only t)
        (prompt (completing-read "System prompt to: "
                                 (gt-make-completion-table
                                  (cl-remove-duplicates
                                   (delq nil
                                         (append (list (gt-chat--current-system-prompt))
                                                 gt-chat-system-prompt-history
                                                 gt-chat-system-prompts))
                                   :from-end t
                                   :test #'string-equal))
                                 nil nil nil 'gt-chat-system-prompt-history)))
    (when (string-blank-p prompt)
      (user-error "System prompt is required"))
    (end-of-line)
    (let ((current (gt-chat-bound-of-current-message)))
      (if (string-prefix-p "<S>" (buffer-substring (car current) (cadr current)))
          (goto-char (caddr current))
        (goto-char (car current))))
    (insert (format "\n<S> %s\n\n" prompt))
    (let ((offset (save-excursion
                    (skip-chars-backward " \t\n")
                    (- (point-max) (point)))))
      (gt-chat-sync-buffer-session gt-chat-buffer-session (buffer-string)
                            (if (re-search-forward "^<S> " nil t) 0))
      (erase-buffer)
      (gt-chat-load-session gt-chat-buffer-session)
      (goto-char (- (point-max) offset))
      (unless (re-search-forward "^<[ASTR]>" nil t)
        (goto-char (point-max))))))

(defun gt-chat--switch-session ()
  (interactive)
  (gt-chat-sync-buffer-session gt-chat-buffer-session (buffer-string))
  (let ((inhibit-read-only t)
        (name (completing-read "Session to: "
                               (cl-remove (oref gt-chat-buffer-session name)
                                          (gt-chat-sessions)
                                          :key #'car :test #'equal)
                               nil nil nil 'gt-chat-session-history)))
    (setq gt-chat-buffer-session
          (if (string-blank-p name)
              (user-error "Session name invalid")
            (if (assoc name (gt-chat-sessions))
                (gt-chat-get-session name)
              (gt-chat-new-session name))))
    (erase-buffer)
    (gt-chat-load-session gt-chat-buffer-session)
    (gt-chat--refresh-point)))

(defun gt-chat-delete-current-session (session)
  (interactive (list (gt-chat-get-session
                      (completing-read "Session to delete: "
                                       (gt-chat-sessions)
                                       nil t nil nil
                                       (if gt-chat-buffer-session (plist-get gt-chat-buffer-session :name))))))
  (gt-plist-let session
    (when (y-or-n-p (format "Delete session `%s' and remove all %s items?" .name (length .items)))
      (gt-chat-del-session .path))
    (when (equal .name (plist-get gt-chat-buffer-session :name))
      (throw 'gt-chat-buffer-prompt nil))))

(defvar-local gt-chat-tracking-marker nil)

(defvar-local gt-chat-last-position nil)

(defvar-local gt-chat-func-name nil)

(defvar-local gt-chat-func-args nil)

(defun gt-chat-openai-filter (buf)
  (message "Typing...")
  (unless gt-chat-last-position (setq gt-chat-last-position (point-min)))
  (goto-char gt-chat-last-position)
  (condition-case err
      (while (re-search-forward "^data: +\\({.+}\\)" nil t)
        (setq gt-chat-last-position (point))
        (let* ((json (json-read-from-string (decode-coding-string (match-string 1) 'utf-8)))
               (choice (aref (alist-get 'choices json) 0))
               (delta (alist-get 'delta choice))
               (content (alist-get 'content delta))
               (func-name (ignore-errors (alist-get 'name (alist-get 'function (aref (alist-get 'tool_calls delta) 0)))))
               (func-args (ignore-errors (alist-get 'arguments (alist-get 'function (aref (alist-get 'tool_calls delta) 0)))))
               (finish-reason (alist-get 'finish_reason choice)))
          (save-match-data
            (with-current-buffer buf
              (unless gt-chat-tracking-marker
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert (format "\n\n<%s> " (if (or func-name func-args) "T" "A")))
                  (save-excursion (gt-chat--property-messages))
                  (setq gt-chat-tracking-marker (point-marker)))))
            (cond
             ((equal finish-reason "tool_calls")
              (let ((r `((name . ,gt-chat-func-name)
                         (arguments . ,(json-parse-string gt-chat-func-args :object-type 'alist)))))
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert (format "%s" (json-encode r)))
                    (insert (format "\n\n<R> %s" r))
                    (let* ((obj (apply #'make-instance
                                       (intern-soft (alist-get 'name r))
                                       (cl-loop for (k . v) in (alist-get 'arguments r)
                                                append (list (intern (format ":%s" k)) v))))
                           (rr (gt-eval obj))
                           (fc (format "%s: %s" (alist-get 'name r) rr)))
                      (insert fc "\n"))
                    (gt-chat-sync-buffer-session gt-chat-buffer-session (buffer-string) t)
                    (save-excursion (gt-chat--property-messages))
                    (gt-chat-send-current)))
                (message "Done")))
             (finish-reason
              (with-current-buffer buf
                (let ((inhibit-read-only t)
                      (p (unless (eobp) (point))))
                  (goto-char (point-max))
                  (insert (format "\n\n<U> "))
                  (if p (goto-char p))
                  (save-excursion
                    (gt-chat-sync-buffer-session gt-chat-buffer-session (buffer-string) t)
                    (gt-chat--property-messages)))
                (message "Done")))
             ((or func-name func-args)
              (if func-name (setq gt-chat-func-name func-name))
              (if func-args (setq gt-chat-func-args (concat gt-chat-func-args func-args))))
             (t (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (when content
                      (if (eobp)
                          (insert content)
                        (save-excursion
                          (goto-char (point-max))
                          (insert content)))
                      (save-excursion (gt-chat--property-messages t))))))))))
    (error (if (string-prefix-p "json" (format "%s" (car err)))
               (setq gt-chat-last-position (line-beginning-position))
             (signal (car err) (cdr err))))))

(defun gt-chat-send-current ()
  (interactive)
  (message "Sending...")
  (with-slots (engines bounds) gt-chat-buffer-translator
    (setq gt-chat-current-session gt-chat-buffer-session)
    (gt-chat-sync-buffer-session gt-chat-buffer-session (buffer-string))
    (setq gt-chat-tracking-marker nil)
    (let ((engine (car (ensure-list engines))))
      (gt-ensure-key engine)
      (with-slots (host key model temperature) engine
        (pcase-let* ((`(,messages ,functions) (gt-chat-generate-reqeust-messages (buffer-string)))
                     (req-headers `(("Content-Type" . "application/json")
                                    ("Authorization" . ,(encode-coding-string (concat "Bearer " key) 'utf-8))))
                     (req-data    `((model . ,(or model gt-chatgpt-model))
                                    (temperature . ,(or temperature gt-chatgpt-temperature))
                                    (stream . t)
                                    (messages . ,(if (vectorp messages) messages (vconcat messages))))))
          (cl-loop for f in functions
                   for fd = (ignore-errors (gt-serialize (intern-soft f)))
                   if fd collect `((type . function) (function . ,fd)) into fds
                   finally (if fds (setq req-data `(,@req-data (tools . [,@fds])))))
          (let ((gt-log-buffer-name "*gt-stream-log*"))
            (gt-log 'openai-request 2 (pp-to-string req-data)))
          (gt-request :url (concat (or host gt-chatgpt-host) "/v1/chat/completions")
                      :headers req-headers
                      :data (encode-coding-string (json-encode req-data) 'utf-8)
                      :filter (lambda () (gt-chat-openai-filter (car bounds)))
                      :done (lambda (raw)
                              (let ((gt-log-buffer-name "*gt-stream-log*"))
                                (gt-log 'openai-response 2 raw)))
                      :fail (lambda (err) (signal 'user-error err))))))))

(cl-defmethod gt-start :around ((translator gt-chat-openai))
  "Make chat buffer can be toggle show."
  (with-slots (taker) translator
    (when (ignore-errors (eq (oref (gt-ensure-plain taker) prompt) 'buffer))
      (if (and (cl-plusp (recursion-depth))
               (buffer-live-p (get-buffer gt-chat-buffer-name)))
          (if (equal (buffer-name) gt-chat-buffer-name)
              (progn (bury-buffer) (delete-window))
            (pop-to-buffer gt-chat-buffer-name gt-chat-buffer-window-config))
        (cl-call-next-method translator)))))

(cl-defmethod gt-prompt ((_taker gt-taker) (translator gt-chat-openai) (_ (eql 'buffer)))
  (with-slots (text bounds target engines _engines) translator
    (if (cdr text) (user-error "Multiple text cannot be prompted"))
    (unless engines (setf engines (ensure-list (gt-ensure-plain _engines))))
    (let* ((session (gt-chat-get-session (if gt-chat-current-session
                                             (oref gt-chat-current-session name)
                                           (caar (gt-chat-sessions)))))
           (oldtext (or (car (gt-collect-bounds-to-text (ensure-list text))) ""))
           (newtext (gt-read-from-buffer
                     :buffer gt-chat-buffer-name
                     :catch 'gt-chat-buffer-prompt
                     :window-config gt-chat-buffer-window-config
                     :keymap gt-chat-buffer-map
                     (setf bounds (list (current-buffer)))
                     (gt-chat-load-session session oldtext)
                     (setq gt-chat-buffer-session session)
                     (setq gt-chat-buffer-translator gt-current-translator)
                     (setq truncate-lines nil)
                     (gt-chat--refresh-point))))
      (when (null newtext)
        (user-error ""))
      (when (zerop (length (string-trim newtext)))
        (user-error "Text should not be null, abort"))
      (setf bounds nil)
      (setf target (list nil (plist-get session :name)))
      (gt-chat-sync-buffer-session session newtext)
      (setq gt-chat-current-session session)
      (unless (equal oldtext newtext) (setf text (ensure-list newtext))))))

(cl-defmethod gt-pick ((_ gt-taker) (translator gt-chat-openai))
  "Do not pick for this TRANSLATOR."
  (oref translator text))



(defclass gt-chat-callback (gt-single) ()
  :documentation "Function tool."
  :abstract t)

(cl-defmethod gt-serialize ((class (subclass gt-chat-callback)))
  (let* ((class (find-class class))
         (name (eieio-class-name class))
         (cdoc (eieio--class-docstring class))
         (slots (eieio-class-slots class))
         (props (cl-loop for slot in slots
                         for type = (cl--slot-descriptor-type slot)
                         for desc = (alist-get :documentation (cl--slot-descriptor-props slot))
                         collect (cons (cl--slot-descriptor-name slot)
                                       `((type . ,type) (description . ,desc)))))
         (required (cl-loop for s in slots
                            for i from 0
                            for init = (cl--slot-descriptor-initform s)
                            if (equal init (list 'quote eieio--unbound))
                            collect (cl--slot-descriptor-name s))))
    `((name . ,name)
      (description . ,cdoc)
      (parameters . ((type . object) (properties . ,props)))
      (required . [,@required]))))

(cl-defmethod gt-eval ((cb gt-chat-callback))
  (let* ((class (eieio-object-class cb))
         (form (or (eieio--class-option (eieio--class-object class) :form)
                   (user-error "FORM is require in `gt-chat-callback'")))
         (funp (memq (car-safe form) '(function lambda)))
         (fn (if funp (if (eq (car form) 'function) (cadr form) form)
               `(lambda ()
                  (let (,@(cl-loop for s in (eieio-class-slots class)
                                   for n = (cl--slot-descriptor-name s)
                                   for v = (slot-value cb n)
                                   collect (list n v)))
                    ,form))))
         (args (if funp (cl-loop with n = (cdr (func-arity fn))
                                 for i below (if (numberp n) n 2)
                                 for s in (eieio-class-slots class)
                                 collect (slot-value cb (cl--slot-descriptor-name s))))))
    (apply fn args)))


;;; Test

(defclass get-current-weather (gt-chat-callback)
  ((location :initarg :location
             :type string
             :documentation "The city and state, e.g. CS.")
   (unit :initarg :unit
         :type string
         :documentation "The unit."
         :initform "C"))
  :documentation "Get the current weather in a given location."
  :form
  (pcase (downcase location)
    ("zhuhai" `(("location" . "珠海") ("temperature" . 35) ("unit" . ,unit)))
    ("shanghai" `(("location" . "上海") ("temperature" . 10) ("unit" . ,unit)))
    ("changchun" `(("location" . "长春") ("temperature" . -5) ("unit" . ,unit)))
    (_    `(("location" . ,location) ("temperature" . "unknown")))))

(provide 'gt-chat-openai)

;;; gt-chat-openai.el ends here
