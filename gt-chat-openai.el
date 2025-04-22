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

(require 'gt-extensions)
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
    (when-let* ((session gt-chat-current-session)
                (items (gt-chat-items session))
                (item (car items)))
      (when (string-blank-p (concat (oref item content)))
        (setq item (cadr items)))
      (with-slots (role content) item
        (cond
         ((eq role 'assistant)
          (oset translator text (list (gt-chat-stringify (nthcdr 1 items))))
          (setf res (substring-no-properties content))
          (funcall next task))
         ((eq role 'user)
          (gt-chat-openai-chat engine
                               (car (gt-chat-req-messages session))
                               (lambda (raw)
                                 (let* ((json (json-read-from-string raw))
                                        (str (alist-get 'content (alist-get 'message (let-alist json (aref .choices 0)))))
                                        (gt-buffer-render-output-hook (lambda () (toggle-truncate-lines -1))))
                                   (oset session items (cons (gt-chat-session-item :role 'assistant :content str) items))
                                   (gt-chat-session-save session)
                                   (setf res (substring-no-properties str))
                                   (funcall next task)))
                               (lambda (err) (gt-fail task err))))
         (t (user-error "Wrong request content")))))))

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


;;; Wrapper of Session

(defvar gt-chat-session-type 'gt-chat-file-session)

(defun gt-chat-sessions ()
  (gt-chat-session-list gt-chat-session-type))

(defun gt-chat-get-session (name)
  (setq gt-chat-current-session nil)
  (setq gt-chat-current-session (gt-chat-session-read gt-chat-session-type name)))

(defun gt-chat-new-session (name &optional system-prompt)
  (setq gt-chat-current-session
        (gt-chat-session-create
         gt-chat-session-type name
         (or system-prompt (car gt-chat-system-prompts)))))

(defun gt-chat-del-session (name)
  (gt-chat-session-delete gt-chat-session-type name)
  (when (and gt-chat-current-session (equal name (oref gt-chat-current-session name)))
    (setq gt-chat-current-session nil)))


;;; Buffer

(defvar-local gt-chat-buffer-session nil)

(defun gt-chat-sync-buffer->session (&optional obj session topid)
  (let ((session (or session gt-chat-buffer-session
                     (user-error "Session is required")))
        (news (cond
               ((null obj) (gt-chat-parse-buffer))
               ((stringp obj) (with-temp-buffer (insert obj) (gt-chat-parse-buffer)))
               ((cl-every #'gt-chat-session-item-p obj) obj)
               (t (user-error "Content is not valid")))))
    (with-slots (items top) session
      (setf items (append news (if-let* ((n (cl-position-if (lambda (c) (equal top (oref c id))) items))) (nthcdr (+ n 1) items))))
      (setf top (or topid (oref (car (last news)) id))))
    (gt-chat-session-save session)))

(defun gt-chat-sync-session->buffer (&optional input session)
  (let ((session (or session gt-chat-buffer-session
                     (user-error "Session is required"))))
    (with-slots (items) session
      (let ((inhibit-read-only t))
        (when input
          (with-slots (role content) (car items)
            (if (eq role 'user)
                (setf content (concat content
                                      (if (string-match-p "[^ ]$" content) " ")
                                      input))
              (push (gt-chat-session-item :role 'user :content input) items))))
        (erase-buffer)
        (insert (gt-chat-stringify session))
        (gt-chat-setup-buffer)))))

;; Pretty

(defvar gt-chat-prompt-logo-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'outline-cycle)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "C-k") #'gt-chat--delete-current-message)
    map))

(cl-defmethod gt-chat-stringify-prompt ((item gt-chat-session-item))
  (with-slots (id role) item
    (let* ((display (propertize (format " %s " (pcase role
                                                 ('assistant "#")
                                                 ('user ">")
                                                 ('system "!!!")
                                                 (_ (substring (symbol-name role) 0 1))))
                                'face (pcase role
                                        ('assistant 'gt-chat-assistant-prompt-logo-face)
                                        ('tool 'gt-chat-assistant-prompt-logo-face)
                                        ('user 'gt-chat-user-prompt-logo-face)
                                        ('callback 'gt-chat-user-prompt-logo-face)
                                        ('system 'gt-chat-system-prompt-logo-face))
                                'pointer 'hand))
           (result (format "<%s:%s> " id role))
           (length (length result)))
      (put-text-property (1- length) length 'rear-nonsticky t result)
      (put-text-property 0 1 'front-sticky t result)
      (add-text-properties 0 length (list 'read-only t 'display display 'keymap gt-chat-prompt-logo-map) result)
      result nil)))

(cl-defmethod gt-chat-stringify-content ((item gt-chat-session-item))
  (with-slots (role content) item
    (let ((face (pcase role
                  ('assistant 'gt-chat-assistant-prompt-face)
                  ('user 'gt-chat-user-prompt-face)
                  ('system 'gt-chat-system-prompt-face)
                  ('tool 'gt-chat-assistant-prompt-face)
                  ('callback 'gt-chat-user-prompt-face)))
          (prefix (list 'line-prefix "   " 'wrap-prefix "   "))
          (result content))
      (add-face-text-property 0 (length result) face nil result)
      (add-text-properties 0 (length result) prefix result)
      result nil)))

(cl-defmethod gt-chat-transform ((item gt-chat-session-item))
  (with-slots (role content) item
    (when (and content (eq role 'assistant))
      (with-temp-buffer
        (require 'markdown-mode)
        (let ((ori markdown-fontify-code-blocks-natively))
          (unwind-protect
              (progn
                (setq markdown-fontify-code-blocks-natively t)
                (markdown-mode)
                (insert content)
                (font-lock-ensure)
                (setf content (buffer-string)))
            (setq markdown-fontify-code-blocks-natively ori)))))))

;; State

(defun gt-chat--update-head-line ()
  (setq header-line-format
        `((:propertize ,(format " Chat with %s, Submit with C-c C-c, Cancel with C-c C-k"
                                (if-let* ((k (car (where-is-internal #'gt-chat-send-current))))
                                    (key-description k) 'gt-chat-send-current))
                       face gt-chat-buffer-head-line-face))))

(defun gt-chat--update-mode-line ()
  (setq mode-line-format
        (cl-flet ((props (fn)
                    `( face font-lock-keyword-face
                       local-map (keymap (mode-line keymap (mouse-1 . ,fn)))
                       mouse-face font-lock-warning-face )))
          `(" Session: "
            (:propertize ,(let ((session gt-chat-buffer-session))
                            (with-temp-buffer
                              (save-excursion (insert (oref session name)))
                              (concat (thing-at-point 'sentence))))
                         ,@(props #'gt-chat--switch-session))
            "  Model: "
            (:propertize ,(with-slots (model temperature) (car (oref gt-chat-buffer-translator engines))
                            (format "%s/%s"
                                    (or model gt-chatgpt-model)
                                    (or temperature gt-chatgpt-temperature)))
                         ,@(props #'gt-chat--config-engine))
            "  System prompt: "
            (:propertize ,(let ((p (or (gt-chat-current-system-prompt 'content-only) "NONE")))
                            (with-temp-buffer
                              (save-excursion (insert p))
                              (concat (thing-at-point 'sentence) "..")))
                         ,@(props #'gt-chat--switch-system-prompt))
            "  Render: "
            (:propertize ,(nth 2 (gt-translator-info gt-chat-buffer-translator))
                         ,@(props #'gt-chat--set-current-render))
            "  Referrer: "
            (:propertize ,(buffer-name (car (oref gt-chat-buffer-translator bounds)))
                         face font-lock-keyword-face)))))

(defun gt-chat--refresh-buffer-point ()
  (goto-char (point-max))
  (when (re-search-backward "^<[0-9.]+:[au][a-z]+> " nil t) (search-forward " "))
  (recenter-top-bottom 2))

(defun gt-chat-setup-buffer ()
  (gt-chat--update-head-line)
  (gt-chat--update-mode-line)
  (setq-local outline-regexp "^<[0-9.]+:[us][a-z]+> ")
  (setq-local line-move-visual t)
  (outline-minor-mode 1))

;; Commands

(defun gt-chat--previous-message ()
  (interactive)
  (with-slots (items top) gt-chat-buffer-session
    (if (= 1 (line-number-at-pos))
        (if (or (null top) (equal top (oref (car (last items)) id)))
            (message "Already at the most top position")
          (let ((offset (- (point-max) (point))))
            (message "Loading...")
            (gt-chat-sync-buffer->session)
            (oset gt-chat-buffer-session top nil)
            (gt-chat-sync-session->buffer)
            (goto-char (- (point-max) offset))))
      (when (looking-back "^<[0-9.]+:[a-z]+> " (point-min))
        (beginning-of-line)
        (backward-char))
      (if (re-search-backward "^<[0-9.]+:[a-z]+> " nil t)
          (goto-char (match-end 0))
        (beginning-of-line)))))

(defun gt-chat--back-to-indentation ()
  (interactive)
  (end-of-line)
  (if (re-search-backward "^<[0-9.]+:[a-z]+> " (line-beginning-position) t)
      (goto-char (match-end 0))
    (back-to-indentation)))

(defun gt-chat--delete-current-message ()
  (interactive)
  (let* ((inhibit-read-only t)
         (current (oref (gt-chat-parse (point)) bound))
         (last (unless (= (point-min) (car current))
                 (save-excursion
                   (goto-char (car current))
                   (backward-char)
                   (oref (gt-chat-parse (point)) bound)))))
    (unless (and (string-prefix-p (buffer-substring (car current) (cadr current)) "<U> ")
                 (not (save-excursion (forward-char) (re-search-forward "^<[AUSTR]> " nil t)))
                 (not (string-prefix-p (buffer-substring (car last) (cadr last)) "<U> ")))
      (kill-region (car current) (caddr current)))))

(defun gt-chat--pick-reference-text ()
  "Yank content from last buffer."
  (interactive)
  (let* ((last-buffer (cl-find-if (lambda (b) (not (string-prefix-p " " (buffer-name b))))
                                  (cdr (buffer-list))))
         (cands (with-current-buffer last-buffer
                  `(,(when (use-region-p)
                       (cons 'region
                             (buffer-substring
                              (region-beginning)
                              (region-end))))
                    ,@(mapcar (lambda (thing) (if-let* ((s (thing-at-point thing))) (cons thing s)))
                              (remove 'buffer gt-taker-text-things))
                    ,(cons 'buffer (buffer-string)))))
         (cands (mapcar (lambda (sc)
                          (cons (propertize (format "%s" (car sc)) 'face 'bold)
                                (replace-regexp-in-string "[\n\t ]+" " " (string-trim (cdr sc)))))
                        (delq nil cands)))
         (annotation (lambda (p) (concat (make-string (- 10 (length p)) ? ) (cdr (assoc p cands)))))
         (table (lambda (input pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (annotation-function . ,annotation)
                        (display-sort-function . ,#'identity))
                    (complete-with-action action cands input pred))))
         (key (completing-read "Yank reference text: " table)))
    (insert (cdr (assoc key cands)))))


;;; Chat with LLM

(defvar gt-chat-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'gt-chat--switch-session)
    (define-key map (kbd "C-c C-p") #'gt-chat--switch-system-prompt)
    (define-key map (kbd "C-c C-r") #'gt-chat--set-current-render)
    (define-key map (kbd "C-c C-u") #'gt-chat--previous-message)
    (define-key map (kbd "C-c C-y") #'gt-chat--pick-reference-text)
    (define-key map (kbd "M-m") #'gt-chat--back-to-indentation)
    (define-key map (kbd "C-c <return>") #'gt-chat-send-current)
    map))

(defvar gt-chat-buffer-name "*gt-chat-taker*")

(defvar gt-chat-buffer-window-config gt-buffer-prompt-window-config)

(defvar gt-chat-engine-models '("gpt-3.5-turbo" "gpt-4o"))

;; Setup

(defun gt-chat--set-current-render ()
  (interactive)
  (gt-set-render gt-chat-buffer-translator)
  (gt-chat--update-mode-line))

(defun gt-chat--config-engine ()
  (interactive)
  (let ((engine (car (oref gt-chat-buffer-translator engines))))
    (unless (cl-typep engine 'gt-chatgpt-engine)
      (user-error "Invalid engine found"))
    (with-slots (model temperature) engine
      (let ((nm (completing-read "Model to use: " gt-chat-engine-models nil t
                                 nil nil (or model gt-chatgpt-model)))
            (nt (read-number "Temperature as: " (or temperature gt-chatgpt-temperature))))
        (setf model nm temperature nt)
        (gt-chat--update-mode-line)))))

(defvar gt-chat-system-prompt-history nil)

(defun gt-chat--switch-system-prompt ()
  (interactive)
  (let* ((inhibit-read-only t)
         (prompt (completing-read "System prompt to: "
                                  (gt-make-completion-table
                                   (cl-remove-duplicates
                                    (delq nil
                                          (append (list (gt-chat-current-system-prompt :content))
                                                  gt-chat-system-prompt-history
                                                  gt-chat-system-prompts))
                                    :from-end t
                                    :test #'string-equal))
                                  nil nil nil 'gt-chat-system-prompt-history))
         (system (if (string-blank-p prompt)
                     (user-error "System prompt is required")
                   (gt-chat-session-item :role 'system :content prompt)))
         (current (gt-chat-parse (point))) offset)
    (with-slots (role bound) current
      (goto-char (if (eq role 'system) (caddr bound) (car bound)))
      (insert "\n" (gt-chat-stringify system) "\n\n"))
    (setq offset (save-excursion
                   (skip-chars-backward " \t\n")
                   (- (point-max) (point))))
    (gt-chat-sync-buffer->session nil nil (oref system id))
    (gt-chat-sync-session->buffer)
    (goto-char (- (point-max) offset))
    (unless (re-search-forward "^<[0-9.]+:[astr][a-z]+> " nil t)
      (goto-char (point-max)))))

(defun gt-chat--switch-session ()
  (interactive)
  (gt-chat-sync-buffer->session)
  (let ((name (completing-read "Session to: "
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
    (gt-chat-sync-session->buffer)
    (gt-chat--refresh-buffer-point)))

;; Request and Response

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
                (let ((inhibit-read-only t)
                      (role (if (or func-name func-args) 'tool 'assistant)))
                  (goto-char (point-max))
                  (insert "\n\n" (gt-chat-stringify (gt-chat-session-item :role role)))
                  (setq gt-chat-tracking-marker (point-marker)))))
            (cond
             ((equal finish-reason "tool_calls")
              (let ((r `((name . ,gt-chat-func-name)
                         (arguments . ,(json-parse-string gt-chat-func-args :object-type 'alist)))))
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert (format "%s" (json-encode r)))
                    (insert "\n\n" (gt-chat-stringify (gt-chat-session-item :role 'callback :content r)))
                    (let* ((obj (apply #'make-instance
                                       (intern-soft (alist-get 'name r))
                                       (cl-loop for (k . v) in (alist-get 'arguments r)
                                                append (list (intern (format ":%s" k)) v))))
                           (rr (gt-eval obj))
                           (fc (format "%s: %s" (alist-get 'name r) rr)))
                      (insert fc "\n"))
                    (gt-chat-sync-buffer->session)
                    (gt-chat-send-current)))
                (message "Done")))
             (finish-reason
              (with-current-buffer buf
                (let* ((inhibit-read-only t)
                       (p (unless (eobp) (point)))
                       (item (gt-chat-parse (point-max)))
                       (bound (oref item bound)))
                  (delete-region (car bound) (caddr bound))
                  (insert (gt-chat-stringify item))
                  (insert "\n\n" (gt-chat-stringify (gt-chat-session-item :role 'user)))
                  (if p (goto-char p))
                  (gt-chat-sync-buffer->session))
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
                          (insert content)))))))))))
    (error (if (string-prefix-p "json" (format "%s" (car err)))
               (setq gt-chat-last-position (line-beginning-position))
             (signal (car err) (cdr err))))))

(defun gt-chat-send-current ()
  (interactive)
  (message "Sending...")
  (with-slots (engines bag) gt-chat-buffer-translator
    (setq gt-chat-current-session gt-chat-buffer-session)
    (gt-chat-sync-buffer->session)
    (setq gt-chat-tracking-marker nil)
    (let ((engine (car (ensure-list engines))))
      (gt-ensure-key engine)
      (with-slots (host key model temperature) engine
        (pcase-let* ((`(,messages ,functions) (gt-chat-req-messages gt-chat-current-session))
                     (url (concat (or host gt-chatgpt-host) "/v1/chat/completions"))
                     (headers `(("Content-Type" . "application/json")
                                ("Authorization" . ,(encode-coding-string (concat "Bearer " key) 'utf-8))))
                     (data    `((model       . ,(or model gt-chatgpt-model))
                                (temperature . ,(or temperature gt-chatgpt-temperature))
                                (messages    . ,(if (vectorp messages) messages (vconcat messages)))
                                (stream      . t)))
                     (fds (cl-loop for f in functions
                                   for fd = (ignore-errors (gt-serialize (intern-soft f)))
                                   if fd collect `((type . function) (function . ,fd))))
                     (data (encode-coding-string (json-encode (append fds data)) 'utf-8)))
          (let ((gt-log-buffer-name "*gt-stream-log*"))
            (gt-log 'openai-request 2 (pp-to-string data)))
          (gt-request :url url :headers headers :data data
                      :filter (lambda () (gt-chat-openai-filter bag))
                      :done (lambda (raw) (let ((gt-log-buffer-name "*gt-stream-log*")) (gt-log 'openai-response 2 raw)))
                      :fail (lambda (err) (signal 'user-error err))))))))

(defun gt-chat--restore-prompt-buffer (translator)
  (if (equal (buffer-name) gt-chat-buffer-name)
      (progn (bury-buffer) (delete-window))
    (with-slots (taker text bounds) translator
      (let ((inhibit-read-only t)
            (rs (ensure-list (gt-text taker translator))))
        (if (stringp (car rs))
            (setf bounds (list (current-buffer)))
          (unless (bufferp (car rs))
            (push (current-buffer) rs))
          (setf bounds rs))
        (setf text (gt-collect-text rs))
        (with-current-buffer
            (pop-to-buffer gt-chat-buffer-name gt-chat-buffer-window-config)
          (gt-chat--update-mode-line)
          (when (car text)
            (goto-char (point-max))
            (save-excursion (insert (car text)))))))))

(cl-defmethod gt-start :around ((translator gt-chat-openai))
  "Make chat buffer can be toggle show."
  (with-slots (text taker) translator
    (when (ignore-errors (eq (oref (gt-ensure-plain taker) prompt) 'buffer))
      (if (and (cl-plusp (recursion-depth)) (buffer-live-p (get-buffer gt-chat-buffer-name)))
          (gt-chat--restore-prompt-buffer translator)
        (cl-call-next-method translator)))))

(cl-defmethod gt-prompt ((_taker gt-taker) (translator gt-chat-openai) (_ (eql 'buffer)))
  (with-slots (text target bag engines _engines) translator
    (if (cdr text) (user-error "Multiple text cannot be prompted"))
    (unless engines (setf engines (ensure-list (gt-ensure-plain _engines))))
    (let* ((session (let ((name (if gt-chat-current-session
                                             (oref gt-chat-current-session name)
                                  (caar (gt-chat-sessions)))))
                      (if name (gt-chat-get-session name)
                        (gt-chat-new-session (read-string "No session exists, create one with name: ")))))
           (oldtext (or (car (gt-collect-text (ensure-list text))) ""))
           (newtext (gt-read-from-buffer
                     :buffer gt-chat-buffer-name
                     :catch 'gt-chat-buffer-prompt
                     :window-config gt-chat-buffer-window-config
                     :window-no-restore t
                     :keymap gt-chat-buffer-map
                     (setf bag (current-buffer))
                     (setq gt-chat-buffer-session session)
                     (setq gt-chat-buffer-translator gt-current-translator)
                     (gt-chat-sync-session->buffer oldtext)
                     (gt-chat--refresh-buffer-point)
                     (setq truncate-lines nil))))
      (when (null newtext)
        (user-error ""))
      (when (zerop (length (string-trim newtext)))
        (user-error "Text should not be null, abort"))
      (gt-chat-sync-buffer->session newtext session)
      (setq gt-chat-current-session session)
      (setf target (list nil (oref session name)))
      (unless (equal oldtext newtext) (setf text (ensure-list newtext))))))

(cl-defmethod gt-pick ((_ gt-taker) (_translator gt-chat-openai)) 'ignore)


;;; Function

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
