;;; gt-chat-session.el --- chat session -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar gt-chat-session-location (locate-user-emacs-file "gt-chats"))

(defclass gt-chat-session ()
  ((name
    :initarg :name
    :type string)
   (llm
    :initarg :llm)
   (items
    :initarg :items)
   (index
    :initarg :index
    :type integer
    :initform 0)))

(defclass gt-chat-session-item ()
  ((id
    :initarg :id
    :initform (time-to-seconds))
   (state
    :initarg :state
    :initform t)
   (role
    :initarg :role)
   (content
    :initarg :content)))

(cl-defgeneric gt-chat-session-save (session))

(cl-defgeneric gt-chat-session-read (session-class filename))

(cl-defmethod gt-chat-items ((session gt-chat-session))
  (with-slots (items index) session
    (cl-subseq items 0 (- (length items) index))))

(defvar gt-chat-prompt-logo-display-function
  (lambda (role)
    (let* ((face (pcase role
                   ('assistant 'gt-chat-assistant-prompt-logo-face)
                   ('tool 'gt-chat-assistant-prompt-logo-face)
                   ('user 'gt-chat-user-prompt-logo-face)
                   ('callback 'gt-chat-user-prompt-logo-face)
                   ('system 'gt-chat-system-prompt-logo-face))))
      (propertize (format " %s " (pcase role
                                   ('assistant "#")
                                   ('user ">")
                                   ('system "!!!")
                                   (_ (substring (symbol-name c) 0 1))))
                  'face face
                  'pointer 'hand))))

(defvar gt-chat-prompt-display-function
  (lambda (role beg end)
    (unless (string-blank-p (buffer-substring-no-properties beg end))
      (add-face-text-property beg end
                              (pcase role
                                ('assistant 'gt-chat-assistant-prompt-face)
                                ('user 'gt-chat-user-prompt-face)
                                ('system 'gt-chat-system-prompt-face)
                                ('tool 'gt-chat-assistant-prompt-face)
                                ('callback 'gt-chat-user-prompt-face)))
      (add-text-properties beg end (list 'line-prefix "   " 'wrap-prefix "   ")))))

(cl-defmethod gt-chat-stringify ((item gt-chat-session-item))
  (with-slots (id role content) item
    (let* ((p (format "<%s:%s>" id role))
           (c (string-trim content))
           (len (length p)))
      (when (eq role 'assistant)
        (setq c (gt-chat-font-lock-string-with-markdown c)))
      (with-temp-buffer
        (insert p " " c)
        (let* ((beg (point-min))
               (pend (+ beg len 1))
               (end (point-max)))
          (funcall gt-chat-prompt-display-function role pend end)
          (put-text-property (- pend 1) pend 'rear-nonsticky t)
          (put-text-property beg (+ beg 1) 'front-sticky t)
          (add-text-properties beg pend
                               (list 'read-only t
                                     'display (funcall gt-chat-prompt-logo-display-function role)
                                     'keymap gt-chat-buffer-message-logo-map)))
        (buffer-string)))))

(cl-defmethod gt-chat-stringify ((session gt-chat-session))
  (cl-loop for item in (gt-chat-items session)
           collect (gt-chat-stringify item) into chats
           finally (return (string-join (reverse chats) "\n\n"))))

(defun gt-chat-bound-of-current-message ()
  "Get the bounds of message at point.
Return cons cells in form of (begin prompt-end end)."
  (save-excursion
    (save-match-data
      (let (beg pend end)
        (end-of-line)
        (if (re-search-backward "^<[0-9.]+:[a-z]+> " nil t)
            (setq beg (point) pend (match-end 0))
          (user-error "Cannot find the begin of prompt"))
        (end-of-line)
        (setq end (if (re-search-forward "^<[0-9.]+:[a-z]+> " nil t)
                      (match-beginning 0)
                    (point-max)))
        (list beg pend end)))))

(defun gt-chat-parse-buffer ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (unless (re-search-forward "^<[0-9.]+:[a-z]+> " nil t)
        (insert (format "<%s:user> " (time-to-seconds))))
      (goto-char (point-max))
      (unless (re-search-backward "^<[0-9.]+:system> " nil t)
        (goto-char (point-min))
        (insert (format "<%s:system> %s\n\n"
                        (time-to-seconds)
                        (car gt-chat-system-prompts))))
      (goto-char (point-min))
      (let (items)
        (while (re-search-forward "^<[0-9.]+:\\([a-z]+\\)> " nil t)
          (let* ((role (intern (match-string 1)))
                 (bds (gt-chat-bound-of-current-message))
                 (item (gt-chat-session-item
                        :role role
                        :content (substring-no-properties (string-trim (buffer-substring (cadr bds) (caddr bds)))))))
            (push item items)))
        items))))


;; Persist in file

(defclass gt-chat-file-session (gt-chat-session eieio-persistent) ())

(cl-defmethod initialize-instance :after ((session gt-chat-file-session) &rest _)
  (with-slots (name file) session
    (setf file (expand-file-name name gt-chat-session-location))))

(cl-defmethod gt-chat-session-save ((session gt-chat-file-session))
  (with-slots (name) session
    (eieio-persistent-save session (expand-file-name name gt-chat-session-location))))

(cl-defmethod gt-chat-session-read ((class (eql 'gt-chat-file-session)) filename)
  (eieio-persistent-read filename class))


;;; Test

(defun gt-test-1i2323 ()
  (let* ((c1 (gt-chat-session-item :role 'system :content "you are a assist"))
         (c2 (gt-chat-session-item :role 'user :content "what is your name"))
         (c3 (gt-chat-session-item :role 'assistant :content "I am chatgpt..."))
         (ss (gt-chat-file-session
              :name "hello"
              :llm 'openai
              :system c1
              :items (list c1 c3 c2 c1))))
    (gt-chat-session-save ss)))

(defun gt-test-231i2323 ()
  (let* ((f "~/.emacs.d/.var/gt-chats/hello")
         (ss (gt-chat-session-read 'gt-chat-file-session f)))
    (with-slots (items) ss
      (push (gt-chat-session-item :role 'user :content "你是个啥玩意") items))
    (gt-chat-session-save ss)))

(provide 'gt-chat-session)

;;; gt-chat-session.el ends here
