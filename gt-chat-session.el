;;; gt-chat-session.el --- chat session -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'gt-core)
(require 'eieio-base)

(defclass gt-chat-session ()
  ((name
    :initarg :name
    :type string)
   (llm
    :initarg :llm)
   (items
    :initarg :items)
   (top
    :initarg :top
    :type (or null float)
    :initform nil)))

(defclass gt-chat-session-item ()
  ((id
    :initarg :id
    :type float
    :initform (time-to-seconds))
   (state
    :initarg :state
    :initform t)
   (role
    :initarg :role)
   (content
    :initarg :content
    :type (or null string)
    :initform nil)
   (bound
    :initform nil)))

(cl-defmethod gt-chat-items ((session gt-chat-session))
  "Return the items available (should be displayed)."
  (with-slots (items top) session
    (if-let* ((p (and top (cl-position-if (lambda (c) (equal (oref c id) top)) items))))
        (cl-subseq items 0 (1+ p))
      items)))


;;; Persist in file

(defclass gt-chat-file-session (gt-chat-session eieio-persistent) ())

(defvar gt-chat-session-location (locate-user-emacs-file "gt-chats"))

(defun gt-chat-ensure-directory ()
  (unless (file-directory-p gt-chat-session-location)
    (make-directory gt-chat-session-location t)))

(cl-defmethod initialize-instance :after ((session gt-chat-file-session) &rest _)
  (with-slots (name file) session
    (setf file (expand-file-name name gt-chat-session-location))))

(cl-defmethod gt-chat-session-list ((_class (eql 'gt-chat-file-session)))
  (gt-chat-ensure-directory)
  (cl-loop for name in (mapcar #'car
                               (sort
                                (directory-files-and-attributes
                                 gt-chat-session-location nil "^[^.]" t)
                                (lambda (x y) (time-less-p (nth 6 y) (nth 6 x)))))
           for path = (expand-file-name name gt-chat-session-location)
           collect (cons name path)))

(cl-defmethod gt-chat-session-read ((class (eql 'gt-chat-file-session)) name)
  (let ((path (expand-file-name name gt-chat-session-location)))
    (unless (file-exists-p path) (user-error "Session `%s' not existed" name))
    (eieio-persistent-read path class)))

(cl-defmethod gt-chat-session-create ((class (eql 'gt-chat-file-session)) name system-prompt)
  (let ((path (expand-file-name name gt-chat-session-location)))
    (when (file-exists-p path)
      (user-error "Session `%s' already existed" name))
    (gt-chat-ensure-directory)
    (gt-chat-session-save
     (make-instance class
                    :llm 'openai
                    :name name
                    :items (when system-prompt
                             (list (gt-chat-session-item
                                    :role 'system
                                    :content system-prompt)))))))

(cl-defmethod gt-chat-session-save ((session gt-chat-file-session))
  (with-slots (name) session
    (eieio-persistent-save session (expand-file-name name gt-chat-session-location))
    session))

(cl-defmethod gt-chat-session-delete ((_class (eql 'gt-chat-file-session)) name)
  (let ((path (expand-file-name name gt-chat-session-location)))
    (unless (file-exists-p path)
      (user-error "Session `%s' not existed" name))
    (delete-file path)))


;;; Stringify

(cl-defgeneric gt-chat-stringify (object)
  "Generate session OBJECT or item to string."
  (:method ((item gt-chat-session-item))
           (with-slots (id role content) item
             (concat (propertize (format "<%s:%s> " id role) 'face 'minibuffer-prompt) content)))
  (:method ((session gt-chat-session))
           (gt-chat-stringify (gt-chat-items session)))
  ;; for session item list
  (if (cl-every #'gt-chat-session-item-p object)
      (cl-loop for item in object
               collect (gt-chat-stringify item) into chats
               finally (return (string-join (reverse chats) "\n\n")))
    (user-error "Unsupported stringify")))

(defun gt-chat-parse (&optional point trimp)
  "Parse message at POINT as session item."
  (save-excursion
    (save-match-data
      (unless point (setq point (point)))
      (goto-char point)
      (end-of-line)
      (unless (re-search-backward "^<\\([0-9.]+\\):\\([a-z]+\\)> " nil t)
        (user-error "Cannot find the begin of prompt"))
      (let* ((beg (point))
             (pend (match-end 0))
             (end (save-match-data
                    (end-of-line)
                    (if (re-search-forward "^<[0-9.]+:[a-z]+> " nil t)
                        (match-beginning 0)
                      (point-max))))
             (content (funcall (if trimp #'string-trim #'identity)
                               (buffer-substring-no-properties pend end)))
             (item (gt-chat-session-item
                    :id (string-to-number (match-string 1))
                    :role (intern (match-string 2))
                    :content content)))
        (oset item bound (list beg pend end point))
        item))))

(defun gt-chat-parse-buffer ()
  (save-excursion
    (goto-char (point-min))
    (let (items)
      (while (re-search-forward "^<[0-9.]+:[a-z]+> " nil t)
        (push (gt-chat-parse (point) t) items))
      items)))

(defun gt-chat-current-system-prompt (&optional content-only)
  (when-let* ((item (save-excursion
                      (goto-char (point-max))
                      (when (re-search-backward "^<[0-9.]+:system> " nil t)
                        (gt-chat-parse (point) t)))))
    (if content-only (oref item content) item)))


;;; Request

(cl-defmethod gt-chat-req-messages ((session gt-chat-session))
  ;; from system message or maybe set a limit?
  (let* ((items (gt-chat-items session)) messages tools)
    (when-let* ((p (cl-position-if (lambda (c) (eq (oref c role) 'system)) items)))
      (setq items (subseq items 0 (1+ p))))
    (dolist (item items)
      (with-slots (role content) item
        (push `((role . ,role) (content . ,(substring-no-properties (or content "")))) messages)))
    (list messages tools)))

(provide 'gt-chat-session)

;;; gt-chat-session.el ends here
