;;; gts-core.el --- Translate engines and utils -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'eieio)
(require 'subr-x)
(require 'url)

(defgroup gts nil
  "Translate Framework for Emacs, asynchronous and flexible."
  :group 'external
  :prefix 'gts-)

(defvar gts-debug-p nil)


;;; Logger/Cacher

(defclass gts-logger () ()
  "Used to log the messages."
  :abstract t)

(cl-defgeneric gts-log (logger tag message)
  "Used to record the messages.")

(defvar gts-default-logger nil
  "The default logger.")

(cl-defun gts-do-log (tag msgs)
  (when gts-debug-p
    (if (and gts-default-logger
             (eieio-object-p gts-default-logger)
             (object-of-class-p gts-default-logger 'gts-logger))
        (dolist (msg (split-string msgs "\n"))
          (gts-log gts-default-logger tag msg))
      (error "Make sure `gts-default-logger' is available. eg:\n
 (setq gts-default-logger (gts-buffer-logger))\n\n\n"))))

(defclass gts-cacher () ()
  "Used to cache the translate results. NOT IMPLEMENTED YET.")


;;; Http Client

(defclass gts-http-client () ()
  "Used to send a request."
  :abstract t)

(cl-defgeneric gts-request (http-client url &key done fail data headers)
  "Use HTTP-CLIENT to request a URL with DATA.
When success execute CALLBACK, or execute ERRORBACK."
  (:method (&rest _) (user-error "Method `gts-request' not implement.")))

(defvar gts-default-http-client nil
  "The default http client used to send a request.")

(cl-defun gts-do-request (url &key done fail data headers)
  (if (and gts-default-http-client
           (eieio-object-p gts-default-http-client)
           (object-of-class-p gts-default-http-client 'gts-http-client))
      (let ((tag (format "request/%s" (eieio-object-class-name gts-default-http-client)))
            (data (gts-format-params data)))
        (gts-do-log tag
                    (concat
                     (format "Start! (%s)" url)
                     (if headers (format "\n  HEADER: %s" headers))
                     (if data (format "\n  DATA:   %s" data))))
        (gts-request gts-default-http-client url
                     :data data
                     :headers headers
                     :done (lambda ()
                             (gts-do-log tag (format "DONE! (%s)" url))
                             (funcall done))
                     :fail (lambda (status)
                             (gts-do-log tag (format "FAIL! (%s) %s" url status))
                             (funcall fail status))))
    (funcall fail "Make sure `gts-default-http-client' is available. eg:\n
 (setq gts-default-http-client (gts-url-http-client))\n\n\n")))


;;; Translation Components

;; translator

(defclass gts-translator ()
  ((text :documentation "source text")
   (from :documentation "source language")
   (to   :documentation "target language")

   (plan-cnt :initform 0 :documentation "count of engines in a translation")
   (task-queue           :documentation "translation tasks")

   (picker  :initarg :picker)
   (engines :initarg :engines)
   (render  :initarg :render)))

;; picker/texter

(defclass gts-picker ()
  ((text)
   (path)
   (single   :initarg :single :initform nil)
   (texter   :initarg :texter))
  "Used to pick the translation source text and translation from/to path.")

(defclass gts-texter () ()
  "Used to get the initial translation text.
Current word under cursor? Selection region? Whole line? Whole buffer? Others?
You can implements your rules.")

;; engine/parser

(defclass gts-engine ()
  ((tag       :initarg :tag :documentation "Used to display as name")
   (base-url  :initarg :url)
   (sub-url)
   (parser    :initarg :parser))
  :abstract t)

(defclass gts-parser ()
  ((tag  :initarg :tag)))

;; render

(defclass gts-render ()
  ((tag :documentation "Used to display as name")))

;; silence byte-compiler
(eieio-declare-slots task-queue text from to)


;;; Translator

(cl-defmethod gts-translate ((o gts-translator) &optional text from to _)
  "Do a translation for O, which is a translator instance.
It will take TEXT/FROM/TO as source if they are present, or will pick infos
from the picker instance in O."
  (unless (and text from to)
    (cl-multiple-value-bind (input path) (gts-pick (oref o picker))
      (setq text input from (car path) to (cdr path))))
  (oset o text text)
  (oset o from from)
  (oset o to to)
  (with-slots (text from to engines render) o
    (unless engines (user-error "No translate engines found"))
    (setq engines (ensure-list engines))
    (let ((buf (current-buffer)))
      (gts-do-log "Translator"
                  (concat "\n"
                          (format "from: %s, to: %s, text: %s\n" from to text)
                          (format "engines: %s\n" (mapconcat (lambda (e) (format "%s" (eieio-object-class-name e))) engines ", "))
                          (format "render:  %s" (eieio-object-class-name render))))
      (cond ((= 1 (length engines))
             (gts-pre render text from to (car engines))
             (gts-translate (car engines) text from to
                            (lambda (result)
                              (with-current-buffer buf
                                (gts-do-log (format "Engine/%s" (eieio-object-class-name (car engines))) "Done!")
                                (gts-out render result)
                                (gts-do-log (format "Render/%s" (eieio-object-class-name render)) "Done!")))))
            (t
             (gts-init o (length engines))
             (cl-loop
              for engine in (ensure-list engines)
              do (let ((id (gts-add-task o engine))
                       (edesc (format "Engine/%-10s" (eieio-object-class-name engine)))
                       (rdesc (format "Render/%-10s" (eieio-object-class-name render))))
                   (gts-me-pre render o)
                   (condition-case err
                       (gts-translate engine text from to
                                      (lambda (result)
                                        (with-current-buffer buf
                                          (gts-do-log edesc (format "Done! (id: %s)" id))
                                          (gts-add-result o id result)
                                          (gts-me-out render o id)
                                          (gts-do-log rdesc (format "Done! (id: %s)" id)))))
                     (error
                      (gts-add-result o id (cadr err))
                      (gts-me-out render o id))))))))))

;; these methods only used for multiple engine tasks.

(cl-defmethod gts-init ((o gts-translator) &optional (cnt 1))
  "Start a multiple engine translating for O. CNT is the count of engines.
It will check slot data in O, and reset all the states."
  (cl-assert (oref o text))
  (cl-assert (oref o from))
  (cl-assert (oref o to))
  (cl-assert (oref o engines))
  (cl-assert (oref o render))
  (oset o plan-cnt cnt)
  (oset o task-queue nil))

(cl-defmethod gts-add-task ((o gts-translator) engine)
  "Add a new task to translator O. Task is started by ENGINE."
  (let ((id (gensym)))
    (with-slots (plan-cnt task-queue) o
      (if (>= (length task-queue) plan-cnt)
          (user-error "Task queue is full, can't add more")
        (setf task-queue
              (append task-queue
                      (list (list :id id :engine engine :result nil))))))
    id))

(cl-defmethod gts-add-result ((o gts-translator) id result)
  "Update the task of ID's RESULT in O."
  (with-slots (task-queue) o
    (when-let ((task (cl-find-if (lambda (task) (equal (plist-get task :id) id)) task-queue)))
      (plist-put task :result result))))

(cl-defmethod gts-done-tasks ((o gts-translator))
  "Return the tasks finished in translator O."
  (with-slots (plan-cnt task-queue) o
    (cl-remove-if-not (lambda (task) (plist-get task :result)) task-queue)))

(cl-defmethod gts-finished-p ((o gts-translator))
  "If all tasks in O is finished."
  (= (length (gts-done-tasks o)) (oref o plan-cnt)))


;;; Picker/Texter

(cl-defgeneric gts-pick (picker)
  "Get the source text and translate from/to path from PICKER.")

(cl-defgeneric gts-path (picker)
  "Use to get the translation path from PICKER.")

(cl-defgeneric gts-next-path (picker text from to &optional backwardp)
  "Get the next available path.")

(cl-defgeneric gts-text (texter)
  "Get the init translation source text via TEXTER.")

(defvar gts-translate-list nil)

(defvar gts-picker-last-path nil)

(defvar gts-picker-lang-match-alist
  (list
   (cons "en"  "^[-a-zA-Z0-9,.;_ ]+$")
   (cons "zh"  "\\cc")
   (cons "ja"  (lambda (text) (string-match-p "\\cj" text)))))

(defun gts-picker-lang-matcher (lang)
  "judge source, sure case, yes:no:unknown.
choose path, from main, then extra path match"
  (let ((p (cdr (cl-find-if
                 (lambda (item) (string-match-p (format "^%s$" (car item)) lang))
                 gts-picker-lang-match-alist))))
    (if (stringp p) (lambda (text) (string-match-p p text)) p)))

(cl-defmethod gts-all-paths ((o gts-picker))
  (when (or (null gts-translate-list) (not (consp (car gts-translate-list))))
    (user-error "Please make sure you set the avaiable `gts-translate-list'. eg:\n
 (setq gts-translate-list '((\"en\" \"zh\") (\"en\" \"ja\"))\n\n"))
  (let (paths)
    (when gts-picker-last-path
      (cl-pushnew gts-picker-last-path paths))
    (cl-loop with candidates = (if (oref o single)
                                   (list (car gts-translate-list))
                                 gts-translate-list)
             for path in candidates
             do (progn
                  (cl-pushnew (cons (car path) (cadr path)) paths :test 'equal)
                  (cl-pushnew (cons (cadr path) (car path)) paths :test 'equal)))
    (reverse paths)))

(cl-defmethod gts-matched-paths ((o gts-picker) text)
  (when text
    (let ((paths (gts-all-paths o))
          (text (with-temp-buffer ; clear punctuations
                  (insert text)
                  (goto-char (point-min))
                  (while (re-search-forward "\\s.\\|\n" nil t) (replace-match ""))
                  (buffer-string))))
      (cl-loop for path in paths
               for matcher = (gts-picker-lang-matcher (car path))
               when (and matcher (funcall matcher text))
               collect path))))

(cl-defmethod gts-path ((o gts-picker) text)
  "Get translate from/to path. TEXT for auto match, TYPE give all. TODO."
  (let ((paths (gts-matched-paths o text)))
    (if paths (car paths) (car (gts-all-paths o)))))

(cl-defmethod gts-next-path ((o gts-picker) text from to &optional backwardp)
  (let (candidates idx)
    (cl-loop for p in (append (gts-matched-paths o text) (gts-all-paths o))
             do (cl-pushnew p candidates :test 'equal))
    (setq candidates (reverse candidates))
    (setq idx (cl-position (cons from to) candidates :test 'equal))
    (if backwardp
        (elt candidates (if (= 0 idx) (- (length candidates) 1) (- idx 1)))
      (elt candidates (if (= (+ 1 idx) (length candidates)) 0 (+ 1 idx))))))


;;; Engine/Parser

(cl-defgeneric gts-translate (translator-or-engine &optional text from to callback)
  "Do the translation for TRANSLATOR-OR-ENGINE.
Use TEXT/FROM/TO as source if they are provided, execute CALLBACK when success."
  (:method ((engine gts-engine) &optional text _from _to callback)
           "Do the translation for ENGINE, use TEXT/FROM/TO as source.
It's an asynchronous request with a CALLBACK that should accept the parsed result."
           (gts-do-request
            (format "%s%s" (oref engine base-url) (oref engine sub-url))
            :done (lambda ()
                    (let* ((parser (oref engine parser))
                           (result (gts-parse parser text (buffer-string))))
                      (put-text-property 0 (length result) 'engine engine result) ; pass engine this way
                      (funcall callback result)))
            :fail (lambda (status)
                    (funcall callback status)))))

(cl-defgeneric gts-tts (engine text lang)
  "TTS, do the speak for TEXT with LANG."
  (:method ((e gts-engine) &rest _)
           (user-error "No TTS service found on engine %s" (oref e tag))))

(cl-defgeneric gts-parse (parser text result)
  (:documentation "PARSE/TEXT/RESULT response content, (cons result args).")
  (:method ((_o gts-parser) text result)
           (when (> (length result) 0)
             (put-text-property 0 1 'text text result))
           result))


;;; Render

(cl-defgeneric gts-pre (render text from to &optional owner)
  (:documentation "Pre-render before request success.
Do some preparation for the gts-out.
OWNER is a translator or engine instance.")
  (:method ((_o gts-render) &rest _) ()))

(cl-defgeneric gts-out (render result)
  (:documentation "Render the RESULT, default output to *Messages*.
Used only for single engine.")
  (:method ((_ gts-render) result)
           (message "------>\n\n%s\n<------\n" result)))

(cl-defgeneric gts-me-pre (render translator)
  (:documentation "Pre-render for TRANSLATOR.
It used only when multiple engines exists.
Default dispatch to gts-pre when first pre-render.")
  (:method ((r gts-render) translator)
           (with-slots (text from to task-queue) translator
             (when (= (length task-queue) 1)
               (gts-pre r text from to translator)))))

(cl-defgeneric gts-me-out (render translator id)
  (:documentation "Render result of ID for TRANSLATOR.
It used only when multiple engines exists.
Default dispatch to gts-out with all results concated.")
  (:method ((r gts-render) translator _id)
           ;; show only when all tasks finished.
           (when (gts-finished-p translator)
             (cl-loop for task in (oref translator task-queue)
                      for result = (plist-get task :result)
                      for engine = (oref (plist-get task :engine) tag)
                      for formatted = (format "%s\n--> by %s" result engine)
                      collect formatted into rlist
                      finally (gts-out r (mapconcat #'identity rlist "\n\n"))))))


;;; Utils

(defun gts-aref-for (vector &rest ns)
  "Recursively find the element in vector. NS is indexes, as the path."
  (while ns
    (setq vector (aref vector (pop ns))))
  vector)

(defun gts-format-params (data)
  "Format alist DATA to k=v style string."
  (cond ((null data) nil)
        ((stringp data) data)
        (t
         (mapconcat (lambda (arg)
                      (format "%s=%s"
                              (url-hexify-string (format "%s" (car arg)))
                              (url-hexify-string (format "%s" (cdr arg)))))
                    data "&"))))


(provide 'gts-core)

;;; gts-core.el ends here
