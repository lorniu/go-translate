;;; gts-core.el --- Translate engines and utils -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The core components and utils of the translator

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)

(defgroup go-translate nil
  "Translate Framework for Emacs, asynchronous and flexible."
  :group 'external
  :prefix 'gts-)

(defvar gts-debug-p nil)


;;; Components

;; logger/http-client/cacher

(defclass gts-logger () ()
  "Used to log the messages."
  :abstract t)

(defclass gts-http-client () ()
  "Used to send a request."
  :abstract t)

(defclass gts-cacher ()
  ((caches  :initform nil)) ; (key . (str . timestamp))
  "Used to cache the translate results."
  :abstract t)

;; translator/task

(defclass gts-translator ()
  ((picker     :initarg :picker)
   (engines    :initarg :engines)
   (render     :initarg :render)

   (plan-cnt   :initform 0   :documentation "count of tasks in a translation")
   (task-queue :initform nil :documentation "translation tasks.")
   (state      :initform 0   :documentation "Inner state of the translator:
0: new translator without any tasks,
1: tasks add finished, but not running,
2: prepare render finished, and begin to running,
3: all tasks running finished,
4: rendered done, all over.")))

(defclass gts-task ()
  ((id         :initform (gensym))

   (text       :initarg :text)
   (from       :initarg :from)
   (to         :initarg :to)

   (translator :initarg :translator :initform nil)
   (engine     :initarg :engine     :initform nil)
   (render     :initarg :render     :initform nil)

   (raw        :initform nil :documentation "raw result responsed by the http-client")
   (result     :initform nil :initarg :result :documentation "formatted result parsed by parser")
   (ecode      :initform nil :initarg :ecode :documentation "if any error, this will not nil")))

;; picker/texter

(defclass gts-picker ()
  ((text)
   (path)
   (single   :initarg :single :initform nil)
   (texter   :initarg :texter))
  "Used to pick the translation source text and translation from/to path."
  :abstract t)

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
  ((tag  :initarg :tag :initform nil)))

;; render

(defclass gts-render () ())

;; silence byte-compiler
(eieio-declare-slots task-queue text from to)


;;; Logger

(cl-defgeneric gts-log (logger tag message)
  "Used to record the messages.")

(defvar gts-default-logger nil "The default logger.")

(defmacro gts-do-log (tag &rest messages)
  (declare (indent 1))
  `(when gts-debug-p
     (if gts-default-logger
         (cl-loop with ms = nil
                  for message in (list ,@messages)
                  if message do (cl-loop for m in (split-string message "\n") do (push m ms))
                  finally (cl-loop
                           for m in (nreverse ms)
                           do (gts-log gts-default-logger (format "%s" ,tag) m)))
       (error "Make sure `gts-default-logger' is available. eg:\n
 (setq gts-default-logger (gts-buffer-logger))\n\n\n"))))


;;; Cacher

(defcustom gts-cache-enable t
  "Enable the cacher if this is t."
  :type 'boolean
  :group 'go-translate)

(defcustom gts-cache-expired-factor (* 30 60)
  "Cache alive time  based on this.
Make word live longer time than sentence."
  :type 'integer
  :group 'go-translate)

(defvar gts-default-cacher nil "The default cacher.")

(cl-defgeneric gts-cache-get (cacher task)
  "Get item for TASK from CACHER.")

(cl-defgeneric gts-cache-set (cacher task result)
  "Add RESULT fro TASK to CACHER.")

(cl-defmethod gts-cache-key ((_ gts-cacher) task)
  (with-slots (text from to engine render) task
    (sha1 (format "%s-%s-%s-%s-%s"
                  text from to
                  (eieio-object-class-name engine)
                  (eieio-object-class-name render)))))

(cl-defmethod gts-clear-expired ((cacher gts-cacher))
  (with-slots (caches expired) cacher
    (cl-delete-if (lambda (c) (> (time-to-seconds) (cddr c))) caches)))

(cl-defmethod gts-clear-all ((cacher gts-cacher))
  (oset cacher caches nil))


;;; Http Client

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
      (let ((tag (format "%s" (eieio-object-class-name gts-default-http-client)))
            (data (gts-format-params data)))
        (gts-do-log tag
          (format "Start! (%s)" url)
          (if headers (format "    HEADER: %s" headers))
          (if data (format "    DATA:   %s" data)))
        (gts-request gts-default-http-client url
                     :headers headers
                     :data data
                     :done (lambda ()
                             (gts-do-log tag (format "DONE! (%s)" url))
                             (funcall done))
                     :fail (lambda (status)
                             (gts-do-log tag (format "FAIL! (%s) %s" url status))
                             (funcall fail status))))
    (funcall fail "Make sure `gts-default-http-client' is available. eg:\n
 (setq gts-default-http-client (gts-url-http-client))\n\n\n")))


;;; Translator

(defvar gts-current-command nil "Record which command is invoked by `gts-translate'.")

(cl-defgeneric gts-translate (translator-or-engine &rest args)
  "Do the translation for TRANSLATOR-OR-ENGINE.
ARGS should be text/from/to/callback or task/callback.")

(cl-defmethod gts-translate ((o gts-translator) &optional text from to)
  "Do a translation for O, which is a translator instance.
It will take TEXT/FROM/TO as source if they are present, or will pick from the Picker."
  (with-slots (picker engines render) o
    ;; check
    (unless picker (user-error "No picker found in the current translator"))
    (unless render (user-error "No render found in the current translator"))
    (unless engines (user-error "No translate engines found in the current translator"))
    (setq gts-current-command this-command) ; record this, so can fixup issue in posframe-pop etc. Weird?
    ;; input
    (unless (and text from to)
      (cl-multiple-value-bind (input path) (gts-pick picker)
        (setq text input from (car path) to (cdr path))))
    (gts-do-log 'translator (format "\nfrom: %s, to: %s, text: %s" from to text))
    ;; translate
    (let ((buf (current-buffer))
          (rd-name (eieio-object-class-name render))
          (engines (if (listp engines) engines (list engines))))
      ;; log
      (gts-do-log 'translator
        (format "engines: %s" (mapconcat (lambda (e) (format "%s" (eieio-object-class-name e))) engines ", "))
        (format "render:  %s" (eieio-object-class-name render)))
      ;; single engine
      (if (= 1 (length engines))
          (let* ((eg (car engines))
                 (eg-name (eieio-object-class-name eg))
                 (task (gts-task :text text :from from :to to :translator o :render render :engine eg)))
            (gts-pre render task)
            (gts-do-log 'translator "pre-render finished.")
            ;; first, try cache
            (if-let ((cache (gts-cache-get gts-default-cacher task)))
                (progn (gts-update-raw task (car cache))
                       (gts-update-parsed task (cdr cache))
                       (gts-out render task)
                       (gts-do-log 'translator (format "%s Done (with cache)!" rd-name)))
              ;; second, try engine
              (gts-translate eg task (lambda (_)
                                       (with-current-buffer buf
                                         (gts-do-log 'translator (format "%s Done!" eg-name))
                                         (gts-out render task)
                                         (gts-do-log 'translator (format "%s Done!" rd-name))
                                         ;; refresh cache
                                         (with-slots (ecode raw result) task
                                           (unless ecode (gts-cache-set gts-default-cacher task (cons raw result)))))))))
        ;; multiple engines
        (gts-init o (length engines))
        (cl-loop for engine in engines
                 for task = (gts-task :text text :from from :to to :translator o :render render :engine engine)
                 do (gts-add-task o task))
        (gts-me-pre render o)
        (gts-do-log 'translator "me-pre-render finished.")
        (cl-loop for task in (oref o task-queue)
                 for cache = (gts-cache-get gts-default-cacher task)
                 do (let* ((task task)
                           (id (oref task id))
                           (eg-name (eieio-object-class-name (oref task engine)))
                           (rd-name (eieio-object-class-name (oref task render))))
                      (cond (cache
                             (progn (gts-update-raw task (car cache))
                                    (gts-update-parsed task (cdr cache))
                                    (gts-me-out render o task)
                                    (gts-do-log 'translator (format "%s: %s done with cache!" id rd-name))))
                            (t
                             (condition-case err
                                 (progn
                                   (gts-do-log 'translator (format "%s: begin to start engine..."  id))
                                   (gts-translate (oref task engine) task
                                                  (lambda (_)
                                                    (with-current-buffer buf
                                                      ;; log, then render
                                                      (gts-do-log 'translator (format "%s: %s Done!" id eg-name))
                                                      (gts-me-out render o task)
                                                      (gts-do-log 'translator (format "%s: %s Done!" id rd-name))
                                                      ;; refresh cache
                                                      (with-slots (ecode raw result) task
                                                        (unless ecode (gts-cache-set gts-default-cacher task (cons raw result))))))))
                               (error
                                (gts-do-log 'translator (format "error? %s" err))
                                (gts-update-parsed task (cadr err) (caddr err))
                                (gts-me-out render o task))))))
                 finally (gts-me-out render o (gts-task :from from :to to :text text)))))))

(cl-defmethod gts-init ((o gts-translator) &optional (cnt 1))
  "Start a multiple engine translating for O. CNT is the count of engines.
It will check slot data in O, and reset all the states."
  (oset o plan-cnt cnt)
  (oset o task-queue nil)
  (oset o state 0))

(cl-defmethod gts-add-task ((o gts-translator) task)
  "Add a new TASK to translator O."
  (with-slots (plan-cnt task-queue state) o
    (when (= state 0)
      (setf task-queue (append task-queue (list task)))
      (gts-do-log 'translator (format "- add task: %s" (oref task id))))
    (when (and (= state 0) (= (length task-queue) plan-cnt))
      (setf state 1))))

(cl-defmethod gts-done-tasks ((translator gts-translator))
  "Return the tasks finished in translator TRANSLATOR."
  (with-slots (task-queue) translator
    (cl-remove-if-not (lambda (task) (oref task raw)) task-queue)))

(cl-defgeneric gts-update-raw (task raw)
  "Update non-parsed RAW result to TASK."
  (:method ((task gts-task) resp)
           (with-slots (translator raw id) task
             (with-slots (task-queue plan-cnt state) translator
               (setf raw resp)
               (gts-do-log 'translator (concat (format "%s: responsed with " id) (if (equal resp t) "error" "result") "."))
               (when (and (= state 2) (= plan-cnt (length (gts-done-tasks translator))))
                 (setf state 3))))))

(cl-defgeneric gts-update-parsed (task parsed &optional ecode)
  "Update TASK with PARSED result, update error code if ECODE not null."
  (:method ((task gts-task) parsed &optional err-code)
           (with-slots (raw result ecode) task
             (unless raw (gts-update-raw task t))
             (setf result parsed)
             (if err-code (setf ecode err-code)))))


;;; Picker/Texter

(cl-defgeneric gts-pick (picker)
  "Get the source text and translate from/to path from PICKER.")

(cl-defgeneric gts-path (picker)
  "Use to get the translation path from PICKER.")

(cl-defgeneric gts-next-path (picker text from to &optional backwardp)
  "Get the next available path.")

(cl-defgeneric gts-text (texter)
  "Get the init translation source text via TEXTER.")

(defcustom gts-translate-list nil
  "Setup your translate languages. Just like:

Single:

 (setq gts-translate-list '((\"en\" \"zh\")))

Or multiple:

 (setq gts-translate-list '((\"en\" \"zh\") (\"en\" \"fr\")))

The picker will give the translate from/to pair according this."
  :type 'alist
  :group 'go-translate)

(defcustom gts-picker-lang-match-alist
  (list
   (cons "en"  "^[-a-zA-Z0-9,.;_ ]+$")
   (cons "zh"  "\\cc")
   (cons "ja"  (lambda (text) (string-match-p "\\cj" text)))
   (cons "ru" "\\cy"))
  "Setup language match rules.

Used to pick the right language for current translate text.

Key should be the language, and value should be a regexp string or function."
  :type 'alist
  :group 'go-translate)

(defvar gts-picker-last-path nil)

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
             for (lang1 . lang2) in candidates
             for lang2 = (if (consp lang2) (car lang2) lang2)
             do (progn
                  (cl-pushnew (cons lang1 lang2) paths :test 'equal)
                  (cl-pushnew (cons lang2 lang1) paths :test 'equal)))
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

(cl-defmethod gts-translate ((engine gts-engine) task callback)
  "Do the translation for TASK by ENGINE.
It's an asynchronous request with a CALLBACK that should accept the parsed task."
  (gts-do-request (format "%s%s" (oref engine base-url) (oref engine sub-url))
                  :done (lambda ()
                          (gts-update-raw task (buffer-string))
                          (gts-parse (oref engine parser) task)
                          (funcall callback))
                  :fail (lambda (status)
                          (gts-update-parsed status t)
                          (funcall callback))))

(cl-defgeneric gts-tts (engine text lang)
  "TTS, do the speak for TEXT with LANG."
  (:method ((e gts-engine) &rest _)
           (user-error "No TTS service found on engine `%s'" (oref e tag))))

(cl-defgeneric gts-parse (parser task)
  (:documentation "Parse RESPONSE for TASK, update the results into TASK.
The RESPONSE is the raw content returned from the http request.
The result can be a string with some properties, or an error cons.

Some properties in string will provide more information to the render,
Eg, beg/end for result position.")
  (:method ((_o gts-parser) task)
           ;; do nothing
           (gts-update-parsed task (oref task raw))))


;;; Render

(cl-defgeneric gts-pre (render task)
  (:documentation "Pre-render before request success.
Do some preparation for the gts-out.")
  (:method ((_o gts-render) _) ()))

(cl-defgeneric gts-out (render task)
  (:documentation "Render the TASK, default output to *Messages*.")
  (:method ((_ gts-render) task)
           (message
            (concat
             "------>\n\n"
             (propertize (oref task result) 'face 'font-lock-keyword-face)
             "<------\n"))))

(cl-defgeneric gts-me-pre (render translator)
  (:documentation "Pre-render for TRANSLATOR.
It used only when multiple engines exists.
Default dispatch to gts-pre when first pre-render.")
  (:method ((render gts-render) translator)
           (with-slots (task-queue state) translator
             (when (= state 1)
               (gts-pre render (car task-queue))
               (setf state 2)))))

(cl-defgeneric gts-me-out (render translator task)
  (:documentation "Render TASK for TRANSLATOR.
It used only when multiple engines exists.
Default dispatch to gts-out with all results concated.")
  (:method ((r gts-render) translator _task)
           ;; show only when all tasks finished.
           (when (= (oref translator state) 3)
             (cl-loop for task in (oref translator task-queue)
                      for result = (format "%s" (oref task result))
                      for header = (oref (oref task engine) tag)
                      collect (cons header result) into results
                      finally (let* ((newlinep (cl-find-if (lambda (r) (string-match-p "\n" (cdr r))) results))
                                     (concated (mapconcat (lambda (r)
                                                            (if newlinep
                                                                (concat (propertize (concat (car r) "\n") 'face 'gts-pop-posframe-me-header-2-face) "\n" (cdr r))
                                                              (concat (propertize (format "%-7s" (car r)) 'face 'gts-pop-posframe-me-header-face) " " (cdr r))))
                                                          results
                                                          (if newlinep "\n\n" "\n"))))
                                (oset translator state 4)
                                (gts-out r (gts-task :result concated :translator translator)))))))


;;; TTS

(defcustom gts-tts-speaker (or (executable-find "mpv")
                               (executable-find "mplayer"))
  "The program to use to speak the translation.

On Windows, if it is not found, will fallback to use `powershell`
to do the job. Although it is not perfect, it seems to work."
  :type 'string
  :group 'go-translate)

(defcustom gts-tts-try-speak-locally t
  "If t then will fallback to locally TTS service when engine's TTS failed.
For example, it will use powershell on Windows to speak the word
when this is set to t. "
  :type 'boolean
  :group 'go-translate)

(defvar gts-tts-playing-process nil)

(defun gts-tts-speak-buffer-data ()
  "Speak the current buffer's data."
  (gts-tts-try-interrupt-playing-process)
  (let ((proc (make-process :name (format "gts-tts-process-%s" (+ 1000 (random 1000)))
                            :command (list gts-tts-speaker "-")
                            :buffer nil
                            :noquery t
                            :connection-type 'pipe)))
    (setq gts-tts-playing-process proc)
    (process-send-region proc (point-min) (point-max))
    (if (process-live-p proc) (process-send-eof proc))))

(defun gts-tts-try-interrupt-playing-process ()
  (when (and gts-tts-playing-process (process-live-p gts-tts-playing-process))
    (ignore-errors (kill-process gts-tts-playing-process))
    (setq gts-tts-playing-process nil)))

(defun gts-do-tts (text _lang handler)
  "TTS TEXT in LANG with possible tts service.
If HANDLER speak failed, then try using locally TTS if `gts-tts-try-speak-locally' is set."
  (condition-case err
      (if gts-tts-speaker
          (funcall handler)
        (user-error "No mpv/mplayer found"))
    (error (if gts-tts-try-speak-locally
               (cond ((executable-find "powershell")
                      (let ((cmd (format "$w = New-Object -ComObject SAPI.SpVoice; $w.speak(\\\"%s\\\")" text)))
                        (shell-command (format "powershell -Command \"& {%s}\""
                                               (encode-coding-string
                                                (replace-regexp-in-string "\n" " " cmd)
                                                (keyboard-coding-system))))))
                     (t (message "[TTS] %s" (cadr err))))))))


;;; Utils

(defun gts-aref-for (vector &rest ns)
  "Recursively find the element in VECTOR. NS is indexes, as the path."
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
