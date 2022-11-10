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
        (t (mapconcat (lambda (arg)
                        (format "%s=%s"
                                (url-hexify-string (format "%s" (car arg)))
                                (url-hexify-string (format "%s" (cdr arg)))))
                      data "&"))))

(defun gts-ensure-list (object)
  "Return OBJECT as a list."
  (if (listp object) object (list object)))

(defun gts-ensure-plain (o)
  "Make sure O if not executable."
  (if (functionp o) (funcall o) o))


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
  ((picker     :initarg :picker  :documentation "`gts-picker' object or a function return it" :initform nil)
   (engines    :initarg :engines :documentation "a list of `gts-engine' objects or a function return them" :initform nil)
   (render     :initarg :render  :documentation "`gts-render' object or a function return it" :initform nil)
   (splitter   :initarg :splitter :documentation "`gts-splitter' object or a function return it" :initform nil)

   (text       :initarg :text    :documentation "Text to be translated")
   (sptext     :initarg :sptext  :documentation "List of strings splitted according `gts-splitter'")
   (path       :initarg :path    :documentation "From/To language cons")

   (plan-cnt   :initform 0       :documentation "count of tasks in a translation")
   (task-queue :initform nil     :documentation "translation tasks.")
   (state      :initform 0       :documentation "Inner state of the translator:
0: new translator without any tasks,
1: tasks add finished, but not running,
2: prepare render finished, and begin to running,
3: all tasks running finished,
4: rendered done, all over.")
   (version    :initform nil     :documentation "Used to distinguish between different translations")))

(defclass gts-task ()
  ((id         :initform (gensym))

   (text       :initarg :text :documentation "Text that sent to engines. It maybe contains delimiters")
   (from       :initarg :from)
   (to         :initarg :to)

   (raw        :initform nil :documentation "raw result responsed by the http-client")
   (parsed     :initform nil :initarg :parsed :documentation "result parsed by parser, string or list")
   (err        :initform nil :initarg :err    :documentation "error info")
   (meta       :initform nil :initarg :meta   :documentation "extra info passed from parser to render. tbeg/tend/ft")

   (engine     :initarg :engine :initform nil)
   (render     :initarg :render :initform nil)
   (translator :initarg :translator :initform nil)))

;; picker/texter

(defclass gts-picker ()
  ((single   :initarg :single :initform nil)
   (texter   :initarg :texter))
  "Used to pick the translation source text and translation from/to path.
No inner state, use return value of methods."
  :abstract t)

(defclass gts-texter () ()
  "Used to get the initial translation text.
Current word under cursor? Selection region? Whole line? Whole buffer? Others?
You can implements your rules.")

;; splitter

(defclass gts-splitter () ()
  "Used to split the translation text by pragraph or other rules."
  :abstract t)

;; engine/parser

(defclass gts-engine ()
  ((tag       :initarg :tag    :documentation "Used to display as name")
   (parser    :initarg :parser :documentation "Used to parse the response result"))
  :abstract t)

(defclass gts-parser ()
  ((tag  :initarg :tag :initform nil)))

;; render

(defclass gts-render () ())

;; generic methods

(cl-defgeneric gts-translate (translator-or-engine &rest args)
  "Do the translation for TRANSLATOR-OR-ENGINE.
ARGS should be text/path or task/callback.")

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
  (with-slots (text from to engine) task
    (sha1 (format "%s-%s-%s-%s"
                  text from to
                  (eieio-object-class-name engine)))))

(cl-defmethod gts-clear-expired ((cacher gts-cacher))
  (with-slots (caches expired) cacher
    (cl-delete-if (lambda (c) (> (time-to-seconds) (cddr c))) caches)))

(cl-defmethod gts-clear-all ((cacher gts-cacher))
  (oset cacher caches nil))


;;; Http Client

(cl-defgeneric gts-request (http-client url &key done fail data headers)
  "Use HTTP-CLIENT to request a URL with DATA.
When success execute CALLBACK, or execute ERRORBACK."
  (:method (&rest _) (user-error "Method `gts-request' not implement")))

(defvar gts-default-http-client nil
  "The default http client used to send a request.")

(cl-defun gts-do-request (url &key done fail data headers)
  "Helper for `gts-request'.
DONE is a callback based on the responsed result buffer.
FAIL is the callback for any error occured in request or DONE."
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
                             (condition-case err
                                 (funcall done)
                               (error
                                (gts-do-log tag (format "FAIL in callback! (%s) %s" url err))
                                (funcall fail err))))
                     :fail (lambda (status)
                             (gts-do-log tag (format "FAIL! (%s) %s" url status))
                             (funcall fail status))))
    (funcall fail "Make sure `gts-default-http-client' is available. eg:\n
 (setq gts-default-http-client (gts-url-http-client))\n\n\n")))


;;; Picker/Texter

(cl-defgeneric gts-pick (picker)
  "Get the source text and translate from/to path from PICKER.")

(cl-defgeneric gts-path (picker text)
  "Use to get the translation path for TEXT from PICKER.")

(cl-defgeneric gts-next-path (picker text path &optional backwardp)
  "Get the next available path.")

(cl-defgeneric gts-text (texter)
  "Get the init translation source text via TEXTER.")

(defcustom gts-translate-list nil
  "Setup your translate languages. Just like:

Single:

 (setq gts-translate-list \\='((\"en\" \"zh\")))

Or multiple:

 (setq gts-translate-list \\='((\"en\" \"zh\") (\"en\" \"fr\")))

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
  "Judge source, sure case, yes:no:unknown.
choose path, from main, then extra path match"
  (let ((p (cdr (cl-find-if
                 (lambda (item) (string-match-p (format "^%s$" (car item)) lang))
                 gts-picker-lang-match-alist))))
    (if (stringp p) (lambda (text) (string-match-p p text)) p)))

(cl-defmethod gts-all-paths ((picker gts-picker))
  (when (or (null gts-translate-list) (not (consp (car gts-translate-list))))
    (user-error "Please make sure you set the avaiable `gts-translate-list'. eg:\n
 (setq gts-translate-list '((\"en\" \"zh\") (\"en\" \"ja\"))\n\n"))
  (let (paths)
    (when gts-picker-last-path
      (cl-pushnew gts-picker-last-path paths))
    (cl-loop with candidates = (if (oref picker single)
                                   (list (car gts-translate-list))
                                 gts-translate-list)
             for (lang1 . lang2) in candidates
             for lang2 = (if (consp lang2) (car lang2) lang2)
             do (progn
                  (cl-pushnew (cons lang1 lang2) paths :test 'equal)
                  (cl-pushnew (cons lang2 lang1) paths :test 'equal)))
    (reverse paths)))

(cl-defmethod gts-matched-paths ((picker gts-picker) text)
  (when text
    (let ((paths (gts-all-paths picker))
          (text (with-temp-buffer ; clear punctuations
                  (insert text)
                  (goto-char (point-min))
                  (while (re-search-forward "\\s.\\|\n" nil t) (replace-match ""))
                  (buffer-string))))
      (cl-loop for path in paths
               for matcher = (gts-picker-lang-matcher (car path))
               when (and matcher (funcall matcher text))
               collect path))))

(cl-defmethod gts-path ((picker gts-picker) text)
  "Get translate from/to path via PICKER.
TEXT for auto match, TYPE give all."
  (let ((paths (gts-matched-paths picker text)))
    (if paths (car paths)
      (or gts-picker-last-path
          (car (gts-all-paths picker))))))

(cl-defmethod gts-next-path ((picker gts-picker) text path &optional backwardp)
  (let (candidates idx)
    (cl-loop for path in (append (gts-matched-paths picker text) (gts-all-paths picker))
             do (cl-pushnew path candidates :test 'equal))
    (setq candidates (reverse candidates))
    (setq idx (cl-position path candidates :test 'equal))
    (if backwardp
        (elt candidates (if (= 0 idx) (- (length candidates) 1) (- idx 1)))
      (elt candidates (if (= (+ 1 idx) (length candidates)) 0 (+ 1 idx))))))


;;; Splitter

(cl-defgeneric gts-split (gts-splitter text)
  "Split TEXT to several parts.
Return list of string, return nil if only one element.")


;;; Engine/Parser

(cl-defmethod gts-translate ((engine gts-engine) task callback)
  "Do the translation for TASK by ENGINE.
It's an asynchronous request with a CALLBACK that should accept the parsed task."
  (gts-do-request (format "%s%s" (oref engine base-url) (oref engine sub-url))
                  :done (lambda ()
                          (gts-update-raw task (buffer-string))
                          (gts-parse (oref engine parser) task)
                          (funcall callback))
                  :fail (lambda (err)
                          (gts-render-fail task err))))

(cl-defmethod gts-translate :around ((engine gts-engine) task callback)
  "Add cache and log support."
  (with-slots (id render translator) task
    (if-let ((render-name (eieio-object-class-name render))
             (engine-name (eieio-object-class-name engine))
             (cache (gts-cache-get gts-default-cacher task)))
        ;; try cache
        (let ((engines (gts-get translator 'engines)))
          (gts-update-raw task cache)
          (gts-parse (oref engine parser) task)
          (if (cdr engines) (gts-me-out render task)
            (gts-out render task))
          (gts-do-log 'render (format "%s: done with cache!" id)))
      ;; request from engine
      (gts-do-log 'engine (format "%s: starting %s..." id engine-name))
      (cl-call-next-method engine task
                           (lambda ()
                             (gts-do-log 'engine (format "%s: %s done!" id engine-name))
                             (funcall callback)
                             ;; refresh cache
                             (with-slots (err raw parsed) task
                               (unless err
                                 (gts-cache-set gts-default-cacher task raw))))))))

(cl-defgeneric gts-with-token (engine done fail)
  (:documentation "Get token for ENGINE, if success then DONE is called.")
  (declare (indent 1)))

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

(cl-defgeneric gts-pre (render translator)
  (:documentation "Pre-render before request success.
Do some preparation for the gts-out.")
  (:method :after ((render gts-render) _)
           (gts-do-log 'render (format "prepare render with %s finished" (eieio-object-class-name render))))
  (:method ((_o gts-render) (_t gts-translator)) ()))

(cl-defgeneric gts-out (render task)
  (:documentation "Render the TASK, default output to *Messages*.")
  (:method :after ((render gts-render) task)
           (gts-do-log 'render (format "%s: %s finished" (oref task id) (eieio-object-class-name render))))
  (:method ((_ gts-render) task)
           (with-slots (err parsed) task
             (when (and (not err) (listp parsed))
               (setq parsed (string-join parsed "\n\f\n")))
             (setq parsed (string-trim (format "%s" (or err parsed))))
             (setq parsed (propertize parsed 'face 'font-lock-keyword-face))
             (message "-+-G-+-T-+-S-+-")
             (message parsed ))))

(cl-defgeneric gts-me-pre (render translator)
  (:documentation "Pre-render for TRANSLATOR.
It used only when multiple engines exists.
Default dispatch to gts-pre when first pre-render.")
  (:method :after ((render gts-render) _)
           (gts-do-log 'render (format "me prepare render with %s finished" (eieio-object-class-name render))))
  (:method ((render gts-render) translator)
           (with-slots (task-queue state) translator
             (when (= state 1)
               (gts-pre render translator)
               (setf state 2)))))

(cl-defgeneric gts-me-out (render task)
  (:documentation "Render TASK with RENDER.
It used only when multiple engines exists.
Default dispatch to gts-out with all results concated.")
  (:method :after ((render gts-render) task)
           (gts-do-log 'render (format "%s: %s finished" (oref task id) (eieio-object-class-name render))))
  (:method ((render gts-render) task)
           (let ((translator (oref task translator)) results)
             ;; show only when all tasks finished.
             (when (= (oref translator state) 3)
               (dolist (task (oref translator task-queue))
                 (with-slots (err parsed engine) task
                   (when (and (not err) (listp parsed))
                     (setq parsed (string-join parsed "\f")))
                   (push (cons (format "[%s]" (oref engine tag)) (or err parsed)) results)))
               (setq results (nreverse results))
               (let* ((newlinep (cl-find-if (lambda (r) (string-match-p "\n" (cdr r))) results))
                      (concated (mapconcat (lambda (r)
                                             (if newlinep
                                                 (concat (propertize (concat (car r) "\n") 'face 'gts-pop-posframe-me-header-2-face)
                                                         (format "\n%s" (cdr r)))
                                               (concat (propertize (format "%-7s" (car r)) 'face 'gts-pop-posframe-me-header-face)
                                                       (format " %s" (cdr r)))))
                                           results
                                           (if newlinep "\n\n" "\n"))))
                 (oset translator state 4)
                 (gts-out render (gts-task :parsed concated :translator translator)))))))


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
when this is set to t."
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
If HANDLER speak failed, then try using locally TTS
if `gts-tts-try-speak-locally' is set."
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


;;; Translator

(defvar gts-text-delimiter "34587612321123")

(defvar gts-current-command nil "The command invoked by `gts-translate'.")

(defun gts-render-fail (task err)
  (declare (indent 1))
  (gts-do-log 'translator (format "error? %s" err))
  (if task
      (with-slots (translator render) task
        (let ((engines (gts-get translator 'engines)))
          (gts-update-raw task nil err)
          (if (cdr engines)
              (gts-me-out render task)
            (gts-out render task))))
    (signal 'error err)))

(cl-defmethod initialize-instance :after ((this gts-translator) &rest _)
  (unless (gts-ensure-plain (oref this render))
    (oset this render (gts-render)))
  (unless (gts-ensure-plain (oref this picker))
    (oset this picker (gts-noprompt-picker))))

(cl-defmethod gts-get ((this gts-translator) &rest slots)
  "Return the normalized value of SLOTS in THIS translator."
  (cl-loop for slot in slots
           for value =
           (cl-flet ((ensure (v) (or v (user-error "No %s found in current translator" slot))))
             (pcase slot
               ((or 'picker 'render) (ensure (gts-ensure-plain (slot-value this slot))))
               ('splitter            (gts-ensure-plain (slot-value this slot)))
               ('engines             (ensure (gts-ensure-list (gts-ensure-plain (slot-value this slot)))))
               (_                    (slot-value this slot))))
           collect value into values
           finally (return (if (cadr slots) (apply #'cl-values values) (car values)))))

(cl-defmethod gts-init ((this gts-translator) text path)
  "Init all status and tasks in THIS translator."
  ;; input
  (unless (and text path)
    (let ((input (gts-pick (gts-get this 'picker))))
      (setq text (cl-nth-value 0 input))
      (setq path (cl-nth-value 1 input))
      (oset this text text)
      (oset this path path)))
  (setq gts-picker-last-path path)
  (gts-do-log 'translator (format "\nBegin to translate %s" path))
  (gts-do-log 'text text)
  (cl-multiple-value-bind (splitter engines render) (gts-get this 'splitter 'engines 'render 'version)
    ;; reset
    (oset this state 0)
    (oset this task-queue nil)
    (oset this plan-cnt (length engines))
    (oset this version (time-to-seconds))
    ;; tasks
    (let* ((splitted (if splitter (gts-split splitter text)))
           (rtext (if splitted
                      (string-join splitted (concat "\n" gts-text-delimiter "\n"))
                    text)))
      (oset this sptext splitted)
      (dolist (engine engines)
        (let ((task (gts-task :translator this
                              :engine engine
                              :render render
                              :text rtext :from (car path) :to (cdr path))))
          (gts-add-task this task)))
      (gts-do-log 'text (format "splitted? %s!" (if splitted "Yes" "No"))))))

(cl-defmethod gts-add-task ((this gts-translator) task)
  "Add a new TASK to THIS translator."
  (with-slots (id engine render) task
    (with-slots (text path plan-cnt task-queue state) this
      (when (= state 0)
        (setf task-queue (append task-queue (list task)))
        (gts-do-log 'translator
          (format "- add task %s: (%s/%s)" id
                  (eieio-object-class-name engine) (eieio-object-class-name render))))
      (when (and (= state 0) (= (length task-queue) plan-cnt))
        (setf state 1)))))

(cl-defmethod gts-done-tasks ((this gts-translator))
  "Return the tasks finished in THIS translator."
  (with-slots (task-queue) this
    (cl-remove-if-not
     (lambda (task) (or (oref task err) (oref task raw))) task-queue)))

(cl-defmethod gts-update-raw (task resp &optional error)
  "Update non-parsed RESP result to TASK, update ERROR if not null."
  (with-slots (id raw err translator) task
    (with-slots (task-queue plan-cnt state) translator
      (setf raw resp)
      (setf err error)
      (when (and (= state 2) (= plan-cnt (length (gts-done-tasks translator))))
        (setf state 3))
      (gts-do-log 'translator (format "%s: **responsed** %s" id (or error "success!"))))))

(cl-defmethod gts-update-parsed ((task gts-task) result &optional metadata)
  "Update TASK with parsed RESULT, update METADATA if exists."
  (with-slots (id raw parsed err meta translator) task
    (if raw
        (condition-case er
            (progn
              (setf parsed
                    (if (or (listp result) (not (gts-get translator 'sptext)))
                        result
                      (gts-do-log 'translator
                        (format "%s: result contains delimiter, so split to list" id))
                      (split-string result gts-text-delimiter nil "[ \t\n\r]+")))
              (if metadata (setf meta metadata)))
          (error (setf err er)))
      (gts-update-raw task t "Mybe Null Response"))))

(cl-defmethod gts-translate ((this gts-translator) &optional text path)
  "Fire a translation for THIS translator instance.
If KEEP-PICK is t, then don't invoke `gts-pick' for text and path, use
the ones already saved in the translator."
  ;; record this, so can fixup issue in posframe-pop etc. Weird?
  (setq gts-current-command this-command)
  ;; initialize
  (gts-init this text path)
  (cl-multiple-value-bind (engines render tasks text path)
      (gts-get this 'engines 'render 'task-queue 'text 'path)
    (let ((buf (current-buffer)))
      ;; single engine
      (if (not (cdr engines))
          (let ((task (car tasks)))
            (gts-pre render this)
            (gts-translate (car engines) task
                           (lambda ()
                             (with-current-buffer buf
                               (gts-out render task)))))
        ;; multiple engines
        (gts-me-pre render this)
        (dolist (task tasks)
          (condition-case er
              (gts-translate (oref task engine) task
                             (lambda ()
                               (with-current-buffer buf
                                 (gts-me-out render task))))
            (error
             (gts-render-fail task er))))
        (gts-me-out render (gts-task :text text :from (car path) :to (cdr path) :translator this))))))


(provide 'gts-core)

;;; gts-core.el ends here
