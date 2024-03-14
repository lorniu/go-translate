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
  "Return O or the result of its execution."
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
  ((picker     :initarg :picker   :documentation "`gts-picker' object or a function return it" :initform nil)
   (engines    :initarg :engines  :documentation "A list of `gts-engine' objects or a function return them" :initform nil)
   (render     :initarg :render   :documentation "`gts-render' object or a function return it" :initform nil)

   (text       :initarg :text     :documentation "Text to be translated. Get from picker, string or list")
   (path       :initarg :path     :documentation "Language cons, (sl . tls). Get from picker")

   (tasks      :initform nil      :documentation "Task queue in a translation")
   (total      :initform 0        :documentation "Count of all tasks in a translation")
   (state      :initform 0        :documentation "Inner state of the translator:
0: new translator without any tasks,
1: tasks add finished, but not running,
2: prepare render finished, and begin to running,
3: all tasks running finished,
4: rendered done, all over")
   (version    :initform nil      :documentation "Used to distinguish between different translations")))

(defclass gts-task ()
  ((id         :initform (gensym "task-"))

   (text       :initarg :text :documentation "Text to be translated")
   (sl         :initarg :sl   :documentation "Source language")
   (tl         :initarg :tl   :documentation "Target language")
   (meta       :initform nil  :initarg :meta :documentation "Other info passing in task. e.g: tbeg/tend")

   (res        :initform nil  :documentation "Result of the transalte")
   (err        :initform nil  :initarg :err  :documentation "Error info")

   (engine     :initarg :engine     :initform nil)
   (render     :initarg :render     :initform nil)
   (translator :initarg :translator :initform nil)))

;; picker/texter

(defclass gts-picker ()
  ((single   :initarg :single :initform nil)
   (texter   :initarg :texter)
   (splitter :initarg :splitter :documentation "`gts-splitter' object or a function return it" :initform nil))
  "Used to pick the translation source text and sl/tl lang."
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
  "Start a translation for TRANSLATOR-OR-ENGINE.
ARGS should be text/path or task/callback.")

;; silence byte-compiler
(eieio-declare-slots tasks text sl tl)


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
  "Cache alive time based on this.
Make word live longer time than sentence."
  :type 'integer
  :group 'go-translate)

(defvar gts-default-cacher nil "The default cacher.")

(cl-defgeneric gts-cache-get (cacher task)
  "Get item for TASK from CACHER.")

(cl-defgeneric gts-cache-set (cacher task result)
  "Add RESULT fro TASK to CACHER.")

(cl-defmethod gts-cache-key ((_ gts-cacher) task)
  (with-slots (text sl tl engine) task
    (sha1 (format "%s-%s-%s-%s"
                  text sl tl
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
  (:method (&rest _) (user-error "Method `gts-request' is not implement")))

(defvar gts-default-http-client nil
  "The default http client used to send a request.")

(cl-defun gts-do-request (url &key done fail data headers)
  "Helper for `gts-request'.
DONE is a callback based on the responsed result buffer.
FAIL is the callback for any error occured in request or parse."
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
  "Get the source text and translate sl/tl path from PICKER.")

(cl-defgeneric gts-path (picker text)
  "Use to get the translation path for TEXT from PICKER.")

(cl-defgeneric gts-next-path (picker text path &optional backwardp)
  "Get the next available path.")

(cl-defgeneric gts-text (texter)
  "Get the init translation source text via TEXTER.")

(defvar gts-picker-last-path nil)

(defcustom gts-translate-list nil
  "Setup your translate languages. Just like:

Single:

 (setq gts-translate-list \\='((\"en\" \"zh\")))

Or multiple:

 (setq gts-translate-list \\='((\"en\" \"zh\") (\"en\" \"fr\")))

The picker will give the translate sl/tl pair according this."
  :type 'alist
  :group 'go-translate)

(defcustom gts-picker-lang-match-alist
  (list
   (cons "zh"  "\\cc")
   (cons "ja"  (lambda (text) (string-match-p "\\cj" text)))
   (cons "ru" "\\cy"))
  "Setup language match rules.
Used to pick the right language for current translate text.
Key should be the language, value should be a regexp string or function.
Picker choose path according this first. If non-match, choose other paths."
  :type 'alist
  :group 'go-translate)

(defun gts-picker-filter-paths (paths text)
  "Filter PATHS according TEXT.
The rules are provided by `gts-picker-lang-match-alist'."
  (when text
    (cl-loop with text = (with-temp-buffer ; clear punctuations
                           (insert text)
                           (goto-char (point-min))
                           (while (re-search-forward "\\s.\\|\n" nil t) (replace-match ""))
                           (buffer-string))
             for path in paths
             for matcher = (let ((p (cdr (cl-find-if ; get match rule
                                          (lambda (item) (string-match-p (format "^%s$" (car item)) (car path)))
                                          gts-picker-lang-match-alist))))
                             (if (stringp p) (lambda (text) (string-match-p p text)) p))
             when (and matcher (funcall matcher text))
             collect path)))

(cl-defmethod gts-paths ((picker gts-picker) &optional keep-order)
  "All paths can be picked by PICKER.
When KEEP-ORDER is not nil then do not try to push last path to the first place."
  (when (or (null gts-translate-list) (not (consp (car gts-translate-list))))
    (user-error "Please make sure you set the avaiable `gts-translate-list'. eg:\n
 (setq gts-translate-list '((\"en\" \"zh\") (\"en\" \"ja\"))\n\n"))
  (let (paths)
    (cl-loop for (lang1 . lang2) in gts-translate-list
             do (let ((lang2 (if (consp lang2) (car lang2) lang2)))
                  (cl-pushnew (cons lang1 lang2) paths :test #'equal)
                  (unless (oref picker single) (cl-pushnew (cons lang2 lang1) paths :test #'equal))))
    (setq paths (nreverse paths))
    (if (and (not keep-order)
             gts-picker-last-path (member gts-picker-last-path paths))
        (cons gts-picker-last-path (remove gts-picker-last-path paths))
      paths)))

(cl-defmethod gts-path ((picker gts-picker) text)
  "Default path for TEXT used by PICKER.
Return the first path matching TEXT. If no path matches, return the non-nil
`gts-picker-last-path', then the first available path."
  (if-let* ((paths (gts-paths picker))
            (matched (gts-picker-filter-paths paths text)))
      (car matched)
    (or (let ((langs (mapcar #'car gts-picker-lang-match-alist)))
          (cl-find-if (lambda (p) (not (member (car p) langs))) paths))
        (car paths))))

(cl-defmethod gts-next-path ((picker gts-picker) text path &optional backwardp)
  (let (candidates idx (paths (gts-paths picker 'keep-order)))
    (cl-loop for path in (append (gts-picker-filter-paths paths text) paths)
             do (cl-pushnew path candidates :test 'equal))
    (setq candidates (reverse candidates))
    (setq idx (cl-position path candidates :test 'equal))
    (if backwardp
        (elt candidates (if (= 0 idx) (- (length candidates) 1) (- idx 1)))
      (elt candidates (if (= (+ 1 idx) (length candidates)) 0 (+ 1 idx))))))


;;; Splitter

(cl-defgeneric gts-split (gts-splitter text)
  "Split TEXT to several parts.
Return list of string, or nil if split failed.")


;;; Engine/Parser

(cl-defmethod gts-translate ((engine gts-engine) _task _next)
  "Translate and update TASK with parsed result using ENGINE.
The NEXT should contain the parse and render logic."
  (user-error "Method `gts-translate' of %s is not implement" (eieio-object-class-name engine)))

(cl-defmethod gts-translate :around ((engine gts-engine) task next)
  "Add cache and log support."
  (with-slots (id res render translator) task
    (gts-do-log 'next (format "%s: prepare to translate" id))
    (if-let ((render-name (eieio-object-class-name render))
             (engine-name (eieio-object-class-name engine))
             (cache (gts-cache-get gts-default-cacher task)))
        ;; try cache
        (let ((engines (gts-get translator 'engines)))
          (setf res cache)
          (if (cdr engines)
              (gts-me-out render task)
            (gts-out render task))
          (gts-do-log 'render (format "%s: done with cache!" id)))
      ;; request from engine
      (cl-call-next-method engine task
                           (lambda (_)
                             (gts-do-log 'next (format "%s: %s translate finished!" id engine-name))
                             (funcall next task)
                             ;; refresh cache
                             (with-slots (res err) task
                               (unless err (gts-cache-set gts-default-cacher task res))))))))

(cl-defgeneric gts-with-token (engine done fail)
  (:documentation "Get token for ENGINE, if success then DONE is called.")
  (declare (indent 1)))

(cl-defgeneric gts-tts (engine text lang)
  "TTS, speak TEXT with LANG."
  (:method ((e gts-engine) &rest _)
           (user-error "No TTS service found on engine `%s'" (oref e tag))))

(cl-defgeneric gts-parse (parser task)
  (:documentation "Parse the raw result and update the parsed result into TASK.")
  (:method :before ((_ gts-parser) task)
           (gts-do-log 'next (format "%s: prepare to parse" (oref task id))))
  (:method :after ((parser gts-parser) task)
           (gts-do-log 'next (format "%s: %s finished" (oref task id) (eieio-object-class-name parser))))
  (:method ((_ gts-parser) task)
           ;; do nothing
           (cl-values (buffer-string))))


;;; Render

(cl-defgeneric gts-pre (render translator)
  (:documentation "Pre-render before request success.
Do some preparation for the gts-out.")
  (:method :after ((render gts-render) _)
           (gts-do-log 'render (format "prepare render with %s finished" (eieio-object-class-name render))))
  (:method ((_o gts-render) (_t gts-translator)) ()))

(cl-defgeneric gts-out (render task)
  (:documentation "Render the TASK, default output to *Messages*.")
  (:method :before ((render gts-render) task)
           (gts-do-log 'next (format "%s: prepare to render" (oref task id))))
  (:method :after ((render gts-render) task)
           (gts-do-log 'next (format "%s: %s render finished" (oref task id) (eieio-object-class-name render))))
  (:method ((_ gts-render) task)
           (with-slots (err res) task
             (when (and (not err) (listp res))
               (setq res (string-join res "\n\f\n")))
             (setq res (string-trim (format "%s" (or err res))))
             (setq res (propertize res 'face 'font-lock-keyword-face))
             (message "-+-G-+-T-+-S-+-")
             (message res ))))

(cl-defgeneric gts-me-pre (render translator)
  (:documentation "Pre-render for TRANSLATOR.
It used only when multiple engines exists.
Default dispatch to gts-pre when first pre-render.")
  (:method :after ((render gts-render) _)
           (gts-do-log 'render (format "me prepare render with %s finished" (eieio-object-class-name render))))
  (:method ((render gts-render) translator)
           (with-slots (tasks state) translator
             (when (= state 1)
               (gts-pre render translator)
               (setf state 2)))))

(cl-defgeneric gts-me-out (render task)
  (:documentation "Render TASK with RENDER.
It used only when multiple engines exists.
Default dispatch to gts-out with all results concated.")
  (:method :before ((render gts-render) task)
           (gts-do-log 'next (format "%s: prepare to render" (oref task id))))
  (:method :after ((render gts-render) task)
           (gts-do-log 'next (format "%s: %s render finished" (oref task id) (eieio-object-class-name render))))
  (:method ((render gts-render) task)
           (let ((translator (oref task translator)) results)
             ;; show only when all tasks finished.
             (when (= (oref translator state) 3)
               (dolist (task (oref translator task))
                 (with-slots (err res engine) task
                   (when (and (not err) (listp res))
                     (setq res (string-join res "\f")))
                   (push (cons (format "[%s]" (oref engine tag)) (or err res)) results)))
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
                 (gts-out render (gts-task :res concated :translator translator)))))))


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
                            :command (append (split-string gts-tts-speaker) (list "-"))
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

(defun gts-do-tts (text lang &optional engine)
  "Speak TEXT in LANG with possible tts service by ENGINE.
If engine failed, then try locally TTS if `gts-tts-try-speak-locally' is set."
  (condition-case err
      (if engine
          (if gts-tts-speaker
              (gts-tts engine text lang)
            (user-error "No mpv/mplayer found"))
        (user-error "No engine TTS service provided"))
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
(defvar gts-current-picker nil "Picker used in current translate.")
(defvar gts-current-engines nil "Engines used in current translate.")
(defvar gts-current-render nil "Render used in current translate.")

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
               ('picker     (ensure (gts-ensure-plain (or gts-current-picker (slot-value this slot)))))
               ('engines    (ensure (gts-ensure-list (gts-ensure-plain (or gts-current-engines (slot-value this slot))))))
               ('render     (ensure (gts-ensure-plain (or gts-current-render (slot-value this slot)))))
               (_           (slot-value this slot))))
           collect value into values
           finally (return (if (cadr slots) (apply #'cl-values values) (car values)))))

(defun gts-add-task (translator task)
  "Add new TASK to TRANSLATOR."
  (with-slots (id engine render) task
    (with-slots (tasks total state) translator
      (when (= state 0)
        (setf tasks (append tasks (list task)))
        (gts-do-log 'translator
          (format "[%d/%d] add task %s: (%s/%s)"
                  (length tasks) total id
                  (eieio-object-class-name engine) (eieio-object-class-name render)))
        (when (= (length tasks) total)
          (setf state 1))))))

(defun gts-done-tasks (translator)
  "Return the finished tasks in TRANSLATOR."
  (with-slots (tasks) translator
    (cl-remove-if-not (lambda (task) (or (oref task err) (oref task res))) tasks)))

(defun gts-fail (task error)
  "Render ERROR message and finish the TASK. TAG is used for log."
  (declare (indent 1))
  (gts-do-log 'next (format "%s: FAIL!!!  %s" (oref task id) error))
  (with-slots (translator render err) task
    (setf err error)
    (if (cdr (gts-get translator 'engines))
        (gts-me-out render task)
      (gts-out render task))))

(defun gts-next (task)
  (condition-case err
      (with-slots (res engine translator id) task
        (cl-multiple-value-bind (render engines tasks total state)
            (gts-get translator 'render 'engines 'tasks 'total 'state)
          ;; parse
          (cl-multiple-value-bind (result info filter)
              (progn (goto-char (point-min))
                     (gts-parse (oref engine parser) task))
            (if info (setf meta (append info meta)))
            (if filter (setq result (funcall filter result)))
            (setf res result)
            ;; state
            (when (and (= state 2) (= total (length (gts-done-tasks translator))))
              (setf state 3)
              (gts-do-log 'translator "All tasks done"))
            ;; render
            (if (cdr engines)
                (gts-me-out render task)
              (gts-out render task)))))
    (gts-fail task err)))

(cl-defmethod gts-translate ((this gts-translator) &optional text path)
  "Fire a translation for THIS translator instance.
When TEXT and PATH is nil then pick them via `gts-pick'."
  ;; dynamic
  (setq gts-current-command this-command
        gts-current-picker (oref this picker)
        gts-current-engines (oref this engines)
        gts-current-render (oref this render))
  ;; init input
  (unless (and text path)
    (cl-multiple-value-setq (text path) (gts-pick (gts-get this 'picker))))
  (oset this text text)
  (oset this path path)
  (setq gts-picker-last-path path)
  (gts-do-log 'translator (format "\nBegin to translate %s" path))
  (gts-do-log 'text text)
  (cl-multiple-value-bind (engines render tasks) (gts-get this 'engines 'render 'tasks)
    (let ((sl (car path)) (tls (gts-ensure-list (cdr path))) (buf (current-buffer)))
      ;; init translator
      (oset this state 0)
      (oset this tasks nil)
      (oset this total (* (length engines) (length tls)))
      (oset this version (time-to-seconds))
      ;; init tasks
      (cl-loop for engine in engines
               do (cl-loop for tl in tls
                           for task = (gts-task :text text
                                                :sl sl
                                                :tl tl
                                                :engine engine
                                                :render render
                                                :translator this)
                           do (gts-add-task this task)))
      ;; translate (single engine)
      (if (not (cdr engines))
          (let ((task (car tasks)))
            (gts-pre render this)
            (gts-translate (car engines) task #'gts-next))
        ;; translate (multiple engines)
        (gts-me-pre render this)
        (dolist (task tasks)
          (condition-case err
              (gts-translate (oref task engine) task #'gts-next)
            (error (gts-fail task err))))
        (gts-me-out render (gts-task :text text :sl (car path) :translator this))))))

(provide 'gts-core)

;;; gts-core.el ends here
