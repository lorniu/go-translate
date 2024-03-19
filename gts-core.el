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

(defcustom gts-debug-p nil
  "Weather enable debuging."
  :type 'boolean
  :group 'go-translate)


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

(defmacro gts-with-slots (spec-list object &rest body)
  (declare (indent 2))
  `(with-slots ,spec-list ,object
     (when (typep ,object 'gts-translator)
       ,@(cl-loop for slot in spec-list
                  for err = `(user-error "No %s found in current translator" ,slot)
                  when (equal `,slot 'picker)
                  collect `(setq ,slot (or (gts-ensure-plain (or gts-current-picker ,slot)) ,err))
                  when (equal `,slot 'render)
                  collect `(setq ,slot (or (gts-ensure-plain (or gts-current-render ,slot)) ,err))
                  when (equal `,slot 'engines)
                  collect `(setq ,slot (or (gts-ensure-list (gts-ensure-plain (or gts-current-engines ,slot))) ,err))))
     ,(if body `(progn ,@body) (car spec-list))))


;;; Logging

(defvar gts-log-buffer-name " *gts-logger*"
  "Log buffer for translation")

(defun gts-log (tag &rest messages)
  "Log MESSAGES to `gts-log-buffer-name'.
TAG is a label for message being logged."
  (declare (indent 1))
  (when gts-debug-p
    (cl-flet ((log (tag msg)
                (with-current-buffer (get-buffer-create gts-log-buffer-name)
                  (goto-char (point-max))
                  (if (or (= (length msg) 0) (string= msg "\n"))
                      (insert "")
                    (insert
                     (propertize (cl-subseq (format "%-.1f" (time-to-seconds)) 6) 'face 'gts-logger-buffer-timestamp-face)
                     " "
                     (propertize (concat "[" tag "] ") 'face 'gts-logger-buffer-tag-face))
                    (insert msg))
                  (insert "\n"))))
      (cl-loop with ms = nil
               for message in messages
               if message do (cl-loop for m in (split-string message "\n") do (push m ms))
               finally (cl-loop for m in (nreverse ms) do (log (format "%s" tag) m))))))


;;; Caching

(defcustom gts-cache-enable t
  "Weather enable caching the translate result."
  :type 'boolean
  :group 'go-translate)

(defcustom gts-cache-expired-factor (* 30 60)
  "Cache alive time based on this.
Make word live longer time than sentence."
  :type 'integer
  :group 'go-translate)

(defvar gts-default-cacher nil)

(defclass gts-cacher ()
  ((caches  :initform nil)) ; (key . (str . timestamp))
  "Used to cache the translate results."
  :abstract t)

(cl-defgeneric gts-cache-get (cacher task)
  "Get item for TASK from CACHER."
  (:method (&rest _)
           (when gts-cache-enable
             (error "Make sure `gts-default-cacher' is properly configured. eg:\n
 (setq gts-default-cacher (gts-memory-cacher))"))))

(cl-defgeneric gts-cache-set (cacher task result)
  "Add RESULT fro TASK to CACHER."
  (:method (&rest _)
           (when gts-cache-enable
             (error "Make sure `gts-default-cacher' is properly configured. eg:\n
 (setq gts-default-cacher (gts-memory-cacher))"))))

(defun gts-cache-key (task)
  "Generate a key for caching TASK."
  (with-slots (text sl tl engine) task
    (sha1 (format "%s-%s-%s-%s"
                  text sl tl
                  (eieio-object-class-name engine)))))

(defun gts-cache-clear-expired (cacher)
  "Clear CACHER items that are expired."
  (when cacher
    (with-slots (caches expired) cacher
      (cl-delete-if (lambda (c) (> (time-to-seconds) (cddr c))) caches))))

(defun gts-cache-clear-all (cacher)
  "Clear all items of CACHER."
  (when cacher
    (oset cacher caches nil)))

;; cacher implement by caching in memory

(defclass gts-memory-cacher (gts-cacher) ()
  "Cache in the memory.")

(cl-defmethod gts-cache-get ((cacher gts-memory-cacher) task)
  (when gts-cache-enable
    (with-slots (caches expired) cacher
      (let ((key (gts-cache-key task)))
        (when-let ((cache (assoc key caches)))
          (when (> (cddr cache) (time-to-seconds))
            (gts-log 'memory-cacher (format "%s: get from cache %s (%s)" (oref task id) key (length caches)))
            (cadr cache)))))))

(cl-defmethod gts-cache-set ((cacher gts-memory-cacher) task result)
  (when gts-cache-enable
    (with-slots (caches expired) cacher
      (let* ((key (gts-cache-key task))
             (cache (assoc key caches))
             (text (oref task text))
             (etime
              ;; sentence with shorter expired time, but word with longer.
              (if (string-match-p "[[:space:]\n]" text)
                  (+ (time-to-seconds) gts-cache-expired-factor)
                (+ (time-to-seconds) (* 100 gts-cache-expired-factor)))))
        (if cache
            (progn
              (setf (cadr cache) result)
              (setf (cddr cache) etime)
              (gts-log 'memory-cacher (format "%s: update cache %s (%s)" (oref task id) key (length caches))))
          (oset cacher caches (cons (cons key (cons result etime)) caches))
          (gts-log 'memory-cacher (format "%s: add to cache %s (%s)" (oref task id) key (length caches))))
        (gts-cache-clear-expired cacher)))))

(setq gts-default-cacher (gts-memory-cacher)) ; use this as default


;;; Http Client

(defvar gts-default-http-client nil)

(defclass gts-http-client () ()
  "Used to send a request."
  :abstract t)

(cl-defgeneric gts-request (http-client &key url done fail data headers)
  "Use HTTP-CLIENT to request a URL with DATA.
When success execute CALLBACK, or execute ERRORBACK."
  (:method (&key url done fail data headers)
           (if (and gts-default-http-client
                    (eieio-object-p gts-default-http-client)
                    (object-of-class-p gts-default-http-client 'gts-http-client))
               (let ((tag (format "%s" (eieio-object-class-name gts-default-http-client)))
                     (data (gts-format-params data)))
                 (gts-log tag
                   (format "> %s" url)
                   (if headers (format "> HEADER: %s" headers))
                   (if data (format "> DATA:   %s" data)))
                 (gts-request gts-default-http-client
                              :url url
                              :headers headers
                              :data data
                              :done (lambda (&rest _)
                                      (gts-log tag (format "✓ %s" url))
                                      (goto-char (point-min))
                                      (condition-case err
                                          (funcall done)
                                        (error
                                         (gts-log tag (format "Request success but fail in callback! (%s) %s" url err))
                                         (funcall fail err))))
                              :fail (lambda (status)
                                      (gts-log tag (format "Request fail! (%s) %s" url status))
                                      (funcall fail status))))
             (funcall fail "Make sure `gts-default-http-client' is available. eg:\n
 (setq gts-default-http-client (gts-url-http-client))\n\n\n"))))

;; http client implemented using `url.el'

(defclass gts-url-http-client (gts-http-client) ())

(defcustom gts-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36"
  "The user agent used when send a request."
  :type 'string
  :group 'go-translate)

(defvar url-http-end-of-headers)

(cl-defmethod gts-request ((_ gts-url-http-client) &key url done fail data headers)
  "Request URL with DATA and HEADERS asynchronous.
Execute DONE when success, or FAIL when failed."
  (cl-assert (and url done fail))
  (let ((inhibit-message t)
        (message-log-max nil)
        (url-debug gts-debug-p)
        (url-user-agent gts-user-agent)
        (url-request-extra-headers headers)
        (url-request-method (if data "POST" "GET"))
        (url-request-data data))
    (url-retrieve url (lambda (status)
                        (unwind-protect
                            (if-let ((err (cond
                                           ((eq (car status) :error) (cadr status))
                                           ((or (null url-http-end-of-headers) (= 1 (point-max)))
                                            "Nothing responsed from server"))))
                                (funcall fail err)
                              (delete-region (point-min) url-http-end-of-headers)
                              (funcall done))
                          (kill-buffer)))
                  nil t)))

(setq gts-default-http-client (gts-url-http-client)) ; use this as default


;;; Components

(defclass gts-translator ()
  ((picker     :initarg :picker   :documentation "`gts-picker' object or a function return it" :initform nil)
   (engines    :initarg :engines  :documentation "A list of `gts-engine' objects or a function return them" :initform nil)
   (render     :initarg :render   :documentation "`gts-render' object or a function return it" :initform nil)

   (text       :initarg :text     :documentation "Text to be translated. Get from picker, string or list")
   (sl         :initarg :sl)
   (tl         :initarg :tl)

   (tasks      :initform nil      :documentation "Task queue in a translation")
   (total      :initform 0        :documentation "Count of all tasks in a translation")
   (state      :initform 0        :documentation "Inner state of the translator:
0: new translator
1: all tasks added
2: render prepared
3: all result parsed")
   (version    :initform nil      :documentation "Used to distinguish between different translations")))

(defclass gts-task ()
  ((id         :initform (gensym "task-"))

   (text       :initarg :text :documentation "Text to be translated")
   (sl         :initarg :sl   :documentation "Source language")
   (tl         :initarg :tl   :documentation "Target language")

   (meta       :initform nil  :initarg :meta :documentation "Other info passing in task. e.g: tbeg/tend")
   (version    :initform nil  :initarg :version)

   (res        :initform nil  :initarg :res  :documentation "Result of the transalte")
   (err        :initform nil  :initarg :err  :documentation "Error info")

   (engine     :initarg :engine     :initform nil)
   (render     :initarg :render     :initform nil)
   (translator :initarg :translator :initform nil)))

(defclass gts-picker ()
  ((single   :initarg :single :initform nil)
   (texter   :initarg :texter))
  "Used to pick the translation source text and sl/tl langs."
  :abstract t)

(defclass gts-texter ()
  ()
  "Used to get the initial translation text.
Current word under cursor? Selection region? Whole line? Whole buffer? Others?
You can implements your rules.")

(defclass gts-engine ()
  ((tag       :initarg :tag    :documentation "Used to display as name")
   (parser    :initarg :parser :documentation "Used to parse the response result"))
  :abstract t)

(defclass gts-parser ()
  ((tag  :initarg :tag :initform nil)))

(defclass gts-render () ())


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


;;; Engine/Parser

(cl-defmethod gts-translate ((engine gts-engine) _task _next)
  "Translate and update TASK with parsed result using ENGINE.
The NEXT should contain the parse and render logic."
  (user-error "Method `gts-translate' of %s is not implement" (eieio-object-class-name engine)))

(cl-defmethod gts-translate :around ((engine gts-engine) task next)
  "Add cache and log support."
  (with-slots (id text render translator) task
    (setf text (string-join (gts-ensure-list text) gts-text-delimiter))
    (if-let ((render-name (eieio-object-class-name render))
             (engine-name (eieio-object-class-name engine))
             (cache (gts-cache-get gts-default-cacher task)))
        ;; try cache
        (with-temp-buffer
          (insert cache)
          (gts-log 'render (format "%s: get result from cache!" id))
          (funcall next task))
      ;; request from engine
      (gts-log 'next (format "%s: %s prepare to translate" id engine-name))
      (cl-call-next-method engine task
                           (lambda (task)
                             (gts-log 'next (format "%s: %s translate success!" id engine-name))
                             (unless (oref task err)
                               (gts-cache-set gts-default-cacher task (buffer-string)))
                             (funcall next task))))))

(cl-defgeneric gts-tts (engine text lang)
  "TTS, speak TEXT with LANG."
  (:method ((e gts-engine) &rest _)
           (user-error "No TTS service found on engine `%s'" (oref e tag))))

(cl-defgeneric gts-parse (parser task)
  (:documentation "Parse the raw result and update the parsed result into TASK.")
  (:method :before ((_ gts-parser) task)
           (gts-log 'next (format "%s: prepare to parse" (oref task id))))
  (:method :after ((parser gts-parser) task)
           (gts-log 'next (format "%s: %s finished." (oref task id) (eieio-object-class-name parser))))
  (:method ((_ gts-parser) task)
           ;; do nothing
           (cl-values (buffer-string))))


;;; Render

(cl-defgeneric gts-pre (render translator)
  (:documentation "Pre-render before request success.")
  (:method :after ((_ gts-render) translator) (gts-update-state translator))
  (:method ((_o gts-render) (_t gts-translator)) ()))

(cl-defgeneric gts-out (render task)
  (:documentation "Render the TASK, default output to *Messages*.")
  (:method :before ((render gts-render) task)
           (gts-log 'next (format "%s: prepare to render" (oref task id))))
  (:method :after ((render gts-render) task)
           (gts-log 'next (format "%s: %s finished." (oref task id) (eieio-object-class-name render))))
  (:method ((_ gts-render) task)
           (with-slots (err res) task
             (when (and (not err) (listp res))
               (setq res (string-join res "\n\f\n")))
             (setq res (string-trim (format "%s" (or err res))))
             (setq res (propertize res 'face 'font-lock-keyword-face))
             (message "\nTranslate result:\n")
             (message res ))))

(cl-defgeneric gts-multi-pre (render translator)
  (:documentation "Pre-render for TRANSLATOR.
It used only when multiple engines exists.
Default dispatch to gts-pre when first pre-render.")
  (:method :after ((_ gts-render) translator)
           (gts-update-state translator))
  (:method ((render gts-render) translator)
           (with-slots (state) translator
             (if (= state 1) (gts-pre render translator)))))

(cl-defgeneric gts-multi-out (render task)
  (:documentation "Render TASK with RENDER.
It used only when multiple engines exists.
Default dispatch to gts-out with all results concated.")
  (:method :before ((render gts-render) task)
           (gts-log 'next (format "%s: prepare to render" (oref task id))))
  (:method :after ((render gts-render) task)
           (gts-log 'next (format "%s: %s finished." (oref task id) (eieio-object-class-name render))))
  (:method ((render gts-render) task)
           (let ((translator (oref task translator)))
             ;; show only when all tasks finished.
             (if (= (oref translator state) 3)
                 (let (results)
                   (dolist (task (oref translator tasks))
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
                                               (if newlinep "\n\n" "\n")))
                          (text (oref translator text)))
                     (message "")
                     (gts-out render (gts-task :text text :res concated :translator translator))))
               (message "Processing...")))))


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

(defconst gts-text-delimiter "\n34587612321123\n")

(defvar gts-current-command nil "The command invoked by `gts-translate'.")
(defvar gts-current-picker nil "Picker used in current translate.")
(defvar gts-current-engines nil "Engines used in current translate.")
(defvar gts-current-render nil "Render used in current translate.")

(cl-defmethod initialize-instance :after ((this gts-translator) &rest _)
  (unless (gts-ensure-plain (oref this render))
    (oset this render (gts-render)))
  (unless (gts-ensure-plain (oref this picker))
    (oset this picker (gts-noprompt-picker))))

(defun gts-add-task (task)
  "Add new TASK for translation."
  (with-slots (id engine render version translator) task
    (with-slots (tasks total state) translator
      (when (= state 0)
        (setf tasks (append tasks (list task)))
        (gts-log (format "%d/%d" (length tasks) total id)
          (format "add task %s: (%s/%s)" id (eieio-object-class-name engine) (eieio-object-class-name render)))
        (gts-update-state translator)))))

(defun gts-done-tasks (translator)
  "Return the finished tasks in TRANSLATOR."
  (with-slots (tasks) translator
    (cl-remove-if-not (lambda (task) (or (oref task err) (oref task res))) tasks)))

(defun gts-update-state (translator)
  (with-slots (state total) translator
    (pcase state
      (0
       (when (= (length (gts-with-slots (tasks) translator)) total)
         (gts-log 'translator "<1> all tasks added")
         (setf state 1)))
      (1
       (gts-log 'translator
         (format "<2> %s prepared" (eieio-object-class-name (gts-with-slots (render) translator))))
       (setf state 2))
      (2
       (when (= total (length (gts-done-tasks translator)))
         (gts-log 'translator "<3> all result parsed")
         (setf state 3)))
      (_ (setf state 0)))))

(defun gts-fail (task error)
  "Render ERROR message and ternimate the TASK."
  (declare (indent 1))
  (with-slots (translator render err version id) task
    (when (equal version (oref translator version))
      (setf err (if (listp error) (cadr error) error))
      (gts-update-state translator)
      (if (cdr (gts-with-slots (engines) translator))
          (gts-multi-out render task)
        (gts-out render task)))
    (gts-log 'next (format "%s: ----- error -----\n %s" id error))))

(defun gts-next (task)
  (with-slots (res meta engine translator version id) task
    (gts-with-slots (render engines tasks total state text) translator
      (if (equal version (oref translator version))
          (condition-case err
              ;; parse
              (cl-multiple-value-bind (result info filter)
                  (gts-parse (oref engine parser) task)
                (if info (setf meta (append info meta)))
                (if filter (setq result (funcall filter result)))
                (when (and (listp text) (cdr text))
                  (setq result (split-string result gts-text-delimiter))
                  (oset task text text))
                (setf res result)
                ;; state
                (gts-update-state translator)
                ;; render
                (if (cdr engines)
                    (gts-multi-out render task)
                  (gts-out render task)))
            (gts-fail task err))
        (gts-log 'next (format "%s: ----- expired -----" id))))))

(cl-defmethod gts-translate ((this gts-translator) &optional text sl tl)
  "Fire a translation for THIS translator instance.
When TEXT and PATH is nil then pick them via `gts-pick'."
  ;; dynamic
  (setq gts-current-command this-command
        gts-current-picker (oref this picker)
        gts-current-engines (oref this engines)
        gts-current-render (oref this render))
  ;; init input
  (unless (and text tl)
    (cl-multiple-value-setq (text tl sl) (gts-pick (gts-with-slots (picker) this))))
  (oset this text text)
  (oset this sl sl)
  (oset this tl tl)
  ;;(setq gts-picker-last-path path)
  (let ((buf (current-buffer))
        (ver (time-to-seconds))
        (tls (gts-ensure-list tl)))
    (gts-log 'translator (format "\n\n%s\n%s . %s\n%s" ver sl tl text))
    (gts-with-slots (engines render) this
      ;; init translator
      (oset this state 0)
      (oset this tasks nil)
      (oset this total (* (length engines) (length tls)))
      (oset this version ver)
      ;; init tasks
      (cl-loop for engine in engines
               do (cl-loop for tl in tls
                           for task = (gts-task
                                       :text text
                                       :sl sl
                                       :tl tl
                                       :engine engine
                                       :render render
                                       :translator this
                                       :version ver)
                           do (gts-add-task task)))
      (let ((tasks (oref this tasks)))
        ;; translate (single engine)
        (if (not (cdr engines))
            (let ((task (car tasks)))
              (gts-pre render this)
              (gts-translate (car engines) task #'gts-next))
          ;; translate (multiple engines)
          (gts-multi-pre render this)
          (dolist (task tasks)
            (condition-case err
                (gts-translate (oref task engine) task #'gts-next)
              (error (gts-fail task err)))))))))

(provide 'gts-core)

;;; gts-core.el ends here
