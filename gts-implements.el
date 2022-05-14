;;; gts-implements.el --- Component implements -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Builtin implements of basic components

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'gts-core)
(require 'gts-faces)


;;; [Logger] Log all to a standalone buffer

(defvar gts-logger-buffer-name " *gts-logger*")

(defclass gts-buffer-logger (gts-logger)
  ((buffer :initarg buf :initform nil))) ; dont set :initarg to buffer, it will make an error on emacsv27.

(cl-defmethod gts-log ((logger gts-buffer-logger) tag msg)
  (with-current-buffer (get-buffer-create (or (oref logger buffer) gts-logger-buffer-name))
    (goto-char (point-max))
    (if (or (= (length msg) 0) (string= msg "\n"))
        (insert "")
      (insert
       (propertize (cl-subseq (format "%-.1f" (time-to-seconds)) 6) 'face 'gts-logger-buffer-timestamp-face)
       " "
       (propertize (concat "[" tag "] ") 'face 'gts-logger-buffer-tag-face))
      (insert msg))
    (insert "\n")))

(setq gts-default-logger (gts-buffer-logger))


;;; [Cacher] Cache in the memory

(defclass gts-memory-cacher (gts-cacher) ()
  "Cache in the memory.")

(cl-defmethod gts-cache-get ((cacher gts-memory-cacher) task)
  (when gts-cache-enable
    (with-slots (caches expired) cacher
      (let ((key (gts-cache-key cacher task)))
        (when-let ((cache (assoc key caches)))
          (when (> (cddr cache) (time-to-seconds))
            (gts-do-log 'memory-cacher (format "%s: get from cache %s (%s)" (oref task id) key (length caches)))
            (cadr cache)))))))

(cl-defmethod gts-cache-set ((o gts-memory-cacher) task result)
  (when gts-cache-enable
    (with-slots (caches expired) o
      (let* ((key (gts-cache-key o task))
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
              (gts-do-log 'memory-cacher (format "%s: update cache %s (%s)" (oref task id) key (length caches))))
          (oset o caches (cons (cons key (cons result etime)) caches))
          (gts-do-log 'memory-cacher (format "%s: add to cache %s (%s)" (oref task id) key (length caches))))
        (gts-clear-expired o)))))

(setq gts-default-cacher (gts-memory-cacher))


;;; [Http Client] implement with builtin `url.el'.
;; This will be used as the default http client.

(defcustom gts-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36"
  "The user agent used when send a request."
  :type 'string
  :group 'go-translate)

(defvar url-http-end-of-headers)

(defclass gts-url-http-client (gts-http-client) ())

(cl-defmethod gts-request ((_ gts-url-http-client) url &key done fail data headers)
  "Request URL with DATA asynchronous.
Execute CALLBACK when success, or ERRORBACK when failed."
  (let ((url-debug gts-debug-p)
        (url-user-agent gts-user-agent)
        (url-request-extra-headers headers)
        (url-request-method (if data "POST" "GET"))
        (url-request-data data))
    (url-retrieve url (lambda (status)
                        (unwind-protect
                            (if (and fail (eq (car status) :error))
                                (funcall fail (cdr status))
                              (when done
                                (delete-region (point-min) url-http-end-of-headers)
                                (funcall done)))
                          (kill-buffer)))
                  nil t)))

(setq gts-default-http-client (gts-url-http-client)) ; use this as default


;;; [Render] buffer render

(defcustom gts-split-width-threshold 80
  "Threshold width for window horizontal split."
  :group 'go-translate
  :type '(choice
	  (const :tag "Disable" nil)
	  (integer :tag "Threshold")))

(defcustom gts-buffer-follow-p t
  "If t then pop to the result window after translation."
  :type 'boolean
  :group 'go-translate)

(defcustom gts-buffer-name "*Go-Translate*"
  "The name of translation result buffer."
  :type 'string
  :group 'go-translate)

(defcustom gts-buffer-window-config nil
  "Window configuration used by the result buffer window.

For example, set to:

 '((display-buffer-reuse-window display-buffer-in-side-window)
   (side . right))

will force opening in right side window."
  :type 'list
  :group 'go-translate)

(defcustom gts-buffer-render-task-header-function #'gts-buffer-render-task-header
  "Function used to render the task header when in multiple engines's query."
  :type 'function
  :group 'go-translate)

(defvar-local gts-buffer-keybinding-messages nil)

(cl-defmacro gts-buffer-set-key ((key &optional desc) form)
  "Helper for define KEY in buffer."
  (declare (indent 1))
  `(progn (local-set-key
           (kbd ,key)
           ,(if (equal 'function (car form)) `,form `(lambda () (interactive) ,form)))
          (when ,desc
            (cl-delete ,key gts-buffer-keybinding-messages :key #'car :test #'string=)
            (push (cons ,key ,desc) gts-buffer-keybinding-messages))))

(defun gts-buffer-init-header-line (from to &optional desc)
  "Setup header line format.
It will show the basic information of the translation,
including FROM/TO and other DESC."
  (setq header-line-format
        (list
         " "
         "[" (propertize from 'face 'gts-render-buffer-header-line-lang-face) "]"
         (if desc (concat " ― " (propertize desc 'face 'gts-render-buffer-header-line-desc-face)) "")
         " → "
         "[" (propertize to 'face 'gts-render-buffer-header-line-lang-face) "]"
         "   (" (propertize "h" 'face 'font-lock-type-face) " for help)"
         "          "
         "Loading...")))

(defun gts-buffer-change-header-line-state (type)
  "Update header line according TYPE."
  (cl-case type
    (done
     (when (string-equal (car (last header-line-format)) "Loading...")
       (setq header-line-format (butlast header-line-format))))))

(defun gts-buffer-display-or-focus-buffer (buffer)
  "Only display the BUFFER, or display and focus."
  (with-current-buffer buffer
    (if gts-buffer-follow-p (pop-to-buffer buffer)
      (display-buffer buffer))))

(defun gts-buffer-ensure-a-blank-line-at-beginning (buffer)
  "Ensure a blank line at beginning of BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (let ((m (mark-marker)))
        (goto-char (point-min))
        (unless (equal (char-after) ?\n)
          (insert "\n"))
        (when (ignore-errors (= (marker-position m) 1))
          (setf (marker-position m) (point)))))))

(defun gts-childframe-of-buffer (&optional buffer)
  "Get the childframe of BUFFER if it exists, or return nil."
  (let ((cf (window-frame (get-buffer-window buffer t))))
    (when (frame-parameter cf 'parent-frame) cf)))

(defun gts-buffer-render-task-header (engine-tag parser-tag)
  (concat (propertize
           (format "%s %s\n" engine-tag (if parser-tag (format "(%s) " parser-tag) ""))
           'face 'gts-render-buffer-me-header-backgroud-face)))

;; functions

(defun gts-render-buffer-prepare (buffer task)
  (with-current-buffer (get-buffer-create buffer)
    (with-slots (text from to translator engine) task
      ;; setup
      (deactivate-mark)
      (read-only-mode -1)
      (visual-line-mode -1)
      (setq-local cursor-type 'hbar)
      (setq-local cursor-in-non-selected-windows nil)
      ;; headline
      (gts-buffer-init-header-line from to (when (= (oref translator plan-cnt) 1)
                                             (oref engine tag)))
      ;; source text
      (erase-buffer)
      (unless (gts-childframe-of-buffer (current-buffer)) (insert "\n")) ; except childframe
      (insert text)
      ;; keybinds.
      ;;  q for quit and kill the window.
      ;;  x for switch sl and tl.
      ;;  M-n and M-p, translate with next/prev direction.
      ;;  g for refresh.
      ;;  y to speak the current selection or word.
      (gts-buffer-set-key ("M-n" "Next direction")
        (let ((next (gts-next-path (oref translator picker) text from to)))
          (gts-translate translator text (car next) (cdr next))))
      (gts-buffer-set-key ("M-p" "Prev direction")
        (let ((prev (gts-next-path (oref translator picker) text from to t)))
          (gts-translate translator text (car prev) (cdr prev))))
      (gts-buffer-set-key ("y" "TTS")
        (if-let ((eg (if (> (oref translator plan-cnt) 1)
                         (get-text-property (point) 'engine)
                       engine)))
            (gts-do-tts text from (lambda () (gts-tts eg text from)))
          (message "[TTS] No engine found at point")))
      (gts-buffer-set-key ("g" "Refresh")          (gts-translate translator text from to))
      (gts-buffer-set-key ("x" "Reverse-Translate") (gts-translate translator text to from))
      (gts-buffer-set-key ("C" "Clean Cache")       (gts-clear-all gts-default-cacher))
      (gts-buffer-set-key ("q" "Quit") #'kill-buffer-and-window)
      (gts-buffer-set-key ("C-g")
        (unwind-protect
            (gts-tts-try-interrupt-playing-process)
          (keyboard-quit)))
      (gts-buffer-set-key ("h")
        (message (mapconcat
                  (lambda (kd) (concat (propertize (car kd) 'face 'font-lock-keyword-face) ": " (cdr kd)))
                  (reverse gts-buffer-keybinding-messages) " ")))
      ;; execute the hook if exists
      (run-hooks 'gts-after-buffer-prepared-hook))))

(defun gts-render-buffer (buffer task)
  "For single engine translation."
  (when-let ((buf (get-buffer buffer)))
    (with-current-buffer buf
      (with-slots (result ecode engine) task
        (if ecode
            (progn ;; error display
              (goto-char (point-max))
              (insert (propertize (format "\n\n\n%s" result) 'face 'gts-render-buffer-error-face)))
          ;; content
          (erase-buffer)
          (insert result)
          ;; try set mark in beginning of the translate result,
          ;; and set currsor position in end of the translate result,
          ;; so you can quick select the translate result with C-x C-x.
          (unless (gts-childframe-of-buffer buf) ; when not childframe
            (when-let* ((meta (get-text-property 0 'meta result))
                        (tbeg (plist-get meta :tbeg))
                        (tend (plist-get meta :tend)))
              (push-mark tbeg 'nomsg)
              (goto-char tend)
              (set-window-point (get-buffer-window buf t) tend))))
        ;; update states
        (set-buffer-modified-p nil)
        (gts-buffer-change-header-line-state 'done)
        (put-text-property (point-min) (point-max) 'engine engine)
        ;; execute the hook if exists
        (run-hooks 'gts-after-buffer-render-hook))
      buf)))

(defun gts-render-buffer-multi-engines (buffer translator task)
  "For multiple engines translation."
  (when-let ((buf (and task (get-buffer buffer))))
    (with-slots (text from to) task
      (with-slots (plan-cnt task-queue engines) translator
        (with-current-buffer buf
          (erase-buffer)
          (insert (propertize (format "\n%s\n\n" text) 'face 'gts-render-buffer-source-face))
          ;; content
          (save-excursion
            (cl-loop for task in task-queue
                     for result = (oref task result)
                     for ecode = (oref task ecode)
                     for engine = (oref task engine)
                     for engine-tag = (oref engine tag)
                     for parser = (oref engine parser)
                     for parser-tag = (oref parser tag)
                     for header = (funcall gts-buffer-render-task-header-function engine-tag parser-tag)
                     for content = (cond ((null result)
                                          (concat header "\nLoading...\n\n"))
                                         (ecode
                                          (concat header
                                                  (propertize
                                                   ;; (msg . code) (http code msg)
                                                   (format "\n%s\n\n" result) 'face 'gts-render-buffer-error-face)))
                                         (t (let* ((meta (get-text-property 0 'meta result))
                                                   (send (plist-get meta :send))
                                                   (tbeg (plist-get meta :tbeg))
                                                   (last (concat
                                                          header "\n"
                                                          (if (and tbeg send (equal 10 (aref result (- send 1))))
                                                              (cl-subseq result (1- tbeg)) result) ; hide source text in me output
                                                          "\n\n")))
                                              (put-text-property 0 (length last) 'engine engine last)
                                              last)))
                     do (insert content)))
          ;; states
          (set-buffer-modified-p nil)
          ;; all tasks finished
          (when (= (oref translator state) 3)
            ;; cursor
            (unless (gts-childframe-of-buffer buf)
              (set-window-point (get-buffer-window buf t) (save-excursion (forward-line 2) (point))))
            ;; state
            (gts-buffer-change-header-line-state 'done))
          ;; execute the hook if exists
          (run-hooks 'gts-after-buffer-multiple-render-hook)))
      buf)))

;; component

(defclass gts-buffer-render (gts-render) ())

(cl-defmethod gts-pre ((_ gts-buffer-render) task)
  ;; init and setup
  (gts-render-buffer-prepare gts-buffer-name task)
  ;; add keybind, toggle follow
  (with-current-buffer gts-buffer-name
    (gts-buffer-set-key ("t" "Toggle-Follow")
      (message "Now, buffer following %s."
               (if (setq gts-buffer-follow-p (not gts-buffer-follow-p)) "allowed" "disabled"))))
  ;; display
  (let ((split-width-threshold (or gts-split-width-threshold split-width-threshold)))
    (display-buffer gts-buffer-name gts-buffer-window-config)))

(cl-defmethod gts-out ((_ gts-buffer-render) task)
  ;; render & display
  (when-let ((buf (gts-render-buffer gts-buffer-name task)))
    (gts-buffer-ensure-a-blank-line-at-beginning buf)
    (gts-buffer-display-or-focus-buffer buf)))

(cl-defmethod gts-me-out ((_ gts-buffer-render) translator task)
  ;; render & display
  (when-let ((buf (gts-render-buffer-multi-engines gts-buffer-name translator task)))
    (when (= (oref translator state) 3)
      (gts-buffer-display-or-focus-buffer buf))))


;;; [Render] Child-Frame Render (Popup Mode)
;; implements via package Posframe, you should install it before use this

(require 'posframe nil t)
(declare-function posframe-show "ext:posframe.el" t t)
(declare-function posframe-delete "ext:posframe.el" t t)
(declare-function posframe-hide "ext:posframe.el" t t)
(declare-function posframe-refresh "ext:posframe.el" t t)
(declare-function posframe-poshandler-frame-top-right-corner "ext:posframe.el" t t)

(defun gts-posframe-init-header-line (from to)
  (setq header-line-format
        (list "[" (propertize from 'face 'gts-render-buffer-header-line-lang-face) "]"
              " → "
              "[" (propertize to 'face 'gts-render-buffer-header-line-lang-face) "]")))

;; implement

(defcustom gts-posframe-pop-render-buffer " *GTS-Pop-Posframe*"
  "Buffer name of Pop Posframe."
  :type 'string
  :group 'go-translate)

(defvar gts-posframe-pop-render-timeout 30)
(defvar gts-posframe-pop-render-poshandler nil)

(defun gts-posframe-render-auto-close-handler ()
  "Close the pop-up posframe window."
  (interactive)
  (unless (or (and gts-current-command
                   (member this-command (list gts-current-command #'exit-minibuffer)))
              (and gts-posframe-pop-render-buffer
                   (string= (buffer-name) gts-posframe-pop-render-buffer)))
    (ignore-errors (posframe-delete gts-posframe-pop-render-buffer))
    (remove-hook 'post-command-hook #'gts-posframe-render-auto-close-handler)))

(defclass gts-posframe-pop-render (gts-render)
  ((width       :initarg :width        :initform 100)
   (height      :initarg :height       :initform 15)
   (forecolor   :initarg :forecolor    :initform nil)
   (backcolor   :initarg :backcolor    :initform nil)
   (padding     :initarg :padding      :initform 12))
  "Pop up a childframe to show the result.
The frame will disappear when do do anything but focus in it.
Manually close the frame with `q'.")

(cl-defmethod gts-pre ((r gts-posframe-pop-render) task)
  (with-slots (width height forecolor backcolor padding) r
    (let* ((buf gts-posframe-pop-render-buffer))
      (posframe-show buf
                     :string "Loading..."
                     :timeout gts-posframe-pop-render-timeout
                     :max-width width
                     :max-height height
                     :foreground-color (or forecolor gts-pop-posframe-forecolor)
                     :background-color (or backcolor gts-pop-posframe-backcolor)
                     :internal-border-width padding
                     :internal-border-color (or backcolor gts-pop-posframe-backcolor)
                     :accept-focus t
                     :position (point)
                     :poshandler gts-posframe-pop-render-poshandler)
      ;; render
      (gts-render-buffer-prepare buf task)
      (posframe-refresh buf)
      ;; setup
      (with-current-buffer buf
        (gts-buffer-set-key ("q" "Close") (posframe-delete buf))))))

(cl-defmethod gts-out ((_ gts-posframe-pop-render) task)
  ;; render & refresh
  (when-let ((buf (gts-render-buffer gts-posframe-pop-render-buffer task)))
    (posframe-refresh buf)
    (add-hook 'post-command-hook #'gts-posframe-render-auto-close-handler)))

(cl-defmethod initialize-instance :after ((_ gts-posframe-pop-render) &rest _)
  (unless (featurep 'posframe)
    (user-error "To use `gts-posframe-render', you should install and load package `posframe' first")))


;;; [Render] Child-Frame Render (Pin Mode)

(defcustom gts-posframe-pin-render-buffer " *GTS-Pin-Posframe*"
  "Buffer name of Pin Posframe."
  :type 'string
  :group 'go-translate)

(defvar gts-posframe-pin-render-frame nil)
(defvar gts-posframe-pin-render-poshandler #'posframe-poshandler-frame-top-right-corner)

(defclass gts-posframe-pin-render (gts-posframe-pop-render)
  ((width       :initarg :width        :initform 60)
   (height      :initarg :height       :initform 20)
   (padding     :initarg :padding      :initform 8)
   (bd-width    :initarg :bd-width     :initform 1)
   (bd-color    :initarg :bd-color     :initform nil)
   (backcolor   :initarg :backcolor    :initform nil)
   (fri-color   :initarg :fringe-color :initform nil)
   (position    :initarg :position     :initform nil))
  "Pin the childframe in a fixed position to display the translate result.
The childframe will not close, until you kill it with `q'.
Other operations in the childframe buffer, just like in `gts-buffer-render'.")

(cl-defmethod gts-pre ((r gts-posframe-pin-render) task)
  ;; create/show frame
  (if (and (get-buffer gts-posframe-pin-render-buffer) gts-posframe-pin-render-frame)
      (make-frame-visible gts-posframe-pin-render-frame)
    (with-slots (width height min-width min-height bd-width forecolor backcolor bd-color padding position) r
      (setq gts-posframe-pin-render-frame
            (posframe-show gts-posframe-pin-render-buffer
                           :string "\nLoading..."
                           :width width
                           :height height
                           :min-width width
                           :min-height height
                           :foreground-color (or forecolor gts-pin-posframe-forecolor)
                           :background-color (or backcolor gts-pin-posframe-backcolor)
                           :internal-border-width bd-width
                           :border-color (or bd-color gts-pin-posframe-bdcolor)
                           :left-fringe padding
                           :right-fringe padding
                           :refresh nil
                           :accept-focus t
                           :respect-header-line t
                           :position position
                           :poshandler (unless position gts-posframe-pin-render-poshandler))))
    (set-frame-parameter gts-posframe-pin-render-frame 'drag-internal-border t)
    (set-frame-parameter gts-posframe-pin-render-frame 'drag-with-header-line t)
    (when-let ((color (or (oref r fri-color) gts-pin-posframe-fringe-color)))
      (set-face-background 'fringe color  gts-posframe-pin-render-frame)))
  ;; render
  (gts-render-buffer-prepare gts-posframe-pin-render-buffer task)
  (gts-buffer-ensure-a-blank-line-at-beginning gts-posframe-pin-render-buffer)
  ;; setup
  (with-current-buffer gts-posframe-pin-render-buffer
    (gts-buffer-set-key ("q" "Hide") (posframe-hide gts-posframe-pin-render-buffer))
    (gts-buffer-set-key ("Q" "Close") (posframe-delete gts-posframe-pin-render-buffer))))

(cl-defmethod gts-out ((_ gts-posframe-pin-render) task)
  ;; render & refresh
  (when-let ((buf (gts-render-buffer gts-posframe-pin-render-buffer task)))
    (gts-buffer-ensure-a-blank-line-at-beginning buf)))

(cl-defmethod gts-me-out ((_ gts-posframe-pin-render) translator task)
  ;; render & refresh
  (gts-render-buffer-multi-engines gts-posframe-pin-render-buffer translator task))


;;; [Render] kill-ring render

(defclass gts-kill-ring-render (gts-render) ())

(cl-defmethod gts-out ((_ gts-kill-ring-render) task)
  (deactivate-mark)
  (with-slots (result ecode) task
    (if ecode
        (user-error "%s" result)
      (kill-new result)
      (message "Translate result already in the kill ring."))))


;;; [Texter] take current-at-point-or-selection as source text

(defclass gts-current-or-selection-texter (gts-texter) ())

(cl-defmethod gts-text ((_ gts-current-or-selection-texter))
  (cond ((eq major-mode 'pdf-view-mode)
         (if (pdf-view-active-region-p)
             (car (pdf-view-active-region-text))))
        ((use-region-p)
         (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))
        (t (current-word t t))))

;; silence!
(declare-function pdf-view-active-region-p "ext:pdf-view.el" t t)
(declare-function pdf-view-active-region-text "ext:pdf-view.el" t t)


;;; [Texter] take whole-buffer for as source text

(defclass gts-whole-buffer-texter (gts-texter) ())

(cl-defmethod gts-text ((_ gts-whole-buffer-texter))
  (buffer-substring-no-properties (point-min) (point-max)))


;;; [Picker] noprompt picker

(defvar gts-picker-current-picker nil)
(defvar gts-picker-current-text nil)
(defvar gts-picker-current-path nil)

(defclass gts-noprompt-picker (gts-picker)
  ((texter :initarg :texter :initform (gts-current-or-selection-texter))))

(cl-defmethod gts-pick ((o gts-noprompt-picker))
  (let ((text (gts-text (oref o texter))))
    (when (= 0 (length (if text (string-trim text) "")))
      (user-error "Make sure there is any word at point, or selection exists"))
    (let ((path (gts-path o text)))
      (setq gts-picker-current-path path)
      (cl-values text path))))


;;; [Picker] prompt, and flexible

(defvar gts-prompt-for-translate-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-g" #'top-level)
    (define-key map "\C-n" #'gts-prompt-picker-next-path)
    (define-key map "\C-p" (lambda () (interactive) (gts-prompt-picker-next-path t)))
    (define-key map "\C-l" #'delete-minibuffer-contents)
    (define-key map [C-return] (lambda () (interactive) (exit-minibuffer)))
    map)
  "Minibuffer keymap used when prompt user input.")

(defclass gts-prompt-picker (gts-picker)
  ((texter :initarg :texter :initform (gts-current-or-selection-texter))))

(defun gts-prompt-picker-next-path (&optional backwardp)
  "Pick next available translate path.
If BACKWARDP is t, then pick the previous one."
  (interactive "P")
  (let ((p (gts-next-path gts-picker-current-picker
                          gts-picker-current-text
                          (car gts-picker-current-path)
                          (cdr gts-picker-current-path)
                          backwardp)))
    (setq gts-picker-current-path p)
    (gts-picker-prompt-pick (minibuffer-contents) p)
    (exit-minibuffer)))

(defun gts-picker-prompt-pick (&optional text path)
  (setq gts-picker-current-path path)
  (let* ((enable-recursive-minibuffers t)
         (minibuffer-allow-text-properties t)
         (prompt (concat (concat "[" (car path) " > " (cdr path) "] ") "Text: "))
         (text (read-from-minibuffer prompt text gts-prompt-for-translate-keymap)))
    (setq gts-picker-current-text text)
    (when (zerop (length (string-trim text)))
      (user-error "Text should not be null"))
    (with-minibuffer-selected-window
      (exit-minibuffer))))

(cl-defmethod gts-pick ((o gts-prompt-picker))
  (setq gts-picker-current-picker o)
  (let* ((text (gts-text (oref o texter)))
         (path (gts-path o text)))
    (gts-picker-prompt-pick text path)
    (setq gts-picker-last-path gts-picker-current-path)
    (cl-values gts-picker-current-text gts-picker-current-path)))


(provide 'gts-implements)

;;; gts-implements.el ends here
