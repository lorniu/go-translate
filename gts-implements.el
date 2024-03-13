;;; gts-implements.el --- Component implements -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Builtin implements of basic components

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'json)
(require 'gts-core)
(require 'gts-faces)


;;; [Logger] Log all to a standalone buffer

(defclass gts-buffer-logger (gts-logger)
  ((buffer :initarg buf :initform nil))) ; dont set :initarg to buffer, it will make an error on emacsv27.

(defvar gts-logger-buffer-name " *gts-logger*")

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

(cl-defmethod gts-cache-set ((cacher gts-memory-cacher) task result)
  (when gts-cache-enable
    (with-slots (caches expired) cacher
      (let* ((key (gts-cache-key cacher task))
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
          (oset cacher caches (cons (cons key (cons result etime)) caches))
          (gts-do-log 'memory-cacher (format "%s: add to cache %s (%s)" (oref task id) key (length caches))))
        (gts-clear-expired cacher)))))

(setq gts-default-cacher (gts-memory-cacher))


;;; [Http Client] implement with builtin `url.el'.
;; This will be used as the default http client.

(defclass gts-url-http-client (gts-http-client) ())

(defcustom gts-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36"
  "The user agent used when send a request."
  :type 'string
  :group 'go-translate)

(defvar url-http-end-of-headers)

(cl-defmethod gts-request ((_ gts-url-http-client) url &key done fail data headers)
  "Request URL with DATA and HEADERS asynchronous.
Execute DONE when success, or FAIL when failed."
  (cl-assert (and url done fail))
  (let ((url-debug gts-debug-p)
        (url-user-agent gts-user-agent)
        (url-request-extra-headers headers)
        (url-request-method (if data "POST" "GET"))
        (url-request-data data))
    (url-retrieve url (lambda (status)
                        (unwind-protect
                            (if-let ((err (cond
                                           ((eq (car status) :error) (cadr status))
                                           ((null url-http-end-of-headers) "Nothing responsed from server"))))
                                (funcall fail err)
                              (when done
                                (delete-region (point-min) url-http-end-of-headers)
                                (funcall done)))
                          (kill-buffer)))
                  nil t)))

(setq gts-default-http-client (gts-url-http-client)) ; use this as default


;;; [Render] buffer render

(defclass gts-buffer-render (gts-render) ())

(defcustom gts-split-width-threshold 80
  "Threshold width for window horizontal split."
  :group 'go-translate
  :type '(choice
          (const :tag "Disable" nil)
          (integer :tag "Threshold")))

(defcustom gts-buffer-follow-p nil
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

 \\='((display-buffer-reuse-window display-buffer-in-side-window)
   (side . right))

will force opening in right side window."
  :type 'list
  :group 'go-translate)

(defcustom gts-buffer-render-task-header-function #'gts-buffer-render-task-header
  "Function used to render the task header when in multiple engines's query."
  :type 'function
  :group 'go-translate)

(defvar-local gts-buffer-translator nil)
(defvar-local gts-buffer-keybinding-messages nil)
(defvar-local gts-buffer-local-map nil)

(defvar gts-buffer-evil-leading-key "SPC" "Leading key for keybinds in evil mode.")

(cl-defmacro gts-buffer-set-key ((key &optional desc) form)
  "Helper for define KEY in buffer."
  (declare (indent 1))
  `(let ((fn ,(if (equal 'function (car form)) `,form `(lambda () (interactive) ,form))))
     (if (bound-and-true-p evil-mode)
         (evil-define-key* 'normal gts-buffer-local-map (kbd ,(concat gts-buffer-evil-leading-key " " key)) fn)
       (define-key gts-buffer-local-map (kbd ,key) fn))
     (when ,desc
       (cl-delete ,key gts-buffer-keybinding-messages :key #'car :test #'string=)
       (push (cons ,key ,desc) gts-buffer-keybinding-messages))))

(defun gts-buffer-init-header-line (path &optional desc)
  "Setup header line format.
It will show the basic information of the translation,
including PATH and other DESC."
  (setq header-line-format
        (list
         " "
         "[" (propertize (car path) 'face 'gts-render-buffer-header-line-lang-face) "]"
         (if desc (concat " ― " (propertize desc 'face 'gts-render-buffer-header-line-desc-face)) "")
         " → "
         "[" (propertize (cdr path) 'face 'gts-render-buffer-header-line-lang-face) "]"
         "   ("
         (if (bound-and-true-p evil-mode) (concat (propertize gts-buffer-evil-leading-key 'face 'font-lock-type-face) " as leading key, "))
         (propertize "h" 'face 'font-lock-type-face) " for help)"
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
    (let ((inhibit-read-only t))
      (save-excursion
        (let ((m (mark-marker)))
          (goto-char (point-min))
          (unless (equal (char-after) ?\n)
            (insert "\n"))
          (when (ignore-errors (= (marker-position m) 1))
            (set-marker m (point))))
        (set-buffer-modified-p nil)))))

(defun gts-childframe-of-buffer (&optional buffer)
  "Get the childframe of BUFFER if it exists, or return nil."
  (let ((cf (window-frame (get-buffer-window buffer t))))
    (when (frame-parameter cf 'parent-frame) cf)))

(defun gts-buffer-render-task-header (engine-tag parser-tag)
  (concat (propertize
           (format "%s %s\n" engine-tag (if parser-tag (format "(%s) " parser-tag) ""))
           'face 'gts-render-buffer-me-header-backgroud-face)))

(defun gts-render-buffer-prepare (buffer translator)
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t)
          (engines (gts-get translator 'engines)))
      (with-slots (text path) translator
        ;; setup
        (deactivate-mark)
        (visual-line-mode -1)
        (font-lock-mode 1)
        (setq-local cursor-type 'hbar)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local gts-buffer-translator translator)
        ;; headline
        (gts-buffer-init-header-line path (unless (cdr engines) (oref (car engines) tag)))
        ;; source text
        (erase-buffer)
        (unless (gts-childframe-of-buffer (current-buffer)) (insert "\n")) ; except childframe
        (insert text)
        ;; keybinds
        (setq gts-buffer-local-map (make-sparse-keymap))
        (gts-buffer-set-key ("x" "Reverse Path")
          (gts-translate translator text (cons (cdr path) (car path))))
        (gts-buffer-set-key ("M-p" "Prev Path")
          (gts-translate translator text (gts-next-path (gts-get translator 'picker) text path t)))
        (gts-buffer-set-key ("M-n" "Next Path")
          (gts-translate translator text (gts-next-path (gts-get translator 'picker) text path)))
        (gts-buffer-set-key ("g" "Refresh")
          (gts-translate translator text path))
        (gts-buffer-set-key ("y" "TTS")
          (if-let ((task (get-text-property (point) 'task)))
              (gts-do-tts text (car path) (oref task engine))
            (message "[TTS] No engine found at point")))
        (gts-buffer-set-key ("O" "Open Externally")
          (when-let ((task (get-text-property (point) 'task)))
            (let* ((meta (oref task meta)) (url (plist-get meta :url)))
              (cond (url (browse-url url) (message "Opening %s... Done!" url))
                    (t (message "Don't know how to open externally for this."))))))
        (gts-buffer-set-key ("C" "Clean Cache")
          (gts-clear-all gts-default-cacher))
        (gts-buffer-set-key ("t" "Toggle-Follow")
          (let ((state (if (setq gts-buffer-follow-p (not gts-buffer-follow-p)) "allowed" "disabled")))
            (message "Now, buffer following %s." state)))
        (gts-buffer-set-key ("q" "Quit") #'kill-buffer-and-window)
        (gts-buffer-set-key ("p") #'previous-line)
        (gts-buffer-set-key ("n") #'next-line)
        (gts-buffer-set-key ("C-g")
          (unwind-protect (gts-tts-try-interrupt-playing-process)
            (keyboard-quit)))
        (gts-buffer-set-key ("h")
          (message (mapconcat
                    (lambda (kd) (concat (propertize (car kd) 'face 'font-lock-keyword-face) ": " (cdr kd)))
                    (reverse gts-buffer-keybinding-messages) " ")))
        (gts-buffer-set-key ("C-x C-q")
          (progn (read-only-mode -1)
                 (use-local-map nil)
                 (local-set-key (kbd "C-x C-q")
                                (lambda ()
                                  (interactive)
                                  (read-only-mode 1)
                                  (use-local-map gts-buffer-local-map)))))
        (read-only-mode 1)
        (use-local-map gts-buffer-local-map)
        ;; execute the hook if exists
        (run-hooks 'gts-after-buffer-prepared-hook)))))

(defun gts-render-buffer (buffer task)
  "For single engine translation."
  (when-let ((inhibit-read-only t)
             (buf (get-buffer buffer)))
    (with-current-buffer buf
      (with-slots (err parsed meta engine translator) task
        ;; error
        (if err
            (progn
              (goto-char (point-max))
              (insert (propertize (format "\n\n%s" err) 'face 'gts-render-buffer-error-face)))
          (erase-buffer)
          (cond
           ;; content (non-split)
           ((stringp parsed)
            (let ((tbeg (plist-get meta :tbeg))
                  (tend (plist-get meta :tend)))
              (unless tbeg
                (insert (propertize (oref translator text) 'face 'gts-render-buffer-source-face) "\n\n"))
              (unless tbeg (setq tbeg (point)))
              (insert parsed)
              (unless tend (setq tend (point)))
              ;; try set mark in beginning of the translate result,
              ;; and set currsor position in end of the translate result,
              ;; so you can quick select the translate result with C-x C-x.
              (when (not (gts-childframe-of-buffer buf)) ; when not childframe
                (push-mark tend 'nomsg)
                (goto-char tbeg)
                (set-window-point (get-buffer-window buf t) tbeg))))
           ;; content (split)
           (t (cl-loop for idx from 0
                       for src in (gts-get translator 'sptext)
                       for tar in parsed
                       concat (concat (propertize src 'face 'gts-render-buffer-source-face) "\n\n" tar "\n\n") into res
                       finally (insert res)))))
        ;; update states
        (set-buffer-modified-p nil)
        (gts-buffer-change-header-line-state 'done)
        (put-text-property (point-min) (point-max) 'task task)
        ;; execute the hook if exists
        (run-hooks 'gts-after-buffer-render-hook))
      buf)))

(defun gts-render-buffer-multi-engines (buffer task)
  "For multiple engines translation."
  (when-let ((inhibit-read-only t)
             (buf (get-buffer buffer)))
    (with-slots (from to translator) task
      (with-slots (text task-queue version) translator
        (with-current-buffer buf
          (when (equal (oref gts-buffer-translator version) version)
            (erase-buffer)
            (insert (propertize (format "\n%s\n\n" text) 'face 'gts-render-buffer-source-face))
            ;; content
            (save-excursion
              (dolist (task task-queue)
                (with-slots (err parsed meta engine) task
                  (let* ((result (if (or err (stringp parsed)) parsed
                                   (string-join parsed "\n\n")))
                         (header (funcall gts-buffer-render-task-header-function
                                          (oref engine tag)
                                          (oref (oref engine parser) tag)))
                         (content (cond
                                   (err
                                    (concat header (propertize (format "\n%s\n\n" err) 'face 'gts-render-buffer-error-face)))
                                   ((string-empty-p result)
                                    (concat header "\nLoading...\n\n"))
                                   (t (concat header "\n" (string-trim result) "\n\n")))))
                    (put-text-property 0 (length content) 'task task content)
                    (insert content)))))
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
        buf))))

(cl-defmethod gts-pre ((_ gts-buffer-render) translator)
  ;; init and setup
  (gts-render-buffer-prepare gts-buffer-name translator)
  ;; display
  (let ((split-width-threshold (or gts-split-width-threshold split-width-threshold)))
    (display-buffer gts-buffer-name gts-buffer-window-config)))

(cl-defmethod gts-out ((_ gts-buffer-render) task)
  ;; render & display
  (when-let ((buf (gts-render-buffer gts-buffer-name task)))
    (gts-buffer-ensure-a-blank-line-at-beginning buf)
    (gts-buffer-display-or-focus-buffer buf)))

(cl-defmethod gts-me-out ((_ gts-buffer-render) task)
  ;; render & display
  (when-let ((buf (gts-render-buffer-multi-engines gts-buffer-name task)))
    (when (= (oref (oref task translator) state) 3)
      (gts-buffer-display-or-focus-buffer buf))))


;;; [Render] Child-Frame Render (Popup Mode)
;; implements via package Posframe, you should install it before use this

(defclass gts-posframe-pop-render (gts-render)
  ((width       :initarg :width        :initform 100)
   (height      :initarg :height       :initform 15)
   (forecolor   :initarg :forecolor    :initform nil)
   (backcolor   :initarg :backcolor    :initform nil)
   (padding     :initarg :padding      :initform 12))
  "Pop up a childframe to show the result.
The frame will disappear when do do anything but focus in it.
Manually close the frame with `q'.")

(require 'posframe nil t)
(declare-function posframe-show "ext:posframe.el" t t)
(declare-function posframe-delete "ext:posframe.el" t t)
(declare-function posframe-hide "ext:posframe.el" t t)
(declare-function posframe-refresh "ext:posframe.el" t t)
(declare-function posframe-poshandler-frame-top-right-corner "ext:posframe.el" t t)

(defcustom gts-posframe-pop-render-buffer " *GTS-Pop-Posframe*"
  "Buffer name of Pop Posframe."
  :type 'string
  :group 'go-translate)

(defvar gts-posframe-pop-render-timeout 30)
(defvar gts-posframe-pop-render-poshandler nil)

(defun gts-posframe-init-header-line (from to)
  (setq header-line-format
        (list "[" (propertize from 'face 'gts-render-buffer-header-line-lang-face) "]"
              " → "
              "[" (propertize to 'face 'gts-render-buffer-header-line-lang-face) "]")))

(defun gts-posframe-render-auto-close-handler ()
  "Close the pop-up posframe window."
  (interactive)
  (unless (or (and gts-current-command
                   (member this-command (list gts-current-command #'exit-minibuffer)))
              (and gts-posframe-pop-render-buffer
                   (string= (buffer-name) gts-posframe-pop-render-buffer)))
    (ignore-errors (posframe-delete gts-posframe-pop-render-buffer))
    (remove-hook 'post-command-hook #'gts-posframe-render-auto-close-handler)))

(cl-defmethod gts-pre ((render gts-posframe-pop-render) translator)
  (with-slots (width height forecolor backcolor padding) render
    (let ((inhibit-read-only t)
          (buf gts-posframe-pop-render-buffer))
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
      (gts-render-buffer-prepare buf translator)
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
Other operations in the childframe buffer, just like in 'gts-buffer-render'.")

(defcustom gts-posframe-pin-render-buffer " *GTS-Pin-Posframe*"
  "Buffer name of Pin Posframe."
  :type 'string
  :group 'go-translate)

(defvar gts-posframe-pin-render-frame nil)
(defvar gts-posframe-pin-render-poshandler #'posframe-poshandler-frame-top-right-corner)

(cl-defmethod gts-pre ((render gts-posframe-pin-render) translator)
  ;; create/show frame
  (if (and (get-buffer gts-posframe-pin-render-buffer) gts-posframe-pin-render-frame)
      (make-frame-visible gts-posframe-pin-render-frame)
    (with-slots (width height min-width min-height bd-width forecolor backcolor bd-color padding position) render
      (setq gts-posframe-pin-render-frame
            (let ((inhibit-read-only t))
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
                             :poshandler (unless position gts-posframe-pin-render-poshandler)))))
    (set-frame-parameter gts-posframe-pin-render-frame 'drag-internal-border t)
    (set-frame-parameter gts-posframe-pin-render-frame 'drag-with-header-line t)
    (when-let ((color (or (oref render fri-color) gts-pin-posframe-fringe-color)))
      (set-face-background 'fringe color  gts-posframe-pin-render-frame)))
  ;; render
  (gts-render-buffer-prepare gts-posframe-pin-render-buffer translator)
  (gts-buffer-ensure-a-blank-line-at-beginning gts-posframe-pin-render-buffer)
  ;; setup
  (with-current-buffer gts-posframe-pin-render-buffer
    (gts-buffer-set-key ("q" "Hide") (posframe-hide gts-posframe-pin-render-buffer))
    (gts-buffer-set-key ("Q" "Close") (posframe-delete gts-posframe-pin-render-buffer))))

(cl-defmethod gts-out ((_ gts-posframe-pin-render) task)
  ;; render & refresh
  (when-let ((buf (gts-render-buffer gts-posframe-pin-render-buffer task)))
    (gts-buffer-ensure-a-blank-line-at-beginning buf)))

(cl-defmethod gts-me-out ((_ gts-posframe-pin-render) task)
  ;; render & refresh
  (gts-render-buffer-multi-engines gts-posframe-pin-render-buffer task))


;;; [Render] kill-ring render

(defclass gts-kill-ring-render (gts-render) ())

(cl-defmethod gts-out ((_ gts-kill-ring-render) task)
  (deactivate-mark)
  (with-slots (err parsed) task
    (if err
        (user-error "%s" err)
      (kill-new (if (listp parsed) (string-join parsed "\n\n") parsed))
      (message "Translate result already in the kill ring."))))


;;; [Render] Alert Render

(require 'alert nil t)
(declare-function alert "ext:alert.el" t t)

(defvar gts-alert-args '(:timeout 10))

(defclass gts-alert-render (gts-render) ())

(cl-defmethod gts-out ((_ gts-alert-render) task)
  (with-slots (text err parsed) task
    (if err
        (user-error "%s" err)
      (apply #'alert
             (if (listp parsed) (string-join parsed "\n\n") parsed)
             :title (if (string-match-p "\n" text) "*Go-Translate*" text)
             gts-alert-args))))


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

(defclass gts-noprompt-picker (gts-picker)
  ((texter :initarg :texter :initform (gts-current-or-selection-texter))))

(defvar gts-picker-current-picker nil)
(defvar gts-picker-current-path nil)

(cl-defmethod gts-pick ((picker gts-noprompt-picker))
  (let ((text (gts-text (oref picker texter))))
    (when (= 0 (length (if text (string-trim text) "")))
      (user-error "Make sure there is any word at point, or selection exists"))
    (let ((path (gts-path picker text)))
      (setq gts-picker-current-path path)
      (cl-values text path))))


;;; [Picker] prompt, and flexible

(defclass gts-prompt-picker (gts-picker)
  ((texter :initarg :texter :initform (gts-current-or-selection-texter))))

(defvar gts-prompt-picker-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-g" #'top-level)
    (define-key map "\C-n" #'gts-prompt-picker-next-path)
    (define-key map "\C-p" (lambda () (interactive) (gts-prompt-picker-next-path t)))
    (define-key map "\C-l" #'delete-minibuffer-contents)
    map)
  "Minibuffer keymap used when prompt user input.")

(defun gts-prompt-picker-next-path (&optional backwardp)
  "Pick next available translate path.
If BACKWARDP is t, then pick the previous one."
  (interactive "P")
  (let* ((text (minibuffer-contents))
         (path (gts-next-path gts-picker-current-picker
                              text gts-picker-current-path backwardp)))
    (setq gts-picker-current-path path)
    (gts-prompt-picker-pick text path)))

(defun gts-prompt-picker-pick (&optional text path)
  (setq gts-picker-current-path path)
  (let* ((enable-recursive-minibuffers t)
         (minibuffer-allow-text-properties t)
         (prompt (concat (concat "[" (car path) " > " (cdr path) "] ") "Text: "))
         (text (read-from-minibuffer prompt text gts-prompt-picker-keymap)))
    (throw 'gts-minibuffer `(,text . ,path))))

(cl-defmethod gts-pick ((picker gts-prompt-picker))
  (setq gts-picker-current-picker picker)
  (pcase-let* ((text (gts-text (oref picker texter)))
               (path (gts-path picker text))
               (`(,current-text . ,current-path)
                (catch 'gts-minibuffer (gts-prompt-picker-pick text path))))
    (when (zerop (length (string-trim current-text)))
      (user-error "Text should not be null"))
    (cl-values current-text current-path)))


;;; [Splitter] split text by paragraph

(defclass gts-paragraph-splitter (gts-splitter) ())

(cl-defmethod gts-split ((_ gts-paragraph-splitter) text)
  (setq text (split-string text "\n\n" t))
  (if (and (listp text) (cadr text)) text))


(provide 'gts-implements)

;;; gts-implements.el ends here
