;;; gts-implements.el --- component implements -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'gts-core)
(require 'gts-faces)


;;; [Logger] Log all to a standalone buffer

(defvar gts-buffer-logger-buffer " *gts-logger*")

(defclass gts-buffer-logger (gts-logger)
  ((buffer :initarg buf :initform nil))) ; dont set :initarg to buffer, it will make an error on emacsv27.

(cl-defmethod gts-log ((logger gts-buffer-logger) tag msg)
  (with-current-buffer (get-buffer-create (or (oref logger buffer) gts-buffer-logger-buffer))
    (goto-char (point-max))
    (if (or (= (length msg) 0) (string= msg "\n"))
        (insert "")
      (insert (propertize (concat "[" tag "] ") 'face 'gts-logger-buffer-tag-face))
      (insert msg))
    (insert "\n")))

(setq gts-default-logger (gts-buffer-logger))


;;; [Http Client] implement with builtin `url.el'.
;; This will be used as the default http client.

(defvar gts-debug-p nil)

(defvar gts-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36")

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
                                (funcall done)))
                          (kill-buffer)))
                  nil t)))

(setq gts-default-http-client (gts-url-http-client)) ; use this as default


;;; [Render] buffer render

(defvar gts-split-width-threshold 80
  "Threshold width for window horizontal split.")

(defcustom gts-buffer-follow-p t
  "If t then pop to the result window after translation."
  :type 'boolean
  :group 'gts)

(defcustom gts-buffer-name "*GT-Buffer*"
  "The name of translation result buffer."
  :type 'string
  :group 'gts)

(defcustom gts-buffer-window-config nil
  "Window configuration used by the result buffer window.

For example, set to:

 '((display-buffer-reuse-window display-buffer-in-side-window)
   (side . right))

will force opening in right side window."
  :type 'list
  :group 'gts)

(defcustom gts-render-buffer-me-header-function #'gts-render-buffer-me-engine-header
  "Function used to render the engine's header when in multiple engines's query."
  :type 'function
  :group 'gts)

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

(defun gts-get-childframe-of-buffer (&optional buffer)
  "Get the childframe of BUFFER if it exists, or return nil."
  (let ((cf (window-frame (get-buffer-window buffer 't))))
    (when (frame-parameter cf 'parent-frame) cf)))

(defun gts-buffer-ensure-a-blank-line-at-beginning (buffer)
  "Ensure a blank line at beginning of BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (equal (char-after) ?\n)
        (insert "\n")))))

(defun gts-render-buffer-me-engine-header (engine-tag parser-tag)
  (concat "\n"
          (propertize
           (format "%s %s\n" engine-tag (if parser-tag (format "(%s) " parser-tag) ""))
           'face 'gts-render-buffer-me-header-backgroud-face)))

;; functions

(defun gts-render-buffer-prepare (buffer text from to &optional provider)
  (with-current-buffer (get-buffer-create buffer)
    ;; setup
    (deactivate-mark)
    (read-only-mode -1)
    (visual-line-mode -1)
    (setq-local cursor-type 'hbar)
    (setq-local cursor-in-non-selected-windows nil)
    ;; headline
    (gts-buffer-init-header-line
     from to (if (and provider
                      (object-of-class-p provider 'gts-engine))
                 (oref provider tag)))
    ;; source text
    (erase-buffer)
    (unless (gts-get-childframe-of-buffer (current-buffer)) ; except childframe
      (insert "\n"))
    (insert text)
    (set-buffer-modified-p nil)
    ;; keybinds.
    ;;  q for quit and kill the window.
    ;;  x for switch sl and tl.
    ;;  M-n and M-p to re-query with next/prev path
    ;;  g for re-query/refresh
    ;;  s to speak the current selection or word
    (local-set-key (kbd "M-n")
                   (lambda ()
                     (interactive)
                     (let ((next (gts-next-path (oref provider picker) text from to)))
                       (gts-translate provider text (car next) (cdr next)))))
    (local-set-key (kbd "M-p")
                   (lambda ()
                     (interactive)
                     (let ((prev (gts-next-path (oref provider picker) text from to t)))
                       (gts-translate provider text (car prev) (cdr prev)))))
    (local-set-key "g" (lambda () (interactive) (gts-translate provider text from to)))
    (local-set-key "x" (lambda () (interactive) (gts-translate provider text to from)))
    (local-set-key "y" (lambda ()
                         (interactive)
                         (when-let ((e (get-text-property (point) 'engine)))
                           (gts-tts e text from))))
    (local-set-key "q" #'kill-buffer-and-window)))

(defun gts-render-buffer (buffer result)
  (when-let ((buf (get-buffer buffer)))
    (with-current-buffer buf
      (cond
       ((stringp result)
        ;; content
        (erase-buffer)
        (insert result)
        ;; try set mark in beginning of the translate result,
        ;; and set currsor position in end of the translate result,
        ;; so you can quick select the translate result with C-x C-x.
        (unless (gts-get-childframe-of-buffer buf) ; when not childframe
          (when-let ((tbeg (get-text-property 0 'tbeg result))
                     (tend (get-text-property 0 'tend result)))
            (push-mark tbeg 'nomsg)
            (goto-char tend)
            (set-window-point (get-buffer-window buf) tend))))
       (t
        ;; error display
        (goto-char (point-max))
        (insert (propertize (format "\n\n\n%s" result) 'face 'gts-render-buffer-error-face))))
      ;; change states
      (set-buffer-modified-p nil)
      (gts-buffer-change-header-line-state 'done))
    buf))

(defun gts-render-buffer-multi-engines (buffer translator _id)
  (when-let ((buf (get-buffer buffer)))
    (with-slots (text from to plan-cnt task-queue engines) translator
      (with-current-buffer buf
        (erase-buffer)
        (insert (propertize (format "\n%s\n" text) 'face 'gts-render-buffer-source-face))
        ;; content
        (save-excursion
          (cl-loop for task in task-queue
                   for result = (plist-get task :result)
                   for engine = (plist-get task :engine)
                   for engine-tag = (slot-value engine 'tag)
                   for parser = (oref engine parser)
                   for parser-tag = (slot-value parser 'tag)
                   for head = (funcall gts-render-buffer-me-header-function engine-tag parser-tag)
                   for content = (if result (format "\n%s\n" result) "\nLoading...\n")
                   do (insert head content)))
        (set-buffer-modified-p nil)
        (when (gts-finished-p translator)
          ;; cursor
          (unless (gts-get-childframe-of-buffer buf)
            (set-window-point (get-buffer-window) (save-excursion (forward-line 3) (point))))
          ;; state
          (gts-buffer-change-header-line-state 'done))))
    buf))

;; component

(defclass gts-buffer-render (gts-render) ())

(cl-defmethod gts-pre ((_ gts-buffer-render) text from to &optional provider)
  ;; init and setup
  (gts-render-buffer-prepare gts-buffer-name text from to provider)
  ;; display
  (let ((split-width-threshold gts-split-width-threshold))
    (display-buffer gts-buffer-name gts-buffer-window-config)))

(cl-defmethod gts-out ((_ gts-buffer-render) result)
  ;; render & display
  (when-let ((buf (gts-render-buffer gts-buffer-name result)))
    (gts-buffer-ensure-a-blank-line-at-beginning buf)
    (gts-buffer-display-or-focus-buffer buf)))

(cl-defmethod gts-me-out ((_ gts-buffer-render) translator _id)
  ;; render & display
  (when-let ((buf (gts-render-buffer-multi-engines gts-buffer-name translator nil)))
    (when (gts-finished-p translator)
      (gts-buffer-display-or-focus-buffer buf))))


;;; [Render] Child-Frame Render (Popup Mode)
;; implements via package Posframe, you should install it before use this

(require 'posframe nil t)
(declare-function posframe-show "ext:posframe.el" t t)
(declare-function posframe-poshandler-frame-top-right-corner "ext:posframe.el" t t)

(defun gts-posframe-init-header-line (from to)
  (setq header-line-format
        (list "[" (propertize from 'face 'gts-render-buffer-header-line-lang-face) "]"
              " → "
              "[" (propertize to 'face 'gts-render-buffer-header-line-lang-face) "]")))

;; implement

(defvar gts-posframe-pop-render-buffer " *GT-Pop-Posframe*")
(defvar gts-posframe-pop-render-timeout 20)
(defvar gts-posframe-pop-render-poshandler nil)

(defun gts-posframe-render-auto-close-handler ()
  "Close the pop-up posframe window."
  (interactive)
  (when (or (null gts-posframe-pop-render-buffer)
            (not (string= (buffer-name) gts-posframe-pop-render-buffer)))
    (ignore-errors (posframe-delete gts-posframe-pop-render-buffer))
    (remove-hook 'post-command-hook #'gts-posframe-render-auto-close-handler)))

(defclass gts-posframe-pop-render (gts-render)
  ((width       :initarg :width        :initform 100)
   (height      :initarg :height       :initform 15)
   (forecolor   :initarg :forecolor    :initform nil)
   (backcolor   :initarg :backcolor    :initform "lightyellow")
   (padding     :initarg :padding      :initform 12))
  "Pop up a childframe to show the result.
The frame will disappear when do do anything but focus in it.
Manually close the frame with `q'.")

(cl-defmethod gts-pre ((r gts-posframe-pop-render) text from to &optional provider)
  (with-slots (width height forecolor backcolor padding) r
    (let* ((buf gts-posframe-pop-render-buffer))
      (posframe-show buf
                     :string "Loading..."
                     :timeout gts-posframe-pop-render-timeout
                     :width width
                     :height height
                     :foreground-color forecolor
                     :background-color backcolor
                     :internal-border-width padding
                     :internal-border-color backcolor
                     :accept-focus t
                     :position (point)
                     :poshandler gts-posframe-pop-render-poshandler)
      ;; render
      (gts-render-buffer-prepare buf text from to provider)
      (posframe-refresh buf)
      ;; setup
      (with-current-buffer buf
        (local-set-key (kbd "q") (lambda () (interactive) (posframe-delete buf)))))))

(cl-defmethod gts-out ((_ gts-posframe-pop-render) result)
  ;; render & refresh
  (when-let ((buf (gts-render-buffer gts-posframe-pop-render-buffer result)))
    (posframe-refresh buf)
    (add-hook 'post-command-hook #'gts-posframe-render-auto-close-handler)))

(cl-defmethod initialize-instance :after ((_ gts-posframe-pop-render) &rest _)
  (unless (featurep 'posframe)
    (user-error "To use `gts-posframe-render', you should install and load package `posframe' first.")))


;;; [Render] Child-Frame Render (Pin Mode)

(defvar gts-posframe-pin-render-buffer " *GT-Pin-Posframe*")
(defvar gts-posframe-pin-render-frame nil)
(defvar gts-posframe-pin-render-poshandler #'posframe-poshandler-frame-top-right-corner)

(defclass gts-posframe-pin-render (gts-posframe-pop-render)
  ((width       :initarg :width      :initform 60)
   (height      :initarg :height     :initform 20)
   (padding     :initarg :padding    :initform 8)
   (bd-width    :initarg :bd-width   :initform 1)
   (bd-color    :initarg :bd-color   :initform "#000000")
   (backcolor   :initarg :backcolor  :initform nil)
   (position    :initarg :position   :initform nil))
  "Pin the childframe in a fixed position to display the translate result.
The childframe will not close, until you kill it with `q'.
Other operations in the childframe buffer, just like in `gts-buffer-render'.")

(cl-defmethod gts-pre ((r gts-posframe-pin-render) text from to &optional provider)
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
                           :foreground-color forecolor
                           :background-color backcolor
                           :internal-border-width bd-width
                           :border-color bd-color
                           :left-fringe padding
                           :right-fringe padding
                           :refresh nil
                           :accept-focus t
                           :respect-header-line t
                           :position position
                           :poshandler (unless position gts-posframe-pin-render-poshandler))))
    (set-frame-parameter gts-posframe-pin-render-frame 'drag-internal-border t)
    (set-frame-parameter gts-posframe-pin-render-frame 'drag-with-header-line t))
  ;; render
  (gts-render-buffer-prepare gts-posframe-pin-render-buffer text from to provider)
  (gts-buffer-ensure-a-blank-line-at-beginning gts-posframe-pin-render-buffer)
  ;; setup
  (with-current-buffer gts-posframe-pin-render-buffer
    (local-set-key (kbd "q") (lambda () (interactive) (posframe-hide gts-posframe-pin-render-buffer)))
    (local-set-key (kbd "Q") (lambda () (interactive) (posframe-delete gts-posframe-pin-render-buffer)))))

(cl-defmethod gts-out ((_ gts-posframe-pin-render) result)
  ;; render & refresh
  (when-let ((buf (gts-render-buffer gts-posframe-pin-render-buffer result)))
    (gts-buffer-ensure-a-blank-line-at-beginning buf)))

(cl-defmethod gts-me-out ((_ gts-posframe-pin-render) translator id)
  ;; render & refresh
  (gts-render-buffer-multi-engines gts-posframe-pin-render-buffer translator id))


;;; [Render] kill-ring render

(defclass gts-kill-ring-render (gts-render) ())

(cl-defmethod gts-out ((_ gts-kill-ring-render) result)
  (deactivate-mark)
  (kill-new result)
  (message "Translate result already in the kill ring."))


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

(defun gts-prompt-picker-next-path (arg)
  (interactive "P")
  (let ((p (gts-next-path gts-picker-current-picker
                          gts-picker-current-text
                          (car gts-picker-current-path)
                          (cdr gts-picker-current-path)
                          arg)))
    (setq gts-picker-current-path p)
    (gts-picker-prompt-pick (minibuffer-contents) p)
    (exit-minibuffer)))

(defun gts-picker-prompt-pick (&optional text path)
  (setq gts-picker-current-path path)
  (let* ((minibuffer-allow-text-properties t)
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
