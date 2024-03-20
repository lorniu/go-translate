;;; gts-implements.el --- Extra components -*- lexical-binding: t -*-

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


;;; [Http Client] request with curl instead of `url.el'
;; implements via package `plz', you should install it before use this

(defclass gts-plz-http-client (gts-http-client) ())

(declare-function plz "ext:plz.el" t t)

(cl-defmethod gts-request :before ((_ gts-plz-http-client) &rest _)
  (unless (and (require 'plz nil t) (executable-find plz-curl-program))
    (error "You should have `plz.el' and `curl' installed before using `gts-plz-http-client'")))

(cl-defmethod gts-request ((_ gts-plz-http-client) &key url done fail data headers)
  (plz (if data 'post 'get) url
    :headers (cons `("User-Agent" . ,gts-user-agent) headers)
    :body data
    :as 'string
    :then (lambda (str)
            (with-temp-buffer
              (insert str)
              (funcall done)))
    :else (lambda (err)
            (funcall fail
                     (or (when-let (cr (plz-error-curl-error err))
                           (or (cdr cr) (format "curl error (%s)" (car cr))))
                         (plz-error-message err)
                         (plz-error-response err))))))


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

(defun gts-render-buffer-prepare-1 (buffer translator)
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t)
          (engines (gts-with-slots (engines) translator)))
      (with-slots (text src trg) translator
        ;; setup
        (deactivate-mark)
        (visual-line-mode -1)
        (font-lock-mode 1)
        (setq-local cursor-type 'hbar)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local gts-buffer-translator translator)
        ;; headline
        (gts-buffer-init-header-line (cons src trg) (unless (cdr engines) (oref (car engines) tag)))
        ;; source text
        (erase-buffer)
        (unless (gts-childframe-of-buffer (current-buffer)) (insert "\n")) ; except childframe
        (insert text)
        ;; keybinds
        (setq gts-buffer-local-map (make-sparse-keymap))
        (gts-buffer-set-key ("x" "Reverse Path")
          (gts-translate translator text (cons trg src)))
        (gts-buffer-set-key ("M-p" "Prev Path")
          (gts-translate translator text (gts-next-path (gts-with-slots (picker) translator) text (cons src trg) t)))
        (gts-buffer-set-key ("M-n" "Next Path")
          (gts-translate translator text (gts-next-path (gts-with-slots (picker) translator) text (cons src trg))))
        (gts-buffer-set-key ("g" "Refresh")
          (gts-translate translator text (cons src trg)))
        (gts-buffer-set-key ("y" "TTS")
          (if-let ((task (get-text-property (point) 'task)))
              (gts-do-tts text src (oref task engine))
            (message "[TTS] No engine found at point")))
        (gts-buffer-set-key ("O" "Open Externally")
          (when-let ((task (get-text-property (point) 'task)))
            (let* ((meta (oref task meta)) (url (plist-get meta :url)))
              (cond (url (browse-url url) (message "Opening %s... Done!" url))
                    (t (message "Don't know how to open externally for this."))))))
        (gts-buffer-set-key ("C" "Clean Cache")
          (gts-cache-clear-all gts-default-cacher))
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

(defun gts-render-buffer-prepare (buffer translator)
  (with-current-buffer (get-buffer-create buffer)
    (let ((inhibit-read-only t))
      (gts-with-slots (text src trg engines) translator
        ;; setup
        (deactivate-mark)
        (visual-line-mode -1)
        (font-lock-mode 1)
        (setq-local cursor-type 'hbar)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local gts-buffer-translator translator)
        ;; headline
        ;;(gts-buffer-init-header-line (cons src trg) (unless (cdr engines) (oref (car engines) tag)))
        ;; source text
        (erase-buffer)
        (unless (gts-childframe-of-buffer (current-buffer)) (insert "\n")) ; except childframe

        (if (cdr text)
            (gts-render-buffer translator)
          (insert (string-join text "\n\n")))

        ;; keybinds

        (read-only-mode 1)
        (use-local-map gts-buffer-local-map)
        ;; execute the hook if exists
        (run-hooks 'gts-after-buffer-prepared-hook)))))

(defun gts-render-buffer-1 (buffer task)
  "For single engine translation."
  (when-let ((inhibit-read-only t)
             (buf (get-buffer buffer)))
    (with-current-buffer buf
      (with-slots (res err meta engine translator) task
        ;; error
        (if err
            (progn
              (goto-char (point-max))
              (insert (propertize (format "\n\n%s" err) 'face 'gts-render-buffer-error-face)))
          (erase-buffer)
          (cond
           ;; content (non-split)
           ((stringp res)
            (let ((tbeg (plist-get meta :tbeg))
                  (tend (plist-get meta :tend)))
              (unless tbeg
                (insert (propertize (oref translator text) 'face 'gts-render-buffer-source-face) "\n\n"))
              (unless tbeg (setq tbeg (point)))
              (insert res)
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
                       for src in text
                       for tar in res
                       concat (concat (propertize src 'face 'gts-render-buffer-source-face) "\n\n" tar "\n\n") into parsed
                       finally (insert parsed)))))
        ;; update states
        (set-buffer-modified-p nil)
        (gts-buffer-change-header-line-state 'done)
        (put-text-property (point-min) (point-max) 'task task)
        ;; execute the hook if exists
        (run-hooks 'gts-after-buffer-render-hook))
      buf)))

(defun gts-render-buffer-multi-engines (buffer task)
  "For multiple engines translation."
  (when-let ((inhibit-read-only t) (buf (get-buffer buffer)))
    (with-slots (translator) task
      (with-slots (text tasks) translator
        (with-current-buffer buf
          (erase-buffer)
          (insert (propertize (format "\n%s\n\n" text) 'face 'gts-render-buffer-source-face))
          ;; content
          (save-excursion
            (dolist (task tasks)
              (with-slots (res err meta engine) task
                (let* ((result (if (or err (stringp res))
                                   res
                                 (string-join res "\n\n")))
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
          ;; all task parsed
          (when (= (oref translator state) 3)
            ;; cursor
            (unless (gts-childframe-of-buffer buf)
              (set-window-point (get-buffer-window buf t) (save-excursion (forward-line 2) (point))))
            ;; state
            (gts-buffer-change-header-line-state 'done))
          ;; execute the hook if exists
          (run-hooks 'gts-after-buffer-multiple-render-hook)
          buf)))))

(defun gts-render-buffer (translator)
  (when-let ((inhibit-read-only t)
             (buf (get-buffer gts-buffer-name)))
    (with-current-buffer buf
      (gts-with-slots (text render tasks) translator
        (let ((ret (gts-res render translator)))
          (save-excursion
            (erase-buffer)
            (if (cdr text)
                (cl-loop for c in text
                         for i from 0
                         for pt = (propertize c 'face 'font-lock-comment-face)
                         do (insert pt "\n\n"
                                    (cl-loop for (res prefix state trg engine) in ret
                                             for pc = (if (consp res) (nth i res) res)
                                             for pp = (if (or (cdr (oref translator trg))
                                                              (cdr (oref translator engines)))
                                                          (propertize (format "%s/%s " (oref engine tag) trg)
                                                                      'face 'font-lock-warning-face))
                                             concat (concat pp pc "\n\n"))))
              ;; format
              (insert "\n" (string-join text "\n\n") "\n\n")
              (cl-loop for (result prefix) in ret
                       for pp = (if (cdr ret) (propertize prefix 'face 'gts-pop-posframe-me-header-face))
                       collect (concat pp (if (stringp result) result (car result))) into results
                       finally (setq ret results))
              (insert (string-join ret "\n")))))

        ;; update states
        (set-buffer-modified-p nil)
        (gts-buffer-change-header-line-state 'done)
        ;;(put-text-property (point-min) (point-max) 'task task)
        ;; execute the hook if exists
        (run-hooks 'gts-after-buffer-render-hook))
      buf)))

(cl-defmethod gts-pre ((_ gts-buffer-render) translator)
  ;; init and setup
  (gts-render-buffer-prepare gts-buffer-name translator)
  ;; display
  (let ((split-width-threshold (or gts-split-width-threshold split-width-threshold)))
    (display-buffer gts-buffer-name gts-buffer-window-config)))

(cl-defmethod gts-res ((render gts-buffer-render) translator)
  (gts-with-slots (text trg tasks) translator
    (let (lst (trgs trg))
      (dolist (task tasks (nreverse lst))
        (with-slots (trg err res engine) task
          (let* ((state (if err 'err (if res 'done)))
                 (result (pcase state
                           ('err err)
                           ('done (gts-ensure-list res))
                           (_ "Loading...")))
                 (prefix (concat "[" (oref engine tag) (if (cdr trgs) (concat "::" trg)) "]" (if (cdr text) "\n" " "))))
            (push (list result prefix state trg engine) lst)))))))

(cl-defmethod gts-output ((_ gts-buffer-render) translator)
  ;; render & display
  (when-let ((buf (gts-render-buffer translator)))
    ;;(gts-buffer-ensure-a-blank-line-at-beginning buf)
    (when (= (oref translator state) 3)
      (gts-buffer-display-or-focus-buffer buf))))

(cl-defmethod gts-output-1 ((_ gts-buffer-render) translator)
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

(defcustom gts-posframe-pop-render-buffer " *GTS-Pop-Posframe*"
  "Buffer name of Pop Posframe."
  :type 'string
  :group 'go-translate)

(defvar gts-posframe-pop-render-timeout 30)
(defvar gts-posframe-pop-render-poshandler nil)

(declare-function posframe-show "ext:posframe.el" t t)
(declare-function posframe-delete "ext:posframe.el" t t)
(declare-function posframe-hide "ext:posframe.el" t t)
(declare-function posframe-refresh "ext:posframe.el" t t)
(declare-function posframe-poshandler-frame-top-right-corner "ext:posframe.el" t t)

(defun gts-posframe-init-header-line (src trg)
  (setq header-line-format
        (list "[" (propertize src 'face 'gts-render-buffer-header-line-lang-face) "]"
              " → "
              "[" (propertize trg 'face 'gts-render-buffer-header-line-lang-face) "]")))

(defun gts-posframe-render-auto-close-handler ()
  "Close the pop-up posframe window."
  (interactive)
  (unless (or (and gts-current-command
                   (member this-command (list gts-current-command #'exit-minibuffer)))
              (and gts-posframe-pop-render-buffer
                   (string= (buffer-name) gts-posframe-pop-render-buffer)))
    (ignore-errors (posframe-delete gts-posframe-pop-render-buffer))
    (remove-hook 'post-command-hook #'gts-posframe-render-auto-close-handler)))

(cl-defmethod gts-pre :before ((_ gts-posframe-pop-render) _)
  (unless (require 'posframe nil t)
    (user-error "To use `gts-posframe-render', you should install and load package `posframe' first")))

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

(cl-defmethod gts-multi-out ((_ gts-posframe-pin-render) task)
  ;; render & refresh
  (gts-render-buffer-multi-engines gts-posframe-pin-render-buffer task))


;;; [Render] kill-ring render

(defclass gts-kill-ring-render (gts-render) ())

(cl-defmethod gts-output ((render gts-kill-ring-render) translator)
  (deactivate-mark)
  (let ((ret (gts-res render translator)))
    ;; break when error
    (when-let (r (cl-find-if (lambda (r) (eq (nth 2 r) 'err)) ret))
      (kill-new "")
      (error "%s" (car r)))
    ;; output only when all tasks responsed
    (when (= (oref translator state) 3)
      (kill-new (mapconcat #'car ret "\n\n"))
      (message "Result already in the kill ring."))))


;;; [Render] Alert Render

(defclass gts-alert-render (gts-render) ())

(defvar gts-alert-args '(:timeout 10))

(declare-function alert "ext:alert.el" t t)

(cl-defmethod gts-pre :before ((_ gts-alert-render) _)
  (unless (require 'alert nil t)
    (user-error "To use `gts-alert-render', you should install and load package `alert' first")))

(cl-defmethod gts-output ((render gts-alert-render) translator)
  (when (= (oref translator state) 3)
    (let ((ret (gts-res render translator)))
      ;; format
      (cl-loop for (result prefix) in ret
               collect (concat (if (cdr ret) prefix) result) into results
               finally (setq ret results))
      ;; output
      (message "")
      (apply #'alert (string-join ret "\n") :title "*Go-Translate*" gts-alert-args))))


;;; [Texter] take current-at-point-or-selection as source text

(defclass gts-current-or-selection-texter (gts-texter) ())

(declare-function pdf-view-active-region-p "ext:pdf-view.el" t t)
(declare-function pdf-view-active-region-text "ext:pdf-view.el" t t)

(cl-defmethod gts-text ((_ gts-current-or-selection-texter))
  (cond ((eq major-mode 'pdf-view-mode)
         (if (pdf-view-active-region-p)
             (car (pdf-view-active-region-text))))
        ((use-region-p)
         (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))
        (t (current-word t t))))


;;; [Texter] take whole-buffer as source text

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
      (list text path))))


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
    (list current-text current-path)))



(defclass gts-fix-picker (gts-picker)
  ((text :initarg :text)
   (src :initarg :src)
   (trg :initarg :trg)))

(cl-defmethod gts-pick ((picker gts-fix-picker))
  (setq gts-picker-current-picker picker)
  (with-slots (text src trg) picker
    (list text src trg)))

(provide 'gts-implements)

;;; gts-implements.el ends here
