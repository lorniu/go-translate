;;; gts.el --- translation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'url)
(require 'facemenu)
(require 'gts-core)
(require 'gts-implements)
(require 'gts-google)

;;;###autoload
(defun gts-do-translate ()
  "Do the translate"
  (interactive)
  (gts-translate gts-default-translator))


;;; Predefined instances

(defvar gts-default-picker (gts-prompt-picker :texter (gts-current-or-selection-texter)))
(defvar gts-default-engine (gts-google-engine :parser (gts-google-parser)))
(defvar gts-default-me-engine (gts-google-engine :parser (gts-google-summary-parser)))
(defvar gts-default-render (gts-buffer-render))
(defvar gts-default-translator)

;;; Custom your translator

(setq gts-debug-p nil)

(setq gts-translate-list '(("en" "zh") ("fr" "zh")))

(setq gts-default-translator
      (gts-translator

       :picker
       ;;(gts-noprompt-picker)
       (gts-prompt-picker)
       ;;(gts-prompt-picker :single t) ;; (a, b)
       ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
       ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

       :engines
       (list
        (gts-google-engine)
        ;;(gts-google-engine :parser (gts-google-summary-parser))
        ;;(gts-google-engine :parser (gts-google-parser))
        )

       :render
       ;;(gts-buffer-render)

       ;;(gts-posframe-pop-render)
       ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")

       ;;(gts-posframe-pin-render)
       ;;(gts-posframe-pin-render :position (cons 1200 20))
       (gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")

       ;;(gts-kill-ring-render)
       ))


(provide 'gts)

;;; gts.el ends here
