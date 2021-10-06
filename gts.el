;;; gts.el --- translation -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/go-translate
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; Version: 0.2

;;; Commentary:

;; First, Install it via MELPA or download from github.
;; Make sure this is on your `load-path'.

;; Then add following lines to your `.emacs':

;; (require 'gts)
;; (setq gts-translate-list '(("en" "zh")))
;; (setq gts-default-translator
;;      (gts-translator
;;       :picker (gts-prompt-picker)
;;       :engines (list (gts-google-engine) (gts-google-rpc-engine))
;;       :render (gts-buffer-render)))

;; Now you can start your translation with `gts-do-translate' command.

;;; Code:

(require 'url)
(require 'facemenu)
(require 'gts-core)
(require 'gts-implements)

;; engines
(require 'gts-google)
(require 'gts-google-rpc)
(require 'gts-deepl)

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

;;; Custom your translator

;; (setq gts-translate-list '(("en" "zh") ("fr" "zh")))

(defvar gts-default-translator
  (gts-translator

   :picker
   ;;(gts-noprompt-picker)
   ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))

   (gts-prompt-picker)
   ;;(gts-prompt-picker :single t)
   ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

   :engines
   (list
    ;;(gts-google-engine)
    (gts-google-rpc-engine)
    ;;(gts-deepl-engine :auth-key "2e20bade-88e9-02f3-169f-ab3c445d7984:fx" :pro nil)

    ;;(gts-google-engine :parser (gts-google-summary-parser))
    ;;(gts-google-engine :parser (gts-google-parser))
    ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser))
    )

   :render
   (gts-buffer-render)

   ;;(gts-posframe-pop-render)
   ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")

   ;;(gts-posframe-pin-render)
   ;;(gts-posframe-pin-render :position (cons 1200 20))
   ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")

   ;;(gts-kill-ring-render)
   ))


(provide 'gts)

;;; gts.el ends here
