;;; go-translate.el --- Translation framework supports multiple engines such as Google/Bing/DeepL -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/go-translate
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; Version: 2.0

;;; Commentary:

;; To be the most powerful translator on Emacs. Supports multiple translation engines such as Google, Bing, deepL.

;; First, Install it via MELPA or download from github. Make sure this is on your `load-path'.

;; Then, add following lines to your `.emacs':

;;   (require 'go-translate)
;;   (setq gts-translate-list '(("en" "zh")))
;;   (setq gts-default-translator
;;        (gts-translator
;;         :picker (gts-prompt-picker)
;;         :engines (list (gts-google-engine) (gts-google-rpc-engine))
;;         :render (gts-buffer-render)))

;; And start your translate with command `gts-do-translate'.

;;; Code:

(require 'gts-core)
(require 'gts-implements)
(require 'gts-engine-bing)
(require 'gts-engine-google)
(require 'gts-engine-google-rpc)
(require 'gts-engine-deepl)
(require 'gts-engine-stardict)
(require 'gts-engine-youdao)


;;; Commands

(defvar gts-default-translator
  (gts-translator :picker
                  (gts-prompt-picker)
                  ;;(gts-noprompt-picker)
                  ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))

                  :engines
                  (list
                   (gts-bing-engine)
                   (gts-google-engine :parser (gts-google-summary-parser))
                   (gts-google-rpc-engine)

                   ;;(gts-google-engine)
                   ;;(gts-google-rpc-engine)
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

                  :splitter nil
                  ;; (gts-paragraph-splitter)
                  ))

;;;###autoload
(defun gts-do-translate ()
  "Do the translate"
  (interactive)
  (gts-translate gts-default-translator))


(provide 'go-translate)

;;; go-translate.el ends here
