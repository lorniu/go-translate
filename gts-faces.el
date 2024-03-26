;;; gts-faces.el --- Face definitions -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The faces and colors.

;;; Code:

(require 'gts-core)

(defgroup go-translate-faces nil
  "The faces for this framework."
  :group 'go-translate)


;;; Common

(defface gts-logger-buffer-tag-face '((t :inherit font-lock-comment-face))
  "Used in the logger buffer tag keyword."
  :group 'go-translate-faces)

(defface gts-logger-buffer-timestamp-face '((t :foreground "grey"))
  "Used in the logger buffer timestamp keyword."
  :group 'go-translate-faces)

(defface gts-render-prefix-face '((t :foreground "grey"))
  "Used in the buffer's block prefix."
  :group 'go-translate-faces)


;;; Buffer Render

(defface gts-buffer-render-header-lang-face '((t :inherit font-lock-keyword-face))
  "Used in the buffer's `header-line-format' for langs."
  :group 'go-translate-faces)

(defface gts-buffer-render-header-desc-face '((t :inherit font-lock-variable-name-face))
  "Used in the buffer's `header-line-format' for description."
  :group 'go-translate-faces)

(defface gts-buffer-render-source-face '((t :inherit font-lock-doc-face))
  "Used in the buffer's source."
  :group 'go-translate-faces)

(defface gts-buffer-render-inline-prefix-face '((t :inherit font-lock-keyword-face))
  "Used in the buffer's inline prefix."
  :group 'go-translate-faces)

(defface gts-buffer-render-block-prefix-face '((t :background "slategray" :foreground "white" :extend t))
  "Used in the buffer's block prefix."
  :group 'go-translate-faces)

(defface gts-buffer-render-error-face '((t :weight bold :inherit font-lock-comment-face))
  "Used in the buffer's error text."
  :group 'go-translate-faces)

(defface gts-buffer-render-loading-face '((t :inherit font-lock-comment-face))
  "Used in the buffer's loading messaget."
  :group 'go-translate-faces)


;;; Google Buffer Render

(defface gts-google-buffer-headline-face '((t :inherit font-lock-function-name-face :weight bold))
  "Propertize the headline in buffer rendering."
  :group 'go-translate-faces)

(defface gts-google-buffer-phonetic-face '((t :inherit font-lock-string-face :slant normal))
  "Propertize the phonetic in buffer rendering."
  :group 'go-translate-faces)

(defface gts-google-buffer-suggestion-desc-face '((t :inherit font-lock-warning-face))
  "Propertize the suggestion description in buffer rendering."
  :group 'go-translate-faces)

(defface gts-google-buffer-suggestion-text-face '((t :slant italic :underline t))
  "Propertize the phonetic text in buffer rendering."
  :group 'go-translate-faces)

(defface gts-google-buffer-brief-result-face '((t :weight bold))
  "Propertize the brief result in buffer rendering."
  :group 'go-translate-faces)

(defface gts-google-buffer-source-face '((t :inherit font-lock-string-face))
  "Propertize the source in buffer rendering."
  :group 'go-translate-faces)

(defface gts-google-buffer-detail-demo-face '((t :inherit font-lock-doc-face))
  "Propertize the detail demo in buffer rendering."
  :group 'go-translate-faces)


;;; Posframe Render

(defcustom gts-pop-posframe-forecolor "white"
  "Default foreground color of pop posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gts-pop-posframe-backcolor "black"
  "Default background color of pop posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gts-pin-posframe-forecolor nil
  "Default foreground color of pin posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gts-pin-posframe-backcolor nil
  "Default background color of pin posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gts-pin-posframe-bdcolor "#000000"
  "Default border color of pin posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gts-pin-posframe-fringe-color nil
  "Used in the posframe pip buffer fringe color."
  :type 'string
  :group 'go-translate-faces)


;;; Overlay Render

(defface gts-overlay-result-face '((t :inherit font-lock-doc-face))
  "Used in the overy's result."
  :group 'go-translate-faces)

(provide 'gts-faces)

;;; gts-faces.el ends here
