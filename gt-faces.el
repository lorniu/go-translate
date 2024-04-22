;;; gt-faces.el --- Face definitions -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Author: lorniu <lorniu@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; The faces and colors.

;;; Code:

(require 'gt-core)

(defgroup go-translate-faces nil
  "The faces for this framework."
  :group 'go-translate)


;;; Common

(defface gt-logger-buffer-tag-face '((t :inherit font-lock-comment-face))
  "Used in the logger buffer tag keyword."
  :group 'go-translate-faces)

(defface gt-logger-buffer-timestamp-face '((t :foreground "grey"))
  "Used in the logger buffer timestamp keyword."
  :group 'go-translate-faces)

(defface gt-render-prefix-face '((t :foreground "grey"))
  "Used in the buffer's block prefix."
  :group 'go-translate-faces)


;;; Buffer Render

(defface gt-buffer-render-header-lang-face '((t :inherit font-lock-keyword-face))
  "Used in the buffer's `header-line-format' for langs."
  :group 'go-translate-faces)

(defface gt-buffer-render-header-desc-face '((t :inherit font-lock-variable-name-face))
  "Used in the buffer's `header-line-format' for description."
  :group 'go-translate-faces)

(defface gt-buffer-render-source-face '((t :inherit font-lock-doc-face))
  "Used in the buffer's source."
  :group 'go-translate-faces)

(defface gt-buffer-render-inline-prefix-face '((t :inherit font-lock-keyword-face))
  "Used in the buffer's inline prefix."
  :group 'go-translate-faces)

(defface gt-buffer-render-block-prefix-face '((t :background "slategray" :foreground "white" :extend t))
  "Used in the buffer's block prefix."
  :group 'go-translate-faces)

(defface gt-buffer-render-error-face '((t :weight bold :inherit font-lock-comment-face))
  "Used in the buffer's error text."
  :group 'go-translate-faces)

(defface gt-buffer-render-loading-face '((t :inherit font-lock-comment-face))
  "Used in the buffer's loading messaget."
  :group 'go-translate-faces)


;;; Google Buffer Render

(defface gt-google-buffer-headline-face '((t :inherit font-lock-function-name-face :weight bold))
  "Propertize the headline in buffer rendering."
  :group 'go-translate-faces)

(defface gt-google-buffer-phonetic-face '((t :inherit font-lock-string-face :slant normal))
  "Propertize the phonetic in buffer rendering."
  :group 'go-translate-faces)

(defface gt-google-buffer-suggestion-desc-face '((t :inherit font-lock-warning-face))
  "Propertize the suggestion description in buffer rendering."
  :group 'go-translate-faces)

(defface gt-google-buffer-suggestion-text-face '((t :slant italic :underline t))
  "Propertize the phonetic text in buffer rendering."
  :group 'go-translate-faces)

(defface gt-google-buffer-brief-result-face '((t :weight bold))
  "Propertize the brief result in buffer rendering."
  :group 'go-translate-faces)

(defface gt-google-buffer-source-face '((t :inherit font-lock-string-face))
  "Propertize the source in buffer rendering."
  :group 'go-translate-faces)

(defface gt-google-buffer-detail-demo-face '((t :inherit font-lock-doc-face))
  "Propertize the detail demo in buffer rendering."
  :group 'go-translate-faces)


;;; Posframe Render

(defcustom gt-pop-posframe-forecolor "white"
  "Default foreground color of pop posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gt-pop-posframe-backcolor "black"
  "Default background color of pop posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gt-pin-posframe-forecolor nil
  "Default foreground color of pin posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gt-pin-posframe-backcolor nil
  "Default background color of pin posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gt-pin-posframe-bdcolor "#000000"
  "Default border color of pin posframe."
  :type 'string
  :group 'go-translate-faces)

(defcustom gt-pin-posframe-fringe-color nil
  "Used in the posframe pip buffer fringe color."
  :type 'string
  :group 'go-translate-faces)


;;; Overlay Render

(defface gt-overlay-source-face '((t :underline t))
  "Used in the overlay's source."
  :group 'go-translate-faces)

(defface gt-overlay-result-face '((t :inherit font-lock-string-face))
  "Used in the overlay's result."
  :group 'go-translate-faces)

(defface gt-overlay-prefix-face '((t :inherit font-lock-comment-face))
  "Used in the overlay's prefix."
  :group 'go-translate-faces)


;;; Misc

(defface gt-bionic-reading-face '((t :weight bold))
  "Face for bionic reading word."
  :group 'go-translate-faces)

(provide 'gt-faces)

;;; gt-faces.el ends here
