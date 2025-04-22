;;; gt-extensions.el --- Extension components -*- lexical-binding: t -*-

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

;; Extension components.

;;; Code:

(require 'gt-core)

(require 'gt-engine-bing)
(require 'gt-engine-google)
(require 'gt-engine-google-rpc)
(require 'gt-engine-deepl)
(require 'gt-engine-stardict)
(require 'gt-engine-osxdict)
(require 'gt-engine-youdao)
(require 'gt-engine-chatgpt)
(require 'gt-engine-libre)
(require 'gt-engine-echo)

(require 'gt-render-alert)
(require 'gt-render-buffer)
(require 'gt-render-insert)
(require 'gt-render-kill-ring)
(require 'gt-render-overlay)
(require 'gt-render-posframe)

(require 'gt-taker-fresh-words)
(require 'gt-taker-buffer-prompt)


;;; [Taker] pdf-view-mode

(declare-function pdf-view-active-region-p "ext:pdf-view.el" t t)
(declare-function pdf-view-active-region-text "ext:pdf-view.el" t t)

(cl-defmethod gt-thing-at-point (_thing (_ (eql 'pdf-view-mode)))
  (if (pdf-view-active-region-p)
      (pdf-view-active-region-text)
    (user-error "You should make a selection before translate")))

(provide 'gt-extensions)

;;; gt-extensions.el ends here
