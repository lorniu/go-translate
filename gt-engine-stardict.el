;;; gt-engine-stardict.el --- StarDict -*- lexical-binding: t -*-

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

;; Engine for sdcv (StarDict Console Version).
;;
;; sdcv is an offline translate software. It can be used when no network.
;;
;; Make sure sdcv is installed on your system:
;;
;;   sudo pacman -S sdcv
;;
;; Then download dictionary data to ~/.stardict/dic or /usr/share/stardict/dic:
;;
;;   # For example from http://download.huzheng.org:
;;
;;   mkdir -p ~/.stardict/dic
;;   cd ~/.stardict/dic
;;   wget http://download.huzheng.org/zh_CN/stardict-langdao-ce-gb-2.4.2.tar.bz2
;;   wget http://download.huzheng.org/zh_CN/stardict-langdao-ce-gb-2.4.2.tar.bz2
;;   tar xvf stardict-langdao-ec-gb-2.4.2.tar.bz2
;;   tar xvf stardict-langdao-ce-gb-2.4.2.tar.bz2
;;   sdcv -l
;;
;; TODO: Provide more options and a better parser.

;;; Code:

(require 'gt-extension)

(defgroup go-translate-stardict nil
  "Configs for stardict engine."
  :group 'go-translate)

(defcustom gt-stardict-program "sdcv"
  "Executable command of sdcv (StarDict Console Version)."
  :type 'string
  :group 'go-translate-stardict)

(defcustom gt-stardict-args '("-n" "-c" "-0" "-1")
  "Arguments passed to command `sdcv'."
  :type '(repeat string)
  :group 'go-translate-stardict)

(defclass gt-stardict-engine (gt-engine)
  ((tag :initform 'StarDict)
   (cache :initform nil)))

(cl-defmethod gt-translate ((_ gt-stardict-engine) task next)
  (with-temp-buffer
    (require 'ansi-color)
    (let ((text (oref task text)))
      (apply #'call-process gt-stardict-program nil t nil
             (append gt-stardict-args (list text))) ;--json-output
      (if (string-equal (buffer-substring 1 16) "Nothing similar")
          (progn
            (delete-region (point-min) (point-max))
            (insert "No translation result found, sorry :("))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (oset task res (buffer-string))
      (funcall next task))))

(provide 'gt-engine-stardict)

;;; gt-engine-stardict.el ends here
