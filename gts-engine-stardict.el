;;; gts-engine-stardict.el --- StarDict -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

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

(require 'gts-implements)

(defclass gts-stardict-engine (gts-engine)
  ((tag      :initform "StarDict")
   (parser   :initform (gts-parser))))

(defcustom gts-stardict-program "sdcv"
  "Executable command of sdcv (StarDict Console Version)."
  :type 'string
  :group 'go-translate)

(defcustom gts-stardict-args '("-n" "-c" "-0" "-1")
  "Arguments passed to command `sdcv'."
  :type 'list
  :group 'go-translate)

(cl-defmethod gts-translate ((engine gts-stardict-engine) task next)
  (with-temp-buffer
    (require 'ansi-color)
    (let* ((text (oref task text))
           (parser (oref engine parser)))
      (apply #'call-process gts-stardict-program nil t nil
             (append gts-stardict-args (list text))) ;--json-output
      (if (string-equal (buffer-substring 1 16) "Nothing similar")
          (progn
            (delete-region (point-min) (point-max))
            (insert (propertize "No translation result found, sorry :(" 'face 'font-lock-warning-face)))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (funcall next task))))

(provide 'gts-engine-stardict)

;;; gts-engine-stardict.el ends here
