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
;; TODO: Improve it.

;;; Code:

(require 'gts-implements)

(defclass gts-stardict-engine (gts-engine)
  ((tag      :initform "StarDict")
   (parser   :initform (gts-stardict-parser))))

(defcustom gts-stardict-program "sdcv"
  "Executable command of sdcv (StarDict Console Version)."
  :type 'string
  :group 'go-translate)

(cl-defmethod gts-translate ((engine gts-stardict-engine) task rendercb)
  (let* ((text (oref task text))
         (parser (oref engine parser))
         (result (with-temp-buffer
                   (apply #'call-process gts-stardict-program nil t nil
                          (list "--non-interactive" text)) ;--json-output
                   (buffer-string))))
    (gts-update-raw task result)
    (gts-parse parser task)
    (funcall rendercb)))


(provide 'gts-engine-stardict)

;;; gts-engine-stardict.el ends here
