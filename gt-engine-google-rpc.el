;;; gt-engine-google-rpc.el --- Google translation with RPC API -*- lexical-binding: t -*-

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

;; http://translate.google.com, new RPC API

;;; Code:

(require 'gt-engine-google)

(defgroup go-translate-google-rpc nil
  "Configs for GoogleRPC engine."
  :group 'go-translate)

(defcustom gt-google-rpc-host "https://translate.google.com"
  "The base url of Google translate used by google-rpc engine.
you can customize it according to your country region."
  :type 'string
  :group 'go-translate-google-rpc)


;;; Components

(defclass gt-google-rpc-parser (gt-google-parser)
  ((tag :initform "Detail")))

(defclass gt-google-rpc-summary-parser (gt-google-summary-parser gt-google-rpc-parser)
  ((tag :initform "Summary")))

(defclass gt-google-rpc-engine (gt-engine)
  ((tag   :initform 'GoogleRPC)
   (host  :initform nil)
   (path  :initform "/_/TranslateWebserverUi/data/batchexecute")
   (parse :initform (gt-google-rpc-parser))

   (rpc-translate :initform "MkEWBc")
   (rpc-tts       :initform "jQ1olc")
   (rpc-sid       :initform "FdrFJe")
   (rpc-bl        :initform "cfb2h")))


;;; Engine

(defcustom gt-google-rpc-request-headers
  '(("Connection" . "Keep-Alive")
    ("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8"))
  "Extra request headers send to google-rpc server."
  :type '(alist :key-type (string :tag "Key") :value-type (string :tag "Value"))
  :group 'go-translate-google-rpc)

(defun gt-google-rpc-with-token (engine done fail)
  (declare (indent 1))
  (with-slots (host path rpc-sid rpc-bl) engine
    (gt-request :url (or host gt-google-rpc-host)
                :done (lambda (raw)
                        (with-temp-buffer
                          (insert raw)
                          (goto-char (point-min))
                          (let* ((f-sid (progn
                                          (re-search-forward (format "\"%s\":\"\\([^\"]*\\)\"" rpc-sid))
                                          (match-string 1)))
                                 (bl (progn (re-search-forward (format "\"%s\":\"\\([^\"]*\\)\"" rpc-bl))
                                            (match-string 1)))
                                 (url-tpl (lambda (rpcid lang)
                                            (format "%s%s?%s" (or host gt-google-rpc-host) path
                                                    (gt-format-params
                                                     `(("rpcids"       . ,rpcid)
                                                       ("f.sid"        . ,f-sid)
                                                       ("bl"           . ,bl)
                                                       ("hl"           . ,lang)
                                                       ("soc-app"      . 1)
                                                       ("soc-platform" . 1)
                                                       ("soc-device"   . 1)
                                                       ("_reqid"       . ,(+ 1000 (random 9000)))
                                                       ("rt"           . "c")))))))
                            (funcall done url-tpl))))
                :fail fail)))

(cl-defmethod gt-translate ((engine gt-google-rpc-engine) task next)
  (gt-google-rpc-with-token engine
    (lambda (url-tpl)
      (with-slots (text src tgt res) task
        (with-slots (rpc-translate) engine
          (gt-request :url (funcall url-tpl rpc-translate tgt)
                      :headers gt-google-rpc-request-headers
                      :data (format
                             "f.req=%s&"
                             (url-hexify-string
                              (json-encode `[[[,rpc-translate ,(json-encode `[[,text ,src ,tgt 1][]]) nil "generic"]]])))
                      :done (lambda (raw) (setf res raw) (funcall next task))
                      :fail (lambda (err) (gt-fail task err))))))
    (lambda (err)
      (gt-fail task (format "Take token failed, %s" err)))))

(cl-defmethod gt-speak ((engine gt-google-rpc-engine) text lang)
  (with-slots (host rpc-tts) engine
    (message "Requesting %s for %s..." (or host gt-google-rpc-host) lang)
    (gt-google-rpc-with-token engine
      (lambda (url-tpl)
        (gt-request :url (funcall url-tpl rpc-tts "en-US")
                    :headers gt-google-rpc-request-headers
                    :data (format
                           "f.req=%s&"
                           (url-hexify-string
                            (json-encode `[[[,rpc-tts ,(json-encode `[,text ,lang nil "undefined" [0]]) nil "generic"]]])))
                    :done (lambda (raw)
                            (with-temp-buffer
                              (insert raw)
                              (goto-char (point-min))
                              (let (beg end json code)
                                (re-search-forward "^[0-9]+$")
                                (setq beg (point))
                                (re-search-forward "^\\([0-9]+\\)$")
                                (setq end (- (point) (length (match-string 1))))
                                (setq json (json-read-from-string (string-trim (buffer-substring-no-properties beg end))))
                                (if-let (data (and (string= (gt-aref json 0 0) "wrb.fr") (gt-aref json 0 2)))
                                    (progn (setq code (aref (json-read-from-string data) 0))
                                           (erase-buffer)
                                           (insert code)
                                           (base64-decode-region (point-min) (point-max))
                                           (gt-play-audio (current-buffer)))
                                  (message "[GoogleRPC-TTS] No tts data responsed")))))
                    :fail (lambda (err) (message "[GoogleRPC-TTS] Error, %s" err))))
      (lambda (err) (message "[GoogleRPC-TTS] Take token failed, %s" err)))))


;;; Parser

(cl-defmethod gt-resp-to-json ((_ gt-google-rpc-parser) resp)
  "Convert the buffer RESP into JSON format."
  (condition-case err
      (with-temp-buffer
        (let (beg end str json)
          (insert resp)
          (goto-char (point-min))
          (re-search-forward "^[0-9]+$")
          (setq beg (point))
          (re-search-forward "^\\([0-9]+\\)$")
          (setq end (- (point) (length (match-string 1))))
          (setq str
                (decode-coding-string
                 (string-trim (buffer-substring-no-properties beg end))
                 'utf-8))
          (setq json (json-read-from-string str))
          (if (string= (aref (aref json 0) 0) "wrb.fr")
              (let ((json-str (aref (aref json 0) 2)))
                (json-read-from-string json-str))
            (error "No results found"))))
    (error (user-error "Result conversion error (%s)" err))))

(cl-defmethod gt-result--brief ((_ gt-google-rpc-parser) json)
  "Get the translation text from JSON."
  (mapconcat
   (lambda (item)
     (if (ignore-errors (aref item 2))
         (format " %s" (aref item 0))
       (aref item 0)))
   (gt-aref json 1 0 0 5)
   ""))

(cl-defmethod gt-result--sphonetic ((_ gt-google-rpc-parser) json)
  "Get the text phonetic from JSON."
  (gt-aref json 0 0))

(cl-defmethod gt-result--tphonetic ((_ gt-google-rpc-parser) json)
  "Get the translation phonetic from JSON."
  (gt-aref json 1 0 0 1))

(cl-defmethod gt-result--details ((_ gt-google-rpc-parser) json)
  "Get the details from JSON.
Result style: ((noun (a (x y z))) (verb (b (m n o))))."
  (cl-loop with dts = (ignore-errors (gt-aref json 3 5 0))
           for i across dts
           collect
           (cons
            (aref i 0)
            (cl-loop for j across (aref i 1)
                     collect
                     (cons
                      (aref j 0)
                      (cl-loop for k across (aref j 2) collect k))))))

(cl-defmethod gt-result--definitions ((_ gt-google-rpc-parser) json)
  "Get the definitions from JSON.
Result style: ((noun (a b)) (verb (c d)))."
  (cl-loop with defs = (ignore-errors (gt-aref json 3 1 0))
           for i across defs
           collect
           (cons
            (aref i 0)
            (cl-loop for j across (aref i 1)
                     collect
                     (cons (aref j 0)
                           (ignore-errors (aref j 1)))))))

(cl-defmethod gt-result--suggestion ((_ gt-google-rpc-parser) json)
  "Get the suggestion from JSON."
  (ignore-errors
    (replace-regexp-in-string "<b>\\|</b>\\|<i>\\|</i>" "" (gt-aref json 0 1 0 0 1))))

(provide 'gt-engine-google-rpc)

;;; gt-engine-google-rpc.el ends here
