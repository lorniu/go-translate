;;; gts-engine-google-rpc.el --- Google translation with rpc api -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; http://translate.google.com, new RPC API

;;; Code:

(require 'gts-engine-google)

(defcustom gts-google-rpc-base-url "https://translate.google.com"
  "The base url of Google translate used by google-rpc engine.
you can customize it according to your country region."
  :type 'string
  :group 'go-translate)


;;; Components

(defclass gts-google-rpc-parser (gts-google-parser)
  ((tag :initform "Detail")))

(defclass gts-google-rpc-summary-parser (gts-google-summary-parser gts-google-rpc-parser)
  ((tag :initform "Summary")))

(defclass gts-google-rpc-engine (gts-engine)
  ((tag       :initform "Google/RPC")
   (sub-url   :initform "/_/TranslateWebserverUi/data/batchexecute")
   (parser    :initform (gts-google-rpc-parser))

   (rpc-translate :initform "MkEWBc")
   (rpc-tts       :initform "jQ1olc")
   (rpc-sid       :initform "FdrFJe")
   (rpc-bl        :initform "cfb2h")))


;;; Engine

(defvar gts-google-rpc-request-headers '(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8")))

(cl-defmethod gts-with-token ((engine gts-google-rpc-engine) done fail)
  (with-slots (sub-url rpc-sid rpc-bl) engine
    (gts-do-request gts-google-rpc-base-url
                    :done
                    (lambda ()
                      (let* ((f-sid (progn
                                      (re-search-forward (format "\"%s\":\"\\([^\"]*\\)\"" rpc-sid))
                                      (match-string 1)))
                             (bl (progn (re-search-forward (format "\"%s\":\"\\([^\"]*\\)\"" rpc-bl))
                                        (match-string 1)))
                             (url-tpl (lambda (rpcid lang)
                                        (format "%s%s?%s" gts-google-rpc-base-url sub-url
                                                (gts-format-params
                                                 `(("rpcids"       . ,rpcid)
                                                   ("f.sid"        . ,f-sid)
                                                   ("bl"           . ,bl)
                                                   ("hl"           . ,lang)
                                                   ("soc-app"      . 1)
                                                   ("soc-platform" . 1)
                                                   ("soc-device"   . 1)
                                                   ("_reqid"       . ,(+ 1000 (random 9000)))
                                                   ("rt"           . "c")))))))
                        (funcall done url-tpl)))
                    :fail fail)))

(cl-defmethod gts-translate ((engine gts-google-rpc-engine) task rendercb)
  (gts-with-token engine
    (lambda (url-tpl)
      (with-slots (text from to) task
        (with-slots (rpc-translate parser) engine
          (gts-do-request (funcall url-tpl rpc-translate to)
                          :headers gts-google-rpc-request-headers
                          :data (format
                                 "f.req=%s&"
                                 (url-hexify-string
                                  (let ((outer [[[rpcid inner nil "generic"]]])
                                        (inner [[text from to t][nil]]))
                                    (setf (aref (aref inner 0) 0) text)
                                    (setf (aref (aref inner 0) 1) from)
                                    (setf (aref (aref inner 0) 2) to)
                                    (setf (aref (aref (aref outer 0) 0) 0) rpc-translate)
                                    (setf (aref (aref (aref outer 0) 0) 1) (json-encode inner))
                                    (json-encode outer))))
                          :done (lambda ()
                                  (gts-update-raw task (buffer-string))
                                  (gts-parse parser task)
                                  (funcall rendercb))
                          :fail (lambda (err)
                                  (gts-render-fail task err))))))
    (lambda (err)
      (gts-render-fail task
        (format "Error occurred when request for token.\n\n%s" err)))))

(cl-defmethod gts-tts ((engine gts-google-rpc-engine) text lang)
  (gts-with-token engine
    (lambda (url-tpl)
      (with-slots (rpc-tts) engine
        (gts-do-request (funcall url-tpl rpc-tts "en-US")
                        :headers gts-google-rpc-request-headers
                        :data (format
                               "f.req=%s&"
                               (url-hexify-string
                                (let ((outer [[[rpcid inner nil "generic"]]])
                                      (inner [text lang nil nil]))
                                  (setf (aref inner 0) text)
                                  (setf (aref inner 1) lang)
                                  (setf (aref (aref (aref outer 0) 0) 0) rpc-tts)
                                  (setf (aref (aref (aref outer 0) 0) 1) (json-encode inner))
                                  (json-encode outer))))
                        :done (lambda ()
                                (let (beg end json code)
                                  (re-search-forward "^[0-9]+$")
                                  (setq beg (point))
                                  (re-search-forward "^\\([0-9]+\\)$")
                                  (setq end (- (point) (length (match-string 1))))
                                  (setq json (json-read-from-string (string-trim (buffer-substring-no-properties beg end))))
                                  (when (string= (gts-aref-for json 0 0) "wrb.fr")
                                    (setq code (aref (json-read-from-string (gts-aref-for json 0 2)) 0))
                                    (erase-buffer)
                                    (insert code)
                                    (base64-decode-region (point-min) (point-max))
                                    (gts-tts-speak-buffer-data))))
                        :fail (lambda (err) (user-error "Error when TTS. %s" err)))))
    (lambda (_) (user-error "Error occurred when request for token"))))


;;; Parser

(cl-defmethod gts-resp-to-json ((_ gts-google-rpc-parser) resp)
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

(cl-defmethod gts-result--brief ((_ gts-google-rpc-parser) json)
  "Get the translation text from JSON."
  (mapconcat
   (lambda (item)
     (if (ignore-errors (aref item 2))
         (format " %s" (aref item 0))
       (aref item 0)))
   (gts-aref-for json 1 0 0 5)
   ""))

(cl-defmethod gts-result--sphonetic ((_ gts-google-rpc-parser) json)
  "Get the text phonetic from JSON."
  (gts-aref-for json 0 0))

(cl-defmethod gts-result--tphonetic ((_ gts-google-rpc-parser) json)
  "Get the translation phonetic from JSON."
  (gts-aref-for json 1 0 0 1))

(cl-defmethod gts-result--details ((_ gts-google-rpc-parser) json)
  "Get the details from JSON.
Result style: ((noun (a (x y z))) (verb (b (m n o))))."
  (cl-loop with dts = (ignore-errors (gts-aref-for json 3 5 0))
           for i across dts
           collect
           (cons
            (aref i 0)
            (cl-loop for j across (aref i 1)
                     collect
                     (cons
                      (aref j 0)
                      (cl-loop for k across (aref j 2) collect k))))))

(cl-defmethod gts-result--definitions ((_ gts-google-rpc-parser) json)
  "Get the definitions from JSON.
Result style: ((noun (a b)) (verb (c d)))."
  (cl-loop with defs = (ignore-errors (gts-aref-for json 3 1 0))
           for i across defs
           collect
           (cons
            (aref i 0)
            (cl-loop for j across (aref i 1)
                     collect
                     (cons (aref j 0)
                           (ignore-errors (aref j 1)))))))

(cl-defmethod gts-result--suggestion ((_ gts-google-rpc-parser) json)
  "Get the suggestion from JSON."
  (ignore-errors
    (replace-regexp-in-string "<b>\\|</b>\\|<i>\\|</i>" "" (gts-aref-for json 0 1 0 0 1))))


(provide 'gts-engine-google-rpc)

;;; gts-engine-google-rpc.el ends here
