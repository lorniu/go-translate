;;; gts-engine-deepl.el --- Translate Engine for DeepL -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; https://www.deepl.com

;;; Code:

(require 'gts-implements)

(defclass gts-deepl-parser (gts-parser) ())

(defclass gts-deepl-engine (gts-engine)
  ((tag      :initform "DeepL")
   (pro      :initform nil :initarg :pro :documentation "If your key is PRO version")
   (free-url :initform "https://api-free.deepl.com")
   (pro-url  :initform "https://api.deepl.com")
   (sub-url  :initform "/v2/translate")
   (auth-key :initform nil :initarg :auth-key)
   (parser   :initform (gts-deepl-parser))))

(cl-defmethod initialize-instance :after ((engine gts-deepl-engine) &rest _)
  (unless (oref engine auth-key)
    (user-error "You should provide a auth-key when instance gts-deepl-engine")))


;;; Engine

(defvar gts-deepl-langs-mapping '(("en" . "EN")
                                  ("zh" . "ZH")
                                  ("de" . "DE") ;; German
                                  ("fr" . "FR") ;; French
                                  ("it" . "IT") ;; Italian
                                  ("ja" . "JA") ;; Japanese
                                  ("es" . "ES") ;; Spanish
                                  ("nl" . "NL") ;; Dutch
                                  ("pl" . "PL") ;; Polish
                                  ("pt" . "PT") ;; Portuguese (all Portuguese varieties mixed)
                                  ("ru" . "RU") ;; Russian
                                  ))

(cl-defmethod gts-get-lang ((_ gts-deepl-engine) lang)
  (let ((mapping (assoc lang gts-deepl-langs-mapping)))
    (if mapping (cdr mapping)
      (user-error
       "Language %s not supported by DeepL.\nSupported list: %s"
       lang (mapconcat #'car gts-deepl-langs-mapping ", ")))))

(cl-defmethod gts-gen-url ((engine gts-deepl-engine))
  (with-slots (free-url pro-url sub-url pro) engine
    (format "%s%s" (if pro pro-url free-url) sub-url)))

(cl-defmethod gts-translate ((engine gts-deepl-engine) task rendercb)
  (with-slots (text from to) task
    (with-slots (auth-key parser) engine
      (gts-do-request (gts-gen-url engine)
                      :headers '(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8"))
                      :data `(("auth_key" . ,auth-key)
                              ("text" . ,text)
                              ("source_lang" . ,(gts-get-lang engine from))
                              ("target_lang" . ,(gts-get-lang engine to)))
                      :done (lambda ()
                              (gts-update-raw task (buffer-string))
                              (gts-parse parser task)
                              (funcall rendercb))
                      :fail (lambda (err)
                              (gts-render-fail task
                                (cond ((ignore-errors (= (caddar err) 403))
                                       "[403] http error, make sure your auth_key is correct")
                                      (t err))))))))


;;; Parser

(cl-defmethod gts-parse ((_ gts-deepl-parser) task)
  (let* ((json (json-read-from-string (oref task raw)))
         (result (mapconcat (lambda (r) (cdr (cadr r))) (cdar json) "\n"))
         tbeg tend)
    (with-temp-buffer
      (erase-buffer)
      (insert (propertize (oref task text) 'face 'gts-google-buffer-brief-result-face) "\n\n")
      (setq tbeg (point))
      (insert (decode-coding-string result 'utf-8))
      (setq tend (point))
      (insert "\n")
      (gts-update-parsed task (buffer-string) (list :tbeg tbeg :tend tend)))))


(provide 'gts-engine-deepl)

;;; gts-engine-deepl.el ends here
