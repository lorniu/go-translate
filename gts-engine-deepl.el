;;; gts-engine-deepl.el --- Translate Engine for DeepL -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; https://www.deepl.com

;;; Code:

(require 'gts-implements)


;;; Components

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


;;; Utils

(defcustom gts-deepl-fill-enable t
  "Controller wheather try to improve the input and output.
Default behavior is removing excess linebreaks in input for better
translation effect, and filling the output for better reading experience.
You can override the behaviors by :around the method `gts-deepl-fill-input'
or `gts-deepl-fill-output'."
  :type 'boolean
  :group 'go-translate)

(cl-defmethod gts-deepl-fill-input (text)
  "Improve the input TEXT for better translation effect.
Mainly remove excess linebreaks. I want to skip unfill on comments and codes,
but don't know how to implement easily. To make it better later, maybe."
  (if gts-deepl-fill-enable
      (with-temp-buffer
        (insert text)
        (let ((fill-column (* 2 (point-max))))
          (fill-region (point-min) (point-max)))
        (buffer-string))
    text))

(cl-defmethod gts-deepl-fill-output (text)
  "Improve the output TEXT for better reading experience.
Mainly fill the text to suitable length."
  (if gts-deepl-fill-enable
      (with-temp-buffer
        (insert text)
        (fill-region (point-min) (point-max))
        (buffer-string))
    text))


;;; Engine

(defvar gts-deepl-langs-mapping '(("en" . "EN")
                                  ("zh" . "ZH")
                                  ("de" . "DE") ; German
                                  ("fr" . "FR") ; French
                                  ("it" . "IT") ; Italian
                                  ("ja" . "JA") ; Japanese
                                  ("es" . "ES") ; Spanish
                                  ("nl" . "NL") ; Dutch
                                  ("pl" . "PL") ; Polish
                                  ("pt" . "PT") ; Portuguese (all Portuguese varieties mixed)
                                  ("ru" . "RU") ; Russian
                                  ))

(cl-defmethod gts-get-lang ((_ gts-deepl-engine) lang)
  (let ((mapping (assoc lang gts-deepl-langs-mapping)))
    (if mapping (cdr mapping)
      (user-error
       "Language %s is not supported by DeepL.\nSupported list: %s"
       lang (mapconcat #'car gts-deepl-langs-mapping ", ")))))

(cl-defmethod gts-gen-url ((engine gts-deepl-engine))
  (with-slots (free-url pro-url sub-url pro) engine
    (format "%s%s" (if pro pro-url free-url) sub-url)))

(cl-defmethod gts-translate ((engine gts-deepl-engine) task rendercb)
  (with-slots (text from to) task
    (with-slots (auth-key parser) engine
      (gts-do-request (gts-gen-url engine)
                      :headers `(("Content-Type"   . "application/x-www-form-urlencoded;charset=UTF-8")
                                 ("Authorization"  . ,(concat "DeepL-Auth-Key " auth-key)))
                      :data    `(("text"           . ,(gts-deepl-fill-input text))
                                 ("source_lang"    . ,(gts-get-lang engine from))
                                 ("target_lang"    . ,(gts-get-lang engine to)))
                      :done (lambda ()
                              (gts-update-raw task (buffer-string))
                              (gts-parse parser task)
                              (funcall rendercb))
                      :fail (lambda (err)
                              (gts-render-fail task
                                (if (ignore-errors (= (caddar err) 403))
                                    "[403] http error, make sure your auth_key is correct"
                                  err)))))))


;;; Parser

(cl-defmethod gts-parse ((_ gts-deepl-parser) task)
  (let* ((json (json-read-from-string (oref task raw)))
         (str (mapconcat (lambda (r) (cdr (cadr r))) (cdar json) "\n"))
         (filter (lambda (parsed)
                   (cl-loop for p in (gts-ensure-list parsed)
                            collect (gts-deepl-fill-output p) into ps
                            finally (return (if (cdr ps) ps (car ps)))))))
    (gts-update-parsed task (decode-coding-string str 'utf-8) nil filter)))


(provide 'gts-engine-deepl)

;;; gts-engine-deepl.el ends here
