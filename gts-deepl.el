;;; gts-deepl.el --- Translate Engine for DeepL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'gts-core)

(defclass gts-deepl-parser (gts-parser)
  ((tag :initform "Normal")))

(defclass gts-deepl-engine (gts-engine)
  ((tag      :initform "DeepL")
   (pro      :initform nil :initarg :pro :documentation "If your key is PRO version")
   (free-url :initform "https://api.deepl.com")
   (pro-url  :initform "https://api-free.deepl.com")
   (sub-url  :initform "/v2/translate")
   (auth-key :initform nil :initarg :auth-key)
   (parser   :initform (gts-deepl-parser))))

(cl-defmethod initialize-instance :after ((o gts-deepl-engine) &rest _)
  (unless (oref o auth-key)
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

(cl-defmethod gts-get-lang ((o gts-deepl-engine) lang)
  (let ((mapping (assoc lang gts-deepl-langs-mapping)))
    (if mapping (cdr mapping)
      (user-error
       "Language %s not supported by DeepL.\nSupported list: %s."
       lang (mapconcat #'car gts-deepl-langs-mapping ", ")))))

(cl-defmethod gts-gen-url ((o gts-deepl-engine))
  (with-slots (free-url pro-url sub-url pro) o
    (format "%s%s" (if pro pro-url free-url) sub-url)))

(cl-defmethod gts-translate ((o gts-deepl-engine) &optional text from to rendercb)
  (with-slots (auth-key parser) o
    (gts-do-request (gts-gen-url o)
                    :headers '(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8"))
                    :data `(("auth_key" . ,auth-key)
                            ("text" . ,text)
                            ("source_lang" . ,(gts-get-lang o from))
                            ("target_lang" . ,(gts-get-lang o to)))
                    :done (lambda ()
                            (let ((result (gts-parse parser text (buffer-string))))
                              (funcall rendercb result)))
                    :fail (lambda (status)
                            (cond ((ignore-errors (= (cl-third (car status)) 403))
                                   (funcall rendercb "403 error, make sure your auth_key is correct."))
                                  (t (funcall rendercb status)))))))


;;; Parser

(cl-defmethod gts-parse ((o gts-deepl-parser) text resp)
  (with-temp-buffer
    (insert resp)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (let* ((rstr (buffer-substring-no-properties (point) (point-max)))
           (json (json-read-from-string rstr))
           (result (mapconcat (lambda (r) (cdr (cadr r))) (cdar json) "\n"))
           tbeg bend)
      (erase-buffer)
      (insert (propertize text 'face 'gts-google-buffer-brief-result-face) "\n\n")
      (setq tbeg (point))
      (insert result)
      (setq tend (point))
      (insert "\n")
      (setq result (buffer-string))
      (add-text-properties 0 (length result) `(text ,text tbeg ,tbeg tend ,tend) result)
      result)))


(provide 'gts-deepl)

;;; gts-deepl.el ends here
