;;; gts-google-rpc.el --- google translation with rpc api -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'gts-google)


;;; Components

(defclass gts-google-rpc-parser (gts-google-parser)
  ((tag :initform "Detail")))

(defclass gts-google-rpc-summary-parser (gts-google-summary-parser gts-google-rpc-parser)
  ((tag :initform "Summary")))

(defclass gts-google-rpc-engine (gts-engine)
  ((tag       :initform "Google-RPC")
   (base-url  :initform "https://translate.google.cn")
   (sub-url   :initform "/_/TranslateWebserverUi/data/batchexecute")
   (parser    :initform (gts-google-rpc-parser))

   (rpc-translate :initform "MkEWBc")
   (rpc-tts       :initform "jQ1olc")
   (rpc-sid       :initform "FdrFJe")
   (rpc-bl        :initform "cfb2h")
   (rpc-fd-trans  :initform "[[[\"%s\",\"[[\\\"%s\\\",\\\"%s\\\",\\\"%s\\\",true],[null]]\",null,\"generic\"]]]")
   (rpc-fd-tts    :initform "[[[\"%s\",\"[\\\"%s\\\",\\\"%s\\\",null]\",null,\"generic\"]]]")))


;;; Engine

(defvar gts-google-rpc-request-headers '(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8")))

(cl-defmethod gts-with-token ((o gts-google-rpc-engine) callback)
  (with-slots (base-url sub-url rpc-sid rpc-bl) o
    (gts-do-request base-url
                    :done
                    (lambda ()
                      (condition-case err
                          (let* ((f-sid (progn
                                          (re-search-forward (format "\"%s\":\"\\([^\"]*\\)\"" rpc-sid))
                                          (match-string 1)))
                                 (bl (progn (re-search-forward (format "\"%s\":\"\\([^\"]*\\)\"" rpc-bl))
                                            (match-string 1)))
                                 (url-tpl (lambda (rpcid lang)
                                            (format "%s%s?%s" base-url sub-url
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
                            (funcall callback url-tpl))
                        (error (error "Error occurred when request with token (%s, %s)" o err))))
                    :fail
                    (lambda (status)
                      (error (format "ERR: %s" status))))))

(cl-defmethod gts-translate ((o gts-google-rpc-engine) &optional text from to rendercb)
  (condition-case err
      (gts-with-token o (lambda (url-tpl)
                          (with-slots (base-url sub-url rpc-translate rpc-fd-trans parser) o
                            (gts-do-request (funcall url-tpl rpc-translate to)
                                            :headers gts-google-rpc-request-headers
                                            :data (format
                                                   "f.req=%s&"
                                                   (url-hexify-string
                                                    (format rpc-fd-trans rpc-translate
                                                            (replace-regexp-in-string "\n" "\\\\\\\\n" text)
                                                            from to)))
                                            :done (lambda ()
                                                    (let ((result (gts-parse parser text (buffer-string))))
                                                      (put-text-property 0 (length result) 'engine o result)
                                                      (funcall rendercb result)))
                                            :fail (lambda (status)
                                                    (funcall rendercb status))))))
    (error (funcall rendercb err))))

(cl-defmethod gts-tts ((o gts-google-rpc-engine) text lang)
  (gts-with-token o (lambda (url-tpl)
                      (with-slots (base-url sub-url rpc-tts rpc-fd-tts parser) o
                        (gts-do-request (funcall url-tpl rpc-tts "en-US")
                                        :headers gts-google-rpc-request-headers
                                        :data (format
                                               "f.req=%s&"
                                               (url-hexify-string
                                                (format rpc-fd-tts rpc-tts (replace-regexp-in-string "\n" "\\\\\\\\n" text) lang)))
                                        :done (lambda ()
                                                (let (beg end json code proc)
                                                  (goto-char (point-min))
                                                  (re-search-forward "\n\n")
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
                                                    (setq proc (make-process :name "gts-tts-process"
                                                                             :command (list gts-tts-speaker "-")
                                                                             :buffer nil
                                                                             :noquery t
                                                                             :connection-type 'pipe))
                                                    (process-send-region proc (point-min) (point-max))
                                                    (if (process-live-p proc) (process-send-eof proc)))))
                                        :fail (lambda (status)
                                                (user-error "Error when TTS (%s)" status)))))))


;;; Parser

(cl-defmethod gts-resp-to-json ((_ gts-google-rpc-parser) resp)
  "Convert the buffer RESP into JSON format."
  (condition-case err
      (with-temp-buffer
        (let (beg end str json)
          (insert resp)
          (goto-char (point-min))
          (re-search-forward "\n\n")
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


(provide 'gts-google-rpc)

;;; gts-google-rpc.el ends here
