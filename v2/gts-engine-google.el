;;; gts-engine-google.el --- Google translate module -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; http://translate.google.com

;;; Code:

(require 'gts-implements)


;;; Components

(defclass gts-google-parser (gts-parser)
  ((tag :initform "Detail")))

(defclass gts-google-summary-parser (gts-google-parser)
  ((tag :initform "Summary")))

(defclass gts-google-engine (gts-engine)
  ((tag                :initform "Google")
   (base-url           :initform "http://translate.googleapis.com")
   (sub-url            :initform "/translate_a/single")
   (token              :initform (cons 430675 2721866130) :initarg token) ; hard code
   (token-time         :initform t)
   (token-expired-time :initform (* 30 60))
   (parser             :initform (gts-google-parser))))


;;; Engine

(defvar gts-google-request-headers '(("Connection" . "Keep-Alive")))

(cl-defmethod gts-gen-url ((engine gts-google-engine) text from to)
  "Generate the url with TEXT, FROM and TO. Return a (url text from to) list."
  (format "%s%s?%s"
          (oref engine base-url)
          (oref engine sub-url)
          (mapconcat (lambda (p)
                       (format "%s=%s" (url-hexify-string (car p)) (url-hexify-string (cdr p))))
                     `(("client" . "gtx")
                       ("ie"     . "UTF-8")
                       ("oe"     . "UTF-8")
                       ("dt"     . "bd")
                       ("dt"     . "ex")
                       ("dt"     . "ld")
                       ("dt"     . "md")
                       ("dt"     . "qc")
                       ("dt"     . "rw")
                       ("dt"     . "rm")
                       ("dt"     . "ss")
                       ("dt"     . "t")
                       ("dt"     . "at")
                       ("pc"     . "1")
                       ("otf"    . "1")
                       ("srcrom" . "1")
                       ("ssel"   . "0")
                       ("tsel"   . "0")
                       ("hl"     . ,to)
                       ("sl"     . ,from)
                       ("tl"     . ,to)
                       ("q"      . ,text)
                       ("tk"     . ,(gts-google-tkk (oref engine token) text)))
                     "&")))

(cl-defmethod gts-token-available-p ((engine gts-google-engine))
  (with-slots (token token-time token-expired-time) engine
    (and token
         (or (eq token-time t)
             (and token-time
                  (<= (float-time (time-subtract (current-time) token-time))
                      token-expired-time))))))

(cl-defmethod gts-with-token ((engine gts-google-engine) done fail)
  (with-slots (token token-time base-url) engine
    (if (gts-token-available-p engine)
        (funcall done)
      (gts-do-request base-url
                      :headers gts-google-request-headers
                      :done (lambda ()
                              (let ((tk (progn
                                          (re-search-forward ",tkk:'\\([0-9]+\\)\\.\\([0-9]+\\)")
                                          (cons (string-to-number (match-string 1))
                                                (string-to-number (match-string 2))))))
                                (setf token tk)
                                (setf token-time (current-time))
                                (funcall done)))
                      :fail fail))))

(cl-defmethod gts-translate ((engine gts-google-engine) task rendercb)
  (gts-with-token engine
    (lambda ()
      (with-slots (text from to) task
        (gts-do-request (gts-gen-url engine text from to)
                        :headers gts-google-request-headers
                        :done (lambda ()
                                (gts-update-raw task (buffer-string))
                                (gts-parse (oref engine parser) task)
                                (funcall rendercb))
                        :fail (lambda (err)
                                (gts-render-fail task err)))))
    (lambda (err)
      (gts-render-fail task
        (format "Error when fetching Token-Key, check your network and proxy, or retry later\n\n%s" err)))))

;; tts

(cl-defmethod gts-tts-gen-urls ((engine gts-google-engine) text lang)
  "Generate the tts urls for TEXT to LANGUAGE."
  (cl-loop with texts = (gts-tts-text-splitter engine text)
           for total = (length texts)
           for index from 0
           for piece in texts
           collect
           (let ((params `(("ie"      . "UTF-8")
                           ("client"  . "gtx")
                           ("prev"    . "input")
                           ("q"       . ,text)
                           ("tl"      . ,lang)
                           ("total"   . ,(number-to-string total))
                           ("idx"     . ,(number-to-string index))
                           ("textlen" . ,(number-to-string (length piece)))
                           ("tk"      . ,(gts-google-tkk (oref engine token) piece)))))
             (format "%s/translate_tts?%s"
                     (oref engine base-url)
                     (mapconcat (lambda (p)
                                  (format "%s=%s"
                                          (url-hexify-string (car p))
                                          (url-hexify-string (cdr p))))
                                params "&")))))

(cl-defmethod gts-tts-text-splitter ((_ gts-google-engine) text)
  "Split TEXT by maxlen at applicable point for translating.
Code from `google-translate', maybe improve it someday."
  (let (result (maxlen 200))
    (if (or (null maxlen) (<= maxlen 0))
	    (push text result)
      ;; split long text?
      (with-temp-buffer
	    (save-excursion (insert text))
	    ;; strategy to split at applicable point
	    ;; 1) fill-region remaining text by maxlen
	    ;; 2) find end of sentence, end of punctuation, word boundary
	    ;; 3) consume from remaining text between start and (2)
	    ;; 4) repeat
	    (let ((fill-column (* maxlen 3))
	          (sentence-end-double-space nil)
	          (pos (point-min)))
	      (while (< pos (point-max))
	        (save-restriction
	          (narrow-to-region pos (point-max))
	          (fill-region pos (point-max))
	          (let ((limit (+ pos maxlen)))
		        (if (>= limit (point-max))
		            (setq limit (point-max))
		          (goto-char limit)
		          ;; try to split at end of sentence
		          (if (> (backward-sentence) pos)
		              (setq limit (point))
		            ;; try to split at end of punctuation
		            (goto-char limit)
		            (if (re-search-backward "[,、]" pos t)
			            (setq limit (1+ (point))) ; include punctuation
		              (goto-char limit)
		              ;; try to split at word boundary
		              (forward-word-strictly -1)
		              (when (> (point) pos)
			            (setq limit (point))))))
		        (push (buffer-substring-no-properties pos limit) result)
		        (goto-char limit)
		        (setq pos limit)))))))
    (reverse result)))

(cl-defmethod gts-tts ((engine gts-google-engine) text lang)
  (let ((urls (gts-tts-gen-urls engine text lang))
        (speaker (split-string gts-tts-speaker)))
    (with-temp-message "Speaking..."
      (gts-tts-try-interrupt-playing-process)
      (apply #'call-process (car speaker) nil nil nil (append (cdr speaker) urls)))))


;;; Parser

;; detail-mode, use as default

(cl-defmethod gts-parse ((parser gts-google-parser) task)
  (let* ((json        (gts-resp-to-json parser (oref task raw)))
         (brief       (gts-result--brief parser json))
         (sphonetic   (gts-result--sphonetic parser json))
         (tphonetic   (gts-result--tphonetic parser json))
         (details     (gts-result--details parser json))
         (definitions (gts-result--definitions parser json))
         (suggestion  (gts-result--suggestion parser json))
         (suggestionp (> (length suggestion) 0)) tbeg tend)
    (cl-flet ((phonetic (ph)
                (if (and (or definitions definitions) (> (length ph) 0))
                    (propertize (format " [%s]" ph) 'face 'gts-google-buffer-phonetic-face)
                  ""))
              (headline (line)
                (propertize (format "[%s]\n" line) 'face 'gts-google-buffer-headline-face)))
      (with-temp-buffer
        ;; suggestion
        (when suggestionp
          (insert (propertize "Do you mean:" 'face 'gts-google-buffer-suggestion-desc-face) " "
                  (propertize suggestion 'face 'gts-google-buffer-suggestion-text-face) "?\n\n"))
        ;; phonetic & translate
        (if (or details definitions)
            (progn
              (insert (if suggestionp suggestion (oref (oref task translator) text)))
              (insert (phonetic sphonetic) " ")
              (setq tbeg (point))
              (insert (propertize brief 'face 'gts-google-buffer-brief-result-face))
              (setq tend (point))
              (insert (phonetic tphonetic) "\n\n"))
          (insert brief))
        ;; details
        (when details
          (insert (headline "Details"))
          (cl-loop for (label . items) in details
                   unless (= 0 (length label))
                   do (insert (format "\n%s:\n" label))
                   do (cl-loop with index = 0
                               for trans in items
                               do (insert
                                   (format "%2d. " (cl-incf index))
                                   (car trans)
                                   " (" (mapconcat #'identity (cdr trans) ", ")  ")"
                                   "\n")))
          (insert "\n"))
        ;; definitions
        (when definitions
          (insert (headline "Definitions"))
          (cl-loop for (label . items) in definitions
                   unless (= 0 (length label))
                   do (insert (format "\n%s:\n" label))
                   do (cl-loop with index = 0
                               for (exp . eg) in items
                               do (insert (format "%2d. " (cl-incf index)) exp)
                               when (> (length eg) 0)
                               do (insert
                                   "\n    > "
                                   (propertize (or eg "") 'face 'gts-google-buffer-detail-demo-face))
                               do (insert "\n"))))
        ;; at last, fill and return
        (gts-update-parsed task (buffer-string) (list :tbeg tbeg :tend tend))))))

;; summary-mode

(cl-defmethod gts-parse ((parser gts-google-summary-parser) task)
  (let* ((json (gts-resp-to-json parser (oref task raw)))
         (result (string-trim (gts-result--brief parser json))))
    (gts-update-parsed task result (list :tbeg 1 :tend (+ 1 (length result))))))

;; Extract results from response

(cl-defmethod gts-resp-to-json ((_ gts-google-parser) resp)
  "Convert the buffer RESP into JSON."
  (condition-case err
      (json-read-from-string (decode-coding-string resp 'utf-8))
    (error (user-error "Result conversion error: %s" err))))

(cl-defmethod gts-result--brief ((_ gts-google-parser) json)
  "Get the translation text from JSON."
  (mapconcat (lambda (item) (aref item 0)) (aref json 0) ""))

(cl-defmethod gts-result--sphonetic ((_ gts-google-parser) json)
  "Get the text phonetic from JSON."
  (mapconcat (lambda (item) (if (> (length item) 3) (aref item 3) "")) (aref json 0) ""))

(cl-defmethod gts-result--tphonetic ((_ gts-google-parser) json)
  "Get the translation phonetic from JSON."
  (mapconcat (lambda (item) (if (> (length item) 2) (aref item 2) "")) (aref json 0) ""))

(cl-defmethod gts-result--details ((_ gts-google-parser) json)
  "Get the details from JSON.
Result style: ((noun (a (x y z))) (verb (b (m n o))))."
  (cl-loop for i across (aref json 1)
           collect
           (cons
            (aref i 0)
            (cl-loop for j across (aref i 2)
                     collect
                     (cons
                      (aref j 0)
                      (cl-loop for k across (aref j 1) collect k))))))

(cl-defmethod gts-result--definitions ((_ gts-google-parser) json)
  "Get the definitions from JSON.
Result style: ((noun (a b)) (verb (c d)))."
  (cl-loop with defs = (ignore-errors (aref json 12))
           for i across defs
           collect
           (cons
            (aref i 0)
            (cl-loop for j across (aref i 1)
                     collect
                     (cons
                      (aref j 0)
                      (ignore-errors (aref j 2)))))))

(cl-defmethod gts-result--suggestion ((_ gts-google-parser) json)
  "Get the suggestion from JSON."
  (let ((info (aref json 7))) (unless (seq-empty-p info) (aref info 1))))


;;; Token Key algorithm (deprecated)

;; This algorithm is from `google-translate' project.
;; https://github.com/atykhonov/google-translate/blob/master/google-translate-tk.el

(defvar gts-google-token--bit-v-len 32)

(defun gts-google-token--bit-v-2comp (v)
  "Return the two's complement of V."
  (let* ((vc (vconcat v))
         (len (length vc)))
    ;; Complement of v
    (cl-loop for i from 0 below len do
             (aset vc i (logxor (aref vc i) 1)))
    ;; vc = complement of v + 1
    (cl-loop for i downfrom (1- len) to 0
             do (aset vc i (logxor (aref vc i) 1))
             when (> (aref vc i) 0) return nil)
    vc))

(defun gts-google-token--number-to-bit-v (n)
  "Return a bit vector from N."
  (if (< n 0) (gts-google-token--bit-v-2comp
               (gts-google-token--number-to-bit-v (abs n)))
    (let ((v (make-vector gts-google-token--bit-v-len 0)))
      (cl-loop for i downfrom (1- gts-google-token--bit-v-len) to 0
               with q
               when (< n 1) return nil do
               (setq q (ffloor (* n 0.5)))
               (aset v i (floor (- n (* 2.0 q))))
               (setq n q))
      v)))

(defun gts-google-token--bit-v-to-number (v)
  "Return a floating-point number from V."
  (if (and (> (aref v 0) 0)
           ;; Exclude [1 0 ... 0]
           (cl-loop for i from 1 below gts-google-token--bit-v-len
                    thereis (> (aref v i) 0)))
      (- (gts-google-token--bit-v-to-number (gts-google-token--bit-v-2comp v)))
    (funcall (if (> (aref v 0) 0)  #'- #'+)
             (cl-reduce (lambda (acc e) (+ (* acc 2.0) e))
                        v :initial-value 0.0))))

(defun gts-google-token--logfn (fn n1 n2)
  "Helper function for logical FN with N1 and N2."
  (let ((v1 (gts-google-token--number-to-bit-v n1))
        (v2 (gts-google-token--number-to-bit-v n2))
        (v (make-vector gts-google-token--bit-v-len 0)))
    (cl-loop for i from 0 below gts-google-token--bit-v-len do
             (aset v i (funcall fn (aref v1 i) (aref v2 i))))
    (gts-google-token--bit-v-to-number v)))

(defun gts-google-token--logand (n1 n2)
  "Return a floating-point number from N1 and N2."
  (gts-google-token--logfn #'logand n1 n2))

(defun gts-google-token--logxor (n1 n2)
  "Return a floating-point number from N1 and N2."
  (gts-google-token--logfn #'logxor n1 n2))

(defun gts-google-token--lsh (n d)
  "Return a floating-point number.
Shift the bits in N to the left or rihgt D places.
D is an integer."
  (let ((v (gts-google-token--number-to-bit-v n))
        (v-result (make-vector gts-google-token--bit-v-len 0)))
    (if (< d 0)
        ;; Shift Right Logical
        (cl-loop for i from (abs d) below gts-google-token--bit-v-len
                 for j from 0 do
                 (aset v-result i (aref v j)))
      ;; Shift Left Logical
      (cl-loop for i from d below gts-google-token--bit-v-len
               for j from 0 do
               (aset v-result j (aref v i))))
    (gts-google-token--bit-v-to-number v-result)))

(defun gts-google-token--gen-rl (a b)
  "Gen rl from A and B."
  (cl-loop for c from 0 below (- (length b) 2) by 3
           for d = (aref b (+ c 2)) do
           (setq d (if (>= d ?a) (- d 87) (- d ?0)))
           (setq d (if (= (aref b (1+ c)) ?+)
                       (gts-google-token--lsh a (- d))
                     (gts-google-token--lsh a d)))
           (setq a (if (= (aref b c) ?+)
                       (gts-google-token--logand (+ a d) 4294967295.0)
                     (gts-google-token--logxor a d))))
  a)

(defun gts-google-tkk (token text)
  "Calculate the Token for search TEXT.

It will use the tkk from Google translate page."
  (let* ((b (car token))
         (d1 (cdr token))
         (ub "+-3^+b+-f")
         (vb "+-a^+6")
         (a (cl-loop for c across (encode-coding-string text 'utf-8)
                     for rr = (gts-google-token--gen-rl (+ (or rr b) c) vb)
                     finally (return rr))))
    (setq a (gts-google-token--gen-rl a ub))
    (setq a (gts-google-token--logxor a d1))
    (if (< a 0) (setq a (+ (gts-google-token--logand a 2147483647.0) 2147483648.0)))
    (setq a (ffloor (mod a 1e6)))
    (format "%s.%s"
            (car (split-string (number-to-string a) "\\."))
            (car (split-string (number-to-string
                                (gts-google-token--logxor a b))
                               "\\.")))))


(provide 'gts-engine-google)

;;; gts-engine-google.el ends here
