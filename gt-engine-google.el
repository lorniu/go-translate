;;; gt-engine-google.el --- Google translate -*- lexical-binding: t -*-

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

;; http://translate.google.com

;;; Code:

(require 'gt-extension)

(defgroup go-translate-google nil
  "Configs for Google engine."
  :group 'go-translate)

(defcustom gt-google-host "http://translate.googleapis.com"
  "The base url of Google translate used by google engine.
you can customize it according to your country region."
  :type 'string
  :group 'go-translate-google)


;;; Components

(defclass gt-google-parser (gt-parser)
  ((tag :initform "Detail")))

(defclass gt-google-summary-parser (gt-google-parser)
  ((tag :initform "Summary")))

(defclass gt-google-engine (gt-engine)
  ((tag                :initform 'Google)
   (host               :initform nil)
   (path               :initform "/translate_a/single")
   (token              :initform (cons 430675 2721866130) :initarg token) ; hard code
   (token-time         :initform t)
   (token-expired-time :initform (* 30 60))
   (parse              :initform (gt-google-parser))))


;;; Engine

(defcustom gt-google-request-headers '(("Connection" . "Keep-Alive"))
  "Extra request headers send to google server."
  :type '(alist :key-type (string :tag "Key") :value-type (string :tag "Value"))
  :group 'go-translate-google)

(defun gt-google-gen-url (engine text src tgt)
  "Generate url for google ENGINE with TEXT, SRC and TGT."
  (with-slots (host path token) engine
    (format "%s%s?%s"
            (or host gt-google-host) path
            (mapconcat (lambda (p)
                         (format "%s=%s"
                                 (url-hexify-string (car p))
                                 (url-hexify-string (format "%s" (cdr p)))))
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
                         ("q"      . ,text)
                         ("sl"     . ,src)
                         ("tl"     . ,tgt)
                         ("hl"     . ,tgt)
                         ("tk"     . ,(gt-google-tkk token text)))
                       "&"))))

(defun gt-google-token-available-p (engine)
  (with-slots (token token-time token-expired-time) engine
    (and token
         (or (eq token-time t)
             (and token-time
                  (<= (float-time (time-subtract (current-time) token-time))
                      token-expired-time))))))

(defun gt-google-with-token (engine done fail)
  (declare (indent 1))
  (with-slots (host token token-time) engine
    (if (gt-google-token-available-p engine)
        (funcall done)
      (gt-request :url (or host gt-google-host)
                  :headers gt-google-request-headers
                  :done (lambda (raw)
                          (with-temp-buffer
                            (insert raw)
                            (goto-char (point-min))
                            (let ((tk (progn
                                        (re-search-forward ",tkk:'\\([0-9]+\\)\\.\\([0-9]+\\)")
                                        (cons (string-to-number (match-string 1))
                                              (string-to-number (match-string 2))))))
                              (setf token tk)
                              (setf token-time (current-time))
                              (funcall done))))
                  :fail fail))))

(cl-defmethod gt-translate ((engine gt-google-engine) task next)
  (gt-google-with-token engine
    (lambda ()
      (with-slots (text src tgt res) task
        (gt-request :url (gt-google-gen-url engine text src tgt)
                    :headers gt-google-request-headers
                    :done (lambda (raw) (setf res raw) (funcall next task))
                    :fail (lambda (err) (gt-fail task err)))))
    (lambda (err)
      (gt-fail task (format "Take token failed, %s" err)))))

;; tts

(defun gt-google-tts-split-text (text)
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

(cl-defmethod gt-speak ((engine gt-google-engine) text lang)
  (message "Requesting from %s for %s..." (or (oref engine host) gt-google-host) lang)
  (cl-loop with texts = (gt-google-tts-split-text text)
           with total = (length texts)
           for c in texts
           for i from 0
           for ps = `(("ie"      . "UTF-8")
                      ("client"  . "gtx")
                      ("prev"    . "input")
                      ("tl"      . ,lang)
                      ("q"       . ,(string-trim c))
                      ("total"   . ,total)
                      ("idx"     . ,i)
                      ("textlen" . ,(length c))
                      ("tk"      . ,(gt-google-tkk (oref engine token) c)))
           for url = (format "%s/translate_tts?%s"
                             (or (oref engine host) gt-google-host)
                             (mapconcat (lambda (p)
                                          (format "%s=%s" (url-hexify-string (car p)) (url-hexify-string (format "%s" (cdr p)))))
                                        ps "&"))
           do (gt-play-audio url 'wait)))


;;; Parser

;; detail-mode, use as default

(cl-defmethod gt-parse ((parser gt-google-parser) task)
  (with-slots (res translator) task
    (let* ((json (gt-resp-to-json parser res))
           (brief-result (gt-result--brief parser json)))
      (if (cdr (oref translator text)) ; multi-parts
          (setf res (string-trim (gt-result--brief parser json) "\n+"))
        (let* ((sphonetic    (gt-result--sphonetic parser json))
               (tphonetic    (gt-result--tphonetic parser json))
               (details      (gt-result--details parser json))
               (definitions  (gt-result--definitions parser json))
               (suggestion   (gt-result--suggestion parser json))
               (suggestionp  (> (length suggestion) 0)) pt)
          (cl-flet ((phonetic (ph)
                      (if (and (or definitions definitions) (> (length ph) 0))
                          (propertize (format " [%s]" ph) 'face 'gt-google-buffer-phonetic-face)
                        ""))
                    (headline (line)
                      (propertize (format "[%s]\n" line) 'face 'gt-google-buffer-headline-face)))
            (with-temp-buffer
              ;; suggestion
              (when suggestionp
                (insert (propertize "Do you mean:" 'face 'gt-google-buffer-suggestion-desc-face) " "
                        (propertize suggestion 'face 'gt-google-buffer-suggestion-text-face) "?\n\n"))
              ;; phonetic & translate
              (if (or details definitions)
                  (progn
                    (insert (if suggestionp suggestion
                              (setq pt (point))
                              (substring-no-properties (oref task text))))
                    (insert (phonetic sphonetic) " ")
                    (insert (propertize brief-result 'face 'gt-google-buffer-brief-result-face))
                    (insert (phonetic tphonetic) "\n\n"))
                (insert brief-result))
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
                                         (propertize (or eg "") 'face 'gt-google-buffer-detail-demo-face))
                                     do (insert "\n"))))
              ;; at last, return
              (add-text-properties (point-min) (point-max) (list 'gt-mark pt 'gt-brief brief-result))
              (setf res (buffer-string)))))))))

;; summary-mode

(cl-defmethod gt-parse ((parser gt-google-summary-parser) task)
  (let* ((json (gt-resp-to-json parser (oref task res)))
         (result (string-trim (gt-result--brief parser json) "\n+")))
    (oset task res result)))

;; Extract results from response

(cl-defmethod gt-resp-to-json ((_ gt-google-parser) resp)
  "Convert the buffer RESP into JSON."
  (condition-case err
      (json-read-from-string (decode-coding-string resp 'utf-8))
    (error (user-error "Result conversion error: %s" err))))

(cl-defmethod gt-result--brief ((_ gt-google-parser) json)
  "Get the translation text from JSON."
  (mapconcat (lambda (item) (aref item 0)) (aref json 0) ""))

(cl-defmethod gt-result--sphonetic ((_ gt-google-parser) json)
  "Get the text phonetic from JSON."
  (mapconcat (lambda (item) (if (> (length item) 3) (aref item 3) "")) (aref json 0) ""))

(cl-defmethod gt-result--tphonetic ((_ gt-google-parser) json)
  "Get the translation phonetic from JSON."
  (mapconcat (lambda (item) (if (> (length item) 2) (aref item 2) "")) (aref json 0) ""))

(cl-defmethod gt-result--details ((_ gt-google-parser) json)
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

(cl-defmethod gt-result--definitions ((_ gt-google-parser) json)
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

(cl-defmethod gt-result--suggestion ((_ gt-google-parser) json)
  "Get the suggestion from JSON."
  (let ((info (aref json 7))) (unless (seq-empty-p info) (aref info 1))))


;;; Token Key algorithm (deprecated)

;; This algorithm is from `google-translate' project.
;; https://github.com/atykhonov/google-translate/blob/master/google-translate-tk.el

(defvar gt-google-token--bit-v-len 32)

(defun gt-google-token--bit-v-2comp (v)
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

(defun gt-google-token--number-to-bit-v (n)
  "Return a bit vector from N."
  (if (< n 0) (gt-google-token--bit-v-2comp
               (gt-google-token--number-to-bit-v (abs n)))
    (let ((v (make-vector gt-google-token--bit-v-len 0)))
      (cl-loop for i downfrom (1- gt-google-token--bit-v-len) to 0
               with q
               when (< n 1) return nil do
               (setq q (ffloor (* n 0.5)))
               (aset v i (floor (- n (* 2.0 q))))
               (setq n q))
      v)))

(defun gt-google-token--bit-v-to-number (v)
  "Return a floating-point number from V."
  (if (and (> (aref v 0) 0)
           ;; Exclude [1 0 ... 0]
           (cl-loop for i from 1 below gt-google-token--bit-v-len
                    thereis (> (aref v i) 0)))
      (- (gt-google-token--bit-v-to-number (gt-google-token--bit-v-2comp v)))
    (funcall (if (> (aref v 0) 0)  #'- #'+)
             (cl-reduce (lambda (acc e) (+ (* acc 2.0) e))
                        v :initial-value 0.0))))

(defun gt-google-token--logfn (fn n1 n2)
  "Helper function for logical FN with N1 and N2."
  (let ((v1 (gt-google-token--number-to-bit-v n1))
        (v2 (gt-google-token--number-to-bit-v n2))
        (v (make-vector gt-google-token--bit-v-len 0)))
    (cl-loop for i from 0 below gt-google-token--bit-v-len do
             (aset v i (funcall fn (aref v1 i) (aref v2 i))))
    (gt-google-token--bit-v-to-number v)))

(defun gt-google-token--logand (n1 n2)
  "Return a floating-point number from N1 and N2."
  (gt-google-token--logfn #'logand n1 n2))

(defun gt-google-token--logxor (n1 n2)
  "Return a floating-point number from N1 and N2."
  (gt-google-token--logfn #'logxor n1 n2))

(defun gt-google-token--lsh (n d)
  "Return a floating-point number.
Shift the bits in N to the left or rihgt D places.
D is an integer."
  (let ((v (gt-google-token--number-to-bit-v n))
        (v-result (make-vector gt-google-token--bit-v-len 0)))
    (if (< d 0)
        ;; Shift Right Logical
        (cl-loop for i from (abs d) below gt-google-token--bit-v-len
                 for j from 0 do
                 (aset v-result i (aref v j)))
      ;; Shift Left Logical
      (cl-loop for i from d below gt-google-token--bit-v-len
               for j from 0 do
               (aset v-result j (aref v i))))
    (gt-google-token--bit-v-to-number v-result)))

(defun gt-google-token--gen-rl (a b)
  "Gen rl from A and B."
  (cl-loop for c from 0 below (- (length b) 2) by 3
           for d = (aref b (+ c 2)) do
           (setq d (if (>= d ?a) (- d 87) (- d ?0)))
           (setq d (if (= (aref b (1+ c)) ?+)
                       (gt-google-token--lsh a (- d))
                     (gt-google-token--lsh a d)))
           (setq a (if (= (aref b c) ?+)
                       (gt-google-token--logand (+ a d) 4294967295.0)
                     (gt-google-token--logxor a d))))
  a)

(defun gt-google-tkk (token text)
  "Calculate the TOKEN for search TEXT."
  (let* ((b (car token))
         (d1 (cdr token))
         (ub "+-3^+b+-f")
         (vb "+-a^+6")
         (a (cl-loop for c across (encode-coding-string text 'utf-8)
                     for rr = (gt-google-token--gen-rl (+ (or rr b) c) vb)
                     finally (return rr))))
    (setq a (gt-google-token--gen-rl a ub))
    (setq a (gt-google-token--logxor a d1))
    (if (< a 0) (setq a (+ (gt-google-token--logand a 2147483647.0) 2147483648.0)))
    (setq a (ffloor (mod a 1e6)))
    (format "%s.%s"
            (car (split-string (number-to-string a) "\\."))
            (car (split-string (number-to-string
                                (gt-google-token--logxor a b))
                               "\\.")))))

(provide 'gt-engine-google)

;;; gt-engine-google.el ends here
