;;; go-translate.el --- Google Translate -*- lexical-binding: t -*-

;; Copyright (C) 2020 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/go-translate
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; SPDX-License-Identifier: MIT
;; Version: 0.1

;;; Commentary:

;; Installation:

;; From MELPA.

;; Just run `M-x package-install RET go-translate RET`

;; Manual installation.

;; Assuming that the file `go-translate.el' is on your load path, add the
;; following lines to your `.emacs' file:

;; (require 'go-translate)
;; (global-set-key "\C-ct" 'go-translate)
;; (global-set-key "\C-cT" 'go-translate-popup)
;;
;; Customizations:
;;
;; The most important variables are `go-translate-local-language' and
;; `go-translate-target-language', represents your local language and
;; default foreign language. And the `go-translate-extra-directions' is
;; a alist to hold the daily used translation directions except the
;; local/target ones. The following is just a simple example:
;;
;; (setq go-translate-local-language "zh-CN")
;; (setq go-translate-target-language "en")
;; (setq go-translate-extra-directions '(("zh-CN" . "jp") ("zh-CN" . "fr")))
;;
;; Change the variable values and key bindings to your own.
;;

;;; Code:

(require 'cl-lib)


;;; Customizations

(defgroup go-translate nil
  "Google Translate for Emacs, asynchronous and simple."
  :group 'external
  :prefix 'go-translate-)

(defcustom go-translate-base-url "https://translate.google.com"
  "The base url of Google Translate.

You can adjust this url according to your country and region. eg:

URL `https://translate.google.cc'."
  :type 'string)

(defcustom go-translate-user-agent "Emacs"
  "User agent used in the translation request."
  :type 'string)

(defcustom go-translate-text-function #'go-translate-default-current-text
  "Function to generate the init translate text.
Default use the current selection or word on cursor.

 (let ((go-translate-text-function (lambda () (buffer-string))))
   (call-interactively #'go-translate))

will read the whole buffer's content to translate."
  :type 'function)

(defcustom go-translate-inputs-function #'go-translate-default-prompt-inputs
  "Function to take the translation text, sl and tl.

 (let ((go-translate-inputs-function
         (lambda () (list (or (funcall go-translate-text-function)
                              (user-error \"No suitalbe text found\"))
                          \"en\" \"fr\"))))
   (call-interactively #'go-translate))

will only translate from en to fr and no prompt for input."
  :type 'function)

(defcustom go-translate-url-function #'go-translate-default-generate-url
  "Function to generate a proper reqeust url to Google.
It will take the user input as parameters, that is, Text/FROM/TO."
  :type 'function)

(defcustom go-translate-prepare-function #'go-translate-default-buffer-prepare
  "A function executed before sending a request to pre-rending etc.
It will take REQ as parameter."
  :type 'function)

(defcustom go-translate-request-function #'go-translate-default-retrieve-async
  "Function to retrieve translations from Google.
Take REQ as parameter."
  :type 'function)

(defcustom go-translate-render-function #'go-translate-default-buffer-render
  "Function to render the translation result.
Take the REQ and RESP as parameters.

 (let ((go-translate-render-function
         (lambda (_ resp)
           (send-it (go-translate-result--translation resp)))))
   (call-interactively #'go-translate))

will send the translation with your `send-it` function."
  :type 'function)

(defvar go-translate-path "/translate_a/single"
  "The query path of google translate url.")

(defvar go-translate-tts-path "/translate_tts"
  "Google Translate tts query path.")


;;; Debug Helper

(defvar go-translate-debug-p nil)

(defun go-translate-debug (type &rest messages)
  "Helper macro for output the TYPE's debug MESSAGES."
  (when go-translate-debug-p
    (with-current-buffer (get-buffer-create "*GO-TRANSLATE-DEBUG*")
      (goto-char (point-max))
      (insert (format
               "\n[%s] - [%s]:\n\n%s\n\n\n"
               (current-time-string)
               type
               (cl-loop for message in messages
                        concat (format "%s\n" message)))))))


;;; Token Key

(defcustom go-translate-token-expired-time (* 30 60)
  "The validity period of the token.

If the cached token is checked for more than this time, it will be retrieved
again.

It is said that a new token can take a long time without refreshing. However,
there are other opinions too. I have not tested it. let’s set it to 30 minutes.
If any problems later, then adjust it."
  :type 'int)

(defcustom go-translate-token-backend-refresh-p t
  "Allow to refresh TTK regularly through a timer.
This timer will do the job asynchronously in the background without
any impact on performance."
  :type 'boolean)

(defvar go-translate-token-current nil "Current token.")

(defvar go-translate-token--timer nil "Timer used to refresh token.")

;; The tk algorithm from `google-translate' project.
;; https://github.com/atykhonov/google-translate/blob/master/google-translate-tk.el

(defvar go-translate-token--bit-v-len 32)

(defun go-translate-token--bit-v-2comp (v)
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

(defun go-translate-token--number-to-bit-v (n)
  "Return a bit vector from N."
  (if (< n 0) (go-translate-token--bit-v-2comp
               (go-translate-token--number-to-bit-v (abs n)))
    (let ((v (make-vector go-translate-token--bit-v-len 0)))
      (cl-loop for i downfrom (1- go-translate-token--bit-v-len) to 0
               with q
               when (< n 1) return nil do
               (setq q (ffloor (* n 0.5)))
               (aset v i (floor (- n (* 2.0 q))))
               (setq n q))
      v)))

(defun go-translate-token--bit-v-to-number (v)
  "Return a floating-point number from V."
  (if (and (> (aref v 0) 0)
           ;; Exclude [1 0 ... 0]
           (cl-loop for i from 1 below go-translate-token--bit-v-len
                    thereis (> (aref v i) 0)))
      (- (go-translate-token--bit-v-to-number (go-translate-token--bit-v-2comp v)))
    (funcall (if (> (aref v 0) 0)  #'- #'+)
             (cl-reduce (lambda (acc e) (+ (* acc 2.0) e))
                        v :initial-value 0.0))))

(defun go-translate-token--logfn (fn n1 n2)
  "Helper function for logical FN with N1 and N2."
  (let ((v1 (go-translate-token--number-to-bit-v n1))
        (v2 (go-translate-token--number-to-bit-v n2))
        (v (make-vector go-translate-token--bit-v-len 0)))
    (cl-loop for i from 0 below go-translate-token--bit-v-len do
             (aset v i (funcall fn (aref v1 i) (aref v2 i))))
    (go-translate-token--bit-v-to-number v)))

(defun go-translate-token--logand (n1 n2)
  "Return a floating-point number from N1 and N2."
  (go-translate-token--logfn #'logand n1 n2))

(defun go-translate-token--logxor (n1 n2)
  "Return a floating-point number from N1 and N2."
  (go-translate-token--logfn #'logxor n1 n2))

(defun go-translate-token--lsh (n d)
  "Return a floating-point number.
Shift the bits in N to the left or rihgt D places.
D is an integer."
  (let ((v (go-translate-token--number-to-bit-v n))
        (v-result (make-vector go-translate-token--bit-v-len 0)))
    (if (< d 0)
        ;; Shift Right Logical
        (cl-loop for i from (abs d) below go-translate-token--bit-v-len
                 for j from 0 do
                 (aset v-result i (aref v j)))
      ;; Shift Left Logical
      (cl-loop for i from d below go-translate-token--bit-v-len
               for j from 0 do
               (aset v-result j (aref v i))))
    (go-translate-token--bit-v-to-number v-result)))

(defun go-translate-token--gen-rl (a b)
  "Gen rl from A and B."
  (cl-loop for c from 0 below (- (length b) 2) by 3
           for d = (aref b (+ c 2)) do
           (setq d (if (>= d ?a) (- d 87) (- d ?0)))
           (setq d (if (= (aref b (1+ c)) ?+)
                       (go-translate-token--lsh a (- d))
                     (go-translate-token--lsh a d)))
           (setq a (if (= (aref b c) ?+)
                       (go-translate-token--logand (+ a d) 4294967295.0)
                     (go-translate-token--logxor a d))))
  a)

;; tk algorithm ends here

(defun go-translate-token--fetch-tkk (&optional syncp)
  "Retrieve the Token-Key from remote, if SYNCP then fetch synchronously.

It will set to `go-translate-token-current' if request successfully.

The `go-translate-token-current' with the format (time . tkk)."
  (let ((url-user-agent go-translate-user-agent)
        (url-request-extra-headers '(("Connection" . "close"))))
    (if syncp
        (with-current-buffer
            (url-retrieve-synchronously go-translate-base-url 'silent nil 3)
          (prog1
              (setq go-translate-token-current
                    (cons (current-time)
                          (go-translate-token--extract-tkk)))
            (kill-buffer)))
      (url-retrieve go-translate-base-url
                    (lambda (status)
                      (unless status
                        (setq go-translate-token-current
                              (cons (current-time)
                                    (go-translate-token--extract-tkk))))
                      (kill-buffer))
                    nil 'silent))))

(defun go-translate-token--extract-tkk ()
  "Get the Token-Key from the page buffer."
  (condition-case nil
      (re-search-forward ",tkk:'\\([0-9]+\\)\\.\\([0-9]+\\)")
    (error (go-translate-debug 'extract-tkk (buffer-string))
           (user-error "Error when fetching Token-Key. Check your network and proxy, or retry later")))
  (cons (string-to-number (match-string 1))
        (string-to-number (match-string 2))))

(defun go-translate-token--tkk ()
  "Get the Token-Key.

First check `go-translate-token-current', if it's not too old, then return tkk
directly, or fetch from remote and use the new one.

If `go-translate-token-backend-refresh-p' is t, start a timer to
refresh the `go-translate-token-current' in background intervally."
  ;; check and run the timer
  (when (and go-translate-token-backend-refresh-p (null go-translate-token--timer))
    (setq go-translate-token--timer
          (run-at-time go-translate-token-expired-time
                       go-translate-token-expired-time
                       #'go-translate-token--fetch-tkk)))
  ;; get from cache if nessary
  (if (and go-translate-token-current
           (< (float-time (time-subtract (current-time)
                                         (car go-translate-token-current)))
              go-translate-token-expired-time))
      (cdr go-translate-token-current)
    ;; the first time, it will fetch synchronously
    (cdr (go-translate-token--fetch-tkk t))))

(defun go-translate-get-token (text)
  "Calculate the Token for search TEXT.

It will use the tkk from Google translate page."
  (let* ((ttk (go-translate-token--tkk))
         (b (car ttk))
         (d1 (cdr ttk))
         (ub "+-3^+b+-f")
         (vb "+-a^+6")
         (a (cl-loop for c across (encode-coding-string text 'utf-8)
                     for rr = (go-translate-token--gen-rl (+ (or rr b) c) vb)
                     finally (return rr))))
    (setq a (go-translate-token--gen-rl a ub))
    (setq a (go-translate-token--logxor a d1))
    (if (< a 0) (setq a (+ (go-translate-token--logand a 2147483647.0) 2147483648.0)))
    (setq a (ffloor (mod a 1e6)))
    (format "%s.%s"
            (car (split-string (number-to-string a) "\\."))
            (car (split-string (number-to-string
                                (go-translate-token--logxor a b)) "\\.")))))


;;; Functions to extract the result

(defun go-translate-result--to-json (result)
  "Strip the string RESULT and then convert it to JSON."
  (setq result (replace-regexp-in-string "[[:space:]\n\r]+" " " result))
  (setq result (replace-regexp-in-string "[ \t\n\r]*\\'" "" result))
  (setq result (replace-regexp-in-string "\\`[ \t\n\r]*" "" result))
  (setq result (with-temp-buffer
                 (set-buffer-multibyte t)
                 (insert result)
                 (goto-char (point-min))
                 (while (re-search-forward "\\(\\[,\\|,,\\|,\\]\\)" (point-max) t)
                   (backward-char)
                   (insert "null"))
                 (buffer-string)))
  (json-read-from-string (decode-coding-string result 'utf-8)))

(defun go-translate-result--text-phonetic (json)
  "Get the text phonetic from JSON."
  (mapconcat (lambda (item) (if (> (length item) 3) (aref item 3) ""))
             (aref json 0) ""))

(defun go-translate-result--translation (json)
  "Get the translation text from JSON."
  (mapconcat (lambda (item) (aref item 0))
             (aref json 0) ""))

(defun go-translate-result--translation-phonetic (json)
  "Get the translation phonetic from JSON."
  (mapconcat (lambda (item) (if (> (length item) 2) (aref item 2) ""))
             (aref json 0) ""))

(defun go-translate-result--suggestion (json)
  "Get the suggestion from JSON."
  (let ((info (aref json 7)))
    (unless (seq-empty-p info)
      (aref info 1))))

(defun go-translate-result--details (json)
  "Get the details from JSON."
  (aref json 1))

(defun go-translate-result--definitions (json)
  "Get the definitions from JSON."
  (if (> (length json) 12)
      (aref json 12)))


;;; Default functions

(defcustom go-translate-local-language "auto"
  "Your local language, used as the default source language."
  :type 'string)

(defcustom go-translate-target-language "en"
  "Your most commonly used foreign language as default target."
  :type 'string)

(defcustom go-translate-extra-directions nil
  "Extra directions as an alist. Like this:

'((\"zh_CN\" . \"ja\")
  (\"zh_CN\" . \"fr\"))

Together with `go-translate-local-language' and `go-translate-target-language',
they combine into a list to represent all translation directions for daily use.

Some functions, such as switching translation languages, are based on them."
  :type '(repeat (cons string string)))

(defcustom go-translate-buffer-name "*Go Translate*"
  "The name of translation result buffer."
  :type 'string)

(defcustom go-translate-buffer-follow-p nil
  "If t then pop to the result window after translation."
  :type 'boolean)

(defcustom go-translate-buffer-source-fold-p nil
  "If t then try to fold the source text in the result buffer."
  :type 'boolean)

(defcustom go-translate-buffer-line-wrap-p t
  "If t then try to turn on the function `visual-line-mode' in result buffer."
  :type 'boolean)

(defcustom go-translate-auto-guess-direction t
  "Automatically determine the languages of the translation based on the input.

If set to nil, will directly circle the available direcitons instead of guessing."
  :type 'boolean)

(defcustom go-translate-buffer-window-config nil
  "Window configuration used by the result buffer window.

For example, set to:

 '((display-buffer-reuse-window display-buffer-in-side-window)
   (side . right))

will force opening in right side window."
  :type 'list)

(defvar go-translate-available-languages
  '(("Afrikaans"          . "af")
    ("Albanian"           . "sq")
    ("Amharic"            . "am")
    ("Arabic"             . "ar")
    ("Armenian"           . "hy")
    ("Azerbaijani"        . "az")
    ("Basque"             . "eu")
    ("Belarusian"         . "be")
    ("Bengali"            . "bn")
    ("Bosnian"            . "bs")
    ("Bulgarian"          . "bg")
    ("Catalan"            . "ca")
    ("Cebuano"            . "ceb")
    ("Chichewa"           . "ny")
    ("Chinese"            . "zh-CN")
    ("Corsican"           . "co")
    ("Croatian"           . "hr")
    ("Czech"              . "cs")
    ("Danish"             . "da")
    ("Dutch"              . "nl")
    ("English"            . "en")
    ("Esperanto"          . "eo")
    ("Estonian"           . "et")
    ("Filipino"           . "tl")
    ("Finnish"            . "fi")
    ("French"             . "fr")
    ("Frisian"            . "fy")
    ("Galician"           . "gl")
    ("Georgian"           . "ka")
    ("German"             . "de")
    ("Greek"              . "el")
    ("Gujarati"           . "gu")
    ("Haitian Creole"     . "ht")
    ("Hausa"              . "ha")
    ("Hawaiian"           . "haw")
    ("Hebrew"             . "iw")
    ("Hindi"              . "hi")
    ("Hmong"              . "hmn")
    ("Hungarian"          . "hu")
    ("Icelandic"          . "is")
    ("Igbo"               . "ig")
    ("Indonesian"         . "id")
    ("Irish"              . "ga")
    ("Italian"            . "it")
    ("Japanese"           . "ja")
    ("Javanese"           . "jw")
    ("Kannada"            . "kn")
    ("Kazakh"             . "kk")
    ("Khmer"              . "km")
    ("Korean"             . "ko")
    ("Kurdish (Kurmanji)" . "ku")
    ("Kyrgyz"             . "ky")
    ("Lao"                . "lo")
    ("Latin"              . "la")
    ("Latvian"            . "lv")
    ("Lithuanian"         . "lt")
    ("Luxembourgish"      . "lb")
    ("Macedonian"         . "mk")
    ("Malagasy"           . "mg")
    ("Malay"              . "ms")
    ("Malayalam"          . "ml")
    ("Maltese"            . "mt")
    ("Maori"              . "mi")
    ("Marathi"            . "mr")
    ("Mongolian"          . "mn")
    ("Myanmar (Burmese)"  . "my")
    ("Nepali"             . "ne")
    ("Norwegian"          . "no")
    ("Pashto"             . "ps")
    ("Persian"            . "fa")
    ("Polish"             . "pl")
    ("Portuguese"         . "pt")
    ("Punjabi"            . "pa")
    ("Romanian"           . "ro")
    ("Russian"            . "ru")
    ("Samoan"             . "sm")
    ("Scots Gaelic"       . "gd")
    ("Serbian"            . "sr")
    ("Sesotho"            . "st")
    ("Shona"              . "sn")
    ("Sindhi"             . "sd")
    ("Sinhala"            . "si")
    ("Slovak"             . "sk")
    ("Slovenian"          . "sl")
    ("Somali"             . "so")
    ("Spanish"            . "es")
    ("Sundanese"          . "su")
    ("Swahili"            . "sw")
    ("Swedish"            . "sv")
    ("Tajik"              . "tg")
    ("Tamil"              . "ta")
    ("Telugu"             . "te")
    ("Thai"               . "th")
    ("Turkish"            . "tr")
    ("Ukrainian"          . "uk")
    ("Urdu"               . "ur")
    ("Uzbek"              . "uz")
    ("Vietnamese"         . "vi")
    ("Welsh"              . "cy")
    ("Xhosa"              . "xh")
    ("Yiddish"            . "yi")
    ("Yoruba"             . "yo")
    ("Zulu"               . "zu"))
  "Alist of the languages supported by Google Translate.

Each element is a cons-cell of the form (NAME . CODE), where NAME
is a human-readable language name and CODE is its code used as a
query parameter in HTTP requests.")

(defvar go-translate-local-language-transformer #'go-translate-clear-punctuations
  "Transform before judging whether the TEXT is local language.

Default behavior is to remove all punctuations.")

(defvar go-translate-local-language-regexp-alist
  '(("zh_CN" . "\\cc")
    ("zh-CN" . "\\cc")
    ("zh"    . "\\cc")
    ("ja"    . "\\cj"))
  "Alist used to judge if input TEXT is your local language text.

The form is (lang . regexp).

This is based that some texts can easily be determine with regexp,
for example, using the \\cx syntax. Maybe work for some languages.")

(defvar go-translate-split-width-threshold 80
  "Threshold width for window horizontal split.")

(defvar go-translate-last-direction nil)

(defvar go-translate--current-direction nil)

(defvar go-translate-buffer-headline-face
  '(:inherit font-lock-function-name-face :weight bold)
  "Propertize the headline in buffer rendering.")

;; Helpers

(defun go-translate-choose-language (&optional prompt def)
  "Choose a language from `go-translate-available-languages'.
PROMPT and DEF are just as `completing-read'."
  (let* ((code (lambda (s)
                 (concat " (" (cdr (assoc s go-translate-available-languages)) ")")))
         (name (completing-read
                (or prompt "Language: ")
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function . ,code))
                    (complete-with-action
                     action go-translate-available-languages string pred)))
                nil nil nil nil
                (when def
                  (car (rassoc def go-translate-available-languages))))))
    (cdr (assoc name go-translate-available-languages))))

(defun go-translate-clear-punctuations (text)
  "Remove all punctuations in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "\\s." nil t)
      (replace-match ""))
    (buffer-string)))

(defun go-translate-text-local-p (text)
  "Check if TEXT if your local language text.
1 for yes, 0 for no, -1 for unknown."
  (let ((pair (assoc go-translate-local-language
                     go-translate-local-language-regexp-alist)))
    (if pair
        (if (string-match-p (cdr pair)
                            (if go-translate-local-language-transformer
                                (funcall go-translate-local-language-transformer text)
                              text))
            1 0)
      -1)))

(defun go-translate-available-directions ()
  "List of all available translation directions."
  (append (list (cons go-translate-target-language go-translate-local-language)
                (cons go-translate-local-language go-translate-target-language))
          go-translate-extra-directions))

(defun go-translate-next-direction (direction &optional backwardp)
  "Find the next DIRECTION from the available list.
If the BACKWARDP is t, then find the previous one."
  (let* ((directions (go-translate-available-directions))
         (pos (cl-position direction
                           (or directions (user-error "Nothing found"))
                           :test
                           (lambda (x y) (and (string-equal (car x) (car y))
                                              (string-equal (cdr x) (cdr y))))))
         (len (length directions)))
    (elt directions
         (if pos
             (if backwardp
                 (if (<= pos 0) (1- len) (1- pos))
               (if (>= (1+ pos) len) 0 (1+ pos)))
           0))))

(defun go-translate-guess-direction (text)
  "Automatically judge the translation languages based on the TEXT content.

If the text is your local language and hit the last direction, use
the last direction. Or choose from the available directions.

Otherwise, choose the most suitable one from the list or directly use
the last direction."
  (let ((check (go-translate-text-local-p text))
        (directions (go-translate-available-directions)))
    (cond ((and go-translate-last-direction
                (or (= check -1)
                    (and (= check 1)
                         (string-equal
                          (car go-translate-last-direction)
                          go-translate-local-language))
                    (and (= check 0)
                         (not (string-equal
                               (car go-translate-last-direction)
                               go-translate-local-language)))))
           go-translate-last-direction)
          ((= check 1)
           (cl-loop for direction in directions
                    if (string-equal (car direction)
                                     go-translate-local-language)
                    return direction))
          ((= check 0)
           (cl-loop for direction in directions
                    unless (string-equal (car direction)
                                         go-translate-local-language)
                    return direction))
          (t
           (cons go-translate-local-language go-translate-target-language)))))

(defun go-translate-minibuffer-next-direction (&optional backwardp)
  "Switch to next direction in minibuffer.
If BACKWARDP is t, then choose prev one."
  (interactive)
  (let ((d (go-translate-next-direction
            go-translate--current-direction backwardp)))
    (setq go-translate--current-direction d)
    (go-translate-default-prompt-inputs (minibuffer-contents) d)
    (exit-minibuffer)))

(defun go-translate-minibuffer-prev-direction ()
  "Switch to prev direction in minibuffer."
  (interactive)
  (go-translate-minibuffer-next-direction t))

(defun go-translate-buffer-source-folding-clear ()
  "Expand the foldings in the buffer."
  (interactive)
  (cl-loop for ov in (overlay-lists)
           if (overlayp ov)
           do (delete-overlay ov)))

;; Functions

(defvar go-translate-inputs-minibuffer-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-g" #'top-level)
    (define-key map "\C-n" #'go-translate-minibuffer-next-direction)
    (define-key map "\C-p" #'go-translate-minibuffer-prev-direction)
    (define-key map "\C-l" #'delete-minibuffer-contents)
    (define-key map [C-return] (lambda ()
                                 (interactive)
                                 ;; add some options to used in the later phase
                                 ;; for example, `follow` as the `focus the window`
                                 (add-text-properties (minibuffer-prompt-end) (point-max)
                                                      '(follow t))
                                 (exit-minibuffer)))
    map)
  "Minibuffer keymap used when prompt user input.")

(declare-function pdf-view-active-region-p "ext:pdf-view.el" t t)
(declare-function pdf-view-active-region-text "ext:pdf-view.el" t t)

(defvar go-translate-buffer-overlay-keymap
  (let ((map (make-sparse-keymap)))
    (cl-loop for key in (list (kbd "C-g") (kbd "C-m") [return] [mouse-1])
             do (define-key map key #'go-translate-buffer-source-folding-clear))
    map)
  "Keymap used in overylays in the result buffer.")

(defun go-translate-default-current-text ()
  "Get current text under cursor, selection or word."
  (cond ((eq major-mode 'pdf-view-mode)
         (if (pdf-view-active-region-p)
             (car (pdf-view-active-region-text))))
        ((use-region-p)
         (string-trim (buffer-substring (region-beginning) (region-end))))
        (t (current-word t t))))

(defun go-translate-default-prompt-inputs (&optional text direction)
  "Prompt for the user input, should return a (TEXT DIRECTION) list."
  (unless direction
    (if current-prefix-arg
        (setq direction
              (cons (go-translate-choose-language "From: " go-translate-target-language)
                    (go-translate-choose-language "To: " go-translate-local-language)))
      (unless (and go-translate-auto-guess-direction
                   (assoc go-translate-local-language
                          go-translate-local-language-regexp-alist))
        (setq direction (or go-translate-last-direction
                            (cons go-translate-local-language
                                  go-translate-target-language))))))
  (unless text
    (setq text (funcall go-translate-text-function)))
  (setq go-translate--current-direction direction)
  (let* ((minibuffer-allow-text-properties t)
         (prompt (concat (if direction
                             (concat "[" (car direction) " > " (cdr direction) "] ")
                           "[Auto] ")
                         "Text: "))
         (text (read-from-minibuffer prompt text go-translate-inputs-minibuffer-keymap)))
    (if (zerop (length (string-trim text)))
        (user-error "Text should not be null"))
    (setq direction
          (or go-translate--current-direction
              (go-translate-guess-direction text)))
    (list text (car direction) (cdr direction))))

(defun go-translate-default-generate-url (text from to)
  "Generate the url with TEXT, FROM and TO.
Return a (url text from to) list."
  (let* ((params `(("client" . "gtx")
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
                   ("tk"     . ,(go-translate-get-token text))))
         (url (format "%s%s?%s"
                      go-translate-base-url
                      go-translate-path
                      (mapconcat (lambda (p)
                                   (format "%s=%s"
                                           (url-hexify-string (car p))
                                           (url-hexify-string (cdr p))))
                                 params "&"))))
    (list url text from to)))

(defun go-translate-default-buffer-prepare (req)
  "Pre-render contents in REQ to result buffer.

REQ is a list with (url text from to) form.

Before sending a request to the host, create a buffer and
render part of the content. Because the request will be asynchronous,
dividing the rendering into two parts will have a better experience."
  (deactivate-mark)
  (with-current-buffer (get-buffer-create go-translate-buffer-name)
    (let ((text (string-trim (cl-second req)))
          (from (cl-third req)) (to (cl-fourth req)))
      (read-only-mode -1)
      (erase-buffer)
      (visual-line-mode -1)

      ;; header line
      (setq header-line-format
            (list
             " "
             "[" (propertize from 'face 'font-lock-keyword-face) "]"
             " → "
             "[" (propertize to 'face 'font-lock-keyword-face) "]"
             "          "
             "Loading..."))

      ;; source text
      (insert "\n" text)

      ;; keybinds.
      ;; q for quit and kill the window.
      ;; x for switch sl and tl.
      ;; M-n and M-p to re-query with next/prev direction
      ;; g for re-query/refresh
      ;; s to speak the current selection or word
      (local-set-key (kbd "M-n")
                     (lambda ()
                       (interactive)
                       (let ((next (go-translate-next-direction (cons from to))))
                         (go-translate text (car next) (cdr next)))))
      (local-set-key (kbd "M-p")
                     (lambda ()
                       (interactive)
                       (let ((prev (go-translate-next-direction (cons from to) 1)))
                         (go-translate text (car prev) (cdr prev)))))
      (local-set-key "g" (lambda () (interactive) (go-translate text from to)))
      (local-set-key "x" (lambda () (interactive) (go-translate text to from)))
      (local-set-key "y" #'go-translate-tts-play-current)
      (local-set-key "q" #'kill-buffer-and-window)

      ;; display window
      (let ((split-width-threshold go-translate-split-width-threshold))
        (display-buffer (current-buffer) go-translate-buffer-window-config)))))

(defun go-translate-default-retrieve-async (req render-fun)
  "Request with url in REQ for the translation, then render with RENDER-FUN.
This should be asynchronous."
  (let ((buf (current-buffer)))
    (url-retrieve (car req)
                  (lambda (status)
                    (if (and status (eq (car status) :error))
                        (with-current-buffer buf
                          ;; if errors occur, pass an error string to the render function
                          (funcall render-fun req (format "Request Error: %s" (cdr status))))
                      (goto-char (point-min))
                      (re-search-forward "\n\n")
                      (let ((content (buffer-substring-no-properties (point) (point-max))))
                        (with-current-buffer buf
                          (funcall render-fun req
                                   ;; catch the error and pass to render function
                                   (condition-case err
                                       (prog1
                                           (go-translate-result--to-json content)
                                         (message "Done."))
                                     (error (format "Result Error: %s" err)))))))
                    (kill-buffer)))))

(defun go-translate-default-buffer-render (req resp)
  "Render the json RESP obtained through REQ to buffer.

The buffer is the one created in the preparation phase.

Use q to kill current buffer and window.
Use \\[exchange-point-and-mark] to select the translation result quickly after focus.

You can use `go-translate-buffer-post-render-hook' to custom more."
  ;; render the translations
  (with-current-buffer go-translate-buffer-name
    (setq header-line-format (butlast header-line-format)) ; remove loading...
    (if (stringp resp) ; an error occurred
        (progn (goto-char (point-max))
               (insert "\n\n\n" (propertize resp 'face 'font-lock-warning-face)))
      (let* ((source (cl-second req))
             (details (go-translate-result--details resp))
             (definitions (go-translate-result--definitions resp))
             (suggestion (go-translate-result--suggestion resp))
             (phonetic (lambda (ph)
                         (if (and (or definitions definitions) (> (length ph) 0))
                             (propertize (format " [%s]" ph) 'face '(:inherit font-lock-string-face :slant normal))
                           "")))
             (headline (lambda (headline)
                         (propertize
                          (format "\n[%s]\n" headline)
                          'face go-translate-buffer-headline-face)))
             (singlep (or details definitions))
             source-end translate-end)
        (goto-char (point-max))

        ;; cache the query direction first
        (setq go-translate-last-direction (cons (cl-third req) (cl-fourth req)))

        ;; try to wrap the lines when they're too long
        (when (and go-translate-buffer-line-wrap-p (not singlep))
          (turn-on-visual-line-mode))

        ;; phonetic & translate
        (if singlep
            (progn
              (insert (funcall phonetic (go-translate-result--text-phonetic resp)))
              (insert " ")
              (push-mark nil 'no-msg)
              (insert (propertize (go-translate-result--translation resp)
                                  'face '(:weight bold)))
              (setq translate-end (point))
              (insert (funcall phonetic (go-translate-result--translation-phonetic resp)))
              (insert "\n\n"))
          (setq source-end (point))
          (facemenu-add-face 'font-lock-doc-face (point-min) (point))
          (insert "\n\n")
          (push-mark nil 'no-msg)
          (insert (go-translate-result--translation resp))
          (setq translate-end (point))
          (insert "\n"))

        ;; suggestion
        (when (> (length suggestion) 0)
          (insert (funcall headline "Suggestion"))
          (insert "\n")
          (insert (propertize "Did you mean:" 'face 'font-lock-warning-face) " ")
          (insert (propertize suggestion 'face '((t (:slant italic :underline t)))))
          (insert "\n"))

        ;; details
        (when details
          (insert (funcall headline "Details"))
          (cl-loop for item across details
                   for label = (aref item 0)
                   unless (string-equal label "")
                   do (let ((index 0))
                        (insert (format "\n%s:\n" label))
                        (cl-loop for trans across (aref item 2)
                                 for content = (format "%s (%s)"
                                                       (aref trans 0)
                                                       (mapconcat #'identity (aref trans 1) ", "))
                                 do (insert (format "%2d. %s\n" (cl-incf index) content)))))
          (insert "\n"))

        ;; definitions
        (when definitions
          (insert (funcall headline "Definitions"))
          (cl-loop for item across definitions
                   for label = (aref item 0)
                   unless (string-equal label "")
                   do (let ((index 0))
                        (insert (format "\n%s:\n" label))
                        (cl-loop for def across (aref item 1)
                                 for content = (concat
                                                (aref def 0)
                                                (when (> (length def) 2)
                                                  (propertize
                                                   (format "\n    %s" (aref def 2))
                                                   'face 'font-lock-string-face)))
                                 do (insert (format "%2d. " (cl-incf index))
                                            content "\n"))))
          (insert "\n"))

        ;; After the render ended, first set the display
        ;; style of the buffer.
        (setq-local cursor-type 'hbar)
        (setq-local cursor-in-non-selected-windows nil)
        (set-buffer-modified-p nil)
        (read-only-mode +1)
        ;; fold source if nessary
        (unless singlep
          (if (and go-translate-buffer-source-fold-p
                   (string-match-p "\n" source))
              (let (beg o l)
                (setq beg (save-excursion
                            (goto-char (point-min))
                            (re-search-forward "^." nil t)
                            (point)))
                (setq o (make-overlay (save-excursion (goto-char beg) (line-end-position)) source-end))
                (setq l (make-overlay (point-min) (1+ source-end)))
                (overlay-put o 'invisible t)
                (overlay-put o 'display (concat " " (propertize "..." 'face 'font-lock-warning-face)))
                (overlay-put o 'keymap go-translate-buffer-overlay-keymap)
                (overlay-put l 'keymap go-translate-buffer-overlay-keymap))))
        ;; Jump to the end of the translated text. Combined with the previous `push-mark',
        ;; you can quickly select the translated text through `C-x C-x'.
        (set-window-point (get-buffer-window)
                          (if singlep translate-end (1+ translate-end)))
        ;; Run hooks if any.
        (run-hook-with-args 'go-translate-after-render-hook req resp)
        ;; At last, switch or just display.
        (if (or go-translate-buffer-follow-p
                (get-text-property 0 'follow source))
            (pop-to-buffer (current-buffer))
          (display-buffer (current-buffer)))))))

;; TTS (Text To Speech)

(defcustom go-translate-tts-speaker (executable-find "mplayer")
  "The program to use to speak the translation.

On Windows, if it is not found, will fallback to use `powershell`
to do the job. Although it is not perfect, it seems to work."
  :type 'string)

(defcustom go-translate-tts-text-spliter 'go-translate-tts-split-text
  "Function to used to split TEXT to suitable length for TTS url.
Should return the split result as a list."
  :type 'string)

(defun go-translate-tts-split-text (text)
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

(defun go-translate-tts-generate-urls (text language)
  "Generate the tts urls for TEXT to LANGUAGE."
  (cl-loop with texts = (funcall go-translate-tts-text-spliter text)
           for total = (length texts)
           for index from 0
           for piece in texts
           collect
           (let ((params `(("ie"      . "UTF-8")
                           ("client"  . "gtx")
                           ("prev"    . "input")
                           ("q"       . ,text)
                           ("tl"      . ,language)
                           ("total"   . ,(number-to-string total))
                           ("idx"     . ,(number-to-string index))
                           ("textlen" . ,(number-to-string (length piece)))
                           ("tk"      . ,(go-translate-get-token piece)))))
             (format "%s%s?%s"
                     go-translate-base-url
                     go-translate-tts-path
                     (mapconcat (lambda (p)
                                  (format "%s=%s"
                                          (url-hexify-string (car p))
                                          (url-hexify-string (cdr p))))
                                params "&")))))

(defun go-translate-tts-play-current (&optional language)
  "Speak the current selection or word at point to LANGUAGE.

If the `go-translate-tts-speaker' is found, then use it for tts.
Otherwise, on windows try to use `powershell` to do the job, others throw error."
  (interactive)
  (let ((text (go-translate-default-current-text)))
    (unless text (user-error "Nothing found at point"))
    (if go-translate-tts-speaker
        (let ((urls (go-translate-tts-generate-urls text (or language "auto"))))
          (when go-translate-debug-p
            (cl-loop for u in urls do (message "> %s" u)))
          (with-temp-message "Speaking..."
            (apply #'call-process go-translate-tts-speaker nil nil nil urls)))
      (if (executable-find "powershell")
          (let ((cmd (format "$w = New-Object -ComObject SAPI.SpVoice; $w.speak(\\\"%s\\\")" text)))
            (shell-command (format "powershell -Command \"& {%s}\""
                                   (encode-coding-string
                                    (replace-regexp-in-string "\n" " " cmd)
                                    (keyboard-coding-system)))))
        (user-error "Program mplayer/powershell or others is need for tts")))))


;;; Entrance

;;;###autoload
(cl-defun go-translate (text from to &key
                             (url-fun go-translate-url-function)
                             (pre-fun go-translate-prepare-function)
                             (req-fun go-translate-request-function)
                             (render-fun go-translate-render-function))
  "Translate TEXT from FROM to TO.

Many things can be customized, with keywords or let-binding.

URL-FUN is used to specify the way to generate request url.
PRE-FUN is used to specify the way to pre-render.
REQ-FUN is used to specify the retrieving method.
RENDER-FUN is used to specify the way to render after request."
  (interactive `,@(funcall go-translate-inputs-function))
  (let ((req (funcall url-fun text from to)))
    (funcall pre-fun req)
    (funcall req-fun req render-fun)))

;;;###autoload
(defun go-translate-change-local-and-target-language ()
  "Config the default local and target language interactively."
  (interactive)
  (setq go-translate-local-language
        (go-translate-choose-language
         "Change local to: " go-translate-local-language))
  (setq go-translate-target-language
        (go-translate-choose-language
         "Change target to: " go-translate-target-language))
  (message "[local] %s [target] %s" go-translate-local-language go-translate-target-language))


;;; Extended Commands (examples)

;; Helpers

(defun go-translate-inputs-noprompt (&optional text)
  "Return the translation TEXT and DIRECTION without any prompt."
  (let* ((text (or text
                   (funcall go-translate-text-function)
                   (user-error "No text found under cursor")))
         (localp (go-translate-text-local-p text))
         (from (if (= localp 1)
                   go-translate-local-language
                 go-translate-target-language))
         (to (if (= localp 1)
                 go-translate-target-language
               go-translate-local-language)))
    (when go-translate-buffer-follow-p
      (add-text-properties 0 (length text) '(follow t) text))
    (list text from to)))

(defun go-translate-inputs-current-or-prompt ()
  "Like `go-translate-inputs-noprompt', but prompt when nothing at point."
  (let ((text (funcall go-translate-text-function)))
    (if text
        (go-translate-inputs-noprompt text)
      (go-translate-default-prompt-inputs))))

;; Commands

(require 'posframe nil t)
(declare-function posframe-show "ext:posframe.el" t t)
(declare-function posframe-poshandler-point-bottom-left-corner-upward "ext:posframe.el" t t)

(defvar go-translate-posframe-buffer " *Go-Translate-Posframe*")

(defun go-translate-posframe-clear ()
  "Close the pop-up posframe window."
  (ignore-errors (kill-buffer go-translate-posframe-buffer))
  (remove-hook 'post-command-hook #'go-translate-posframe-clear))

;;;###autoload
(defun go-translate-popup (text from to)
  "Show the short translation of TEXT from FROM to TO quickly.

It will show in `posframe' and dispear in 20 seconds, and can be
broken by any user action.

You should make sure `posframe' is in your `load-path' to use this.

This example shows that it's very simple to extend functions
with current `go-translate'. Here we use the keyword style."
  (interactive `,@(funcall go-translate-inputs-function))
  (let ((fn (lambda (_req resp)
              (deactivate-mark)
              (posframe-show go-translate-posframe-buffer
                             :string (go-translate-result--translation resp)
                             :position (point)
                             :timeout 20
                             :internal-border-width 10
                             :foreground-color "#ffffff"
                             :background-color "#000000"
                             :x-pixel-offset -1
                             :y-pixel-offset -1
                             :poshandler #'posframe-poshandler-point-bottom-left-corner-upward)
              (add-hook 'post-command-hook #'go-translate-posframe-clear))))
    (go-translate text from to :pre-fun #'ignore :render-fun fn)))

;;;###autoload
(defun go-translate-popup-current ()
  "Translate the content under cursor: selection or word.
Auto judge the direction, if failed then take the default local/target
as the direction.

This will not prompt anything."
  (interactive)
  (let ((go-translate-inputs-function #'go-translate-inputs-noprompt))
    (call-interactively #'go-translate-popup)))

;;;###autoload
(defun go-translate-kill-ring-save ()
  "Translate and just put result into kill ring for later yank.

Here we implement it with let-binding style."
  (interactive)
  (let ((go-translate-prepare-function #'ignore)
        (go-translate-render-function
         (lambda (_req resp)
           (deactivate-mark)
           (kill-new (go-translate-result--translation resp))
           (message "Translate result already in the kill ring."))))
    (call-interactively #'go-translate)))

;;;###autoload
(defun go-translate-echo-area ()
  "Output the translate result to the echo area."
  (interactive)
  (let ((go-translate-prepare-function #'ignore)
        (go-translate-render-function
         (lambda (_req resp)
           (deactivate-mark)
           (message "%s" (go-translate-result--translation resp)))))
    (call-interactively #'go-translate)))


(provide 'go-translate)

;;; go-translate.el ends here
