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
;; The most important variables are `go-translate-native-language' and
;; `go-translate-target-language', represents your native language and
;; default foreign language. And the `go-translate-extra-directions' is
;; a alist to hold the daily used translation directions except the
;; native/target ones. The following is just a simple example:
;;
;; (setq go-translate-native-language "zh_CN")
;; (setq go-translate-target-language "en")
;; (setq go-translate-extra-directions '(("zh_CN" . "jp") ("zh_CN" . "fr")))
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

(defcustom go-translate-query-path "/translate_a/single"
  "The query path part of google translate url."
  :type 'string)

(defcustom go-translate-user-agent "Emacs"
  "User agent used in the translation request."
  :type 'string)

(defcustom go-translate-input-function #'go-translate-default-prompt-input
  "Function to take the translation text, sl and tl."
  :type 'function)

(defcustom go-translate-url-function #'go-translate-default-generate-url
  "Function to generate a proper reqeust url to Google.
It will take the user input as parameters, that is, Text/FROM/TO."
  :type 'function)

(defcustom go-translate-prepare-function #'go-translate-default-buffer-prepare
  "A function executed before sending a request to pre-rending etc.
It will take URL as parameter."
  :type 'function)

(defcustom go-translate-request-function #'go-translate-default-retrieve-async
  "Function to retrieve translations from Google.
Take URL as parameter."
  :type 'function)

(defcustom go-translate-render-function #'go-translate-default-buffer-render
  "Function to render the translation result.
Take the url and result as parameters."
  :type 'function)


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

(defvar go-translate-token-current nil)

(defvar go-translate-token--timer nil)

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
        (with-temp-buffer
          (with-current-buffer
              (url-retrieve-synchronously go-translate-base-url)
            (setq go-translate-token-current
                  (cons (current-time)
                        (go-translate-token--extract-tkk)))))
      (url-retrieve go-translate-base-url
                    (lambda (_) ; TODO: if error, set nil
                      (setq go-translate-token-current
                            (cons (current-time)
                                  (go-translate-token--extract-tkk)))
                      (kill-buffer))))))

(defun go-translate-token--extract-tkk ()
  "Get the Token-Key from the page buffer."
  (re-search-forward ",tkk:'\\([0-9]+\\)\\.\\([0-9]+\\)")
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
          (run-at-time 0 go-translate-token-expired-time #'go-translate-token--fetch-tkk)))
  ;; get from cache if nessary
  (if (and go-translate-token-current
           (< (float-time (time-subtract (current-time)
                                         (car go-translate-token-current)))
              go-translate-token-expired-time))
      (cdr go-translate-token-current)
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

(defcustom go-translate-native-language "auto"
  "Your native language, used as the default source language."
  :type 'string)

(defcustom go-translate-target-language "en"
  "Your most commonly used foreign language as default target."
  :type 'string)

(defcustom go-translate-extra-directions nil
  "Extra directions as an alist. Like this:

'((\"zh_CN\" . \"ja\")
  (\"zh_CN\" . \"fr\"))

Together with `go-translate-native-language' and `go-translate-target-language',
they combine into a list to represent all translation directions for daily use.

Some functions, such as switching translation languages, are based on them."
  :type '(repeat (cons string string)))

(defcustom go-translate-buffer-name "*Go Translate*"
  "The name of translation result buffer."
  :type 'string)

(defcustom go-translate-buffer-follow-p nil
  "If t then pop to the result window after translation."
  :type 'boolean)

(defcustom go-translate-auto-guess-direction t
  "Automatically determine the languages of the translation based on the input.

If set to nil, will directly circle the available direcitons instead of guessing."
  :type 'boolean)

(defcustom go-translate-buffer-window-config '((display-buffer-reuse-window display-buffer-in-side-window)
                                               (side . right))
  "Window configuration used by the result buffer window.
Default is on right side."
  :type 'list)

(defvar go-translate-native-language-regexp-alist
  '(("zh_CN" . "\\cc")
    ("zh"    . "\\cc")
    ("ja"    . "\\cj"))
  "Alist used to judge if input is your native language text.

The form is (lang . regexp).

This is based that some texts can easily be determine with regexp,
for example, using the \\cx syntax. Maybe work for some languages.")

(defvar go-translate-last-direction nil)

(defvar go-translate--current-direction nil)

(defvar go-translate-buffer-headline-face
  '(:inherit font-lock-function-name-face :weight bold)
  "Propertize the headline in buffer rendering.")

;;;

(defun go-translate-check-text-native (text)
  "Check if TEXT if your native language text.
1 for yes, 0 for no, -1 for unknown."
  (let ((pair (assoc go-translate-native-language
                     go-translate-native-language-regexp-alist)))
    (if pair
        (if (string-match-p (cdr pair) text) 1 0)
      -1)))

(defun go-translate-available-directions ()
  "List of all available translation directions."
  (append (list (cons go-translate-target-language go-translate-native-language)
                (cons go-translate-native-language go-translate-target-language))
          go-translate-extra-directions))

(defun go-translate-next-available-direction (direction &optional backwardp)
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

(defun go-translate-guess-the-direction (text)
  "Automatically judge the translation languages based on the TEXT content.

If the text is your native language and hit the last direction, use
the last direction. Or choose from the available directions.

Otherwise, choose the most suitable one from the list or directly use
the last direction."
  (let ((check (go-translate-check-text-native text))
        (directions (go-translate-available-directions)))
    (cond ((and go-translate-last-direction
                (or (= check -1)
                    (and (= check 1)
                         (string-equal
                          (car go-translate-last-direction)
                          go-translate-native-language))
                    (and (= check 0)
                         (not (string-equal
                               (car go-translate-last-direction)
                               go-translate-native-language)))))
           go-translate-last-direction)
          ((= check 1)
           (cl-loop for direction in directions
                    if (string-equal (car direction)
                                     go-translate-native-language)
                    return direction))
          ((= check 0)
           (cl-loop for direction in directions
                    unless (string-equal (car direction)
                                         go-translate-native-language)
                    return direction))
          (t
           (cons go-translate-native-language go-translate-target-language)))))

(defun go-translate-minibuffer-switch-next-direction (&optional backwardp)
  "Switch to next direction in minibuffer.
If BACKWARDP is t, then choose prev one."
  (interactive)
  (let ((d (go-translate-next-available-direction
            go-translate--current-direction backwardp)))
    (setq go-translate--current-direction d)
    (go-translate-default-prompt-input (minibuffer-contents) d)
    (exit-minibuffer)))

(defun go-translate-minibuffer-switch-prev-direction ()
  "Switch to prev direction in minibuffer."
  (interactive)
  (go-translate-minibuffer-switch-next-direction t))

;;;

(defvar go-translate-default-minibuffer-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-g" #'top-level)
    (define-key map "\C-n" #'go-translate-minibuffer-switch-next-direction)
    (define-key map "\C-p" #'go-translate-minibuffer-switch-prev-direction)
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

(defun go-translate-default-prompt-input (&optional text direction)
  "Prompt for the user input, should return a (TEXT DIRECTION) list."
  (unless direction
    (if current-prefix-arg
        (setq direction
              (cons (read-from-minibuffer "Source: ")
                    (read-from-minibuffer "Target: ")))
      (unless (and go-translate-auto-guess-direction
                   (assoc go-translate-native-language
                          go-translate-native-language-regexp-alist))
        (setq direction (or go-translate-last-direction
                            (cons go-translate-native-language
                                  go-translate-target-language))))))
  (unless text
    (setq text (if (use-region-p)
                   (string-trim (buffer-substring (region-beginning) (region-end)))
                 (current-word t t))))
  (setq go-translate--current-direction direction)
  (let* ((minibuffer-allow-text-properties t)
         (prompt (concat (if direction
                             (concat "[" (car direction) " > " (cdr direction) "] ")
                           "[Auto] ")
                         "Text: "))
         (text (read-from-minibuffer prompt text go-translate-default-minibuffer-keymap)))
    (if (zerop (length (string-trim text)))
        (user-error "Text should not be null"))
    (setq direction
          (or go-translate--current-direction
              (go-translate-guess-the-direction text)))
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
                      go-translate-query-path
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
    (let ((text (cl-second req))
          (from (cl-third req))
          (to (cl-fourth req)))
      (read-only-mode -1)
      (visual-line-mode -1)
      (erase-buffer)

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
      (insert (format "\n%s" text ))

      ;; keybinds.
      ;; q for quit and kill the window.
      ;; x for switch sl and tl.
      ;; M-n and M-p to re-query with next/prev direction
      ;; g for re-query/refresh
      (local-set-key (kbd "M-n")
                     (lambda ()
                       (interactive)
                       (let ((next (go-translate-next-available-direction (cons from to))))
                         (go-translate text (car next) (cdr next)))))
      (local-set-key (kbd "M-p")
                     (lambda ()
                       (interactive)
                       (let ((prev (go-translate-next-available-direction (cons from to) 1)))
                         (go-translate text (car prev) (cdr prev)))))
      (local-set-key "g" (lambda () (interactive) (go-translate text from to)))
      (local-set-key "x" (lambda () (interactive) (go-translate text to from)))
      (local-set-key "q" 'kill-buffer-and-window)

      ;; display window
      (display-buffer (current-buffer) go-translate-buffer-window-config))))

(defun go-translate-default-retrieve-async (req render-fun)
  "Request with url in REQ for the translation, then render with RENDER-FUN.
This should be asynchronous."
  (let ((buf (current-buffer)))
    (url-retrieve (car req)
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "\n\n")
                    (let ((content (buffer-substring-no-properties (point) (point-max))))
                      (with-current-buffer buf
                        (funcall render-fun req
                                 (go-translate-result--to-json content))))
                    (kill-buffer)))))

(defun go-translate-default-buffer-render (req resp)
  "Render the json RESP obtained through REQ to buffer.

The buffer is the one created in the preparation phase.

Use q to kill current buffer and window.
Use \\[exchange-point-and-mark] to select the translation result quickly after focus.

You can use `go-translate-buffer-post-render-hook' to custom more."
  ;; render the translations
  (with-current-buffer go-translate-buffer-name
    (setq header-line-format (butlast header-line-format))
    (let* ((details (go-translate-result--details resp))
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
           (savepoint))
      (goto-char (point-max))

      ;; cache the query direction first
      (setq go-translate-last-direction
            (cons (cl-third req)
                  (cl-fourth req)))

      ;; phonetic & translate
      (if singlep
          (progn
            (insert (funcall phonetic (go-translate-result--text-phonetic resp)))
            (insert " ")
            (push-mark nil 'no-msg)
            (insert (propertize (go-translate-result--translation resp)
                                'face '(:weight bold)))
            (setq savepoint (point))
            (insert (funcall phonetic (go-translate-result--translation-phonetic resp)))
            (insert "\n\n"))
        (facemenu-add-face 'font-lock-doc-face (point-min) (point))
        (insert "\n\n")
        (push-mark)
        (insert (go-translate-result--translation resp))
        (setq savepoint (point))
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
      (unless singlep (visual-line-mode +1))
      ;; Jump to the end of the translated text. Combined with the previous `push-mark',
      ;; you can quickly select the translated text through `C-x C-x'.
      (set-window-point (get-buffer-window) savepoint)
      ;; Run hooks if any.
      (run-hook-with-args 'go-translate-after-render-hook req resp)
      ;; At last, switch or just display.
      (if (or go-translate-buffer-follow-p (get-text-property 0 'follow (cl-second req)))
          (pop-to-buffer (current-buffer) go-translate-buffer-window-config)
        (display-buffer (current-buffer) go-translate-buffer-window-config)))))


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
  (interactive `,@(funcall go-translate-input-function))
  (let ((req (funcall url-fun text from to)))
    (funcall pre-fun req)
    (funcall req-fun req render-fun)))


;;; Pop-up with Posframe

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

It will show in posframe and dispear in 20 seconds, and can be
broken by any user action.

This example shows that it's very simple to extend functions
with current `go-translate'."
  (interactive `,@(funcall go-translate-input-function))
  (go-translate text from to
                :pre-fun #'ignore
                :render-fun
                (lambda (_req resp)
                  (deactivate-mark)
                  (posframe-show
                   go-translate-posframe-buffer
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


(provide 'go-translate)

;;; go-translate.el ends here
