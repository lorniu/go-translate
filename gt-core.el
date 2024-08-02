;;; gt-core.el --- Core implement of the Translator -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Author: lorniu <lorniu@gmail.com>
;; Package-Requires: ((emacs "28.1"))

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

;; The core implement of the translator

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dom)
(require 'eieio)
(require 'text-property-search)

(defgroup go-translate nil
  "Translation framework on Emacs, with high configurability and extensibility."
  :group 'external
  :prefix 'gt-)

(defcustom gt-debug-p nil
  "Whether enable the debug message."
  :type 'boolean
  :group 'go-translate)


;;; Components and Variables

(defclass gt-single ()
  ((insts :allocation :class :initform nil))
  :abstract t
  :documentation "Only create one instance for same slots.")

(cl-defmethod make-instance ((class (subclass gt-single)) &rest slots)
  (if-let* ((key (sha1 (format "%s" slots)))
            (insts (oref-default class insts))
            (old (cdr-safe (assoc key insts))))
      old
    (let ((inst (cl-call-next-method)))
      (prog1 inst
        (oset-default class insts `((,key . ,inst) ,@insts))))))

(defclass gt-validator ()
  ((if :initarg :if
     :type (or function symbol list)
     :documentation "See `gt-valid' for details."))
  :abstract t
  :documentation "Use to detect whether component is available")

(defclass gt-task ()
  ((text
    :initarg :text
    :type (or string list)
    :documentation "Source text list to be translated.")
   (src
    :initarg :src
    :initform nil
    :type symbol
    :documentation "Generally, the source language.")
   (tgt
    :initarg :tgt
    :type symbol
    :documentation "Generally, the target language.")
   (meta
    :initform nil
    :initarg :meta
    :type t
    :documentation "Extra data passed in translation.")
   (res
    :initform nil
    :type (or string list function)
    :documentation "Translation result.")
   (err
    :initform nil
    :documentation "Translation error.")
   (cache
    :initform nil
    :documentation "Used to temporarily save the data from cache.")
   (markers
    :initform nil
    :documentation "Markers tracking output bounds.")
   (process
    :initform nil
    :documentation "Which process current task running.")

   (id
    :initform (gensym "task-")
    :documentation "Used to distinguish between different tasks.")
   (version
    :initform nil
    :initarg :version
    :documentation "If this is different with the one in translator, task is expired.")

   (engine
    :initarg :engine
    :documentation "Engine of current task.")
   (render
    :initarg :render
    :documentation "Render of current task.")
   (translator
    :initarg :translator
    :documentation "Translator of current task."))
  :documentation
  "An object contains basic translation infomations.
Its life cycle is in one translation. It is created by translator, then derived
to engines for result, at last extracted by render for output.")

(defclass gt-taker (gt-single gt-validator)
  ((text
    :initarg :text
    :type (or function symbol string)
    :documentation "See `gt-taker-text' for details.")
   (pick
    :initarg :pick
    :type (or function symbol)
    :documentation "See `gt-taker-pick' for details.")
   (pick-pred
    :initarg :pick-pred
    :initform nil
    :type (or function null)
    :documentation "Filter the picked element with this function.")
   (langs
    :initarg :langs
    :type list
    :documentation "See `gt-langs' for details.")
   (prompt
    :initarg :prompt
    :type (or function symbol)
    :documentation "See `gt-taker-prompt' for details.")
   (then
    :initarg :then
    :documentation "A function with current translator as argument.
Used to do extra works after taking done. For example, one can filter the
source text list to remove unnecessary elements, or modifiy the translate
target as a final step."))
  :documentation
  "A component used to take the translate source text and targets for translator.

The steps are:
1. take the initial text by `text'
2. get the available targets from `langs'
3. ensure the text and langs via `prompt' if necessary
4. split or filter text via `pick' if possible

So you can define a taker fully like this:

  (gt-taker :text `buffer :langs `(en it) :prompt `buffer :pick `sentence)

Or use the default value or corresponding variables as:

  (setq gt-langs `(en it))
  (gt-taker)

Then use the taker in the translator and start your translation.")

(defclass gt-parser (gt-single gt-validator)
  ((tag
    :initarg :tag
    :initform nil
    :type (or symbol string)
    :documentation "Display label for this parser."))
  :documentation
  "It is a component of `gt-engine', used to parse the response of the engine
to a user-friendly string. One engine can have different parsers, but can only
use one of them at a time.")

(defclass gt-engine (gt-single gt-validator)
  ((tag
    :initarg :tag
    :type (or string symbol)
    :documentation "Display label for this engine.")
   (parse
    :initarg :parse
    :initform (gt-parser)
    :type (or function gt-parser)
    :documentation "Used to generate user-friendly result from response.")
   (delimiter
    :initarg :delimiter
    :type (or null string)
    :documentation "The delimiter for multiple paragraphs or parts translation.")
   (stream
    :initarg :stream
    :type boolean
    :initform nil
    :documentation "Enable stream support for this engine.
This effect both engine and render. If this is t, then retrieve results
part by part. Render supporting stream output should refresh the display
every time new data arrived.")
   (cache
    :initarg :cache
    :type (or symbol function gt-cacher)
    :documentation "Cache style used by the engine.
If this is nil, current engine will use no cache at all.
If this is literally symbol, see `gt-valid-literally' for details.
If this is a cacher, then cache using the cacher instead.")
   (salt
    :initarg :salt
    :initform nil
    :documentation "Distinguish from... now for cache key only.")
   (then
    :initarg :then
    :type function
    :documentation "Function run after result are responsed and parsed.
Take current task as argument. you can do extra works for current task
before rendering."))
  :documentation "Translate engine, retrieve the result and parse it.

If `delimiter' is non nil, the translation for multiple paragraphs or parts will
take a strategy as `join-translate-split'. That is, join the source text list to
a single string separated by `delimiter', translate the whole string, and then
split the result by `delimiter' into multiple parts. This assumes that the
`delimiter' in string is kept by engine in the translation progress.

Set `delimiter' to nil to avoid this strategy. In that case, a string list will
be passed to the engine, and a translated string list is expected.

Also in some cases, you should turn the cache off by setting `no-cache' to t."
  :abstract t)

(defclass gt-render (gt-single gt-validator)
  ((output
    :initarg :output
    :initform nil
    :type (or function null)
    :documentation "If this is not nil, then will override the default output logic.")
   (then
    :initarg :then
    :initform nil
    :type (or function gt-render null)
    :documentation "Chain results to next render if possible.
If this is a function, pass current translator as argument and execute it only.")
   (prefix
    :initarg :prefix
    :type (or function t)
    :documentation "Custom prefix displayed in the render.
If this is nil, then don't display prefix. If this is function use its return value
as prefix. If this is a string, use the string as prefix."))
  :documentation
  "A component used to output the all the results made by translator.
Method `gt-init' only works once before output, method `gt-output' will run after
every task finished.")

(defclass gt-translator ()
  ((text
    :type list
    :initform nil
    :initarg :text
    :documentation "The text taken by taker, and prepare to translate.
This should be a list, every element is a string as one pargraph or one part.
This list will be sent to translate engines later.")

   (bounds
    :type list
    :initform nil
    :documentation "The bounds in buffer corresponds to the `text' slot.
The first element is the working buffer, while rest are buffer bounds.")

   (target
    :type list
    :initform nil
    :documentation "The target taken by taker.

This should be a list. Generally, the first element is the source language and
the rest are the target languages. Target languages can be one or multiple,
that is, in one translation, can translate same text into multiple languages.

Of course, the target may be more than just the languages. If translator is not
used to translate, then target may be something else representing the transform
targets.")

   (tasks
    :initform nil
    :type list
    :documentation "All tasks created according the source text and targets.")

   (total
    :initform 0
    :type (integer 0 *)
    :documentation "Total tasks in current translation.")

   (state
    :initform 0
    :type (member 0 1 2 3 4 9)
    :documentation "The inner state of the translator for current translation:
0: new translator
1: all tasks added
2: render prepared
3: all result parsed
4: translate abort")

   (keep
    :initform nil
    :type boolean
    :documentation "If t then don't clean text and target in a new translation.")

   (version
    :documentation "Used to distinguish with expired translations.")

   (taker
    :initarg :taker
    :initform nil
    :type (or function gt-taker list)
    :documentation "Used to take input for the translator.
If this is a function, use its return value as the taker. It will take the
source text and targets, and save them into `text', `bounds' and `target' slots
for the following translate task. If this is a list, use the first available one.")

   (engines
    :initarg :engines
    :initform nil
    :type (or function gt-engine list)
    :documentation "The engines used to translate.
This should be one or more `gt-engine' instances or a function that return them.
The tasks created after `gt-take' will be passed to every engines. Every engine
should translate the task and parse the result. After every task finished, the
rendering process will be performed.")

   (render
    :initarg :render
    :initform nil
    :type (or function gt-render list)
    :documentation "Used to output the translation results.
This should be a `gt-render' instance or a function return one.
If this is a list, use the first available one.")

   (_taker
    :type (or function gt-taker list)
    :documentation "Backup of initial value of `taker', which may be a
function or `gt-taker' object. The value will be normalized at the start of
every translation and stored into the `taker' slot. Avoid changing this during
translation progress unless you are confident in what you are doing.")

   (_engines
    :type (or function gt-engine list)
    :documentation "Backup of initial `engines' like above.")

   (_render
    :type (or function gt-render list)
    :documentation "Backup of initial `render' like above.")

   (bag :initform nil))
  :documentation "Core component of the translation.

The basic components of a translator is taker, engines and render:
1. Use `gt-taker' to capture the source text and target
2. Then use one or more `gt-engine' to get the translate result
3. At last use a `gt-render' to output the results to user.

The start a translation, call `gt-start' on a translator instance like this:

  (gt-start (make-instance `gt-translator
                   :taker (gt-taker)
                   :engines (gt-google-engine)
                   :render (gt-buffer-render)))

That is: 1) define the translator 2) executable `gt-start'.")

(defconst gt-lang-codes
  '((en . "English")
    (zh . "Chinese")
    (ru . "Russian")
    (fr . "French")
    (it . "Italian")
    (es . "Spanish")
    (de . "German")
    (ko . "Korean")
    (ja . "Japanese")
    (yue . "Cantonese")
    (af . "Afrikaans")
    (ak . "Akan")
    (am . "Amharic")
    (ar . "Arabic")
    (as . "Assamese")
    (asa . "Asu")
    (az . "Azerbaijani")
    (be . "Belarusian")
    (bem . "Bemba")
    (bez . "Bena")
    (bg . "Bulgarian")
    (bm . "Bambara")
    (bn . "Bengali")
    (bo . "Tibetan")
    (bs . "Bosnian")
    (ca . "Catalan")
    (cgg . "Chiga")
    (chr . "Cherokee")
    (cs . "Czech")
    (da . "Danish")
    (dav . "Taita")
    (ebu . "Embu")
    (ee . "Ewe")
    (el . "Greek")
    (eo . "Esperanto")
    (et . "Estonian")
    (eu . "Basque")
    (fa . "Persian")
    (ff . "Fulah")
    (fi . "Finnish")
    (fil . "Filipino")
    (fo . "Faroese")
    (ga . "Irish")
    (gl . "Galician")
    (gsw . "Swiss German")
    (gu . "Gujarati")
    (guz . "Gusii")
    (gv . "Manx")
    (ha . "Hausa")
    (haw . "Hawaiian")
    (he . "Hebrew")
    (hi . "Hindi")
    (hr . "Croatian")
    (hu . "Hungarian")
    (hy . "Armenian")
    (id . "Indonesian")
    (ig . "Igbo")
    (ii . "Sichuan Yi")
    (is . "Icelandic")
    (jmc . "Machame")
    (ka . "Georgian")
    (kab . "Kabyle")
    (kam . "Kamba")
    (kde . "Makonde")
    (kea . "Kabuverdianu")
    (khq . "Koyra Chiini")
    (ki . "Kikuyu")
    (kk . "Kazakh")
    (kl . "Kalaallisut")
    (kln . "Kalenjin")
    (km . "Khmer")
    (kn . "Kannada")
    (kok . "Konkani")
    (kw . "Cornish")
    (lag . "Langi")
    (lg . "Ganda")
    (lt . "Lithuanian")
    (luo . "Luo")
    (luy . "Luyia")
    (lv . "Latvian")
    (mas . "Masai")
    (mer . "Meru")
    (mfe . "Morisyen")
    (mg . "Malagasy")
    (mk . "Macedonian")
    (ml . "Malayalam")
    (mr . "Marathi")
    (ms . "Malay")
    (mt . "Maltese")
    (my . "Burmese")
    (naq . "Nama")
    (nd . "North Ndebele")
    (ne . "Nepali")
    (nl . "Dutch")
    (nn . "Norwegian Nynorsk")
    (nyn . "Nyankole")
    (om . "Oromo")
    (or . "Oriya")
    (pa . "Punjabi")
    (pl . "Polish")
    (ps . "Pashto")
    (pt . "Portuguese")
    (rm . "Romansh")
    (ro . "Romanian")
    (rof . "Rombo")
    (rw . "Kinyarwanda")
    (rwk . "Rwa")
    (saq . "Samburu")
    (seh . "Sena")
    (ses . "Koyraboro Senni")
    (sg . "Sango")
    (shi . "Tachelhit")
    (si . "Sinhala")
    (sk . "Slovak")
    (sl . "Slovenian")
    (sn . "Shona")
    (so . "Somali")
    (sq . "Albanian")
    (sr . "Serbian")
    (sv . "Swedish")
    (sw . "Swahili")
    (ta . "Tamil")
    (te . "Telugu")
    (teo . "Teso")
    (th . "Thai")
    (ti . "Tigrinya")
    (to . "Tonga")
    (tr . "Turkish")
    (tzm . "Central Morocco Tamazight")
    (uk . "Ukrainian")
    (ur . "Urdu")
    (uz . "Uzbek")
    (vi . "Vietnamese")
    (vun . "Vunjo")
    (xog . "Soga")
    (cy . "Welsh")
    (yo . "Yoruba")
    (zu . "Zulu")))

(defconst gt-word-classes
  '(pron adj adv adt art aux conj prep det abbr int vt vi v n a))

(defvar gt-current-command nil)

(defvar gt-current-translator nil)

(defvar-local gt-tracking-marker nil)

(cl-defgeneric gt-init (object &rest _args)
  "Initialization for OBJECT.")

(cl-defgeneric gt-desc (object)
  "Return the string representation of the OBJECT."
  (format "%s" (eieio-object-class object)))


;;; Utility

(defun gt-aref (vector &rest ns)
  "Recursively find the element in VECTOR. NS is indexes, as the path."
  (while ns
    (setq vector (aref vector (pop ns))))
  vector)

(defmacro gt-orefs (instance &rest slots)
  "Get all the SLOTS in INSTANCE as a list."
  `(list ,@(cl-loop for slot in slots
                    collect `(slot-value ,instance ',slot))))

(defun gt-ensure-plain (obj &rest args)
  "Ensure OBJ is non-function and non-symbol.
If OBJ is a function, call it with ARGS and return the result.
If OBJ is symbol, return its value."
  (if (functionp obj) (apply obj args) obj))

(defun gt-functionp (object)
  "Check if OBJECT is a function and not a class."
  (and (functionp object) (not (class-p object))))

(defun gt-clean-properties (text &optional props)
  "Remove specified PROPS from TEXT."
  (unless props (setq props '(gt-result gt-task gt-done read-only)))
  (remove-list-of-text-properties 0 (length text) props text)
  text)

(defun gt-face-lazy (str face &optional display)
  "Propertize STR to add FACE only when no face exists on it.
Set DISPLAY property to STR if it is not nil.

For example:

  (gt-face-lazy s `font-lock-doc-face `(space (:width 30) raise 0.5))

When `space' in DISPLAY, prepend a space to the STR."
  (if (or (null face)
          (with-temp-buffer
            (insert str)
            (or (text-property-search-backward 'face)
                (text-property-search-backward 'font-lock-face)
                (text-property-search-backward 'display))))
      str
    (let ((r (if face (propertize str 'face face) str))
          (ds (cl-loop for (k v) on display by #'cddr
                       if (eq k 'space) append v into spaces
                       else append (list k v) into normals
                       finally (return (cons normals spaces)))))
      (when-let (normals (car ds))
        (setq r (propertize r 'display normals)))
      (when-let (spaces (cdr ds))
        (setq r (concat (propertize " " 'display (cons 'space spaces)) r)))
      r)))

(defun gt-line-height-separator (pixel)
  "This can set line-height of the following line to PIXEL."
  (concat (propertize "\s" 'display `(space :height (,pixel)))
          (propertize "\n" 'line-height t)))

(defmacro gt-simple-keymap (&rest definitions)
  "A small wrapper to define new keymap for DEFINITIONS."
  `(let ((map (make-sparse-keymap)))
     (progn
       ,@(cl-loop for (key value) on definitions by #'cddr
                  for v = (if (memq (car value) '(function quote lambda))
                              value
                            `(lambda () (interactive) ,value))
                  collect `(define-key map ,key ,v)))
     map))

(defun gt-collect-text (text-or-bounds)
  "Collect the text corresponding to parts in TEXT-OR-BOUNDS."
  (let* ((items (ensure-list text-or-bounds)) (car (car items)))
    (unless (stringp car)
      (with-current-buffer (if (bufferp car) car (current-buffer))
        (setq items
              (mapcar (lambda (bd) (buffer-substring (car bd) (cdr bd)))
                      (if (bufferp car) (cdr items) items)))))
    (mapcar #'gt-clean-properties items)))

(defun gt-lookup-password (&rest params)
  "Query password stored in '.authinfo'.
PARAMS contains search keys like :user, :host same as `auth-source-search'."
  (when-let (secret (plist-get (car (apply 'auth-source-search params)) :secret))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun gt-insert-text-at-marker (str marker &optional end-marker keep-cursor)
  "Insert STR to position of MARKER.
If END-MARKER exists, delete the content between the markers first.
If KEEP-CURSOR is not nil, keep the cursor at front."
  (with-current-buffer (marker-buffer marker)
    (let ((inhibit-read-only t))
      (when end-marker
        (set-marker-insertion-type end-marker t)
        (delete-region marker end-marker))
      (goto-char marker)
      (deactivate-mark)
      (if keep-cursor
          (save-excursion (insert str))
        (insert str)))))

(defun gt-parse-html-dom (str)
  "Parse html STR and return the DOM body."
  (with-temp-buffer
    (insert str)
    (xml-remove-comments (point-min) (point-max))
    (dom-by-tag (libxml-parse-html-region) 'body)))

(declare-function let-alist--deep-dot-search "ext:let-alist" t t)

(defmacro gt-plist-let (varlist &rest body)
  "Let-bind dotted symbols to plist and execute BODY like `let-alist'.

The last form of VARLIST is the plist to be bound with dot, bind other variables
like those in `let*'.

For example:

  (gt-plist-let plist (list .a .b))

  (gt-plist-let \\=`(:a 3 :b 4) (+ .a .b))

  (gt-plist-let ((x 1) \\='(:y 2 :z 3)) (+ x .y .z))

See gt-tests.el for details."
  (declare (indent 1) (debug t))
  (require 'let-alist)
  (let ((var (make-symbol "plist")))
    `(let* (,@(if (or (atom varlist)
                      (eq (car varlist) 'quote)
                      (eq (car varlist) '\`))
                  `((,var ,varlist))
                `(,@(butlast varlist) ,(cons var (last varlist))))
            ,@(cl-loop for (.k . k) in (delete-dups (let-alist--deep-dot-search body))
                       collect `(,.k (plist-get ,var ,(intern (format ":%s" k))))))
       ,@body)))

(defun gt-make-completion-table (items &optional order)
  "Make completion table that ensure ITEMS sort by original ORDER."
  (lambda (input pred action)
    (if (eq action 'metadata)
        `(metadata (category . "go-translate")
                   (display-sort-function . ,(or order #'identity)))
      (complete-with-action action items input pred))))

(defmacro gt-read-from-buffer (&rest forms)
  "Read text in a new created buffer interactively.

Available options can be specified as keyword arguments:
  :buffer           The buffer used for reading user input
  :initial-contents The initial contents of the buffer
  :keymap           The keymap to use in the buffer
  :window-config    The window configuration for the window display the buffer
  :catch            Catch tag used in for inner `throw'
  :apply-key        The keybinding for applying changes (default: \"C-c C-c\")
  :cancel-key       The keybinding for canceling changes (default: \"C-c C-k\")
  :apply-hook       Hook run before apply operation
  :cancel-hook      Hook run before cancel operation

Other FORMS is used to do extra initialization, you can config head line, mode
line format or bind keys here.

User can modify the buffer, and submit with `apply-key', then the contents of
the buffer will returned as the result."
  (gt-plist-let
      ((cl-loop for item in forms by #'cddr
                while (keywordp item) append (list (pop forms) (pop forms))))
    (let ((buffer (or .buffer (format "*gt-%s*" (gensym "tmp-"))))
          (tag (or .catch ''gt-read-from-buffer))
          (apply-key  (or .apply-key  "C-c C-c"))
          (cancel-key (or .cancel-key "C-c C-k")))
      `(if (and (cl-plusp (recursion-depth)) (buffer-live-p (get-buffer ,buffer)))
           (throw 'exit t) ; avoid recursion
         (save-window-excursion
           (catch ,tag
             (unwind-protect
                 (with-current-buffer (get-buffer-create ,buffer)
                   ,(if .keymap `(use-local-map ,.keymap))
                   (local-set-key (kbd ,apply-key)
                                  (lambda ()
                                    (interactive)
                                    ,(if .apply-hook `(funcall ,.apply-hook))
                                    (throw ,tag (buffer-string))))
                   (local-set-key (kbd ,cancel-key)
                                  (lambda ()
                                    (interactive)
                                    ,(if .cancel-hook `(funcall ,.cancel-hook))
                                    (throw ,tag nil)))
                   (add-hook 'kill-buffer-query-functions
                             (lambda () ; only allow quit with C-c C-k
                               (not (and (memq this-command '(kill-buffer kill-this-buffer))
                                         (message "Quit the buffer using `%s' please" ,cancel-key))))
                             nil t)
                   (erase-buffer)
                   ,(if .initial-contents `(insert ,.initial-contents))
                   ,@forms
                   (pop-to-buffer ,buffer ,.window-config)
                   (recursive-edit))
               (ignore-errors (kill-buffer ,buffer)))))))))

(cl-defgeneric gt-word-p (lang text)
  "Whether TEXT is a single word in LANG.
This is a generic method, improve it for specific LANG as you wish."
  (setq text (string-trim text))
  (if (eq lang 'zh)
      (and (< (length text) 5)
           (with-temp-buffer
             (insert text)
             (goto-char (point-min))
             (while (re-search-forward "\\s." nil t) (replace-match ""))
             (equal (thing-at-point 'word) text)))
    (with-temp-buffer
      (insert text)
      (equal (thing-at-point 'word) text))))


;;; Validator Trait

;; add validate feature to `taker/engine/render/cache' through `:if' slot

(cl-defgeneric gt-valid-literally (v text src tgt)
  "Check literally symbol or form V to determine next step.

It can be used by cacher and engine.

V should be a symbol like `word', `parts', `src:en', `tgt:en', `selection',
`xxx-mode', `read-only', or symbol with `not-' or `no-' prefix such as
`not-word', `not-src:en', `no-parts' etc.

V also can be a list form grouping above symbols with `and/or', for example:

  (or word not-parts (and tgt:cn #`xxxx)).

TEXT, SRC and TGT are translation text and targets.

This is a generic method, you can extend the V as you wish."
  (:method :around (v text src tgt)
           (cond ((or (null v) (eq v t)) v)
                 ((symbolp v)
                  (setq text (ensure-list text))
                  (let ((vn (symbol-name v)) (notp nil))
                    (when (string-match "^not?-\\(.*\\)" vn) ; not-xxx style
                      (setq vn (match-string 1 vn))
                      (setq v (intern vn))
                      (setq notp t))
                    (funcall
                     (if notp #'not #'identity)
                     (cl-call-next-method v text src text))))
                 ((consp v) ; compose as: (or word (and not-parts src:zh))
                  (unless (member (car v) '(or and)) (push 'or v))
                  (funcall (if (eq (pop v) 'or) #'cl-some #'cl-every) #'identity
                           (mapcar (lambda (x) (gt-valid-literally x text src tgt)) v)))))
  (let ((vn (symbol-name v)))
    (cond
     ((eq v 'word) (and (car text) ; text is word
                        (not (cdr text))
                        (gt-word-p (intern-soft src) (car text))))
     ((eq v 'parts) (cdr text)) ; text is multiple parts
     ((eq v 'selection) (use-region-p)) ; use region is active
     ((eq v 'read-only) buffer-read-only) ; buffer is read-only
     ((string-suffix-p "-mode" vn) ; major-mode or minor-mode
      (if (boundp v) (symbol-value v) (eq major-mode v)))
     ((string-match "^\\(src\\|tgt\\):\\(.+\\)" vn)
      (member (match-string 2 vn) ; src/tgt is specific one, as src:en
              (mapcar (lambda (item) (format "%s" item))
                      (ensure-list (if (equal (match-string 1 vn) "src") src tgt))))))))

(cl-defgeneric gt-valid (component &rest _args)
  "Validate COMPONENT, return t if it's available."
  (:method ((component gt-validator) carrier)
           "Determine if current COMPONENT is available through `if' slot.
Slot value can be a function, a symbol or a list. CARRIER maybe a task, a
translator or a list to provide more validation data."
           (let (text src tgt)
             (cond
              ((cl-typep carrier 'gt-task)
               (setq text (oref (oref carrier translator) text)
                     src (oref carrier src)
                     tgt (oref carrier tgt)))
              ((cl-typep carrier 'gt-translator)
               (setq text (oref carrier text)
                     src (car (oref carrier target))
                     tgt (cdr (oref carrier target))))
              ((consp carrier)
               (setq text (car carrier)
                     src (cadr carrier)
                     tgt (caddr carrier)))
              (t (user-error "Carrier type is not supported")))
             (with-slots (if) component
               (cond ((not (slot-boundp component 'if)) t)
                     ((and (functionp if) (= 2 (car (func-arity if))))
                      (funcall if component carrier))
                     ((and (functionp if) (= 1 (car (func-arity if))))
                      (funcall if carrier))
                     (t (gt-valid-literally if text src tgt)))))))


;;; Logging

(defvar gt-log-buffer-name "*gt-log*"
  "Log buffer for translator.")

(defun gt-log (tag &rest messages)
  "Log MESSAGES to `gt-log-buffer-name'.
TAG is a label for message being logged."
  (declare (indent 1))
  (when gt-debug-p
    (with-current-buffer (get-buffer-create gt-log-buffer-name)
      (goto-char (point-max))
      (if (and (null messages) (stringp tag))
          (insert tag)
        (let* ((tag (when tag
                      (concat
                       (gt-face-lazy (cl-subseq (format "%-.1f" (time-to-seconds)) 6) 'gt-logger-buffer-timestamp-face)
                       (gt-face-lazy (if (ignore-errors (string-blank-p tag)) tag (format " [%s]" tag)) 'gt-logger-buffer-tag-face)
                       " ")))
               (count (if (numberp (car messages)) (pop messages) (length tag)))
               (messages (mapconcat (lambda (s) (format "%s" s)) (delq nil messages) "\n"))
               (msg (if (string-blank-p messages)
                        messages
                      (with-temp-buffer
                        (insert messages)
                        (goto-char (point-min))
                        (unless (= (length tag) count)
                          (insert "\n")
                          (goto-char (point-min)))
                        (while (re-search-forward "\n" nil t)
                          (save-excursion
                            (beginning-of-line)
                            (insert (make-string count ? ))))
                        (buffer-string)))))
          (insert (or tag "") (or msg "") "\n"))))))

(defmacro gt-log-funcall (fmt &rest objects)
  `(gt-log ""
     (gt-face-lazy
      (format ,(if (string-match-p "^[ \n]" fmt) fmt (concat "gt-" fmt))
              ,@(cl-loop
                 for o in objects collect
                 `(let ((obj ,o)) (if (eieio-object-p obj) (gt-desc obj) obj))))
      'font-lock-doc-face)))


;;; Caching

(defclass gt-cacher (gt-validator)
  ((storage :initform nil :documentation "Where to save the caches."))
  "Used to cache the translate results."
  :abstract t)

(defcustom gt-cache-alive-time (* 30 60)
  "Default cache alive time."
  :type 'integer
  :group 'go-translate)

(defvar gt-default-cacher nil)

;; generic

(defun gt-cacher-barking ()
  (unless gt-default-cacher
    (user-error "Make sure `gt-default-cacher' is properly configured. eg:\n
 (setq gt-default-cacher (gt-memory-cacher))")))

(cl-defgeneric gt-cache-get (_cacher _key &rest _)
  "Query result of KEY from CACHER."
  (:argument-precedence-order _key _cacher)
  (gt-cacher-barking))

(cl-defgeneric gt-cache-set (_cacher _key &rest _)
  "Save result of KEY to CACHER."
  (:argument-precedence-order _key _cacher)
  (gt-cacher-barking))

(cl-defgeneric gt-cache-purge (cacher &optional only-expired)
  "Purge the storage of CACHER.
If ONLY-EXPIRED not nil, purge caches expired only.")

;; implement of caching in memory

(defclass gt-memory-cacher (gt-cacher gt-single) ())

(cl-defmethod gt-cache-get ((cacher gt-memory-cacher) key)
  (with-slots (storage expired) cacher
    (when-let (cache (ignore-errors (gethash key storage)))
      (if (> (time-to-seconds) (cdr cache))
          (remhash key storage)
        (gt-log 'memory-cacher (format "get [%s] from caches (%s)" key (hash-table-count storage)))
        (car cache)))))

(cl-defmethod gt-cache-set ((cacher gt-memory-cacher) key value)
  (with-slots (storage) cacher
    (unless storage
      (setf storage (make-hash-table :test #'equal)))
    (if (null value)
        (remhash key storage)
      (let ((existp (gethash key storage))
            (etime (+ (time-to-seconds) gt-cache-alive-time)))
        (puthash key (cons value etime) storage)
        (gt-cache-purge cacher t)
        (gt-log 'memory-cacher
          (format "%s [%s] to caches (%s)" (if existp "update" "add") key (hash-table-count storage)))))))

(cl-defmethod gt-cache-purge ((cacher gt-memory-cacher) &optional only-expired)
  (with-slots (storage) cacher
    (when storage
      (if only-expired
          (maphash (lambda (key value)
                     (when (> (time-to-seconds) (cdr value))
                       (remhash key storage)))
                   storage)
        (clrhash storage)))))

;; caching task

(cl-defmethod gt-valid ((cacher gt-cacher) (task gt-task) pred text)
  (with-slots (src tgt) task
    (or (and (null pred) (not (slot-boundp cacher 'if)))
        (let ((pred (or pred (oref cacher if))))
          (or (and (functionp pred) (funcall pred task))
              (gt-valid-literally pred text src tgt))))))

(cl-defmethod gt-cache-key ((task gt-task) &optional n)
  "Generate caching key for the Nth text in TASK."
  (with-slots (text src tgt engine parse) task
    (sha1 (format "%s:%s:%s:%s:%s:%s"
                  (nth (or n 0) (ensure-list text)) src tgt
                  (gt-desc engine)
                  (gt-desc (oref engine parse))
                  (or (oref engine salt) "1")))))

(cl-defmethod gt-cache-get (cacher (task gt-task))
  (let ((results (cl-loop
                  for i from 1 to (length (ensure-list (oref task text)))
                  collect (gt-cache-get cacher (gt-cache-key task (1- i))))))
    (when (cl-some #'identity results)
      (gt-log 'render (format "%s: result from cache!" (oref task id))))
    results))

(cl-defmethod gt-cache-set (cacher (task gt-task) &optional pred)
  (with-slots (text src tgt res) task
    (cl-loop for c in text
             for r in res
             for i from 0
             if (gt-valid cacher task pred c)
             do (gt-cache-set cacher (gt-cache-key task i) r))))

(defun gt-purge-cache (cacher)
  "Purge storage of CACHER."
  (interactive (list gt-default-cacher))
  (when (y-or-n-p (format "Purge all caches from %s now?" (gt-desc cacher)))
    (gt-cacher-barking)
    (gt-cache-purge (or cacher gt-default-cacher))
    (message "Cache purged.")))


;;; Http Client

(require 'gt-httpx)

(defvar plz-curl-program)

(defvar gt-default-http-client
  (if (and (require 'plz nil t) (executable-find plz-curl-program))
      (gt-plz-http-client)
    (gt-url-http-client)))

(cl-defmethod gt-request (&rest args &key url method headers data filter done fail sync retry cache)
  "Simple wrapper for http client, and add cache support."
  (ignore method headers filter sync retry)
  (let ((client (gt-ensure-plain gt-default-http-client (url-host (url-generic-parse-url url)))))
    (if (and client (eieio-object-p client) (object-of-class-p client 'gt-http-client))
        (let* ((ckey (sha1 (format "%s%s" url data)))
               (donefn (when done
                         (lambda (raw)
                           ;; try to cache the result
                           (when cache
                             (let ((gt-cache-alive-time (if (numberp cache) cache gt-cache-alive-time)))
                               (gt-cache-set gt-default-cacher ckey raw)))
                           (funcall done raw)))))
          ;; try to get from cache first
          (if-let (r (and cache (gt-cache-get gt-default-cacher ckey)))
              (progn (gt-log 'cacher (format "Find %s in cache..." ckey))
                     (if donefn (funcall donefn r) r))
            (cl-remf args :cache)
            (apply #'gt-request client `(:done ,donefn ,@args))))
      (let ((errmsg "Make sure `gt-default-http-client' is available. eg:\n\n(setq gt-default-http-client (gt-url-http-client))\n\n\n"))
        (if fail (funcall fail errmsg) (user-error errmsg))))))


;;; Taker

(defcustom gt-langs nil
  "Translation languages or targets.

This is a list which element is a language string or symbol described in
ISO 939-1. The taker will use this to determine the translate source and target.

For example, if you want to translate between a and b, just set like:

  (setq gt-langs `(a b)) or (setq gt-langs `(\"a\" \"b\"))

If your working languages is more than two, just set like:

  (setq gt-langs `(a b c d ..))

The taker will choose the most possible translate source and target from
the list, also you can switch between them in the following translation.

Notice, the value can be overrided by `langs' slot in taker of translator. So
config it for specific translator using slot is effectively."
  :type '(repeat (choice string symbol))
  :group 'go-translate)

(defcustom gt-polyglot-p nil
  "Toggle polyglot for translation.

If this is t, the text will be translated into multiple targets.

For example, assuming `gt-langs' or slot of `langs' is (a b c), the text
may be translated from a to a and c if this is t, but translated from
a to b or a to c if this is nil."
  :type 'boolean
  :group 'go-translate)

(defvar gt-taker-text-things '(word paragraph sentence buffer line page list sexp defun symbol))

(defvar gt-taker-pick-things '(word paragraph sentence line list sexp defun page symbol))

(defcustom gt-taker-text 'word
  "The initial text taken by taker of translator.

If the region is actived, take the selection text. Otherwise, if this is a
symbol, then try to use `thing-at-point' to take the text at point. If this
is a function then invoke it and use its result as the initial text. Specially
if this is t, take the text interactively.

The default value is to take current active region or current word at point.

Notice, the value can be overrided by `text' slot in taker of translator. So
config it for specific translator using slot is effectively."
  :type `(choice function
                 (choice ,@(mapcar (lambda (item) `(const ,item)) gt-taker-text-things)))
  :group 'go-translate)

(defcustom gt-taker-pick 'paragraph
  "Filter the initial text and pick some for translation.

If this is nil, do not pick and use the initial text as source text to
translate directly.

If this is a symbol, then pick elements by thing from initial text for
translation, thing is just like those in `thing-at-point'.

If this is a function then pass the initial text into it and use its result
as source text to translate.

Specially if this is t, pick the text interactively.

The default value is paragraph, that is, try to split initial text into
paragraphs and do a multiple parts translation.

Notice, the value can be overrided by `pick' slot in taker of translator. So
config it for specific translator using slot is effectively."
  :type `(choice function
                 (choice ,@(mapcar (lambda (item) `(const ,item)) gt-taker-pick-things)))
  :group 'go-translate)

(defcustom gt-taker-prompt nil
  "Whether or how to prompt for initial text and target.

If you want to take the translation text and target interactively, set this to
a non nil value.

If this is t, then read and confirm using minibuffer. If this is `buffer' then
open a new buffer instead to do the stuff.

Notice, the value can be overrided by `prompt' slot in taker of translator. So
config it for specific translator using slot is effectively."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "With-Minibuffer" t)
                 (const :tag "With-Buffer" buffer)
                 symbol)
  :group 'go-translate)

(defcustom gt-lang-rules
  (list (cons 'ja "[\u3040-\u30FF]")
        (cons 'ko "[\uAC00-\uD7A3]")
        (cons 'ru "[\u0410-\u044F]") ; if matched then text is ru, otherwise it's not
        (cons 'zh (lambda () (re-search-forward "[\u4e00-\u9fa5]" nil t))))
  "Language match rules used to check the language of a given text.

This is alist, key is the language and value is a regexp string or function.

This is used by `gt-available-langs' to determine who are source language from
target list by matching this. Text will match against the rules one by one,
if matched, the language in rule serves as the source language, otherwise the
language is excluded from the source candidates.

This is not perfect. It's not an easy thing to check the language precisely.
Maybe use the third party, for example python's langid, to check language is a
good idea.

Please add rule for your own languages if possible, or fixup the mistakes in the
default rules to fit your need."
  :type '(alist :key-type (symbol :tag "Lang")
                :value-type (choice (function :tag "Function Rule")
                                    (regexp :tag "Regexp rule")))
  :group 'go-translate)

(defvar gt-skip-lang-rules-p nil)

(defvar gt-ignore-target-history-p nil)

(defvar gt-last-target nil)

(defvar gt-target-history nil)

(cl-defgeneric gt-langs-maybe (text langs)
  "Which languages this TEXT maybe belongs to, pick from LANGS."
  (let* ((srcs langs)
         (hit (catch 'gt-langs
                (with-temp-buffer
                  ;; insert text
                  (insert text)
                  ;; clear punctuations
                  (goto-char (point-min))
                  (while (re-search-forward "\\s.\\|\n" nil t) (replace-match ""))
                  ;; apply rules
                  (cl-loop with rules = (cl-loop for r in gt-lang-rules if (member (car r) langs) collect r)
                           for (l . m) in rules
                           do (goto-char (point-min))
                           do (if (if (functionp m) (funcall m) (re-search-forward m nil t))
                                  (throw 'gt-langs l) ; hit the one, return directly
                                (setq srcs (remove l srcs))) ; not the one, exclude it
                           finally (return nil))))))
    (if hit (ensure-list hit) srcs)))

(cl-defgeneric gt-thing-at-point (thing _mode)
  "Retrieve text at point or from the selected region.

If there is an active region, return its bounds. Otherwise return
the bounds of the thing at point as a list of cons cells.

THING should be an element of `gt-taker-text-things', it is used as the type
of thing to take. If it is nil, it uses the value of `gt-taker-text'.
If it is t, prompt the user to choose one from the list.

This is a generic method, so you can extend the THING or override it for
specific MODE."
  (:method :around (thing mode)
           (if (use-region-p)
               (region-bounds)
             (when thing
               (cl-call-next-method thing mode))))
  (unless (member thing gt-taker-text-things)
    (setq thing (intern (completing-read "Take _ at point as source text: " gt-taker-text-things nil t nil 'gt-text-hist))))
  (gt-log 'taker (format "take text by: %s" thing))
  (if (eq thing 'buffer)
      (let ((beg (save-excursion
                   (goto-char (point-min))
                   (skip-chars-forward " \t\n")
                   (beginning-of-line)
                   (point)))
            (end (save-excursion
                   (goto-char (point-max))
                   (skip-chars-backward " \t\n")
                   (point))))
        (when (> end beg)
          (list (cons beg end))))
    (when-let (rt (bounds-of-thing-at-point thing))
      (list rt))))

(cl-defgeneric gt-forward-thing (thing _mode)
  "Get bound of THING after point and goto the end of THING.

THING is is the ones in `gt-taker-pick-things'.

This is a generic method, so you can override it for specific MODE."
  (let ((ori (point)) pt end)
    (pcase thing
      ((or 'word 'symbol) (skip-syntax-forward "^w"))
      (_ (skip-chars-forward " \r\n\t\f")
         (when (string-match-p "[\n\r]" (buffer-substring ori (point)))
           (skip-chars-backward " \t\f"))))
    (setq pt (point))
    (forward-thing thing)
    (setq end (if (memq thing '(paragraph))
                  (point)
                (save-excursion (skip-chars-backward " \r\n\t\f") (point))))
    (when (> end pt) (cons pt end))))

(defun gt-available-langs (langs text)
  "Find available translation languages for given TEXT.

LANGS is a list of languages to choose from, TEXT is used to determine
the available ones according rules in `gt-taker-lang-match-alist'.

Return a list of language pairs (src . rest) where src is the source
language and rest is the target languages. Rest should be a list contains
one or more languages."
  (if (cdr langs)
      (setq langs (mapcar #'intern-soft langs))
    (user-error "At least two languages should be configed, current: %s" langs))
  (let* ((str (string-join (or (ensure-list text) "")))
         (srcs (or (if gt-skip-lang-rules-p langs (gt-langs-maybe str langs))
                   (user-error "Maybe no language match current translation")))
         (pairs (if gt-polyglot-p
                    (cl-loop for lang in srcs
                             collect (cons lang (remove lang langs)))
                  (cl-loop for lang in srcs
                           append (cl-loop for tgt in (remove lang langs)
                                           collect (list lang tgt))))))
    (if gt-ignore-target-history-p
        pairs
      (let* ((head (cl-delete-duplicates (cl-remove-if-not (lambda (l) (member l pairs)) gt-target-history)))
             (tail (cl-remove-if (lambda (l) (member l head)) pairs)))
        (append head tail)))))

(defun gt-pick-items-by-thing (text-or-bounds &optional thing pred)
  "Pick elements by THING from TEXT-OR-BOUNDS.

THING is the one in `gt-taker-pick-things', PRED is a function with one
argument that used to filter the pick element.

TEXT-OR-BOUNDS is a string or bound cell in the buffer. If it is a string,
should return a list of strings representing the pieces picked from the text.
If it is a bound, return a list of sub-bounds."
  (if (null thing)
      text-or-bounds
    (unless (memq thing gt-taker-pick-things)
      (setq thing (intern (completing-read "Pick _ for source text: " gt-taker-pick-things nil t nil 'gt-pick-hist))))
    (cl-flet ((ps (beg end mode)
                (save-excursion
                  (save-restriction
                    (narrow-to-region beg end)
                    (goto-char (point-min))
                    (let (bds)
                      (while (not (save-excursion (skip-chars-forward " \r\n\t\f") (eobp)))
                        (when-let (bd (gt-forward-thing thing mode))
                          (cl-destructuring-bind (a . b) bd
                            (when (or (null pred) (funcall pred (buffer-substring a b)))
                              (push (cons a b) bds)))))
                      (nreverse bds))))))
      (gt-log 'taker (format "pick from text by: %s" thing))
      (let ((mode major-mode))
        (cond ((stringp text-or-bounds)
               (with-temp-buffer
                 (insert text-or-bounds)
                 (mapcar (lambda (p) (buffer-substring (car p) (cdr p)))
                         (ps (point-min) (point-max) mode))))
              ((consp text-or-bounds)
               (let ((bd text-or-bounds)) ; (a . b) or (buffer (a . b))
                 (with-current-buffer (if (bufferp (car bd)) (pop bd) (current-buffer))
                   (if (consp (car bd)) (setq bd (car bd)))
                   (ps (car bd) (cdr bd) mode))))
              (t (user-error "Pick items error, maybe invalid input")))))))

(cl-defgeneric gt-take (taker translator)
  "Take source text and targets for TRANSLATOR.

This is core method of TAKER. It combines `gt-text', `gt-target', `gt-prompt'
and `gt-pick' together, saves text/bounds/target to be used in later translation
into the translator instance at last.

See type `gt-taker' for more description."
  (:method :before ((taker gt-taker) translator)
           (gt-log-funcall "take (%s %s)" taker translator))
  (:method ((taker gt-taker) translator)
           (with-slots (text bounds target keep) translator
             (let ((prompt (and (null text) (null target)
                                (if (slot-boundp taker 'prompt)
                                    (oref taker prompt)
                                  gt-taker-prompt)))
                   (nopick (or (and text keep)
                               (gt-functionp (if (slot-boundp taker 'text)
                                                 (oref taker text)
                                               gt-taker-text)))))
               ;; 1) text
               (unless text
                 (let ((rs (ensure-list (gt-text taker translator))))
                   ;; bounds: (buffer (a . b) (c . d))
                   (if (stringp (car rs))
                       (setf bounds (list (current-buffer)))
                     (unless (bufferp (car rs))
                       (push (current-buffer) rs))
                     (setf bounds rs))
                   ;; text: ("aaa" "bbb")
                   (setf text (gt-collect-text rs))))
               ;; 2) target
               (unless target
                 (setf target (gt-target taker translator)))
               ;; 3) prompt
               (when prompt (gt-prompt taker translator prompt))
               (unless text (user-error "No source text found at all"))
               ;; 4) pick
               (unless nopick (gt-pick taker translator)))))
  (:method :after ((taker gt-taker) translator)
           (with-slots (text target) translator
             (unless text
               (user-error "Source Text should not be null"))
             (with-slots (then) taker ; if `then' slot exists
               (when (and (slot-boundp taker 'then) then (gt-functionp then))
                 (funcall then translator))))))

(cl-defgeneric gt-text (_taker translator)
  "Used to take initial text for TRANSLATOR.
Return text, text list or bounds in buffer. This is non-destructive."
  (:method ((taker gt-taker) _translator)
           "Return the text selected by the TAKER."
           (let ((text (if (slot-boundp taker 'text)
                           (oref taker text)
                         gt-taker-text)))
             (cond ((symbolp text) (gt-thing-at-point text major-mode))
                   ((gt-functionp text) (funcall text))
                   (t text)))))

(cl-defgeneric gt-target (_taker translator &optional dir)
  "Used to pick target for TRANSLATOR.
Return the target will be used. If DIR is `next or `prev, return the next or
previous one. This is non-destructive."
  (:method ((taker gt-taker) translator &optional dir)
           (when-let* ((langs (ensure-list
                               (if (slot-boundp taker 'langs)
                                   (oref taker langs)
                                 gt-langs)))
                       (tgts (if (car langs)
                                 (gt-available-langs langs (oref translator text))
                               (cdr langs)))
                       (index (let ((n (cl-position gt-last-target tgts :test #'equal)))
                                (if n (+ n (pcase dir ('next 1) ('prev -1) (_ 0))) 0)))
                       (target (nth (if (>= index (length tgts))
                                        0
                                      (if (< index 0) (- (length tgts) 1) index))
                                    tgts)))
             (setq gt-last-target target))))

(defvar gt-prompt-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-g" #'top-level)
    (define-key map "\C-n" #'gt-prompt-next-target)
    (define-key map "\C-p" (lambda () (interactive) (gt-prompt-next-target t)))
    (define-key map "\C-l" #'delete-minibuffer-contents)
    map)
  "The keymap used when taker prompt with minibuffer.")

(defvar gt-prompt-target nil)

(defvar gt-prompt-overlay nil)

(defun gt-prompt-prefix (target)
  (when target
    (format "[%s%s] "
            (if-let (src (car target)) (format "%s > " src) "")
            (mapconcat (lambda (s) (format "%s" s)) (ensure-list (cdr target)) ", "))))

(defun gt-prompt-next-target (&optional backwardp)
  "Switch to next target in prompt minibuffer.
If BACKWARDP is not nil then switch to previous one."
  (interactive)
  (let ((gt-skip-lang-rules-p (or current-prefix-arg (string-blank-p (minibuffer-contents)))))
    (setq gt-prompt-target
          (gt-target (oref gt-current-translator taker)
                     (make-instance (eieio-object-class gt-current-translator)
                                    :text (list (minibuffer-contents)))
                     (if backwardp 'prev 'next)))
    (overlay-put gt-prompt-overlay 'before-string (gt-prompt-prefix gt-prompt-target))))

(cl-defgeneric gt-prompt (_taker translator _type)
  "Prompt user with inital text and target, then update TRANSLATOR with new values."
  (:method :before ((_taker gt-taker) translator _type)
           (when (cdr (oref translator text)) ; for solo part only
             (user-error "Multiple part text cannot be prompted")))
  (:method ((_ gt-taker) translator _type)
           (with-slots (text bounds target) translator
             (let* ((minibuffer-allow-text-properties t)
                    (newtext (minibuffer-with-setup-hook
                                 (lambda ()
                                   (setq gt-prompt-target target)
                                   (setq gt-prompt-overlay (make-overlay 1 2))
                                   (overlay-put gt-prompt-overlay 'before-string (gt-prompt-prefix gt-prompt-target)))
                               (read-from-minibuffer "Text: " (car text) gt-prompt-map))))
               (when (string-blank-p newtext)
                 (user-error "Text should not be null"))
               (unless (equal newtext (car text))
                 (setf bounds (list (car bounds))))
               (setf text (ensure-list newtext))
               (setf target gt-prompt-target)))))

(cl-defgeneric gt-pick (_taker translator)
  "Update TRANSLATOR with pieces picked from initial text or bounds."
  (:method :around ((taker gt-taker) translator)
           (with-slots (text bounds) translator
             (unless (bufferp (car bounds)) ; ensure buffer is recorded
               (push (current-buffer) bounds))
             (when (and (not (cdr text)) ; for solo part only
                        (if (slot-boundp taker 'pick) (oref taker pick) gt-taker-pick)) ; pick is non-nil
               (cl-call-next-method taker translator))))
  (:method ((taker gt-taker) translator)
           (with-slots (text bounds) translator
             (if-let* ((buf (car bounds))
                       (car (if (cdr bounds) (cl-subseq bounds 0 2) ; buffer bound
                              (or (car text) ; text
                                  (user-error "Where should I pick from?"))))
                       (pick (if (slot-boundp taker 'pick) (oref taker pick) gt-taker-pick))
                       (res (ensure-list
                             (cond
                              ((gt-functionp pick) (funcall pick car))
                              ((memq pick gt-taker-pick-things)
                               (gt-pick-items-by-thing car pick (oref taker pick-pred)))
                              ((symbolp pick) (gt-pick pick translator))))))
                 ;; string pieces vs bound pieces
                 (if (stringp (car res))
                     (setf text res)
                   (unless (bufferp (car res)) (push buf res))
                   (setf bounds res text (gt-collect-text res)))
               (user-error "Nothing picked, empty request")))))


;;; Engine

(defcustom gt-cache-p t
  "Whether enable the cache."
  :type 'boolean
  :group 'go-translate)

(defconst gt-text-delimiter "314141592926666")

(defun gt-current-cacher (engine task)
  "Return the cacher used by ENGINE for current TASK."
  (let (cacher condition)
    (when gt-cache-p
      (let ((cc (gt-ensure-plain
                 (if (slot-boundp engine 'cache) (if-let (c (gt-ensure-plain (oref engine cache) task)) c) t)
                 task)))
        (cond ((null cc))
              ((eq cc t) (setq cacher gt-default-cacher condition nil))
              ((symbolp cc) (setq cacher gt-default-cacher condition cc))
              (t (setq cacher cc condition nil)))))
    (cl-values cacher condition)))

(cl-defmethod initialize-instance :after ((engine gt-engine) &rest _)
  (unless (slot-boundp engine 'tag)
    (oset engine tag (gt-desc engine))))

(cl-defmethod gt-init ((engine gt-engine) task)
  "Preprocessing text in TASK for ENGINE. Try cache first if possible."
  (with-slots (id text cache) task
    (gt-log-funcall "init (%s %s)" engine id)
    ;; if stream, not cache
    (if (gt-stream-p engine) (oset engine cache nil))
    ;; query cache first
    (when-let (cacher (car-safe (gt-current-cacher engine task)))
      (setf cache (gt-cache-get cacher task)))
    ;; only translate the parts not in cache
    (if (and cache (cl-every #'identity cache))
        (gt-next engine task)
      (let ((text-left (if cache (cl-loop for c in text for r in cache unless r collect c) text)))
        ;; join with delimiter if possible
        (if-let (delimiter (if (slot-boundp engine 'delimiter) (oref engine delimiter) gt-text-delimiter))
            (setf text (string-join text-left (concat "\n\n" delimiter "\n\n")))
          (setf text text-left))))))

(cl-defgeneric gt-translate (engine task &optional next)
  "Translate or transform text in TASK and parse the results using ENGINE.

The raw response will save into `res' slot of translator. Then NEXT runs, that
will do the parse and render job. See type `gt-engine' for more description."
  (:method :around ((engine gt-engine) task &optional _next)
           (gt-log-funcall "translate (%s %s)" engine (oref task id))
           (gt-init engine task)
           (with-slots (id text err res render translator process) task
             (unless (or res err)
               (gt-log 'next (format "%s: %s prepare to translate" id (gt-desc engine)))
               (let ((ret (cl-call-next-method
                           engine task
                           (lambda (task)
                             (if (equal (oref translator version) (oref task version))
                                 (condition-case err1
                                     (if (gt-stream-p engine)
                                         (if gt-http-client-stream-abort-flag
                                             (gt-log 'next (format "%s: %s translate streaming abort!" id (gt-desc engine)))
                                           (gt-next-for-stream render task))
                                       (gt-log 'next (format "%s: %s translate success!" id (gt-desc engine)))
                                       (gt-next engine task))
                                   (gt-fail task err1))
                               (gt-log 'next (format "%s: ----- expired -----" id)))))))
                 (if (processp ret) (setf process ret))))))
  (:method ((engine gt-engine) _task _next)
           (user-error "Method `gt-translate' of %s is not implement" (gt-desc engine))))

(cl-defgeneric gt-parse (_parser task)
  "Parse results of TASK to user-friendly string."
  (:method :before ((parser gt-parser) task)
           (gt-log-funcall "parse (%s %s)" parser (oref task id)))
  (:method ((_ gt-parser) _task) nil))

(cl-defgeneric gt-stream-p (_engine)
  "Whether streaming query is on for ENGINE."
  nil)

(cl-defgeneric gt-next (_engine task)
  "Chain to the parse and render phase for TASK."
  (:method ((engine gt-engine) task)
           (gt-log-funcall "next (%s %s)" engine (oref task id))
           (with-slots (text res meta cache translator) task
             (with-slots (parse delimiter then) engine
               ;; parse
               (when res
                 (when-let (parser (and (slot-boundp engine 'parse) parse))
                   (if (gt-functionp parser)
                       (funcall parser task)
                     (gt-parse parser task)))
                 ;; split with delimiter if possible
                 (when-let (delimiter (and (stringp res) (if (slot-boundp engine 'delimiter) delimiter gt-text-delimiter)))
                   (setf res (mapcar (lambda (item) (string-trim item "\n+")) (split-string res delimiter))))
                 (setf res (ensure-list res))
                 ;; run res-hook if possible
                 (when-let (hook (plist-get meta :res-hook))
                   (setf res (funcall hook res))))
               ;; resume text and merge cache to res
               (setf text (oref translator text))
               (if cache (setf res (cl-loop for c in cache if (null c) collect (pop res) else collect c)))
               ;; verify
               (unless res
                 (user-error "No translate result found"))
               (unless (= (length (remove nil res)) (length text))
                 (user-error "Source text and result text have no same length"))
               ;; set cache if possible
               (cl-destructuring-bind (cacher condition) (gt-current-cacher engine task)
                 (when cacher (gt-cache-set cacher task condition)))
               ;; invoke then when it is exists
               (when (and (slot-boundp engine 'then) then (gt-functionp then))
                 (funcall then task))
               ;; all right, render
               (gt-update-state translator)
               (gt-output (oref translator render) translator)))))

(cl-defgeneric gt-next-for-stream (_render task)
  "Output result in TASK to specific marker position directly.
Notice, the result should propertized with `gt-result' to avoid issue.
This is used for streaming output for _RENDER."
  (:method :around ((render gt-render) task)
           (with-slots (markers) task
             (if (null markers)
                 (progn
                   (setq gt-http-client-stream-abort-flag t)
                   (message "Stream output is unavailable for current task."))
               (setf markers (ensure-list markers))
               (cl-call-next-method render task))))
  (:method ((_render gt-render) task)
           (with-slots (markers res) task
             (save-excursion
               (gt-insert-text-at-marker
                (propertize (string-join (ensure-list res) "\n") 'gt-result 'stream)
                (car markers) (cdr markers) t)))))


;;; Text to Speech

(defcustom gt-tts-speaker (cl-find-if #'executable-find (list "mpv" "mplayer" "cvlc"))
  "The program used to speak the translation result.
It also can be a command with options like `mpv --af=xxx'.
The value should not contain any space in the command path."
  :type 'string
  :group 'go-translate)

(defvar gt-tts-langs nil)

(defvar gt-tts-last-engine nil)

(defvar gt-tts-speak-process nil)

(defun gt-interrupt-speak-process ()
  (when (and gt-tts-speak-process (process-live-p gt-tts-speak-process))
    (ignore-errors (kill-process gt-tts-speak-process))
    (setq gt-tts-speak-process nil)))

(cl-defmethod gt-play-audio (data &optional wait)
  "Play DATA with `gt-tts-playing-process'.
DATA is raw audio data or audio url, or a buffer contains the raw data.
If WAIT is not nil, play after current process finished."
  (unless (and gt-tts-speaker (executable-find (car (split-string gt-tts-speaker))))
    (user-error "You should install `mpv' first or config `gt-tts-speaker' correctly"))
  (when wait
    (condition-case _
        (while (and gt-tts-speak-process (process-live-p gt-tts-speak-process))
          (sleep-for 0.5))
      (quit (gt-interrupt-speak-process) (user-error ""))))
  (gt-interrupt-speak-process)
  (cl-flet ((speak ()
              (let ((proc (make-process
                           :name (format "gt-tts-process-%s" (+ 1000 (random 1000)))
                           :command (append (split-string-shell-command gt-tts-speaker) (list "-"))
                           :buffer nil
                           :noquery t
                           :sentinel (lambda (_ s) (if (string-match-p "finished" s) (message "")))
                           :connection-type 'pipe)))
                (message "Speaking...")
                (setq gt-tts-speak-process proc)
                (process-send-region proc (point-min) (point-max))
                (if (process-live-p proc) (process-send-eof proc)))))
    (if (bufferp data)
        (with-current-buffer data (speak))
      (with-temp-buffer
        (let ((buf (current-buffer)) state)
          (if (string-prefix-p "http" data)
              (progn (gt-request :url data
                                 :cache 10
                                 :done (lambda (raw)
                                         (setq state 'done)
                                         (with-current-buffer buf (insert raw)))
                                 :fail (lambda (err) (setq state err)))
                     (while (null state)
                       (accept-process-output nil 0.5)))
            (insert data))
          (if (and state (not (eq state 'done))) (signal 'user-error state))
          (speak))))))

(cl-defgeneric gt-speak (engine text &optional lang)
  "Speak TEXT with LANG by ENGINE."
  (:method ((_ (eql 'local)) text &optional _lang)
           "TEXT to speech with local program."
           (cond ((and (eq system-type 'darwin) (executable-find "say"))
                  (with-temp-buffer
                    (insert text)
                    (shell-command-on-region (point-min) (point-max) "say" t)))
                 ((and (memq system-type '(cygwin windows-nt)) (executable-find "powershell"))
                  (let ((cmd (format "$w = New-Object -ComObject SAPI.SpVoice; $w.speak(\\\"%s\\\")" text)))
                    (shell-command (format "powershell -Command \"& {%s}\""
                                           (encode-coding-string
                                            (replace-regexp-in-string "\n" " " cmd)
                                            (keyboard-coding-system))))))
                 (t (user-error "No suitable local TTS service found"))))
  (:method ((_ (eql 'interact)) text &optional noprompt)
           "Text to speech with local program or using selected engine."
           (let* ((prompt "Text to Speech: ")
                  (regexp (format "^ *\\(%s\\)\\." (mapconcat #'symbol-name (mapcar #'car gt-lang-codes) "\\|")))
                  (engines `(locally ,@(cl-loop
                                        for m in (cl--generic-method-table (cl--generic #'gt-speak))
                                        for n = (car (cl--generic-method-specializers m))
                                        unless (or (consp n) (memq n '(t gt-engine))) collect n)))
                  (overlay nil)
                  (text (if noprompt text
                          (minibuffer-with-setup-hook
                              (lambda ()
                                (use-local-map (make-composed-keymap nil (current-local-map)))
                                (setq overlay (make-overlay (1- (length prompt)) (length prompt)))
                                (unless gt-tts-last-engine (setq gt-tts-last-engine 'locally))
                                (overlay-put overlay 'display (format " (%s):" gt-tts-last-engine))
                                (local-set-key (kbd "C-n")
                                               (lambda ()
                                                 (interactive)
                                                 (setq gt-tts-last-engine
                                                       (let ((i (cl-position gt-tts-last-engine engines)))
                                                         (nth (if (>= (+ i 1) (length engines)) 0 (1+ i)) engines)))
                                                 (overlay-put overlay 'display (format " (%s):" gt-tts-last-engine)))))
                            (read-string prompt (if (consp text) (buffer-substring (caar text) (cdar text)) text)))))
                  (lang (if (string-match regexp text)
                            (prog1 (intern-soft (match-string 1 text))
                              (setq text (substring text (match-end 0))))
                          (car (gt-langs-maybe text (or gt-tts-langs (mapcar #'car gt-lang-codes)))))))
             (when (= (length (string-trim text)) 0)
               (user-error "Input should not be null"))
             (unless gt-tts-last-engine (setq gt-tts-last-engine 'locally))
             (gt-log 'tts (format "Speak with %s to %s" gt-tts-last-engine lang))
             (if (equal gt-tts-last-engine 'locally)
                 (gt-speak 'local text lang)
               (gt-speak (make-instance gt-tts-last-engine) text lang))))
  (:method :around ((engine gt-engine) text &optional lang)
           (cl-call-next-method engine text (intern-soft lang)))
  (:method ((engine gt-engine) &rest _)
           (user-error "No TTS service found on engine `%s'" (oref engine tag))))

;;;###autoload
(defun gt-do-speak ()
  "Speak content around point.

If the text under point or with selection has `gt-task' or `gt-tts-url'
property, try to speak it with current translation engine.

Otherwise try to TTS with local program or using selected engine.

When TTS with specific engine, you can specify the language with `lang.' prefix."
  (interactive)
  (if-let (url (get-char-property (point) 'gt-tts-url))
      ;; 1. play the url
      (gt-play-audio url)
    (let (items engine)
      (if-let (task (get-char-property (point) 'gt-task))
          (with-slots (src tgt res err translator) task
            (with-slots (text target) translator
              ;; 2. play current task
              (setq engine (oref task engine))
              (unless (cl-find-method #'gt-speak '() `(,(eieio-object-class engine) t t))
                (user-error "No TTS service found on current engine `%s'" (oref engine tag)))
              (let ((part (or (get-char-property (point) 'gt-part) 0))
                    (col (lambda (l c) (push (format "%s. %s" l (substring-no-properties c)) items))))
                (if (use-region-p)
                    (cl-loop with text = (buffer-substring (region-beginning) (region-end))
                             for l in (gt-langs-maybe text (list src tgt)) do (funcall col l text))
                  (when-let (r (nth part text))
                    (funcall col (car target) r))
                  (when-let (r (and (not err) (or (get-pos-property (point) 'gt-brief) (nth part (ensure-list res)))))
                    (funcall col tgt r)))
                (when items
                  (let* ((cand (completing-read
                                (format "Text to Speech (with %s): " (oref engine tag))
                                (gt-make-completion-table items)))
                         (lang (if (string-match (format "^ *\\(%s\\)\\." (mapconcat #'symbol-name (mapcar #'car gt-lang-codes) "\\|")) cand)
                                   (prog1 (intern-soft (match-string 1 cand))
                                     (setq cand (substring cand (match-end 0))))
                                 (car (gt-langs-maybe cand (list src tgt))))))
                    (unless lang (user-error "Guess language of text failed"))
                    (gt-log 'tts (format "Speak with %s to %s" engine lang))
                    (gt-speak engine cand lang))))))
        ;; 3. play with local program or using selected engine
        (gt-speak 'interact (gt-text (gt-taker :text 'word) nil))))))


;;; Render

(cl-defmethod gt-init :around ((render gt-render) translator)
  (gt-log-funcall "init (%s %s)" render translator)
  (message "Processing...")
  (condition-case err
      (progn (cl-call-next-method render translator)
             (gt-update-state translator))
    (error (gt-log 'render (format "%s initialize failed, abort" (gt-desc render)))
           (user-error (format "[output init error] %s" err)))))

(cl-defmethod gt-init ((_render gt-render) _translator)
  "Initialize the RENDER. Only invoke once before output."
  nil)

(cl-defgeneric gt-extract (render translator)
  "Extract TRANSLATOR's responses that to be consumed by RENDER."
  (:method ((render gt-render) translator)
           (with-slots (text tasks) translator
             (cl-loop with tgts = (cl-delete-duplicates (mapcar (lambda (item) (oref item tgt)) tasks))
                      with engines = (cl-delete-duplicates (mapcar (lambda (task) (oref task engine)) tasks))
                      for task in tasks
                      for (tgt res err engine) = (gt-orefs task tgt res err engine)
                      for state = (if err 1 (if res 2 0)) ; 0 loading 1 error 2 result
                      for prefix = (with-slots (prefix) render
                                     (if (and (slot-boundp render 'prefix) (not (eq prefix t)))
                                         (when prefix
                                           ;; custom prefix with :prefix slot
                                           (if (gt-functionp prefix)
                                               (funcall prefix task)
                                             (format "%s" prefix)))
                                       (when (or (cdr tgts) (cdr engines))
                                         ;; style as: en.Google.Detail
                                         (let ((lst (list (if (cdr tgts) tgt)
                                                          (if (cdr engines) (oref engine tag))
                                                          (if (cdr engines) (ignore-errors (oref (oref engine parse) tag))))))
                                           (concat
                                            (mapconcat (lambda (item) (format "%s" item)) (remove nil lst) ".")
                                            (if (and (cdr engines) (gt-stream-p engine)) " (stream)"))))))
                      for result = (pcase state (0 "Loading...") (1 err) (2 res))
                      collect (list :result result :prefix prefix :state state :task task :tgt tgt :stream (gt-stream-p engine)) into lst
                      finally (return ;; sort by target, then by engine
                               (if (cdr tgts)
                                   (cl-loop for tgt in tgts append
                                            (cl-remove-if-not (lambda (tr) (equal tgt (plist-get tr :tgt))) lst))
                                 lst))))))

(cl-defgeneric gt-output (render translator)
  "Output result of TRANSLATOR with RENDER, invoked after every task parsed.
Output to minibuffer by default."
  (:method :around ((render gt-render) translator)
           (with-slots (state) translator
             (gt-log-funcall "output (%s %s) %s" render translator state)
             (when (<= state 4)
               (with-slots (output then) render
                 ;; error handler & functionp case
                 (condition-case err
                     (if (gt-functionp output)
                         (funcall output render translator)
                       (cl-call-next-method render translator)
                       ;; chain next render if possible
                       (when (and (= state 3) then)
                         (cond ((gt-functionp then)
                                (funcall then translator))
                               ((and (eieio-object-p then) (object-of-class-p then 'gt-render))
                                (gt-init then translator)
                                (gt-output then translator))
                               (t (error "Bad then value specified in %s" (gt-desc render))))))
                   (error (setf state 4)
                          (message "[output error] %s" (cadr err))))))))
  (:method ((render gt-render) translator)
           (when (= 3 (oref translator state))
             (cl-loop with ret = (gt-extract render translator)
                      with mpp = (cdr (oref translator text))
                      for tr in ret
                      for (prefix result) = (gt-plist-let tr (list (concat "[" .prefix "]" (if mpp "\n" " ")) .result))
                      collect (concat (if (cdr ret) (propertize prefix 'face 'gt-render-prefix-face))
                                      (if (consp result) (string-join result "\n") result))
                      into lst
                      finally (message "%s" (string-join lst "\n"))))))


;;; Translator

(defun gt-add-task (task)
  "Add new TASK for translation."
  (with-slots (id engine render version translator) task
    (with-slots (tasks state) translator
      (when (= state 0)
        (setf tasks (append tasks (list task)))
        (gt-log (format "%d" (length tasks))
          (format "add task %s: (%s/%s)" id (gt-desc engine) (gt-desc render)))))))

(defun gt-update-state (translator)
  "Update state for TRANSLATOR."
  (with-slots (state total tasks render) translator
    (pcase state
      (0
       (when (= (length tasks) total)
         (gt-log 'translator "<1> all tasks added")
         (setf state 1)))
      (1
       (gt-log 'translator
         (format "<2> %s prepared" (gt-desc render)))
       (setf state 2))
      (2
       (when (= total (length (cl-remove-if-not (lambda (task) (or (oref task err) (oref task res))) tasks)))
         (gt-log 'translator "<3> all result parsed")
         (setf state 3))))))

(defun gt-fail (task error)
  "Render ERROR message and ternimate the TASK."
  (declare (indent 1))
  (gt-log-funcall "fail (%s)" (oref task id))
  (with-slots (translator render err version id) task
    (when (equal version (oref translator version))
      (setf err (if (consp error)
                    (mapconcat (lambda (r)
                                 (format (if (string-suffix-p "error" (format "%s" r)) "[%s]" "%s") r))
                               (cl-remove-if (lambda (r) (memq r '(error user-error))) error)
                               " ")
                  (or error "")))
      (gt-update-state translator)
      (gt-output render translator))
    (gt-log 'next (format "%s: [----- error -----] %s" id error))
    (gt-log nil 1 (with-temp-buffer
                    (let ((standard-output (current-buffer)))
                      (backtrace)
                      (buffer-string))))))

(cl-defgeneric gt-reset (translator)
  "Reset status and other variables before translation for TRANSLATOR."
  (:method ((translator gt-translator))
           (gt-log-funcall "reset (%s)" translator)
           (unless gt-default-cacher
             (setq gt-default-cacher (gt-memory-cacher)))
           (unless gt-default-http-client
             (setq gt-default-http-client (gt-url-http-client)))
           (with-slots (version state tasks bag total taker engines render _taker _engines _render) translator
             (unless (slot-boundp translator '_taker)
               (unless taker (setf taker (gt-taker)))
               (unless render (setf render (gt-render)))
               (setf _taker taker _engines engines _render render))
             (setf version (time-to-seconds) state 0 tasks nil total 0 taker nil engines nil render nil bag nil))))

(cl-defmethod gt-init ((translator gt-translator) &rest _)
  "Initialize the components, text and target for TRANSLATOR."
  (gt-log-funcall "init (%s)" translator)
  (with-slots (keep text bounds target version taker engines render _taker _engines _render) translator
    ;; reset
    (gt-reset translator)
    ;; take
    (unless keep (setf text nil bounds nil target nil))
    (setf taker (or (cl-find-if (lambda (tk) (gt-valid tk translator))
                                (ensure-list (gt-ensure-plain _taker)))
                    (user-error "No taker found in this translator")))
    (gt-take taker translator)
    (setf keep nil)
    (let ((history-delete-duplicates t))
      (add-to-history 'gt-target-history target 8))
    ;; prepare engines and render
    (setf engines (ensure-list (gt-ensure-plain _engines)))
    (setf render (cl-find-if (lambda (rd) (gt-valid rd translator))
                             (ensure-list (gt-ensure-plain _render))))
    ;; log
    (gt-log 'translator (format "version: %s\ntarget: %s\nbounds: %s\ntext: %s\ntaker: %s, engines: %s, render: %s"
                                version target bounds text
                                (gt-desc taker)
                                (mapcar (lambda (en) (oref en tag)) engines)
                                (gt-desc render)))))

(cl-defgeneric gt-start (translator)
  "Start a new translation with TRANSLATOR."
  (gt-log "\n\n")
  (gt-log-funcall "start (%s)" translator)
  (setq gt-current-command this-command)
  (setq gt-current-translator translator)
  ;; init
  (gt-init translator nil)
  (with-slots (text target version total engines render) translator
    ;; tasks
    (cl-loop for engine in engines
             do (cl-loop for tgt in (cdr target)
                         for task = (gt-task
                                     :text text
                                     :src (car target)
                                     :tgt tgt
                                     :engine engine
                                     :render render
                                     :translator translator
                                     :version version)
                         if (gt-valid engine task)
                         do (cl-incf total) and do (gt-add-task task))
             finally (gt-update-state translator))
    (when (zerop total)
      (user-error "No task created, please check engines and langs setup"))
    ;; translate
    (if render (gt-init render translator))
    (cl-loop for task in (oref translator tasks)
             do (condition-case err
                    (gt-translate (oref task engine) task)
                  (error (gt-fail task err))))))

(provide 'gt-core)

;;; gt-core.el ends here
