;;; gt-core.el --- Core implement of the Translator -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>
;; Author: lorniu <lorniu@gmail.com>
;; Package-Requires: ((emacs "28.1") (pdd "0.21"))

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
(require 'eieio)
(require 'text-property-search)
(require 'pdd)

(defgroup go-translate nil
  "Translation framework on Emacs, with high configurability and extensibility."
  :group 'external
  :prefix 'gt-)

(defcustom gt-debug-p nil
  "Whether enable the debug message."
  :type 'boolean
  :group 'go-translate)


;;; Variables and Components

(defconst gt-lang-codes
  '((en  . "English")
    (zh  . "Chinese")
    (ru  . "Russian")
    (fr  . "French")
    (it  . "Italian")
    (es  . "Spanish")
    (de  . "German")
    (ko  . "Korean")
    (ja  . "Japanese")
    (yue . "Cantonese")
    (af  . "Afrikaans")
    (ak  . "Akan")
    (am  . "Amharic")
    (ar  . "Arabic")
    (as  . "Assamese")
    (asa . "Asu")
    (az  . "Azerbaijani")
    (be  . "Belarusian")
    (bem . "Bemba")
    (bez . "Bena")
    (bg  . "Bulgarian")
    (bm  . "Bambara")
    (bn  . "Bengali")
    (bo  . "Tibetan")
    (bs  . "Bosnian")
    (ca  . "Catalan")
    (cgg . "Chiga")
    (chr . "Cherokee")
    (cs  . "Czech")
    (da  . "Danish")
    (dav . "Taita")
    (ebu . "Embu")
    (ee  . "Ewe")
    (el  . "Greek")
    (eo  . "Esperanto")
    (et  . "Estonian")
    (eu  . "Basque")
    (fa  . "Persian")
    (ff  . "Fulah")
    (fi  . "Finnish")
    (fil . "Filipino")
    (fo  . "Faroese")
    (ga  . "Irish")
    (gl  . "Galician")
    (gsw . "Swiss German")
    (gu  . "Gujarati")
    (guz . "Gusii")
    (gv  . "Manx")
    (ha  . "Hausa")
    (haw . "Hawaiian")
    (he  . "Hebrew")
    (hi  . "Hindi")
    (hr  . "Croatian")
    (hu  . "Hungarian")
    (hy  . "Armenian")
    (id  . "Indonesian")
    (ig  . "Igbo")
    (ii  . "Sichuan Yi")
    (is  . "Icelandic")
    (jmc . "Machame")
    (ka  . "Georgian")
    (kab . "Kabyle")
    (kam . "Kamba")
    (kde . "Makonde")
    (kea . "Kabuverdianu")
    (khq . "Koyra Chiini")
    (ki  . "Kikuyu")
    (kk  . "Kazakh")
    (kl  . "Kalaallisut")
    (kln . "Kalenjin")
    (km  . "Khmer")
    (kn  . "Kannada")
    (kok . "Konkani")
    (kw  . "Cornish")
    (lag . "Langi")
    (lg  . "Ganda")
    (lt  . "Lithuanian")
    (luo . "Luo")
    (luy . "Luyia")
    (lv  . "Latvian")
    (mas . "Masai")
    (mer . "Meru")
    (mfe . "Morisyen")
    (mg  . "Malagasy")
    (mk  . "Macedonian")
    (ml  . "Malayalam")
    (mr  . "Marathi")
    (ms  . "Malay")
    (mt  . "Maltese")
    (my  . "Burmese")
    (naq . "Nama")
    (nd  . "North Ndebele")
    (ne  . "Nepali")
    (nl  . "Dutch")
    (nn  . "Norwegian Nynorsk")
    (nyn . "Nyankole")
    (om  . "Oromo")
    (or  . "Oriya")
    (pa  . "Punjabi")
    (pl  . "Polish")
    (ps  . "Pashto")
    (pt  . "Portuguese")
    (rm  . "Romansh")
    (ro  . "Romanian")
    (rof . "Rombo")
    (rw  . "Kinyarwanda")
    (rwk . "Rwa")
    (saq . "Samburu")
    (seh . "Sena")
    (ses . "Koyraboro Senni")
    (sg  . "Sango")
    (shi . "Tachelhit")
    (si  . "Sinhala")
    (sk  . "Slovak")
    (sl  . "Slovenian")
    (sn  . "Shona")
    (so  . "Somali")
    (sq  . "Albanian")
    (sr  . "Serbian")
    (sv  . "Swedish")
    (sw  . "Swahili")
    (ta  . "Tamil")
    (te  . "Telugu")
    (teo . "Teso")
    (th  . "Thai")
    (ti  . "Tigrinya")
    (to  . "Tonga")
    (tr  . "Turkish")
    (tzm . "Central Morocco Tamazight")
    (uk  . "Ukrainian")
    (ur  . "Urdu")
    (uz  . "Uzbek")
    (vi  . "Vietnamese")
    (vun . "Vunjo")
    (xog . "Soga")
    (cy  . "Welsh")
    (yo  . "Yoruba")
    (zu  . "Zulu")))

(defconst gt-word-classes
  '(pron adj adv adt art aux conj prep det abbr int vt vi v n a))

(defvar gt-current-command nil)

(defvar gt-current-translator nil)

(defvar-local gt-tracking-marker nil)

(defclass gt-single ()
  ((insts :allocation :class :initform nil))
  :abstract t
  :documentation "Only create one instance for same slots.")

(cl-defmethod make-instance ((class (subclass gt-single)) &rest slots)
  (if-let* ((key (sha1 (prin1-to-string slots)))
            (insts (oref-default class insts))
            (old (cdr-safe (assoc key insts))))
      old
    (let ((inst (cl-call-next-method)))
      (prog1 inst
        (oset-default class insts `((,key . ,inst) ,@insts))))))

(defclass gt-valid-trait ()
  ((if :initarg :if
     :type (or function symbol list)
     :documentation "See `gt-validate' for details."))
  :abstract t
  :documentation "Add :if to detect whether component is available")

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
    :type (or string list vector function)
    :documentation "Translation result.")
   (err
    :initform nil
    :documentation "Translation error.")
   (markers
    :initform nil
    :documentation "Markers tracking output bounds.")
   (proc
    :initform nil
    :documentation "A pdd-task represents the backend process.")

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
    :documentation "Renderer of current task.")
   (translator
    :initarg :translator
    :initform nil
    :documentation "Translator of current task."))
  :documentation
  "An object contains basic translation informations.
Its life cycle is in one translation. It is created by translator, then derived
to engine for result, at last extracted by renderer for output.")

(defclass gt-taker (gt-single gt-valid-trait)
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
source text list to remove unnecessary elements, or modify the translate
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

(defclass gt-parser (gt-single gt-valid-trait)
  ((tag
    :initarg :tag
    :initform nil
    :type (or symbol string)
    :documentation "Display label for this parser."))
  :documentation
  "It is a component of `gt-engine', used to parse the response of the engine
to a user-friendly string. One engine can have different parsers, but can only
use one of them at a time.")

(defclass gt-engine (gt-single gt-valid-trait)
  ((tag
    :initarg :tag
    :type (or string symbol)
    :documentation "Display label for this engine.")
   (parse
    :initarg :parse
    :initform (make-instance 'gt-parser)
    :type (or function gt-parser)
    :documentation "Used to generate user-friendly result from response.")
   (delimit
    :initarg :delimit
    :initform nil
    :type (or null t string)
    :documentation "The delimiter for multiple paragraphs or parts translation.")
   (stream
    :initarg :stream
    :initform nil
    :type boolean
    :documentation "Enable stream support for this engine.
This effect both engine and renderer. If this is t, then retrieve results
part by part. Renderer supporting stream output should refresh the display
every time new data arrived.")
   (then
    :initarg :then
    :type function
    :documentation "Function run after result are response and parsed.
Take current task as argument. you can do extra works for current task
before rendering."))
  :documentation "Translate engine, retrieve the result and parse it.

If `delimit' is non nil, the translation for multiple paragraphs or parts will
take a strategy as `join-translate-split'. That is, join the source text list to
a single string separated by `delimit', translate the whole string, and then
split the result by `delimit' into multiple parts. This assumes that the
`delimit' in string is kept by engine in the translation progress. This is
sometimes a more efficient way to get a lot of results. In this case, a string
list will be passed to the engine, and a translated string list is expected.

Also in some cases, you should turn the cache off by setting `no-cache' to t."
  :abstract t)

(defclass gt-web-engine (gt-engine)
  ((cache :initarg :cache
          :type (or symbol function)
          :initform 'word
          :documentation "Cache style used by the component.
If this is nil, current component will use no cache at all.
If this is literally symbol, see `gt-valid-literally' for details."))
  "Translate engine which use http request for results."
  :abstract t)

(defclass gt-api-engine (gt-web-engine)
  ((key
    :initarg :key
    :initform nil
    :type (or null symbol function string)
    :documentation "Optional API key")
   (rate-limit
    :initarg :rate-limit
    :initform nil
    :type (or null (integer 1 *) float)
    :documentation "Limit request concurrency"))
  "Engine which get result from http api."
  :abstract t)

(defclass gt-render (gt-single gt-valid-trait)
  ((output
    :initarg :output
    :initform nil
    :type (or function null)
    :documentation "If this is not nil, then will override the default output logic.")
   (init
    :initarg :init
    :initform nil
    :type (or function null)
    :documentation "Extra initialize configs for render.")
   (then
    :initarg :then
    :initform nil
    :type (or function gt-render null)
    :documentation "Chain results to next renderer if possible.
If this is a function, pass current translator as argument and execute it only.")
   (prefix
    :initarg :prefix
    :type (or function t)
    :documentation "Custom prefix displayed in the renderer.
If this is nil, then don't display prefix. If this is function use its return value
as prefix. If this is a string, use the string as prefix."))
  :documentation
  "A component used to output results of translator.
Method `gt-init' only works once before output, method `gt-output' will run after every task finished.")

(defclass gt-translator ()
  ((text
    :initarg :text
    :initform nil
    :type list
    :documentation "The text taken by taker, and prepare to translate.
This should be a list, every element is a string as one paragraph or one part.
This list will be sent to translate engines later.")

   (bounds
    :initform nil
    :type list
    :documentation "The bounds in buffer corresponds to the `text' slot.
The first element is the working buffer, while rest are buffer bounds.")

   (target
    :initform nil
    :type list
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
2: renderer prepared
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

The basic components of a translator is taker, engines and renderer:
1. Use `gt-taker' to capture the source text and target
2. Then use one or more `gt-engine' to get the translate result
3. At last use a `gt-render' to output the results to user.

The run a translation, call `gt-start' on a translator instance like this:

  (gt-start (make-instance `gt-translator
                   :taker (gt-taker)
                   :engines (gt-google-engine)
                   :render (gt-buffer-render)))

That is: 1) define the translator 2) executable `gt-start'.")

(cl-defgeneric gt-reset (_component &rest _args)
  "Reset states for COMPONENT."
  nil)

(cl-defgeneric gt-init (_component &rest _args)
  "Initialize states for COMPONENT."
  nil)

(cl-defgeneric gt-update (_component &rest _args)
  "Update states for COMPONENT."
  nil)

(cl-defgeneric gt-str (object)
  "Return the string representation of the OBJECT."
  (format "%s" (eieio-object-class object)))

(cl-defgeneric gt-tag (object)
  "Return the display tag for OBJECT."
  (let ((tag (if (not (eieio-object-p object))
                 object
               (if (and (slot-exists-p object 'tag)
                        (slot-boundp object 'tag))
                   (oref object tag)
                 (eieio-object-class object)))))
    (if (or (null tag) (stringp tag)) tag (format "%s" tag))))


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
  (if (functionp obj) (pdd-funcall obj args) obj))

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
      (when-let* ((normals (car ds)))
        (setq r (propertize r 'display normals)))
      (when-let* ((spaces (cdr ds)))
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
  (when-let* ((secret (plist-get (car (apply 'auth-source-search params)) :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun gt-insert-text-at-marker (str marker &optional end-marker keep-cursor)
  "Insert STR to position of MARKER.
If END-MARKER exists, delete the content between the markers first.
If KEEP-CURSOR is not nil, keep the cursor at front."
  (with-current-buffer (marker-buffer marker)
    (let ((inhibit-read-only t)
          (fn (lambda ()
                (goto-char marker)
                (deactivate-mark)
                (insert str))))
      (if (and end-marker (string-prefix-p (buffer-substring marker end-marker) str))
          (setq str (substring str (- (marker-position end-marker) (marker-position marker)))
                marker end-marker)
        (when end-marker
          (set-marker-insertion-type end-marker t)
          (delete-region marker end-marker)))
      (if keep-cursor
          (save-excursion (funcall fn))
        (funcall fn)))))

(defun gt-parse-html-dom (str)
  "Parse html STR and return the DOM body."
  (require 'dom)
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

(defun gt-system-language ()
  "Detect the system's UI language and return it as a symbol."
  (let ((lang-code-str
         (cond
          ((memq system-type '(gnu/linux darwin))
           (or (getenv "LC_ALL") (getenv "LANG")))
          ((eq system-type 'windows-nt)
           (ignore-errors
             (shell-command-to-string "powershell -NoProfile -NonInteractive -Command \"Get-UICulture.Name\"")))
          (t nil))))
    (if (and lang-code-str (string-match "\\`[a-zA-Z]\\{2\\}" lang-code-str))
        (intern (downcase (match-string 0 lang-code-str)))
      'en)))


;;; Validate Trait

;; add validate feature to `taker/engine/render' through `:if' slot

(cl-defmethod gt-validate ((component gt-valid-trait) carrier)
  "Determine if current COMPONENT is available through `if' slot.
Slot value can be a function, a symbol or a list. CARRIER maybe a task, a
translator or a list to provide more validation data."
  (let (text src tgt)
    (cond ((cl-typep carrier 'gt-task)
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
            ((functionp if) (pdd-funcall if (list component carrier)))
            (t (gt-valid-literally if text src tgt))))))

(cl-defgeneric gt-valid-literally (v text src tgt)
  "Check literally symbol or form V to determine next step.

It can be used by cache and engine.

V should be a symbol like `word', `parts', `src:en', `tgt:en', `selection',
`xxx-mode', `read-only', `at-word', or symbol with `not-' or `no-' prefix such
as `not-word', `not-src:en', `no-parts' etc.

V also can be a list form grouping above symbols with `and/or', for example:

  (or word not-parts (and tgt:cn #`xxxx)).

TEXT, SRC and TGT are translation text and targets.

This is a generic method, you can extend the V as you wish."
  (let ((vn (symbol-name v)))
    (cond ((eq v 'word)
           (and (car text) ; text is word
                (not (cdr text))
                (gt-word-p (intern-soft src) (car text))))
          ((eq v 'parts) (cdr text)) ; text is multiple parts
          ((eq v 'selection) (use-region-p)) ; use region is active
          ((eq v 'read-only) buffer-read-only) ; buffer is read-only
          ((eq v 'at-word) (symbol-at-point)) ; there is something at point
          ((string-suffix-p "-mode" vn) ; major-mode or minor-mode
           (if (boundp v) (symbol-value v) (eq major-mode v)))
          ((string-match "^\\(src\\|tgt\\):\\(.+\\)" vn)
           (member (match-string 2 vn) ; src/tgt is specific one, as src:en
                   (mapcar (lambda (item) (format "%s" item))
                           (ensure-list (if (equal (match-string 1 vn) "src") src tgt))))))))

(cl-defmethod gt-valid-literally :around (v text src tgt)
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


;;; Logging

(defvar gt-log-buffer "*gt-log*"
  "Log buffer for translator.")

(defun gt-log (tag &rest messages)
  "Log MESSAGES to `gt-log-buffer'.
TAG is a label for message being logged."
  (declare (indent 1))
  (when gt-debug-p
    (with-current-buffer (get-buffer-create gt-log-buffer)
      (goto-char (point-max))
      (if (and (null messages) (stringp tag))
          (insert tag)
        (let* ((tag (when tag
                      (concat
                       (gt-face-lazy (cl-subseq (format "%-.2f" (time-to-seconds)) 6) 'gt-logger-buffer-timestamp-face)
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
                 `(let ((obj ,o)) (if (eieio-object-p obj) (gt-str obj) obj))))
      'font-lock-doc-face)))


;;; Http Client

(defcustom gt-http-backend pdd-backend
  "Http client used by `gt-request' by default.
See the descriptions from `pdd-http-backend'."
  :type 'sexp
  :group 'go-translate)

(defcustom gt-http-proxy pdd-proxy
  "Proxy used by http request.
See the descriptions from `pdd-proxy'."
  :type '(choice string function)
  :group 'go-translate)

(defcustom gt-http-timeout pdd-timeout
  "Timeout for http request.
See the descriptions from `pdd-timeout'."
  :type 'natnum
  :group 'go-translate)

(defcustom gt-http-max-retry pdd-max-retry
  "Max retry times for http request.
See the descriptions from `pdd-max-retry'."
  :type 'natnum
  :group 'go-translate)

(cl-defun gt-request (url &rest args &key fail max-retry &allow-other-keys)
  "Send request for URL with pdd backend specified by `gt-http-backend'.
All the ARGS like FAIL, MAX-RETRY are just like those of method `pdd'."
  (declare (indent 1))
  (let* ((pdd-debug gt-debug-p)
         (pdd-proxy gt-http-proxy)
         (pdd-timeout gt-http-timeout)
         (pdd-max-retry (or max-retry gt-http-max-retry))
         (pdd-log-redirect #'gt-log)
         (pdd-backend gt-http-backend)
         (pdd-backend (pdd-ensure-default-backend (cons url args))))
    (if (and pdd-backend (eieio-object-p pdd-backend)
             (object-of-class-p pdd-backend 'pdd-http-backend))
        (apply #'pdd url args)
      (let ((errmsg "Make sure `gt-http-backend' is available. eg:\n\n(setq gt-http-backend (pdd-url-backend))\n\n\n"))
        (if fail (funcall fail errmsg) (user-error errmsg))))))

(cl-defmacro gt-dolist-concurrency ((var items &optional (rate-limit nil supplied)) &rest body)
  "Helper to make concurrency requests for ITEMS.
LIMIT should be a float or interger used to create queue with :rate or :limit.
Bind VAR to item of ITEMS which can be used in BODY."
  (declare (indent 1))
  (let ((sym (gensym)))
    `(let* ((,sym ,(if supplied rate-limit (or rate-limit 6)))
            (pdd-active-queue
             ,(if sym `(pcase ,sym
                         ((pred integerp) (pdd-queue :limit ,sym))
                         ((pred floatp) (pdd-queue :rate ,sym))
                         (_ pdd-active-queue))
                'pdd-active-queue)))
       (pdd-all (mapcar (lambda (,var) ,@body) ,items)))))


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

(defvar gt-taker-text-things '(word paragraph sentence buffer line page list sexp defun symbol point))

(defvar gt-taker-pick-things '(word paragraph sentence line list sexp defun page symbol))

(defcustom gt-taker-text 'word
  "The initial text taken by taker of translator.

If the region is activated, take the selection text. Otherwise, if this is a
symbol, then try to use `thing-at-point' to take the text at point. If this
is a function then invoke it and use its result as the initial text. Specially
if this is t, take the text interactively.

Specially, `buffer' as buffer string and `point' as empty string at point.

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

If this is a function then pass the initial text and target into it and use its
result as the final prompt.

Notice, the value can be overrided by `prompt' slot in taker of translator. So
config it for specific translator using slot is effectively."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "With-Minibuffer" t)
                 (const :tag "With-Buffer" buffer)
                 symbol function)
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
  (pcase thing
    ('buffer
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
         (list (cons beg end)))))
    ('point (list (cons (point) (point))))
    (_ (when-let* ((rt (bounds-of-thing-at-point thing))) (list rt)))))

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
                        (when-let* ((bd (gt-forward-thing thing mode)))
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

See `gt-taker' for more description."
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
               (when prompt
                 (if (functionp prompt)
                     (funcall prompt translator)
                   (gt-prompt taker translator prompt)))
               (unless text (user-error "No source text found at all"))
               ;; 4) pick
               (unless nopick (gt-pick taker translator)))))
  (:method :after ((taker gt-taker) translator)
           (with-slots (text target) translator
             (unless text
               (user-error "Source Text should not be null"))
             (with-slots (then) taker ; if `then' slot exists
               (when (and (slot-boundp taker 'then) then (gt-functionp then))
                 (pdd-funcall then (list translator)))))))

(cl-defgeneric gt-text (_taker translator)
  "Used to take initial text for TRANSLATOR.
Return text, text list or bounds in buffer. This is non-destructive."
  (:method ((taker gt-taker) translator)
           "Return the text selected by the TAKER."
           (let ((text (if (slot-boundp taker 'text)
                           (oref taker text)
                         gt-taker-text)))
             (cond ((symbolp text) (gt-thing-at-point text major-mode))
                   ((gt-functionp text) (pdd-funcall text (list translator)))
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
            (if-let* ((src (car target))) (format "%s > " src) "")
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
  "Prompt user with initial text and target for TRANSLATOR."
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
                              ((gt-functionp pick) (pdd-funcall pick (list car translator)))
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

(defcustom gt-cache-ttl (* 30 60)
  "Default cache alive time."
  :type 'integer
  :group 'go-translate)

(defvar gt-cache-store (make-hash-table :test #'equal))

(defconst gt-text-delimiter "314141592926666")

(cl-defgeneric gt-execute (engine &rest _args)
  "Execute the translate or transform logics for ENGINE."
  (user-error "Method `gt-execute' of %s is not implement" (gt-str engine)))

(cl-defmethod gt-execute :around ((engine gt-engine) task &optional skip-parse)
  "The primary method return the result or a `pdd-task' contains the result.
This around method add the init and finalize progress to the primary method.
If SKIP-PARSE is t, return the raw results directly."
  (gt-log-funcall "translate (%s %s)" engine (oref task id))
  (with-slots (id text src tgt res err meta proc) task
    (with-slots (delimit parse stream then) engine
      (unless (or res err)
        (gt-log 'next (format "%s: %s prepare to translate" id (gt-str engine)))
        ;; enable caching if necessary
        (let* ((cache-pred nil)
               (pdd-active-cacher
                (when (and gt-cache-p
                           (not stream)
                           (slot-exists-p engine 'cache)
                           (slot-boundp engine 'cache)
                           (setq cache-pred (oref engine cache))
                           (if (functionp cache-pred)
                               (pdd-funcall cache-pred (list task))
                             (gt-valid-literally cache-pred
                                                 (cl-reduce
                                                  (lambda (longest-so-far current-string)
                                                    (if (> (length current-string) (length longest-so-far))
                                                        current-string
                                                      longest-so-far))
                                                  (ensure-list text))
                                                 src tgt)))
                  (pdd-cacher :ttl gt-cache-ttl :key '(url data) :store 'gt-cache-store)))
               (delimiter (if (eq delimit t) gt-text-delimiter delimit)))
          ;; join with delimiter if necessary (join-process-split)
          (when (and delimiter (not (oref engine stream)))
            (setf text (string-join text (concat "\n\n" delimiter "\n\n"))))
          ;; execute the engine and postprocess the results
          (setf proc
                (pdd-then (cl-call-next-method engine task)
                  (lambda (result)
                    (if (ignore-errors (gt-task-expired-p task))
                        (gt-log 'next (format "%s: ----- expired -----" id))
                      (unless (oref engine stream)
                        (setf res (if delimiter (car (ensure-list result)) result))
                        ;; parse
                        (when res
                          (unless skip-parse
                            (when-let* ((parser (and (slot-boundp engine 'parse) parse)))
                              (if (gt-functionp parser)
                                  (pdd-funcall parser (list task))
                                (gt-parse parser task))))
                          ;; split with delimiter if possible
                          (when delimiter
                            (setf res (mapcar (lambda (item) (string-trim item "\n+")) (split-string res delimiter))))
                          (setf res (ensure-list res))
                          ;; run res-hook if possible
                          (when-let* ((hook (plist-get meta :res-hook)))
                            (setf res (funcall hook res))))
                        ;; verify
                        (unless res
                          (user-error "No translate result found"))
                        ;; invoke then when it is exists
                        (if (and (slot-boundp engine 'then) then (gt-functionp then))
                            (pdd-funcall then (list task))
                          task)))))))))))

(cl-defmethod gt-finalize ((engine gt-engine) task)
  (gt-log-funcall "finalize (%s %s)" engine (oref task id))
  (with-slots (id res render translator) task
    (if (oref engine stream)
        (if (gt-task-cancelled-p task)
            (gt-log 'next (format "%s: %s translate streaming abort!" id (gt-str engine)))
          (gt-output render task))
      (unless (= (length res) (length (oref translator text)))
        (user-error "Source and results not matched (count missmatch)"))
      (gt-log 'next (format "%s: %s translate success!" id (gt-str engine)))
      ;; all right, render
      (gt-update translator)
      (gt-output (oref translator render) translator))))

(cl-defgeneric gt-parse (_parser _task-or-data)
  "Parse TASK-OR-DATA using PARSER."
  (:method :around (parser target)
           (condition-case err
               (cl-call-next-method parser target)
             (error (gt-log 'parse
                      (format "Error occurs when parsing %s !" (gt-str parser)))
                    (signal (car err) (cdr err)))))
  nil)

(cl-defgeneric gt-resolve-key (api-engine)
  "Return the api key for API-ENGINE."
  (:method ((engine gt-api-engine)) (oref engine key)))

(defmacro gt-with-slots-for-key (slots engine &rest body)
  (declare (indent 2))
  `(with-slots ,slots ,engine
     (if (and (slot-exists-p engine 'key)
              (stringp (oref engine key)))
         (oref engine key)
       (let ((key-found (progn ,@body)))
         (unless (stringp key-found)
           (user-error "You should provide a valid api key for `%s'" (gt-str ,engine)))
         key-found))))


;;; Renderer

(cl-defgeneric gt-output (render translator-or-task)
  "Output TRANSLATOR-OR-TASK with RENDER.
When output a task, hint that this is for a stream request.")

(cl-defgeneric gt-extract-data (render translator)
  "Extract TRANSLATOR's responses that to be consumed by RENDER.")

(cl-defmethod gt-init :around ((render gt-render) translator)
  "Initialize the RENDER. Only invoke once before output."
  (gt-log-funcall "init (%s %s)" render translator)
  (let (message-log-max)
    (message "Processing..."))
  (condition-case err
      (progn (cl-call-next-method render translator)
             (gt-update translator))
    (error (gt-log 'render (format "%s initialize failed, abort" (gt-str render)))
           (user-error (format "[output init error] %s" err)))))

(cl-defmethod gt-output :around ((render gt-render) (translator gt-translator))
  (with-slots (state) translator
    (gt-log-funcall "output (%s %s) %s" render translator state)
    (when (<= state 4)
      (with-slots (output then) render
        ;; error handler & functionp case
        (condition-case err
            (if (gt-functionp output)
                (pdd-funcall output (list (oref translator tasks) translator))
              (cl-call-next-method render translator)
              ;; chain next render if possible
              (when (and (= state 3) then)
                (cond ((gt-functionp then)
                       (pdd-funcall then (list translator)))
                      ((and (eieio-object-p then) (object-of-class-p then 'gt-render))
                       (gt-init then translator)
                       (gt-output then translator))
                      (t (error "Bad then value specified in %s" (gt-str render))))))
          (error (setf state 4)
                 (message "[output error] %s" (cadr err))))))))

(cl-defmethod gt-output ((render gt-render) (translator gt-translator))
  "The default RENDER which outputing results to echo area for TRANSLATOR."
  (when (= 3 (oref translator state))
    (cl-loop with data = (gt-extract-data render translator)
             with multip = (cdr (oref translator text))
             for item in data
             for (prefix result) = (gt-plist-let item (list (concat "[" .prefix "]" (if multip "\n" " ")) .result))
             collect (concat (if (cdr data) (propertize prefix 'face 'gt-render-prefix-face))
                             (if (consp result) (string-join result "\n") result))
             into lst finally (message "%s" (string-join lst "\n")))))

(cl-defmethod gt-output :around ((render gt-render) (task gt-task))
  (with-slots (markers res) task
    (if (null markers)
        (when (pdd-task-p res)
          (pdd-signal res 'cancel)
          (message "Stream output is unavailable for current task."))
      (setf markers (ensure-list markers))
      (cl-call-next-method render task))))

(cl-defmethod gt-output ((_render gt-render) (task gt-task))
  (with-slots (markers res) task
    (save-excursion
      (gt-insert-text-at-marker
       (propertize (string-join (ensure-list res) "\n") 'gt-result 'stream)
       (car markers) (cdr markers) t))))

(cl-defmethod gt-extract-data ((render gt-render) translator)
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
                                      (pdd-funcall prefix (list task))
                                    (format "%s" prefix)))
                              (when (or (cdr tgts) (cdr engines))
                                ;; style as: en.Google.Detail
                                (let ((lst (list (if (cdr tgts) tgt)
                                                 (if (cdr engines) (gt-tag engine))
                                                 (if (cdr engines) (ignore-errors (gt-tag (oref engine parse)))))))
                                  (concat
                                   (mapconcat (lambda (item) (format "%s" item)) (remove nil lst) ".")
                                   (if (and (cdr engines) (oref engine stream)) " (stream)"))))))
             for result = (pcase state (0 "Loading...") (1 err) (2 res))
             collect (list :result result :prefix prefix :state state :task task :tgt tgt :stream (oref engine stream)) into lst
             finally (return ;; sort by target, then by engine
                      (if (cdr tgts)
                          (cl-loop for tgt in tgts append
                                   (cl-remove-if-not (lambda (entry) (equal tgt (plist-get entry :tgt))) lst))
                        lst)))))


;;; Task

(cl-defmethod gt-init ((task gt-task))
  "Initial new TASK for translator."
  (with-slots (id engine render translator) task
    (with-slots (tasks state) translator
      (when (= state 0)
        (setf tasks (nconc tasks (list task)))
        (gt-log (format "%d" (length tasks))
          (format "add task %s: (%s/%s)" id (gt-str engine) (gt-str render)))))))

(defun gt-task-fail (task error)
  "Render ERROR message and terminate the TASK."
  (declare (indent 1))
  (gt-log-funcall "task-fail (%s)" (oref task id))
  (with-slots (id err render translator) task
    (unless (gt-task-expired-p task)
      (setf err (if (consp error)
                    (mapconcat (lambda (r)
                                 (format (if (string-suffix-p "error" (format "%s" r)) "[%s]" "%s") r))
                               (cl-remove-if (lambda (r) (memq r '(error user-error))) error)
                               " ")
                  (or error "")))
      (gt-update translator)
      (gt-output render translator))
    (gt-log 'next (format "%s: [----- error -----] %s" id error))
    (gt-log nil 1 (with-temp-buffer
                    (let ((standard-output (current-buffer)))
                      (backtrace)
                      (buffer-string))))))

(defun gt-task-expired-p (task)
  (with-slots (version translator) task
    (not (equal version (oref translator version)))))

(defun gt-task-cancelled-p (task)
  (when (vectorp task)
    (eq (car-safe (aref (oref task proc) 3)) 'cancel)))


;;; Text to Speech

(defcustom gt-tts-speaker (cl-find-if #'executable-find (list "mpv" "mplayer" "cvlc"))
  "The program used to speak the translation result.
It also can be a command with options like `mpv --af=xxx'.
The value should not contain any space in the command path."
  :type 'string
  :group 'go-translate)

(defvar gt-tts-cache-ttl 30)

(defvar gt-tts-cache-store (make-hash-table :test #'equal))

(defvar gt-tts-langs nil)

(defvar gt-tts-last-engine nil)

;; generic

(cl-defgeneric gt-speech (engine _text _lang &optional _play-fn)
  "Speak TEXT with LANG by ENGINE."
  (:method :around ((engine gt-engine) text lang &optional play-fn)
           (cl-call-next-method engine text (intern-soft lang) play-fn))
  (user-error "No TTS service found on engine `%s'" (gt-tag engine)))

(defun gt-play-audio (data)
  "Play audio DATA and return a promise that resolves when finished."
  (unless (and gt-tts-speaker (executable-find (car (split-string gt-tts-speaker))))
    (user-error "Speaker '%s' not found. Please install it or configure `gt-tts-speaker'" gt-tts-speaker))
  (pdd-then (cond ((bufferp data)
                   (with-current-buffer data (buffer-string)))
                  ((and (stringp data) (string-prefix-p "http" data))
                   (gt-request data :cache 10 :as 'identity))
                  (t data))
    (lambda (audio-data)
      (if (functionp audio-data)
          (funcall audio-data)
        (pdd-exec (split-string-shell-command gt-tts-speaker) "-"
          :pipe t :init (concat audio-data "\n"))))))

;; native

(defcustom gt-tts-native-engine
  (cond ((and (eq system-type 'darwin) (executable-find "say"))
         'mac-say)
        ((and (memq system-type '(cygwin windows-nt)) (executable-find "powershell"))
         'win-ps1)
        ((executable-find "edge-tts")
         'edge-tts)
        (t system-type))
  "Engine used as the native one."
  :type 'sexp
  :group 'go-translate)

(cl-defmethod gt-speech ((_ (eql 'native)) text lang &optional play-fn)
  (gt-speech gt-tts-native-engine text lang play-fn))

;; mac-say

(defcustom gt-tts-mac-say-voice nil
  "Speak voice, query with command `gt-tts-mac-say-voices'."
  :type '(choice string (const nil))
  :group 'go-translate)

(defcustom gt-tts-mac-say-speed nil
  "A natnum number representing words per minute."
  :type '(choice natnum (const nil))
  :group 'go-translate)

(defun gt-tts-mac-say-voices ()
  "Return the avaiable speech voices on macOS."
  (interactive)
  (pdd-exec `(say -v \?) :done 'princ))

(cl-defmethod gt-speech ((_ (eql 'mac-say)) text _lang &optional play-fn)
  (let (args)
    (when gt-tts-mac-say-voice
      (setq args (append `(-v ,gt-tts-mac-say-voice) args)))
    (when gt-tts-mac-say-speed
      (setq args (append `(-r ,gt-tts-mac-say-speed) args)))
    (funcall (or play-fn #'gt-play-audio)
             (lambda () (pdd-exec `(say ,@args) :init (concat text "\n"))))))

;; win-ps1

(defcustom gt-tts-win-ps1-voice nil
  "Speak voice, query with command `gt-tts-win-ps1-voices'."
  :type '(choice string (const nil))
  :group 'go-translate)

(defcustom gt-tts-win-ps1-speed nil
  "A number from -10 to 10."
  :type '(choice number (const nil))
  :group 'go-translate)

(defun gt-tts-win-ps1-voices ()
  "Return the avaiable speech voices on Windows."
  (interactive)
  (let ((ps1 (concat "Add-Type -AssemblyName System.Speech; "
                     "$synthesizer = New-Object System.Speech.Synthesis.SpeechSynthesizer; "
                     "$synthesizer.GetInstalledVoices().VoiceInfo | Select-Object Name, Gender, Age, Culture; ")))
    (pdd-exec `(powershell -Command ,ps1) :done 'princ)))

(cl-defmethod gt-speech ((_ (eql 'win-ps1)) text _lang &optional play-fn)
  (let* ((ps1 (concat "Add-Type -AssemblyName System.Speech;\n"
                      "$synth = New-Object System.Speech.Synthesis.SpeechSynthesizer;\n"
                      (if gt-tts-win-ps1-voice (format "$synth.SelectVoice('%s');\n" gt-tts-win-ps1-voice))
                      (if gt-tts-win-ps1-speed (format "$synth.Rate = %d;\n" gt-tts-win-ps1-speed))
                      "$text = $input | Out-String;\n"
                      "if ($text.Length -gt 0) { $synth.Speak($text.Trim()) };\n"
                      "$synth.Dispose();"))
         (command `(powershell -NoProfile -ExecutionPolicy Bypass -Command ,ps1)))
    (funcall (or play-fn #'gt-play-audio)
             (lambda () (pdd-exec command :pipe t :init text)))))

;; edge-tts (a more robust alternative to the `gt-bing-engine')

(defcustom gt-tts-edge-tts-voice nil
  "Speak voice, query with: edge-tts --list-voices."
  :type '(choice string (const nil))
  :group 'go-translate)

(defcustom gt-tts-edge-tts-speed nil
  "A number from -10 to 10."
  :type '(choice number (const nil))
  :group 'go-translate)

(defcustom gt-tts-edge-tts-volume nil
  "A number from -10 to 10, representing dec/inc volume."
  :type '(choice number (const nil))
  :group 'go-translate)

(defcustom gt-tts-edge-tts-pitch nil
  "A number representing dec/inc pitch, like -50 as -50Hz."
  :type '(choice number (const nil))
  :group 'go-translate)

(defun gt-tts-edge-tts-change-voice ()
  "Change voice for `tts-edge' engine."
  (interactive)
  (let* ((voices (mapcar #'pdd-split-string-by
                         (cl-remove-if-not
                          (lambda (item) (string-match-p "Neural" item))
                          (pdd-exec `(edge-tts --list-voices) :as 'line :sync t))))
         (annfn (lambda (v)
                  (concat (make-string (- 35 (length v)) ? )
                          (cdr (assoc v voices)))))
         (voice (completing-read
                 "Voice: "
                 (lambda (input pred action)
                   (if (eq action 'metadata)
                       `(metadata (annotation-function . ,annfn))
                     (complete-with-action action (cons "default" voices) input pred)))
                 nil t nil 'gt-tts-tts-edge-voice-history gt-tts-edge-tts-voice)))
    (pdd-cacher-clear gt-tts-cache-store t)
    (setq gt-tts-edge-tts-voice (unless (equal voice "default") voice))
    (message "Change voice to `%s'." voice)))

(cl-defmethod gt-speech ((_ (eql 'edge-tts)) text _lang &optional play-fn)
  (unless (executable-find "edge-tts")
    (user-error "You should install `edge-tts' first (pip install edge-tts)"))
  (let (args)
    (when gt-tts-edge-tts-voice
      (setq args (append `(-v ,gt-tts-edge-tts-voice) args)))
    (when (numberp gt-tts-edge-tts-speed)
      (setq args (cons (format "--rate=%s%d%%"
                               (if (>= gt-tts-edge-tts-speed 0) "+" "")
                               (* 10 gt-tts-edge-tts-speed))
                       args)))
    (when (numberp gt-tts-edge-tts-volume)
      (setq args (cons (format "--volume=%s%d%%"
                               (if (>= gt-tts-edge-tts-volume 0) "+" "")
                               (* 10 gt-tts-edge-tts-volume))
                       args)))
    (when (numberp gt-tts-edge-tts-pitch)
      (setq args (cons (format "--pitch=%s%dHz"
                               (if (>= gt-tts-edge-tts-pitch 0) "+" "")
                               gt-tts-edge-tts-pitch)
                       args)))
    (pdd-then (pdd-exec `(edge-tts -t ,text ,@args)
                :pipe t :cache `(,gt-tts-cache-ttl (edge-tts ,text) gt-tts-cache-store))
      (or play-fn #'gt-play-audio))))

;; interact

(cl-defmethod gt-speech ((_ (eql 'interact)) text auto &optional play-fn)
  (let* ((prompt "Text to Speech: ")
         (regexp (format "^ *\\(%s\\)\\." (mapconcat #'symbol-name (mapcar #'car gt-lang-codes) "\\|")))
         (engines `(native ,@(cl-loop
                              for m in (cl--generic-method-table (cl--generic #'gt-speech))
                              for n = (car (cl--generic-method-specializers m))
                              unless (or (consp n) (memq n '(t gt-engine))) collect n)))
         (overlay nil)
         (text (if auto text
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (use-local-map (make-composed-keymap nil (current-local-map)))
                       (setq overlay (make-overlay (1- (length prompt)) (length prompt)))
                       (unless gt-tts-last-engine (setq gt-tts-last-engine 'native))
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
    (unless gt-tts-last-engine (setq gt-tts-last-engine 'native))
    (gt-log 'tts (format "Speak with %s to %s" gt-tts-last-engine lang))
    (if (equal gt-tts-last-engine 'native)
        (gt-speech 'native text lang play-fn)
      (gt-speech (make-instance gt-tts-last-engine) text lang play-fn))))

;; user command

(defvar gt-speak-task nil)

;;;###autoload
(defun gt-speak ()
  "Speak content around point.

If the text under point or with selection has `gt-task' or `gt-tts-url'
property, try to speak it with current translation engine.

Otherwise try to TTS with native program or using selected engine.

Switch engine with key `C-n'.

When TTS with specific engine, you can specify the language with `lang.' prefix."
  (interactive)
  (if-let* ((url (get-char-property (point) 'gt-tts-url)))
      ;; 1. play the url
      (setq gt-speak-task (gt-play-audio url))
    (let (items engine)
      (if-let* ((task (get-char-property (point) 'gt-task)))
          (with-slots (src tgt res err translator) task
            (with-slots (text target) translator
              ;; 2. play current task
              (setq engine (oref task engine))
              (unless (cl-find-method #'gt-speech '() `(,(eieio-object-class engine) t t))
                (user-error "No TTS service found on current engine `%s'" (gt-tag engine)))
              (let ((part (or (get-char-property (point) 'gt-part) 0))
                    (col (lambda (l c) (push (format "%s. %s" l (substring-no-properties c)) items))))
                (if (use-region-p)
                    (cl-loop with text = (buffer-substring (region-beginning) (region-end))
                             for l in (gt-langs-maybe text (list src tgt)) do (funcall col l text))
                  (when-let* ((r (nth part text)))
                    (funcall col (car target) r))
                  (when-let* ((r (and (not err) (or (get-pos-property (point) 'gt-brief) (nth part (ensure-list res))))))
                    (funcall col tgt r)))
                (when items
                  (let* ((cand (with-temp-buffer
                                 (completing-read
                                  (format "Text to Speech (with %s): " (gt-tag engine))
                                  (gt-make-completion-table items))))
                         (lang (if (string-match (format "^ *\\(%s\\)\\." (mapconcat #'symbol-name (mapcar #'car gt-lang-codes) "\\|")) cand)
                                   (prog1 (intern-soft (match-string 1 cand))
                                     (setq cand (substring cand (match-end 0))))
                                 (car (gt-langs-maybe cand (list src tgt))))))
                    (unless lang (user-error "Guess language of text failed"))
                    (gt-log 'tts (format "Speak with %s to %s" engine lang))
                    (setq gt-speak-task (gt-speech engine cand lang)))))))
        ;; 3. play with native program or using selected engine
        (setq gt-speak-task (gt-speech 'interact (gt-text (gt-taker :text 'word) nil) nil))))))


;;; Translator

(cl-defmethod gt-reset ((translator gt-translator))
  "Reset status and other variables before translation for TRANSLATOR."
  (gt-log-funcall "reset (%s)" translator)
  (unless gt-http-backend
    (setq gt-http-backend (pdd-url-backend)))
  (with-slots (version state tasks bag total taker engines render _taker _engines _render) translator
    (unless (slot-boundp translator '_taker)
      (unless taker (setf taker (gt-taker)))
      (unless render (setf render (gt-render)))
      (setf _taker taker _engines engines _render render))
    (setf version (time-to-seconds) state 0 tasks nil total 0 taker nil engines nil render nil bag nil)))

(cl-defmethod gt-init ((translator gt-translator) &rest _)
  "Initialize the components, text and target for TRANSLATOR."
  (gt-log-funcall "init (%s)" translator)
  (with-slots (keep text bounds target version taker engines render _taker _engines _render) translator
    ;; reset
    (gt-reset translator)
    (unless keep (setf text nil bounds nil target nil))
    (with-current-buffer (or (or (and bounds (car bounds)) (current-buffer)))
      ;; taker
      (setf taker (or (cl-find-if (lambda (tk) (gt-validate tk translator))
                                  (ensure-list (gt-ensure-plain _taker)))
                      (user-error "No taker found in this translator")))
      (gt-take taker translator)
      (setf keep nil)
      (let ((history-delete-duplicates t))
        (add-to-history 'gt-target-history target 8))
      ;; engines
      (setf engines (ensure-list (gt-ensure-plain _engines)))
      ;; renderer
      (setf render (cl-find-if (lambda (rd) (gt-validate rd translator))
                               (ensure-list (gt-ensure-plain _render)))))
    ;; log
    (gt-log 'translator (format "version: %s\ntarget: %s\nbounds: %s\ntext: %s\ntaker: %s, engines: %s, renderer: %s"
                                version target bounds text
                                (gt-str taker)
                                (mapcar (lambda (eng) (gt-tag eng)) engines)
                                (gt-str render)))))

(cl-defmethod gt-update ((translator gt-translator))
  "Update state for TRANSLATOR."
  (with-slots (state total tasks render) translator
    (pcase state
      (0
       (when (= (length tasks) total)
         (gt-log 'translator "<1> all tasks added")
         (setf state 1)))
      (1
       (gt-log 'translator
         (format "<2> %s prepared" (gt-str render)))
       (setf state 2))
      (2
       (when (= total (length (cl-remove-if-not (lambda (task) (or (oref task err) (oref task res))) tasks)))
         (gt-log 'translator "<3> all result parsed")
         (setf state 3))))))

(cl-defgeneric gt-start (translator)
  "Start a new translation with TRANSLATOR."
  (gt-log-funcall "start (%s)" translator)
  (setq gt-current-command this-command)
  (setq gt-current-translator translator)
  (gt-init translator nil)
  (with-slots (text target version total engines render tasks) translator
    ;; tasks
    (dolist (engine engines)
      (cl-loop for tgt in (cdr target)
               for task = (gt-task :text text
                                   :src (car target)
                                   :tgt tgt
                                   :engine engine
                                   :render render
                                   :translator translator
                                   :version version)
               if (gt-validate engine task)
               do (cl-incf total) and
               do (gt-init task)))
    (when (zerop total)
      (user-error "No task created, please check engines and langs setup"))
    ;; translate
    (gt-update translator)
    (when render
      (gt-init render translator))
    (dolist (task tasks)
      (condition-case err
          (let ((engine (oref task engine)))
            (pdd-chain t
              (lambda () (gt-init engine task))
              (lambda () (gt-execute engine task))
              (lambda () (gt-finalize engine task))
              :fail (lambda (r) (gt-task-fail task r))))
        (error (gt-task-fail task err))))))

(provide 'gt-core)

;;; gt-core.el ends here
