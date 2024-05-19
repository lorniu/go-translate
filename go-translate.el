;;; go-translate.el --- Translation framework, configurable and scalable -*- lexical-binding: t -*-

;; Copyright (C) 2024 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/go-translate
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience
;; Version: 3.0.5

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

;; Translation framework on Emacs, with high configurability and extensibility.
;;
;;  - Support multiple translation engines, such as Google, Bing, DeepL, ChatGPT...
;;  - With variety of output styles, such as Buffer, Overlay, Childframe and so on.
;;  - With a flexible taker for easy retrieval of translated content and targets.
;;  - Support multiple paragraphs/parts and multi-language translation.
;;  - Support different http backends, such as url.el, curl. Async and non-blocking.
;;  - Support caches, proxy and more.
;;
;; Notice, it is not limited to just being a translation framework. It can fulfill
;; any text transformation tasks, such as ChatGPT and more.
;;
;; Custom it as you need, extend it using your creativity.

;; You can install it via MELPA or from github. Make sure it is on your `load-path'.
;;
;; For the most basic use, add the following configuration:
;;
;;   (require 'go-translate)
;;
;;   (setq gt-default-translator
;;        (gt-translator
;;         :taker (gt-taker :langs '(en zh))
;;         :engines (list (gt-google-engine) (gt-bing-engine))
;;         :render (gt-buffer-render)))
;;
;; Then start your translate with command `gt-do-translate'.
;;
;; See README.org for details.

;;; Code:

(require 'transient)
(require 'gt-core)
(require 'gt-extension)
(require 'gt-engine-bing)
(require 'gt-engine-google)
(require 'gt-engine-google-rpc)
(require 'gt-engine-deepl)
(require 'gt-engine-stardict)
(require 'gt-engine-youdao)
(require 'gt-engine-chatgpt)
(require 'gt-engine-echo)
(require 'gt-text-utility)

;; Compat old version
(ignore-errors
  (add-to-list 'load-path (expand-file-name "v2" (file-name-directory (or load-file-name (buffer-file-name)))))
  (require 'go-translate-v2))

;;; Mask these commands in M-x
(dolist (cmd '(gt-prompt-next-target
               gt-buffer-render--cycle-next
               gt-buffer-render--refresh
               gt-buffer-render--browser
               gt-buffer-render--keyboard-quit
               gt-buffer-render--toggle-readonly
               gt-buffer-render--toggle-polyglot
               gt-buffer-render--delete-cache
               gt-buffer-render--show-tips
               gt-buffer-render--unfold-source-text
               gt-posframe-render-auto-close-handler
               gt-stardict-switch-dict
               gt-overlay-render-save-to-kill-ring))
  (put cmd 'completion-predicate (lambda (&rest _) nil)))


;;; Presets

(defun gt-define-custom-taker ()
  (let ((text (completing-read "Initial text: "
                               (gt-make-completion-table `(,@gt-taker-text-things nil))))
        (pick (completing-read "Pick style: "
                               (gt-make-completion-table `(nil ,@gt-taker-pick-things))))
        (prompt (completing-read "Prompt style: "
                                 (gt-make-completion-table (list 'disable 'buffer 'minibuffer)) nil t)))
    (gt-taker :text (intern text) :pick (intern pick)
              :prompt (pcase prompt ("buffer" 'buffer) ("minibuffer" t) (_ nil)))))

(defcustom gt-preset-takers
  (lambda ()
    `((,(gt-face-lazy "new..." 'bold)  . ,#'gt-define-custom-taker)
      (default                         . ,(gt-taker))
      (interactively                   . ,(gt-taker :text t :pick t :prompt t))
      (paragraph-at-point              . ,(gt-taker :text 'paragraph :pick nil))
      (whole-buffer                    . ,(gt-taker :text 'buffer :pick 'nil))
      (whole-buffer-by-paragraph       . ,(gt-taker :text 'buffer :pick 'paragraph))))
  "Preset takers.

It is an alist or a function return the alist, which the value is a valid
instance of `gt-taker' and the key is a string or symbol, representing the
display label of the taker.

Custom your own takers and put them into this list, then change the taker
of `gt-default-translator' at any time in `gt-do-setup'."
  :type '(choice function
                 (alist :key-type (choice string symbol)
                        :value-type (sexp :tag "Instance of gt-taker")))
  :group 'gt-do-translate)

(defcustom gt-preset-engines
  (lambda ()
    `((Bing                 . ,(gt-bing-engine))
      (DeepL                . ,(gt-deepl-engine))
      (Google               . ,(gt-google-engine))
      (ChatGPT              . ,(gt-chatgpt-engine))
      (ChatGPT-Stream       . ,(gt-chatgpt-engine :stream t))
      (Youdao-Dict          . ,(gt-youdao-dict-engine))
      (Youdao-Suggest       . ,(gt-youdao-suggest-engine))
      (StarDict             . ,(gt-stardict-engine))
      (GoogleRPC            . ,(gt-google-rpc-engine))
      (Google-Summary       . ,(gt-google-engine :parse (gt-google-summary-parser)))
      (Bionic_Reading       . ,(gt-echo-engine :do '(clean br) :tag "Bionic Reading"))))
  "Preset engines.

It is an alist or a function return the alist, which the value is a valid
instance of `gt-engine' and the key is a string or symbol, representing the
display label of the engine.

Custom your own engines and put them into this list, then change the engines
of `gt-default-translator' at any time in `gt-do-setup'."
  :type '(choice function
                 (alist :key-type (choice string symbol)
                        :value-type (sexp :tag "Instance of gt-engine")))
  :group 'gt-do-translate)

(defcustom gt-preset-renders
  (lambda ()
    `((,gt-buffer-render-buffer-name  . ,(gt-buffer-render))
      (insert/after                   . ,(gt-insert-render  :type 'after))
      (insert/replace                 . ,(gt-insert-render  :type 'replace))
      (overlay/after                  . ,(gt-overlay-render :type 'after))
      (overlay/help-echo              . ,(gt-overlay-render :type 'help-echo))
      (message->echo-area             . ,(gt-render))
      (save->kill-ring                . ,(gt-kill-ring-render))
      (Pop-Posframe                   . ,(gt-posframe-pop-render))
      (Pin-Posframe                   . ,(gt-posframe-pin-render))
      (overlay-or-insert              . ,(lambda ()
                                           (if buffer-read-only
                                               (gt-overlay-render :type 'after :then (gt-kill-ring-render))
                                             (gt-insert-render :type 'after))))
      (system-notification            . ,(gt-alert-render))))
  "Preset renders.

It is an alist or a function return the alist, which the value is a valid
instance of `gt-render' and the key is a string or symbol, representing the
display label of the render.

Custom your own render and put them into this list, then change the render
of `gt-default-translator' at any time in `gt-do-setup'."
  :type '(choice function
                 (alist :key-type (choice string symbol)
                        :value-type (sexp :tag "Instance of gt-render")))
  :group 'gt-do-translate)

(defcustom gt-preset-translators
  (lambda ()
    `((default . ,(gt-translator :taker   (cdar (gt-ensure-plain gt-preset-takers))
                                 :engines (cdar (gt-ensure-plain gt-preset-engines))
                                 :render  (cdar (gt-ensure-plain gt-preset-renders))))
      (Text-Utility . ,(gt-text-utility
                        :taker (gt-taker :pick nil)
                        :render (gt-buffer-render)))))
  "Preset translators.

It is an alist or a function return the alist, which the value is a valid
instance of `gt-translator' and the key is a string or symbol, representing the
display label of the translator.

Custom your own translator and put them into this list, then change the the
default translator to one of them at any time in `gt-do-setup'."
  :type '(choice function
                 (alist :key-type (choice string symbol)
                        :value-type (sexp :tag "Instance of gt-translator")))
  :group 'gt-do-translate)

(defcustom gt-default-translator nil
  "The translator used by `gt-do-translate'.

If you leave this nil, then the first translator in `gt-preset-translators'
will be used as the default translator."
  :type '(restricted-sexp :match-alternatives (gt-translator-p 'nil))
  :group 'gt-do-translate)

(defun gt-ensure-default-translator ()
  "Initial `gt-default-translator' if possible and make sure it's valid."
  (unless gt-default-translator
    (setq gt-default-translator (cdar (gt-ensure-plain gt-preset-translators))))
  (if (cl-typep gt-default-translator 'gt-translator)
      (let (gt-debug-p) (gt-reset gt-default-translator))
    (user-error "The `gt-default-translator' is unavailable"))
  gt-default-translator)

(defun gt-translator-info (translator)
  "Return TRANSLATOR's basic info for displaying."
  (with-slots (taker engines render _taker _engines _render) translator
    (cl-macrolet ((desc1 (name &rest body)
                    `(if (not (slot-boundp translator ',(intern (format "_%s" name)))) "unbound"
                       (when-let (,name (or ,name ,(intern (format "_%s" name))))
                         (if (gt-functionp ,name)
                             (replace-regexp-in-string "[ \n\t]+" " " (format "%s" ,name))
                           ,@body)))))
      (list (desc1 taker (cl-flet ((desc2 (slot) (when (slot-boundp taker slot)
                                                   (format "%s: %s" slot (slot-value taker slot)))))
                           (format "<%s> %s" (eieio-object-class taker)
                                   (string-join (remove nil (mapcar #'desc2 '(langs text pick prompt))) ", "))))
            (desc1 engines (mapconcat (lambda (en) (concat (format "%s" (oref en tag)) (if (gt-stream-p en) " (stream)")))
                                      (ensure-list (gt-ensure-plain engines)) ", "))
            (desc1 render (format "<%s>" (eieio-object-class (gt-ensure-plain render))))))))

(defun gt-set-taker (&optional translator taker)
  "Set TRANSLATOR's TAKER to one from `gt-preset-takers'."
  (interactive)
  (unless translator (setq translator gt-default-translator))
  (unless taker
    (let ((cands (gt-ensure-plain gt-preset-takers)))
      (setq taker (gt-ensure-plain
                   (alist-get
                    (completing-read "Taker to use: " (gt-make-completion-table cands) nil t)
                    cands nil nil #'string-equal)))))
  (oset translator taker nil)
  (oset translator _taker taker)
  (message "Changed taker done."))

(defun gt-set-engines (&optional translator engines)
  "Set TRANSLATOR's ENGINES to ones from `gt-preset-engines'."
  (interactive)
  (unless translator (setq translator gt-default-translator))
  (unless engines
    (let ((cands (gt-ensure-plain gt-preset-engines)))
      (setq engines
            (mapcar (lambda (item)
                      (let ((engine (gt-ensure-plain (alist-get item cands nil nil #'string-equal))))
                        (if (cl-typep engine 'gt-engine) engine
                          (user-error "Invalid engine detected. Abort"))))
                    (completing-read-multiple "Engines to use (can choose multiple): "
                                              (gt-make-completion-table cands))))))
  (oset translator engines nil)
  (oset translator _engines engines)
  (message "Changed engines done."))

(defun gt-set-render (&optional translator render)
  "Set TRANSLATOR's RENDER to one from `gt-preset-renders'."
  (interactive)
  (unless translator (setq translator gt-default-translator))
  (unless render
    (let ((cands (gt-ensure-plain gt-preset-renders)))
      (setq render (gt-ensure-plain
                    (alist-get
                     (completing-read "Render to use: " (gt-make-completion-table cands) nil t)
                     cands nil nil #'string-equal)))))
  (oset translator render nil)
  (oset translator _render render)
  (message "Changed render done."))

(defun gt-translator-copy-of-presets ()
  (let* ((tss (gt-ensure-plain gt-preset-translators))
         (tsn (completing-read "Preset translator: " tss nil t))
         (translator (alist-get tsn tss nil nil #'string-equal)))
    (list (clone translator) tsn)))

(defun gt-switch-translator ()
  "Switch `gt-default-translator' to another defined in `gt-preset-translators'."
  (interactive)
  (cl-destructuring-bind (translator name)
      (gt-translator-copy-of-presets)
    (setq gt-default-translator translator)
    (gt-ensure-default-translator)
    (message "Switch default translator to: %s" name)))

(transient-define-prefix gt-do-setup ()
  "Setup `gt-default-translator' in user interface provided by transient."
  :transient-non-suffix #'transient--do-exit
  [:description
   (lambda ()
     (format "Current Default Translator:\n\n    %s\n"
             (condition-case err
                 (apply #'format "Taker: %s\n    Engines: %s\n    Render: %s"  (gt-translator-info (gt-ensure-default-translator)))
               (error (format "%s" err)))))
   [("t"  "Set taker..."   gt-set-taker   :transient t)]
   [("e"  "Set engines..." gt-set-engines :transient t)]
   [("r"  "Set render..."  gt-set-render  :transient t)]
   [("c"  "Switch preset translator..." gt-switch-translator)]]
  (interactive)
  (gt-ensure-default-translator)
  (transient-setup 'gt-do-setup))


;;; Entrance

;;;###autoload
(defun gt-do-translate (&optional arg)
  "Translate using `gt-default-translator'.

Define your default translator like this:

  (setq gt-default-translator
    (gt-translator :engines (gt-bing-engine)))

  (setq gt-default-translator
    (gt-translator :taker (gt-taker :langs `(en fr) :text `sentence :prompt t)
                   :engines (list (gt-google-engine) (gt-deepl-engine))
                   :render (gt-buffer-render)))

Or define several different translators and put them in `gt-preset-translators',
and switch with `gt-do-setup' at any time.

This is just a simple wrapper of `gt-start' method. Create other translate
commands in the same way using your creativity.

If ARG is not nil, translate with translator select by `gt-preset-translators'."
  (interactive "P")
  (let ((gt-default-translator (if arg (car (gt-translator-copy-of-presets)) gt-default-translator)))
    (gt-ensure-default-translator)
    (gt-start gt-default-translator)))

(provide 'go-translate)

;;; go-translate.el ends here
