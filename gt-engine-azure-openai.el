;;; gt-engine-azure-openai.el --- Engine for Azure-OpenAI -*- lexical-binding: t -*-

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

;; https://platform.openai.com/docs/api-reference

;;; Code:

(require 'gt-extension)

(defgroup go-translate-azure-openai nil
  "Configs for Azure-OpenAI engine."
  :group 'go-translate)


;;; Components

(defclass gt-azure-openai-engine (gt-engine)
  ((tag         :initform 'Azure-OpenAI)
   (host        :initarg :host :initform nil)
   (model       :initarg :model :initform nil)
   (temperature :initarg :temperature :initform nil)
   (key         :initarg :key :initform 'apikey
                :documentation "The apikey of Azure-OpenAI.
Can also put into .authinfo file as:
  machine api.openai.com login apikey password ***")))


;;; Translate

(defcustom gt-azure-openai-host nil
  "API host of Azure-OpenAI."
  :type 'string
  :group 'go-translate-azure-openai)

(defcustom gt-azure-openai-key nil
  "Auth Key of Azure-OpenAI. Recommend to save in .authinfo file instead."
  :type 'string
  :group 'go-translate-azure-openai)

(defcustom gt-azure-openai-model "gpt-3.5-turbo"
  "Model to be used in chat."
  :type 'string
  :group 'go-translate-azure-openai)

(defcustom gt-azure-openai-temperature 0.8
  "Temperature of Azure-OpenAI."
  :type 'number
  :group 'go-translate-azure-openai)

(defcustom gt-azure-openai-system-prompt "You are a translation assistant"
  "System prompt send to server when translation."
  :type 'string
  :group 'go-translate-azure-openai)

(defcustom gt-azure-openai-user-prompt-template "Translate the text to %s, text is: \n%s"
  "Template for user prompt when translation.
When it is string, %s is placeholders of lang and text.
When it is function, arguments passed to it should be text and lang."
  :type '(choice string function)
  :group 'go-translate-azure-openai)

(declare-function pulse-momentary-highlight-region "pulse")

(defvar gt-azure-openai-streaming-finished-hook #'pulse-momentary-highlight-region
  "The logic runs after all streams finished.
With two arguments BEG and END, which are the marker bounds of the result.")

(cl-defmethod gt-ensure-key ((engine gt-azure-openai-engine))
  (with-slots (host key) engine
    (unless (stringp key)
      (if-let (apikey (or (gt-lookup-password
                           :user (if key (format "%s" key) "apikey")
                           :host (url-host (url-generic-parse-url (or host gt-azure-openai-host))))
                          gt-azure-openai-key))
          (setf key apikey)
        (user-error "You should provide a apikey for gt-azure-openai-engine")))))

(cl-defmethod gt-translate ((engine gt-azure-openai-engine) task next)
  (gt-ensure-key engine)
  (with-slots (text src tgt res translator markers) task
    (with-slots (host key model temperature stream) engine
      (when (and stream (cdr (oref translator text)))
        (user-error "Multiple parts not support streaming"))
      (gt-request :url (concat (or host gt-azure-openai-host) "/chat/completions?api-version=2023-05-15")
                  :headers `(("Content-Type" . "application/json")
                             ("api-key" . ,(encode-coding-string key 'utf-8)))
                  :data (encode-coding-string
                         (json-encode
                          `((model . ,(or model gt-azure-openai-model))
                            (temperature . ,(or temperature gt-azure-openai-temperature))
                            (stream . ,stream)
                            (messages . [((role . system)
                                          (content . ,gt-azure-openai-system-prompt))
                                         ((role . user)
                                          (content . ,(if (functionp gt-azure-openai-user-prompt-template)
                                                          (funcall gt-azure-openai-user-prompt-template text tgt)
                                                        (format gt-azure-openai-user-prompt-template (alist-get tgt gt-lang-codes) text))))])))
                         'utf-8)
                  :filter (when stream
                            (lambda ()
                              (unless gt-tracking-marker
                                (setq gt-tracking-marker (make-marker))
                                (set-marker gt-tracking-marker (point-min)))
                              (goto-char gt-tracking-marker)
                              (condition-case err
                                  (while (re-search-forward "^data: +\\({.+}\\)" nil t)
                                    (let* ((json (json-read-from-string (decode-coding-string (match-string 1) 'utf-8)))
                                           (choice (aref (alist-get 'choices json) 0))
                                           (content (alist-get 'content (alist-get 'delta choice)))
                                           (finish (alist-get 'finish_reason choice)))
                                      (if finish
                                          (progn (message "")
                                                 (when gt-azure-openai-streaming-finished-hook
                                                   (with-current-buffer (marker-buffer (car markers))
                                                     (funcall gt-azure-openai-streaming-finished-hook (car markers) (cdr markers)))))
                                        (setf res (concat res content))
                                        (set-marker gt-tracking-marker (point))
                                        (unless (string-blank-p (concat res))
                                          (funcall next task)))))
                                (error (unless (string-prefix-p "json" (format "%s" (car err)))
                                         (signal (car err) (cdr err)))))))
                  :done (unless stream
                          (lambda (raw)
                            (with-slots (res) task
                              (let* ((json (json-read-from-string raw))
                                     (str (alist-get 'content (alist-get 'message (let-alist json (aref .choices 0))))))
                                (setf res str))
                              (funcall next task))))
                  :fail (lambda (err) (gt-fail task err))))))

(cl-defmethod gt-stream-p ((engine gt-azure-openai-engine))
  (oref engine stream))


(provide 'gt-engine-azure-openai)

;;; gt-engine-azure-openai.el ends here
