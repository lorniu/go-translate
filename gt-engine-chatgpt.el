;;; gt-engine-chatgpt.el --- Engine for ChatGPT -*- lexical-binding: t -*-

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

(defgroup go-translate-chatgpt nil
  "Configs for ChatGPT engine."
  :group 'go-translate)


;;; Components

(defclass gt-chatgpt-parser (gt-parser) ())

(defclass gt-chatgpt-engine (gt-engine)
  ((tag         :initform 'ChatGPT)
   (host        :initarg :host :initform nil)
   (model       :initarg :model :initform nil)
   (temperature :initarg :temperature :initform nil)
   (parse       :initform (gt-chatgpt-parser))
   (key         :initarg :key :initform 'apikey
                :documentation "The apikey of ChatGPT.
Can also put into .authinfo file as:
  machine api.openai.com login apikey password ***")))


;;; Translate

(defcustom gt-chatgpt-host "https://api.openai.com"
  "API host of ChatGPT."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-key nil
  "Auth Key of ChatGPT. Recommend to save in .authinfo file instead."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-model "gpt-3.5-turbo"
  "Model to be used in chat."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-temperature 0.8
  "Temperature of ChatGPT."
  :type 'number
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-system-prompt "You are a translation assistant"
  "System prompt send to server when translation."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-user-prompt-template "Translate the text to %s, text is: \n%s"
  "Template for user prompt when translation.
When it is string, %s is placeholders of lang and text.
When it is function, arguments passed to it should be text and lang."
  :type '(choice string function)
  :group 'go-translate-chatgpt)

(cl-defmethod gt-ensure-key ((engine gt-chatgpt-engine))
  (with-slots (host key) engine
    (unless (stringp key)
      (if-let (apikey (or (gt-lookup-password
                           :user (if key (format "%s" key) "apikey")
                           :host (url-host (url-generic-parse-url (or host gt-chatgpt-host))))
                          gt-chatgpt-key))
          (setf key apikey)
        (user-error "You should provide a apikey for gt-chatgpt-engine")))))

(cl-defmethod gt-translate ((engine gt-chatgpt-engine) task next)
  (gt-ensure-key engine)
  (with-slots (text src tgt res) task
    (with-slots (host key model temperature) engine
      (gt-request :url (concat (or host gt-chatgpt-host) "/v1/chat/completions")
                  :headers `(("Content-Type" . "application/json")
                             ("Authorization" . ,(concat "Bearer " key)))
                  :data (json-encode
                         `((model . ,(or model gt-chatgpt-model))
                           (temperature . ,(or temperature gt-chatgpt-temperature))
                           (messages . [((role . system)
                                         (content . ,gt-chatgpt-system-prompt))
                                        ((role . user)
                                         (content . ,(if (functionp gt-chatgpt-user-prompt-template)
                                                         (funcall gt-chatgpt-user-prompt-template text tgt)
                                                       (format gt-chatgpt-user-prompt-template (alist-get tgt gt-lang-codes) text))))])))
                  :done (lambda (raw) (setf res raw) (funcall next task))
                  :fail (lambda (err) (gt-fail task err))))))

(cl-defmethod gt-parse ((_ gt-chatgpt-parser) task)
  (with-slots (res) task
    (let* ((json (json-read-from-string res))
           (str (alist-get 'content (alist-get 'message (let-alist json (aref .choices 0))))))
      (setf res str))))


;;; Text to Speech

(defcustom gt-chatgpt-tts-model "tts-1"
  "Model used by TTS of ChatGPT."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-tts-speed 1.0
  "Speech speed of return audio from ChatGPT."
  :type 'number
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-tts-voice "alloy"
  "Which voice to use by ChatGPT Speech.
alloy, echo, fable, onyx, nova, or shimmer."
  :type 'string
  :group 'go-translate-chatgpt)

(cl-defmethod gt-speak ((engine gt-chatgpt-engine) text _lang)
  (gt-ensure-key engine)
  (with-slots (host key) engine
    (gt-request :url (concat (or host gt-chatgpt-host) "/v1/audio/speech")
                :headers `(("Content-Type" . "application/json")
                           ("Authorization" . ,(concat "Bearer " key)))
                :data (json-encode
                       `((input . ,text)
                         (model . ,gt-chatgpt-tts-model)
                         (speed . ,gt-chatgpt-tts-speed)
                         (voice . ,gt-chatgpt-tts-voice)))
                :done #'gt-play-audio)))

(provide 'gt-engine-chatgpt)

;;; gt-engine-chatgpt.el ends here
