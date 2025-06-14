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

(require 'gt-core)

(defgroup go-translate-chatgpt nil
  "Configs for ChatGPT engine."
  :group 'go-translate)


;;; Components

(defclass gt-chatgpt-parser (gt-parser) ())

(defclass gt-chatgpt-engine (gt-api-engine)
  ((host        :initarg :host :initform nil)
   (path        :initarg :path :initform nil)
   (model       :initarg :model :initform nil)
   (temperature :initarg :temperature :initform nil)
   (key         :initarg :key :initform 'apikey) ; machine api.openai.com login apikey password ***
   (parse       :initform (gt-chatgpt-parser))))


;;; Translate

(defcustom gt-chatgpt-host "https://api.openai.com"
  "API host of ChatGPT."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-path "/v1/chat/completions"
  "API endpoint of ChatGPT."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-key nil
  "Auth Key of ChatGPT. Recommend to save in .authinfo file instead."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-model "gpt-4o-mini"
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

(defcustom gt-chatgpt-user-prompt-template "Translate the text to %s, and only return the translate result without any markers. The text is: \n%s"
  "Template for user prompt when translation.
When it is string, %s is placeholders of lang and text.
When it is function, arguments passed to it should be text and lang."
  :type '(choice string function)
  :group 'go-translate-chatgpt)

(cl-defmethod initialize-instance :after ((engine gt-chatgpt-engine) &rest _)
  (oset engine tag (concat "model:" (or (oref engine model) gt-chatgpt-model))))

(declare-function pulse-momentary-highlight-region "pulse")

(defvar gt-chatgpt-streaming-finished-hook #'pulse-momentary-highlight-region
  "The logic runs after all streams finished.
With two arguments BEG and END, which are the marker bounds of the result.")

(cl-defmethod gt-resolve-key ((engine gt-chatgpt-engine))
  (gt-with-slots-for-key (host key) engine
    (or (gt-lookup-password
         :user (if key (format "%s" key) "apikey")
         :host (url-host (url-generic-parse-url (or host gt-chatgpt-host))))
        gt-chatgpt-key (getenv "OPENAI_API_KEY"))))

(cl-defmethod gt-execute ((engine gt-chatgpt-engine) task)
  (with-slots (text src tgt res translator markers) task
    (with-slots (host path model temperature stream rate-limit) engine
      (when (and stream (cdr (oref translator text)))
        (user-error "Multiple parts not support streaming"))
      (let ((url (concat (or host gt-chatgpt-host) (or path gt-chatgpt-path)))
            (headers `(json (bear ,(encode-coding-string (gt-resolve-key engine) 'utf-8)))))
        (gt-dolist-concurrency (item text rate-limit)
          (gt-request url
            :headers headers
            :cache (if pdd-active-cacher 5)
            :data `((model . ,(or model gt-chatgpt-model))
                    (temperature . ,(or temperature gt-chatgpt-temperature))
                    (stream . ,stream)
                    (messages . [((role . system)
                                  (content . ,gt-chatgpt-system-prompt))
                                 ((role . user)
                                  (content . ,(if (functionp gt-chatgpt-user-prompt-template)
                                                  (funcall gt-chatgpt-user-prompt-template item tgt)
                                                (format gt-chatgpt-user-prompt-template (alist-get tgt gt-lang-codes) item))))]))
            :peek (when stream
                    (lambda ()
                      (if (and (> (point) 1) gt-tracking-marker)
                          (goto-char gt-tracking-marker)
                        (setq gt-tracking-marker (make-marker))
                        (set-marker gt-tracking-marker (point-min)))
                      (condition-case err
                          (let (json)
                            (while (and (re-search-forward "^data: +" nil t)
                                        (ignore-errors
                                          (setq json (json-parse-string
                                                      (decode-coding-string (buffer-substring (point) (line-end-position)) 'utf-8)
                                                      :object-type 'alist))))
                              (let* ((choice (ignore-errors (aref (alist-get 'choices json) 0)))
                                     (content (alist-get 'content (alist-get 'delta choice)))
                                     (finish (alist-get 'finish_reason choice)))
                                (goto-char (line-end-position))
                                (set-marker gt-tracking-marker (point))
                                (if (equal finish "stop")
                                    (progn (message "")
                                           (when gt-chatgpt-streaming-finished-hook
                                             (with-current-buffer (marker-buffer (car markers))
                                               (funcall gt-chatgpt-streaming-finished-hook (car markers) (cdr markers)))))
                                  (setf res (concat res content))
                                  (unless (string-blank-p (concat res))
                                    (gt-output (oref task render) task))))))
                        (error (unless (string-prefix-p "json" (format "%s" (car err)))
                                 (signal (car err) (cdr err)))))))
            :done (unless stream #'identity)
            :max-retry (if stream 0 gt-http-max-retry)))))))

(cl-defmethod gt-parse ((_ gt-chatgpt-parser) task)
  (cl-loop for item in (oref task res)
           for str = (alist-get 'content (alist-get 'message (let-alist item (aref .choices 0))))
           collect str into lst
           finally (oset task res lst)))


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

(cl-defmethod gt-speech ((engine gt-chatgpt-engine) text _lang)
  (gt-request (concat (or (oref engine host) gt-chatgpt-host) "/v1/audio/speech")
    :headers `(json (bear ,(encode-coding-string (gt-resolve-key engine) 'utf-8)))
    :data `((input . ,text)
            (model . ,gt-chatgpt-tts-model)
            (speed . ,gt-chatgpt-tts-speed)
            (voice . ,gt-chatgpt-tts-voice))
    :done #'gt-play-audio
    :cache (length text)))

(provide 'gt-engine-chatgpt)

;;; gt-engine-chatgpt.el ends here
