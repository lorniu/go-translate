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
  ((host          :initarg :host :initform nil)
   (path          :initarg :path :initform nil)
   (model         :initarg :model :initform nil)
   (root          :initarg :root :initform nil) ; gt-chatgpt-system-prompt
   (prompt        :initarg :prompt :initform nil) ; gt-chatgpt-user-prompt-template
   (temperature   :initarg :temperature :initform nil)
   (extra-options :initarg :extra-options :initform nil)
   (key           :initarg :key :initform 'apikey) ; machine api.openai.com login apikey password ***
   (timeout       :initarg :timeout :initform 60)
   (tts-host      :initarg :tts-host :initform nil)
   (tts-path      :initarg :tts-path :initform "/v1/audio/speech")
   (tts-model     :initarg :tts-model :initform "gpt-4o-mini-tts")
   (parse         :initform (gt-chatgpt-parser))))


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

(defcustom gt-chatgpt-temperature 1
  "Temperature of ChatGPT."
  :type 'natnum
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-extra-options nil
  "Extra options send with completion API.

Example: \\='((n . 1) (max_completion_tokens . 10))"
  :type 'alist
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-system-prompt "You are a translation assistant"
  "System prompt send to server when translation."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-user-prompt-template "Translate the text to {{lang}}, and only return the translate result without any markers. The text is: \n{{text}}"
  "Template for user prompt when translation.
When it is string, {{lang}} and {{text}} is placeholders of lang and text.
When it is function, arguments passed to it should be text and lang."
  :type '(choice string function)
  :group 'go-translate-chatgpt)

(declare-function pulse-momentary-highlight-region "pulse")

(defvar gt-chatgpt-streaming-finished-hook #'pulse-momentary-highlight-region
  "The logic runs after all streams finished.
With two arguments BEG and END, which are the marker bounds of the result.")

(cl-defmethod gt-tag ((engine gt-chatgpt-engine))
  (concat "model:" (or (oref engine model) gt-chatgpt-model)))

(cl-defmethod gt-resolve-key ((engine gt-chatgpt-engine))
  (gt-with-slots-for-key (host key) engine
    (or (gt-lookup-password
         :user (if key (format "%s" key) "apikey")
         :host (url-host (url-generic-parse-url (or host gt-chatgpt-host))))
        gt-chatgpt-key (getenv "OPENAI_API_KEY"))))

(cl-defun gt-chatgpt-send (prompt &key root url model temperature extra-options key stream timeout sync)
  (declare (indent 1))
  (gt-request (or url (concat gt-chatgpt-host gt-chatgpt-path))
    :sync sync
    :cache (if pdd-active-cacher 5)
    :headers `(json (bear ,(encode-coding-string (or key (gt-resolve-key (gt-chatgpt-engine))) 'utf-8)))
    :data `((model . ,(or model gt-chatgpt-model))
            (temperature . ,(or temperature gt-chatgpt-temperature))
            (stream . ,stream)
            (messages . ,(if (consp prompt)
                             `[((role . system) (content . ,(or root gt-chatgpt-system-prompt)))
                               ,@prompt]
                           `[((role . system) (content . ,(or root gt-chatgpt-system-prompt)))
                             ((role . user) (content . ,prompt))]))
            ,@(or extra-options gt-chatgpt-extra-options))
    :peek (when (and stream (functionp pdd-peek)) pdd-peek)
    :done (unless stream #'identity)
    :timeout timeout
    :max-retry (if stream 0 gt-http-max-retry)))

(cl-defmacro gt-chatgpt-with-stream-buffer ((content finish) &rest args)
  "Help to parse the stream buffer and output every hunk.

(gt-chatgpt-with-stream-buffer (c f) (xxx) :finish (yyy) t)"
  (declare (indent 1))
  (let* ((pos (cl-position :finish args))
         (args (if pos
                   (cons (cl-subseq args 0 pos) (cl-subseq args (1+ pos)))
                 (cons args nil)))
         (final '(when gt-chatgpt-streaming-finished-hook
                   (with-current-buffer (marker-buffer (car markers))
                     (funcall gt-chatgpt-streaming-finished-hook (car markers) (cdr markers))))))
    `(condition-case err
         (let (json)
           (if (and (> (point) 1) gt-tracking-marker)
               (goto-char gt-tracking-marker)
             (setq gt-tracking-marker (make-marker))
             (set-marker gt-tracking-marker (point-min)))
           (while (and (re-search-forward "^data: +" nil t)
                       (ignore-errors
                         (setq json (json-parse-string
                                     (decode-coding-string (buffer-substring (point) (line-end-position)) 'utf-8)
                                     :object-type 'alist))))
             (let* ((content-and-finish (gt-parse parse json))
                    (,content (car content-and-finish))
                    (,finish (cadr content-and-finish)))
               (set-marker gt-tracking-marker (line-end-position))
               (if (equal ,finish "stop")
                   ,(if (cdr args)
                        `(progn
                           ,@(cl-loop for arg in (cdr args)
                                      if (eq arg t) collect final
                                      else collect arg))
                      `(progn (message "") ,final))
                 ,@(car args)))))
       (error (unless (string-prefix-p "json" (format "%s" (car err)))
                (signal (car err) (cdr err)))))))

(cl-defmethod gt-execute ((engine gt-chatgpt-engine) task)
  (with-slots (text src tgt res translator markers) task
    (with-slots (host path model prompt root temperature extra-options timeout stream rate-limit parse) engine
      (when (and stream (cdr (oref translator text)))
        (user-error "Multiple parts not support streaming"))
      (let ((url (concat (or host gt-chatgpt-host) (or path gt-chatgpt-path)))
            (prompt (or prompt gt-chatgpt-user-prompt-template))
            (pdd-peek (when stream
                        (lambda ()
                          (gt-chatgpt-with-stream-buffer (content finish)
                            (setf res (concat res content))
                            (unless (string-blank-p (concat res))
                              (gt-output (oref task render) task)))))))
        (gt-dolist-concurrency (item text rate-limit)
          (gt-chatgpt-send (if (functionp prompt)
                               (pdd-funcall prompt (list item tgt))
                             (let ((s (string-replace "{{lang}}" (alist-get tgt gt-lang-codes) prompt)))
                               (string-replace "{{text}}" item s)))
            :url url :model model :temperature temperature :extra-options extra-options
            :key (gt-resolve-key engine) :root root :timeout timeout :stream stream))))))

(cl-defmethod gt-parse ((_ gt-chatgpt-parser) (task gt-task))
  (cl-loop for item in (oref task res)
           for msg = (alist-get 'message (let-alist item (aref .choices 0)))
           for rc = (alist-get 'reasoning_content msg) ; compact with DeepSeek r1
           for content = (alist-get 'content msg)
           collect (concat (if (stringp rc) (propertize rc 'face 'gt-chatgpt-reasoning-face)) content) into lst
           finally (oset task res lst)))

(cl-defmethod gt-parse ((_ gt-chatgpt-parser) json-hunk) ; parse for streaming output
  (let* ((choice (ignore-errors (aref (alist-get 'choices json-hunk) 0)))
         (rc (alist-get 'reasoning_content (alist-get 'delta choice))) ; maybe :null
         (content (alist-get 'content (alist-get 'delta choice)))
         (finish (alist-get 'finish_reason choice)))
    (list (if (stringp rc)
              (propertize rc 'face 'gt-chatgpt-reasoning-face) ; compact with DeepSeek r1
            (if (stringp content) content ""))
          finish)))


;;; Text to Speech

(defcustom gt-chatgpt-tts-voice "onyx"
  "Which voice to use by ChatGPT Speech.
alloy, echo, fable, onyx, nova, or shimmer."
  :type 'string
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-tts-speed 1.0
  "Speech speed of return audio from ChatGPT."
  :type 'number
  :group 'go-translate-chatgpt)

(defcustom gt-chatgpt-tts-extra-options nil
  "Extra options provided to tts engine."
  :type 'alist
  :group 'go-translate-chatgpt)

(cl-defmethod gt-speech ((engine gt-chatgpt-engine) text _lang &optional play-fn)
  (with-slots (host tts-host tts-path tts-model) engine
    (let ((data `((input . ,text)
                  (model . ,tts-model)
                  (voice . ,gt-chatgpt-tts-voice)
                  (speed . ,gt-chatgpt-tts-speed)
                  ,@gt-chatgpt-tts-extra-options)))
      (pdd-then
          (gt-request (concat (or tts-host host gt-chatgpt-host) tts-path)
            :headers `(json (bear ,(encode-coding-string (gt-resolve-key engine) 'utf-8)))
            :data data :done #'identity
            :cache `(,gt-tts-cache-ttl chatgpt-tts ,(prin1-to-string data) (store . gt-tts-cache-store)))
        (or play-fn #'gt-play-audio)))))

(provide 'gt-engine-chatgpt)

;;; gt-engine-chatgpt.el ends here
