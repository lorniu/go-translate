;;; gt-engine-bing.el --- Microsoft Translate -*- lexical-binding: t -*-

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

;; https://bing.com/translator

;;; Code:

(require 'gt-extension)

(defclass gt-bing-parser (gt-parser) ())

(defclass gt-bing-engine (gt-web-engine)
  ((tag       :initform 'Bing)
   (base-url  :initform "https://www.bing.com")
   (sub-url   :initform "/ttranslatev3")

   (tld-url   :initform nil)
   (ig        :initform nil)
   (key       :initform nil)
   (token     :initform nil)
   (last-time :initform nil)
   (expired-time :initform (* 30 60))

   (ttsk-url  :initform "/tfetspktok")
   (tts-url   :initform "https://%s.tts.speech.microsoft.com/cognitiveservices/v1")
   (tts-tpl   :initform "<speak version='1.0' xml:lang='%s'><voice xml:lang='%s' xml:gender='Female' name='%s'><prosody rate='-20.00%%'>%s</prosody></voice></speak>")

   (parse     :initform (gt-bing-parser))))


;;; Engine

(defvar gt-bing-extra-langs-mapping '(("zh" . "zh-Hans")))

(defvar gt-bing-token-maybe-invalid nil)

(defun gt-bing-get-lang (lang)
  (or (cdr-safe (assoc lang gt-bing-extra-langs-mapping)) lang))

(defun gt-bing-token-available-p (engine)
  (with-slots (token key ig last-time expired-time) engine
    (and token key ig last-time
         (not gt-bing-token-maybe-invalid)
         (< (- (time-to-seconds) last-time) expired-time))))

(defun gt-bing-with-token (engine done fail)
  (declare (indent 1))
  (with-slots (token key ig base-url) engine
    (if (gt-bing-token-available-p engine)
        (funcall done)
      (gt-request :url (concat base-url "/translator")
                  :done (lambda (raw)
                          (with-temp-buffer
                            (insert raw)
                            (goto-char (point-min))
                            (let (key token ig tld)
                              (re-search-forward "curUrl=.*/\\([a-z]+\\.bing.com\\)")
                              (setq tld (match-string 1))
                              (re-search-forward "\"ig\":\"\\([^\"]+\\).*params_AbusePreventionHelper = \\[\\([0-9]+\\),\"\\([^\"]+\\)")
                              (setq ig (match-string 1) key (match-string 2) token (match-string 3))
                              (oset engine ig ig)
                              (oset engine key key)
                              (oset engine token token)
                              (oset engine last-time (time-to-seconds))
                              (oset engine tld-url (concat "https://" tld))
                              (setq gt-bing-token-maybe-invalid nil)
                              (gt-log 'bing (format "url: %s\nkey: %s\ntoken: %s\nig: %s" tld key token ig))
                              (funcall done))))
                  :fail fail))))

(cl-defmethod gt-translate ((engine gt-bing-engine) task next)
  (gt-bing-with-token engine
    (lambda ()
      (with-slots (text src tgt res) task
        (with-slots (tld-url sub-url token key ig) engine
          (gt-request :url (format "%s%s?isVertical=1&IG=%s&IID=translator.5022.1" tld-url sub-url ig)
                      :headers `(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8"))
                      :data `(("fromLang" . ,(gt-bing-get-lang src))
                              ("to"       . ,(gt-bing-get-lang tgt))
                              ("text"     . ,text)
                              ("key"      . ,key)
                              ("token"    . ,token))
                      :done (lambda (raw) (setf res raw) (funcall next task))
                      :fail (lambda (err)
                              (gt-fail task (pcase (car-safe (cdr-safe err))
                                              (429 "[429] Too many requests! Please try later")
                                              (_ err))))))))
    (lambda (err)
      (gt-fail task (format "Bing failed to take token: %s" err)))))


;;; TTS

(defvar gt-bing-tts-langs-mapping '(("zh" . ("zh-CN" . "zh-CN-XiaoxiaoNeural"))
                                    ("en" . ("en-US" . "en-US-AriaNeural"))
                                    ("fr" . ("fr-CA" . "fr-CA-SylvieNeural"))
                                    ("de" . ("de-DE" . "de-DE-KatjaNeural"))))

(defun gt-bing-tts-payload (engine lang text)
  (with-slots (tts-tpl) engine
    (let (l n (mt (assoc lang gt-bing-tts-langs-mapping)))
      (if mt (setq l (cadr mt) n (cddr mt))
        (user-error "Add the mapping of your language into `gt-bing-tts-langs-mapping' :)"))
      (format tts-tpl l l n (encode-coding-string text 'utf-8)))))

(cl-defmethod gt-tts ((engine gt-bing-engine) text lang)
  (message "Requesting from bing.com...")
  (gt-bing-with-token engine
    (lambda ()
      (with-slots (tld-url sub-url token key ig parse ttsk-url tts-url tts-tpl) engine
        (gt-request :url (format "%s%s?isVertical=1&IG=%s&IID=translator.5022.2" tld-url ttsk-url ig)
                    :headers '(("content-type" . "application/x-www-form-urlencoded"))
                    :data `(("token" . ,token) ("key" . ,key))
                    :done (lambda (raw)
                            (let* ((json (json-read-from-string raw))
                                   (token (cdr (assoc 'token json)))
                                   (region (cdr (assoc 'region json))))
                              (gt-log 'bing-tts (format "token: %s\nregion: %s" token region))
                              (gt-request :url (format tts-url region)
                                          :data (gt-bing-tts-payload engine lang text)
                                          :headers `(("content-type" . "application/ssml+xml")
                                                     ("authorization" . ,(format "Bearer %s" token))
                                                     ("x-microsoft-outputformat" . "audio-16khz-32kbitrate-mono-mp3"))
                                          :done (lambda (raw)
                                                  (with-temp-buffer
                                                    (insert raw)
                                                    (gt-tts-speak-buffer-data)))
                                          :fail (lambda (err)
                                                  (message "[BING-TTS] error when try to play: %s" err)))))
                    :fail (lambda (err) (message "[BING-TTS] error in request, %s" err)))))
    (lambda (err) (message "[BING-TTS] Failed to get token (%s)" err))))


;;; Parser

(cl-defmethod gt-parse ((_ gt-bing-parser) task)
  (with-slots (res err) task
    (if-let* ((json (json-read-from-string (decode-coding-string res 'utf-8)))
              (result (ignore-errors (cdr (assoc 'text (aref (cdr (assoc 'translations (aref json 0))) 0))))))
        (setf res result)
      (setq gt-bing-token-maybe-invalid t) ; refresh token when error occurred
      (setf err res)
      (error "error %s" res))))

(provide 'gt-engine-bing)

;;; gt-engine-bing.el ends here
