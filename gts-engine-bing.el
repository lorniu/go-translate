;;; gts-engine-bing.el --- Microsoft Translate -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; https://bing.com/translator

;;; Code:

(require 'gts-implements)

(defclass gts-bing-parser (gts-parser) ())

(defclass gts-bing-engine (gts-engine)
  ((tag       :initform "Bing")
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

   (parser    :initform (gts-bing-parser))))


;;; Engine

(defvar gts-bing-extra-langs-mapping '(("zh" . "zh-Hans")))

(defvar gts-bing-token-maybe-invalid nil)

(defun gts-bing-get-lang (lang)
  (or (cdr-safe (assoc lang gts-bing-extra-langs-mapping)) lang))

(defun gts-bing-token-available-p (engine)
  (with-slots (token key ig last-time expired-time) engine
    (and token key ig last-time
         (not gts-bing-token-maybe-invalid)
         (< (- (time-to-seconds) last-time) expired-time))))

(defun gts-bing-with-token (engine done fail)
  (declare (indent 1))
  (with-slots (token key ig base-url) engine
    (if (gts-bing-token-available-p engine)
        (funcall done)
      (gts-request :url (concat base-url "/translator")
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
                               (setq gts-bing-token-maybe-invalid nil)
                               (gts-log 'bing (format "url: %s\nkey: %s\ntoken: %s\nig: %s" tld key token ig))
                               (funcall done))))
                   :fail fail))))

(cl-defmethod gts-translate ((engine gts-bing-engine) task next)
  (gts-bing-with-token engine
    (lambda ()
      (with-slots (text src trg) task
        (with-slots (tld-url sub-url token key ig) engine
          (gts-request :url (format "%s%s?isVertical=1&IG=%s&IID=translator.5022.1" tld-url sub-url ig)
                       :headers `(("Content-Type" . "application/x-www-form-urlencoded;charset=UTF-8"))
                       :data `(("fromLang" . ,(gts-bing-get-lang src))
                               ("to"       . ,(gts-bing-get-lang trg))
                               ("text"     . ,text)
                               ("key"      . ,key)
                               ("token"    . ,token))
                       :done (lambda (raw) (funcall next task raw))
                       :fail (lambda (err)
                               (gts-fail task
                                 (cond ((ignore-errors (= (caddar err) 429))
                                        "[429] Too many requests! Please retry later")
                                       (t err))))))))
    (lambda (err)
      (gts-fail task (format "Take token failed, %s" err)))))


;;; TTS

(defvar gts-bing-tts-langs-mapping '(("zh" . ("zh-CN" . "zh-CN-XiaoxiaoNeural"))
                                     ("en" . ("en-US" . "en-US-AriaNeural"))
                                     ("fr" . ("fr-CA" . "fr-CA-SylvieNeural"))
                                     ("de" . ("de-DE" . "de-DE-KatjaNeural"))))

(defun gts-bing-tts-payload (engine lang text)
  (with-slots (tts-tpl) engine
    (let (l n (mt (assoc lang gts-bing-tts-langs-mapping)))
      (if mt (setq l (cadr mt) n (cddr mt))
        (user-error "Add the mapping of your language into `gts-bing-tts-langs-mapping' :)"))
      (format tts-tpl l l n (encode-coding-string text 'utf-8)))))

(cl-defmethod gts-tts ((engine gts-bing-engine) text lang)
  (message "Requesting from bing.com...")
  (gts-bing-with-token engine
    (lambda ()
      (with-slots (tld-url sub-url token key ig parser ttsk-url tts-url tts-tpl) engine
        (gts-request :url (format "%s%s?isVertical=1&IG=%s&IID=translator.5022.2" tld-url ttsk-url ig)
                     :headers '(("content-type" . "application/x-www-form-urlencoded"))
                     :data `(("token" . ,token) ("key" . ,key))
                     :done (lambda (raw)
                             (let* ((json (json-read-from-string raw))
                                    (token (cdr (assoc 'token json)))
                                    (region (cdr (assoc 'region json))))
                               (gts-log 'bing-tts (format "token: %s\nregion: %s" token region))
                               (gts-request :url (format tts-url region)
                                            :data (gts-bing-tts-payload engine lang text)
                                            :headers `(("content-type" . "application/ssml+xml")
                                                       ("authorization" . ,(format "Bearer %s" token))
                                                       ("x-microsoft-outputformat" . "audio-16khz-32kbitrate-mono-mp3"))
                                            :done (lambda (raw)
                                                    (with-temp-buffer
                                                      (insert raw)
                                                      (gts-tts-speak-buffer-data)))
                                            :fail (lambda (err)
                                                    (message "[BING-TTS] error when try to play: %s" err)))))
                     :fail (lambda (err) (message "[BING-TTS] error in request, %s" err)))))
    (lambda (err) (message "[BING-TTS] Failed to get token (%s)" err))))


;;; Parser

(cl-defmethod gts-parse ((_ gts-bing-parser) _task raw)
  (if-let* ((json (json-read-from-string raw))
            (result (ignore-errors (cdr (assoc 'text (aref (cdr (assoc 'translations (aref json 0))) 0))))))
      (cl-values result)
    (setq gts-bing-token-maybe-invalid t) ; refresh token when error occurred
    (error "error %s" raw)))

(provide 'gts-engine-bing)

;;; gts-engine-bing.el ends here
