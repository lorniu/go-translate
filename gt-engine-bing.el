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

(require 'gt-core)

(defgroup go-translate-bing nil
  "Configs for Bing engine."
  :group 'go-translate)



(defclass gt-bing-parser (gt-parser) ())

(defclass gt-bing-engine (gt-web-engine)
  ((tag       :initform 'Bing)
   (host      :initform "https://www.bing.com")
   (host-tld  :initform nil)
   (ig        :initform nil)
   (key       :initform nil)
   (token     :initform nil)
   (last-time :initform nil)
   (expired-time :initform (* 30 60))
   (delimit   :initform t)
   (parse     :initform (gt-bing-parser))))


;;; Translate

(defvar gt-bing-extra-langs-mapping '((zh . "zh-Hans")))

(defvar gt-bing-token-maybe-invalid nil)

(defun gt-bing-lang (lang)
  (or (cdr-safe (assoc lang gt-bing-extra-langs-mapping)) lang))

(defun gt-bing-token (engine)
  (with-slots (ig key token host host-tld last-time expired-time) engine
    (if (and host-tld token key ig last-time (not gt-bing-token-maybe-invalid)
             (< (- (time-to-seconds) last-time) expired-time))
        (pdd-resolve)
      (gt-request (concat host "/translator")
        :cache nil
        :done (lambda (raw)
                (with-temp-buffer
                  (save-excursion (insert raw))
                  (re-search-forward "IG:\"\\([A-Za-z0-9]+\\)\"")
                  (setf ig (match-string 1))
                  (re-search-forward "curUrl=\"\\(http[a-z]*\\).*?\\([a-zA-Z]+\\.bing.com\\)")
                  (setf host-tld (concat (match-string 1) "://" (match-string 2)))
                  (re-search-forward "var params_AbusePreventionHelper = \\[\\([0-9]+\\), *\"\\([^\"]+\\)")
                  (setf key (match-string 1) token (match-string 2))
                  (setf last-time (time-to-seconds))
                  (setq gt-bing-token-maybe-invalid nil)
                  (gt-log 'bing (format "url: %s\nkey: %s\ntoken: %s\nig: %s" host-tld key token ig))))))))

(cl-defmethod gt-execute ((engine gt-bing-engine) task)
  (pdd-then (gt-bing-token engine)
    (lambda ()
      (with-slots (text src tgt) task
        (with-slots (host-tld ig key token) engine
          (gt-request (format "%s/ttranslatev3" host-tld)
            :cache (if pdd-active-cacher `(t bing ,src ,tgt ,text))
            :headers `(www-url-u8)
            :params `((isVertical . 1) (IG . ,ig) (IID . "translator.5022.1"))
            :data `(("key"      . ,key)
                    ("token"    . ,token)
                    ("text"     . ,text)
                    ("fromLang" . ,(gt-bing-lang src))
                    ("to"       . ,(gt-bing-lang tgt)))))))
    (lambda (err)
      (signal (car err)
              (pcase (car-safe (cdr-safe err))
                (429 '("[429] Too many requests! Please try later"))
                (_ (cdr err)))))))

(cl-defmethod gt-parse ((_ gt-bing-parser) task)
  (with-slots (res err) task
    (if-let* ((result (ignore-errors (cdr (assoc 'text (aref (cdr (assoc 'translations (aref res 0))) 0))))))
        (setf res result)
      (setq gt-bing-token-maybe-invalid t) ; refresh token when error occurred
      (setf err res)
      (error "error %s" res))))


;;; Text to Speech

(defcustom gt-bing-tts-speed 1.0
  "Playing speed of TTS audio, 1.0 is normal speed."
  :type 'number
  :group 'go-translate-bing)

(defvar gt-bing-tts-mapping '((zh . ("zh-CN" "Female" "zh-CN-XiaoxiaoNeural"))
                              (en . ("en-US" "Female" "en-US-AriaNeural"))
                              (af . ("af-ZA" "Female" "af-ZA-AdriNeural"))
                              (am . ("am-ET" "Female" "am-ET-MekdesNeural"))
                              (ar . ("ar-SA" "Male" "ar-SA-HamedNeural"))
                              (bn . ("bn-IN" "Female" "bn-IN-TanishaaNeural"))
                              (bg . ("bg-BG" "Male" "bg-BG-BorislavNeural"))
                              (ca . ("ca-ES" "Female" "ca-ES-JoanaNeural"))
                              (cs . ("cs-CZ" "Male" "cs-CZ-AntoninNeural"))
                              (cy . ("cy-GB" "Female" "cy-GB-NiaNeural"))
                              (da . ("da-DK" "Female" "da-DK-ChristelNeural"))
                              (de . ("de-DE" "Female" "de-DE-KatjaNeural"))
                              (el . ("el-GR" "Male" "el-GR-NestorasNeural"))
                              (es . ("es-ES" "Female" "es-ES-ElviraNeural"))
                              (et . ("et-EE" "Female" "et-EE-AnuNeural"))
                              (fa . ("fa-IR" "Female" "fa-IR-DilaraNeural"))
                              (fi . ("fi-FI" "Female" "fi-FI-NooraNeural"))
                              (fr . ("fr-FR" "Female" "fr-FR-DeniseNeural"))
                              (ga . ("ga-IE" "Female" "ga-IE-OrlaNeural"))
                              (gu . ("gu-IN" "Female" "gu-IN-DhwaniNeural"))
                              (he . ("he-IL" "Male" "he-IL-AvriNeural"))
                              (hi . ("hi-IN" "Female" "hi-IN-SwaraNeural"))
                              (hr . ("hr-HR" "Male" "hr-HR-SreckoNeural"))
                              (hu . ("hu-HU" "Male" "hu-HU-TamasNeural"))
                              (id . ("id-ID" "Male" "id-ID-ArdiNeural"))
                              (is . ("is-IS" "Female" "is-IS-GudrunNeural"))
                              (it . ("it-IT" "Male" "it-IT-DiegoNeural"))
                              (ja . ("ja-JP" "Female" "ja-JP-NanamiNeural"))
                              (kk . ("kk-KZ" "Female" "kk-KZ-AigulNeural"))
                              (km . ("km-KH" "Female" "km-KH-SreymomNeural"))
                              (kn . ("kn-IN" "Female" "kn-IN-SapnaNeural"))
                              (ko . ("ko-KR" "Female" "ko-KR-SunHiNeural"))
                              (lo . ("lo-LA" "Female" "lo-LA-KeomanyNeural"))
                              (lv . ("lv-LV" "Female" "lv-LV-EveritaNeural"))
                              (lt . ("lt-LT" "Female" "lt-LT-OnaNeural"))
                              (mk . ("mk-MK" "Female" "mk-MK-MarijaNeural"))
                              (ml . ("ml-IN" "Female" "ml-IN-SobhanaNeural"))
                              (mr . ("mr-IN" "Female" "mr-IN-AarohiNeural"))
                              (ms . ("ms-MY" "Male" "ms-MY-OsmanNeural"))
                              (mt . ("mt-MT" "Female" "mt-MT-GraceNeural"))
                              (my . ("my-MM" "Female" "my-MM-NilarNeural"))
                              (nl . ("nl-NL" "Female" "nl-NL-ColetteNeural"))
                              (nb . ("nb-NO" "Female" "nb-NO-PernilleNeural"))
                              (pl . ("pl-PL" "Female" "pl-PL-ZofiaNeural"))
                              (ps . ("ps-AF" "Female" "ps-AF-LatifaNeural"))
                              (pt . ("pt-BR" "Female" "pt-BR-FranciscaNeural"))
                              (ro . ("ro-RO" "Male" "ro-RO-EmilNeural"))
                              (ru . ("ru-RU" "Female" "ru-RU-DariyaNeural"))
                              (sk . ("sk-SK" "Male" "sk-SK-LukasNeural"))
                              (sl . ("sl-SI" "Male" "sl-SI-RokNeural"))
                              (sv . ("sv-SE" "Female" "sv-SE-SofieNeural"))
                              (ta . ("ta-IN" "Female" "ta-IN-PallaviNeural"))
                              (te . ("te-IN" "Male" "te-IN-ShrutiNeural"))
                              (th . ("th-TH" "Male" "th-TH-NiwatNeural"))
                              (tr . ("tr-TR" "Female" "tr-TR-EmelNeural"))
                              (uk . ("uk-UA" "Female" "uk-UA-PolinaNeural"))
                              (ur . ("ur-IN" "Female" "ur-IN-GulNeural"))
                              (uz . ("uz-UZ" "Female" "uz-UZ-MadinaNeural"))
                              (vi . ("vi-VN" "Male" "vi-VN-NamMinhNeural"))
                              (yue . ("zh-HK" "Female" "zh-HK-HiuGaaiNeural"))))

(cl-defmethod gt-bing-tts-payload (lang text)
  (let ((lm (assoc lang gt-bing-tts-mapping)))
    (unless lm (user-error "Add the mapping of your language into `gt-bing-tts-langs-mapping' :)"))
    (format "<speak version='1.0' xml:lang='%s'><voice xml:lang='%s' xml:gender='%s' name='%s'><prosody rate='%s'>%s</prosody></voice></speak>"
            (cadr lm) (caddr lm) (cadr lm) (cadddr lm)
            gt-bing-tts-speed (encode-coding-string text 'utf-8))))

(cl-defmethod gt-speech ((engine gt-bing-engine) text lang &optional play-fn)
  (with-slots (host host-tld ig key token) engine
    (message "Speaking from %s..." (oref engine host))
    (pdd-chain (gt-bing-token engine)
      (lambda ()
        (gt-request (format "%s/tfettts" host-tld)
          :headers '(www-url)
          :params `((isVertical . 1) (IG . ,ig) (IID . "translator.5022.2"))
          :data `(("token" . ,token) ("key" . ,key) ("ssml" . ,(gt-bing-tts-payload lang text)))
          :done #'identity
          :cache `(,gt-tts-cache-ttl bing-tts ,text ,lang (store . gt-tts-cache-store))))
      (or play-fn #'gt-play-audio)
      :fail (lambda (err) (user-error "[BING-TTS] error: %s" err)))))

(provide 'gt-engine-bing)

;;; gt-engine-bing.el ends here
