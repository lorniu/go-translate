;;; go-translate-tests.el --- Tests for go-translate.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Unit Tests

;;; Code:

(require 'ert)
(require 'go-translate)

;;; Tests

(ert-deftest picker-pick-paths ()
  (let ((p1 (gts-prompt-picker))
        (p2 (gts-noprompt-picker :single t))
        (gts-translate-list '(("en" "zh") ("en" "fr"))))
    (let (gts-picker-last-path)
      (should (equal (gts-paths p1) '(("en" . "zh") ("zh" . "en") ("en" . "fr") ("fr" . "en"))))
      (should (equal (gts-paths p2) '(("en" . "zh") ("en" . "fr")))))
    (let ((gts-picker-last-path (cons "en" "fr")))
      (should (equal (gts-paths p1) '(("en" . "fr") ("en" . "zh") ("zh" . "en") ("fr" . "en"))))
      (should (equal (gts-paths p2) '(("en" . "fr") ("en" . "zh")))))
    (let ((gts-picker-last-path (cons "fr" "en")))
      (should (equal (gts-paths p1) '(("fr" . "en") ("en" . "zh") ("zh" . "en") ("en" . "fr"))))
      (should (equal (gts-paths p2) '(("en" . "zh") ("en" . "fr")))))
    (let ((gts-picker-last-path (cons "zh" "it")))
      (should (equal (gts-paths p1) '(("en" . "zh") ("zh" . "en") ("en" . "fr") ("fr" . "en"))))
      (should (equal (gts-paths p2) '(("en" . "zh") ("en" . "fr")))))))

(provide 'go-translate-tests)

;;; go-translate-tests.el ends here
