;;; gt-render-overlay.el --- Render results with overlays -*- lexical-binding: t -*-

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

;; Render results with overlays

;;; Code:

(require 'gt-core)
(require 'gt-faces)

(defclass gt-overlay-render (gt-render)
  ((type
    :initarg :type
    :initform 'after
    :type (or (member after replace help-echo before) boolean)
    :documentation "How to display the result.")
   (rfmt
    :initarg :rfmt
    :initform nil
    :documentation
    "Used to format the translation result to fit the overlay display.
See `gt-overlay-render-format' for details.")
   (sface
    :initarg :sface
    :initform 'gt-overlay-source-face
    :documentation "The propertize face of the source text after translation.
If this is nil then do nothing, if this is a face or a function return a face,
just propertize the source text with the face.")
   (rface
    :initarg :rface
    :initform 'gt-overlay-result-face
    :documentation "Result face.")
   (rdisp
    :initarg :rdisp
    :initform nil
    :documentation "Same as rface but used in `display' property.")
   (pface
    :initarg :pface
    :initform 'gt-overlay-prefix-face
    :documentation "Prefix face.")
   (pdisp
    :initarg :pdisp
    :initform nil
    :documentation "Same as pface but used in `display' property.")))

(defcustom gt-overlay-render-type 'after
  "How to display result in Overlay-Render.

If this is `help-echo', display with help echo, if this is `replace', display
by covering the source text, otherwise, display before or after the source text.

The value can be overrided by `type' slot of render."
  :type '(choice (const :tag "Repace" replace)
                 (const :tag "Help Echo" help-echo)
                 (const :tag "After" after)
                 (other :tag "Before" before))
  :group 'go-translate)

(defvar gt-overlay-render-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") #'gt-delete-render-overlays)
    (define-key map (kbd "<mouse-3>") #'gt-overlay-render-save-to-kill-ring)
    map)
  "Keymap used in overlay render.")

(defun gt-overlay-render-get-overlays (beg &optional end)
  "Return overlays made by Overlay-Render in the region from BEG to END.
If END is nil, return the overlays at BEG."
  (cl-remove-if-not (lambda (ov) (overlay-get ov 'gt))
                    (if end
                        (overlays-in beg end)
                      (overlays-at beg))))

(defun gt-delete-render-overlays (beg &optional end)
  "Delete overlays made by Overlay-Render in the region from BEG to END.
If called interactively, delete overlays around point or in region. With
`current-prefix-arg' non nil, delete all overlays in the buffer."
  (interactive (cond (current-prefix-arg (list (point-min) (point-max)))
                     ((use-region-p) (list (region-beginning) (region-end)))
                     (t (list
                         (if (and (> (point) 1) ; for overlay before point
                                  (not (gt-overlay-render-get-overlays (point)))
                                  (gt-overlay-render-get-overlays (1- (point))))
                             (1- (point))
                           (point))
                         nil))))
  (mapc #'delete-overlay (gt-overlay-render-get-overlays beg end)))

(defun gt-overlay-render-save-to-kill-ring ()
  "Copy translate content at point to kill ring for Overlay-Render."
  (interactive)
  (if-let* ((ov (car (gt-overlay-render-get-overlays (point))))
            (rs (overlay-get ov 'gt)))
      (progn (kill-new (string-trim rs))
             (message "Result already in the kill ring."))
    (user-error "No translate overlay found at point")))

(defun gt-overlay-render-format (render src res prefix)
  "Format RES for overlay RENDER.

SRC is the source text, RES and PREFIX are list extracted from translate task.

Join them to a string, format or pretty it, at last return it as the result that
used to display.

If slot `rfmt' is a string contains `%s', format every part of results with
function `format' and join them.

   (gt-overlay-render :rfmt \" <%s>\" :rface `font-lock-warning-face)

If `rfmt' is a function with solo argument, apply the function on every part of
results and join them. If with two arguments, pass source text as the first
argument. If four arguments, then chain the formatting task to the function.

   (gt-overlay-render :rfmt (lambda (w) (format \" [%s]\"))
                      :rface `font-lock-warning-face)

   (gt-overlay-render :rfmt (lambda (s w)
                              (if (length< s 3)
                                (format \"\\n- %s\")
                               (propertize w `face `font-lock-warning-face))))

Otherwise, join the results use the default logic."
  (with-slots (type rfmt rface rdisp pface pdisp) render
    (cond
     ((stringp rfmt)
      (mapconcat (lambda (r) (gt-face-lazy (format rfmt r)
                                           (gt-ensure-plain rface r)
                                           (gt-ensure-plain rdisp r)))
                 res "\n"))
     ((functionp rfmt)
      (let ((n (cdr (func-arity rfmt))))
        (if (<= n 2)
            (mapconcat (lambda (r)
                         (let ((ret (or (if (= n 2) (funcall rfmt src r) (funcall rfmt r)) r)))
                           (gt-face-lazy ret (gt-ensure-plain rface r) (gt-ensure-plain rdisp r))))
                       res "\n")
          (funcall rfmt render src res prefix))))
     (t (cl-loop with before = (when (eq type 'after)
                                 (if (gt-word-p nil src) " "
                                   (if (or (looking-at "\n\n")
                                           (save-excursion (skip-chars-forward " \n\t") (eobp)))
                                       "\n\n"
                                     "\n")))
                 with after = (when (eq type 'before) (if (gt-word-p nil src) " " "\n"))
                 for r in res for p in prefix
                 for pr = (gt-face-lazy r (gt-ensure-plain rface r) (gt-ensure-plain rdisp r))
                 for pp = (if p (concat " " (gt-face-lazy p (gt-ensure-plain pface p) (gt-ensure-plain pdisp p))))
                 collect (concat pr pp) into ret
                 finally (return (concat before (string-join ret (or before "\n\n")) after)))))))

(cl-defmethod gt-init ((render gt-overlay-render) translator)
  (with-slots (bounds) translator
    (unless (cdr bounds)
      (error "%s only works for buffer bounds, abort" (gt-str render)))
    (unless (buffer-live-p (car bounds))
      (error "Source buffer is unavailable, abort"))))

(cl-defmethod gt-output ((render gt-overlay-render) (translator gt-translator))
  (with-slots (bounds state) translator
    (when (= 3 state)
      (let ((ret (gt-extract-data render translator)))
        (when-let* ((err (cl-find-if (lambda (tr) (<= (plist-get tr :state) 1)) ret)))
          (user-error "Error in translation, %s" (plist-get err :result)))
        (with-current-buffer (car bounds)
          (cl-loop with start = (point-marker)
                   with type = (let ((type (or (oref render type) gt-overlay-render-type))
                                     (types '(help-echo replace after before)))
                                 (if (memq type types) type
                                   (intern (completing-read "Display with overlay as: " types nil t))))
                   with hooks = `((lambda (o &rest _) (delete-overlay o)))
                   for (beg . end) in (cdr bounds) for i from 0
                   do (save-excursion
                        (goto-char beg)
                        (skip-chars-forward "\n")
                        (setq beg (point))
                        (goto-char end)
                        (skip-chars-backward " \t\n")
                        (setq end (point)))
                   do (gt-delete-render-overlays beg end)
                   for src = (buffer-substring beg end)
                   do (save-excursion
                        (goto-char end)
                        (let* ((ov (make-overlay beg (point) nil t t))
                               (fres (gt-overlay-render-format
                                      render src
                                      (mapcar (lambda (tr) (nth i (plist-get tr :result))) ret)
                                      (mapcar (lambda (tr) (plist-get tr :prefix)) ret)))
                               (sface (oref render sface))
                               (sface (unless (eq type 'replace) (gt-ensure-plain sface src))))
                          (pcase type
                            ('help-echo (overlay-put ov 'help-echo fres))
                            ('after (overlay-put ov 'after-string fres))
                            ('before (overlay-put ov 'before-string fres)))
                          (overlay-put ov 'gt fres)
                          (overlay-put ov 'evaporate t)
                          (overlay-put ov 'pointer 'arrow)
                          (overlay-put ov 'insert-in-front-hooks hooks)
                          (overlay-put ov 'insert-behind-hooks hooks)
                          (overlay-put ov 'modification-hooks hooks)
                          (overlay-put ov 'keymap gt-overlay-render-map)
                          (if (eq type 'replace)
                              (progn (overlay-put ov 'display fres)
                                     (overlay-put ov 'help-echo src))
                            (if sface (overlay-put ov 'face sface)))))
                   finally (if (cddr bounds) (goto-char start)))
          (deactivate-mark)
          (message "ok."))))))

(cl-defmethod gt-str ((render gt-overlay-render))
  (format "gt-overlay-render/%s" (oref render type)))

(provide 'gt-render-overlay)

;;; gt-render-overlay.el ends here
