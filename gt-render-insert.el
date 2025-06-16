;;; gt-render-insert.el --- Insert result at point -*- lexical-binding: t -*-

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

;; Insert result before/after/replace the source text.

;;; Code:

(require 'gt-core)
(require 'gt-faces)

(defclass gt-insert-render (gt-render)
  ((type
    :initarg :type
    :initform 'after
    :type (or (member after replace) boolean)
    :documentation "How to insert the result.")
   (rfmt
    :initarg :rfmt
    :initform nil
    :documentation "Used to format the result string for insertion.
See `gt-insert-render-format' for details.")
   (sface
    :initarg :sface
    :initform nil
    :documentation "The propertize face of the source text after translation.
If this is nil then do nothing, if this is a face or a function return a face,
just propertize the source text with the face.")
   (rface
    :initarg :rface
    :initform nil
    :documentation "Result face.")))

(defcustom gt-insert-render-type 'after
  "Where to insert the result in Insert-Render.

If this is `replace', insert the result by taking place the source text,
otherwise, insert after the source text.

The value can be overrided by `type' slot of render."
  :type '(choice (const :tag "Repace" replace)
                 (other :tag "Insert after" after))
  :group 'go-translate)

(defcustom gt-insert-render-output-hook nil
  "Hook run after output finished in Insert-Render.
With current translator as the only argument."
  :type 'hook
  :group 'go-translate)

(defun gt-insert-render-format (render src res)
  "Format RES for insert RENDER.

SRC is the source text, RES is list extracted from translate task.

Join them to a string, format or pretty it, at last return it as the result that
used to insert.

If slot `rfmt' is a string contains `%s', format every part of results with
function `format' and join them.

   (gt-insert-render :rfmt \" <%s>\" :rface `font-lock-warning-face)

If `rfmt' is a function with solo argument, apply the function on every part of
results and join them. If with two arguments, pass source text as the first
argument. If four arguments, then chain the formatting task to the function.

   (gt-insert-render :rfmt (lambda (w) (format \" [%s]\"))
                     :rface `font-lock-warning-face)

   (gt-insert-render :rfmt (lambda (s w)
                             (if (length< s 3)
                               (format \"\\n- %s\" w)
                              (propertize w `face `font-lock-warning-face))))

Otherwise, join the results use the default logic."
  (with-slots (type rfmt rface) render
    (cond
     ((stringp rfmt)
      (mapconcat (lambda (r) (gt-face-lazy (format rfmt r) (gt-ensure-plain rface r))) res "\n"))
     ((functionp rfmt)
      (let ((n (cdr (func-arity rfmt))))
        (if (<= n 2)
            (mapconcat (lambda (r)
                         (let ((ret (or (if (= n 2) (funcall rfmt src r) (funcall rfmt r)) r)))
                           (gt-face-lazy ret (gt-ensure-plain rface r))))
                       res "\n")
          (funcall rfmt render src res))))
     (t (let* ((before (when (eq type 'after)
                         (if (string-blank-p src) ""
                           (if (gt-word-p nil src) " "
                             (if (or (looking-at "\n\n")
                                     (save-excursion (skip-chars-forward " \n\t") (eobp)))
                                 "\n\n"
                               "\n")))))
               (facefn (lambda (r) (gt-face-lazy r (gt-ensure-plain rface r))))
               (separator (if (= (length before) 0) "\n\n" before)))
          (concat before (mapconcat facefn res separator)))))))

(cl-defmethod gt-init ((render gt-insert-render) translator)
  (with-slots (text bounds tasks) translator
    (unless (buffer-live-p (car bounds))
      (error "Source buffer is unavailable, abort"))
    (when (with-current-buffer (car bounds) buffer-read-only)
      (error "Source buffer is readonly, can not insert"))
    ;; only simple translate (single part, single target) allowing streaming output
    ;; take bounds as output markers for it
    (when (and (not (cdr text))
               (not (cdr tasks))
               (oref (oref (car tasks) engine) stream))
      (with-slots (markers engine) (car tasks)
        (let ((type (oref render type))
              (beg (if-let* ((p (caadr bounds)))
                       (save-excursion
                         (goto-char p)
                         (skip-chars-forward "\n" (cdadr bounds))
                         (point))
                     (point)))
              (end (if-let* ((p (cdadr bounds)))
                       (save-excursion
                         (goto-char p)
                         (skip-chars-backward " \t\n" (caadr bounds))
                         (point))
                     (point))))
          (setf markers (cons (set-marker (make-marker) (if (eq type 'after) end beg))
                              (set-marker (make-marker) end))))))))

(cl-defmethod gt-output ((render gt-insert-render) (translator gt-translator))
  (with-slots (bounds state) translator
    (when (= 3 state)
      (let ((ret (gt-extract-data render translator)))
        (when-let* ((err (cl-find-if (lambda (tr) (<= (plist-get tr :state) 1)) ret)))
          (user-error "Error in translation, %s" (plist-get err :result)))
        (with-current-buffer (car bounds)
          (with-slots (rfmt sface) render
            (cl-loop with start = (point-marker)
                     with bds = (mapcar (lambda (bd)
                                          (cons (save-excursion
                                                  (goto-char (car bd))
                                                  (skip-chars-forward "\n")
                                                  (point-marker))
                                                (save-excursion
                                                  (goto-char (cdr bd))
                                                  (skip-chars-backward " \t\n")
                                                  (point-marker))))
                                        (cdr bounds))
                     with type = (let ((type (or (oref render type) gt-insert-render-type))
                                       (types '(after replace)))
                                   (if (member type types) type
                                     (intern (completing-read "Insert text as: " types nil t))))
                     with hash = (when (and (eq type 'replace) (not (buffer-modified-p)))
                                   (buffer-hash))
                     with len = (length (plist-get (car ret) :result))
                     for i from 0 below len
                     for (beg . end) = (if bds (nth i bds)
                                         (if (use-region-p)
                                             (car (region-bounds))
                                           (cons (point) (point))))
                     for src = (if bds (buffer-substring beg end) "")
                     for res = (mapcar (lambda (tr) (nth i (plist-get tr :result))) ret)
                     do (goto-char end)
                     for fres = (gt-insert-render-format render src res)
                     do (let (p)
                          (if (eq type 'replace)
                              (delete-region beg end)
                            (when-let* ((face (and (eq type 'after) (gt-ensure-plain sface src))))
                              (delete-region beg end)
                              (insert (propertize src 'face face))))
                          (setq p (point))
                          (insert (propertize fres 'type 'gt-insert-result))
                          (save-excursion
                            (goto-char p)
                            (skip-chars-forward " \t\n")
                            (push-mark)))
                     finally (progn
                               (if (> len 1) (goto-char start))
                               (when (and hash (equal hash (buffer-hash)))
                                 (set-buffer-modified-p nil))))
            (deactivate-mark)
            (run-hook-with-args 'gt-insert-render-output-hook translator)
            (message "ok.")))))))

(cl-defmethod gt-output ((render gt-insert-render) (task gt-task))
  (with-slots (markers res) task
    (let* ((beg (car markers)) (end (cdr markers))
           (bounds (oref (oref task translator) bounds))
           (insert-text (with-current-buffer (car bounds)
                          (save-excursion
                            (goto-char end)
                            (gt-insert-render-format
                             render
                             (if (and (eq (oref render type) 'after) (caadr bounds))
                                 (buffer-substring (caadr bounds) (cdadr bounds))
                               "")
                             (list (string-join (ensure-list res) "\n")))))))
      (gt-insert-text-at-marker insert-text beg end))))

(cl-defmethod gt-str ((render gt-insert-render))
  (format "gt-insert-render/%s" (oref render type)))

(provide 'gt-render-insert)

;;; gt-render-insert.el ends here
