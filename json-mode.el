;;; json-mode.el --- basic JSON editing mode -*- lexical-binding: t; coding: utf-8 -*-
;;; Version: 0.2.0

;; Author: DoMiNeLa10 (https://github.com/DoMiNeLa10)

;;; license: GPLv3 or newer

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines a major mode for editing JSON files. It provides an
;; option to pretty print JSON files when they're opened and provides a way to
;; fold Array and Object literals (bound to C-c C-f by default
;; (`json-mode-fold'.)) The entire buffer can be unfolded quickly with the
;; command bound to C-c C-u (`json-mode-unfold-all'.)

;; Content can be pretty printed (with a command bound to C-c C-p by default
;; (`json-mode-pretty-print-buffer')) and minified (with a command bound to
;; C-c C-m by default (`json-mode-minify-buffer'.)) Both of these commands try
;; to validate JSON before doing their thing.

;; Buffer can be also validated with a command bound to C-c C-v by default
;; (`json-mode-validate-buffer'.)

;; Files with .json extension will be opened with this major mode by default.

(require 'json)

;;; group and customizable options
(defgroup json-mode '()
  "A simple mode for JSON editing."
  :group 'languages)

(defcustom json-mode-pretty-print-on-open t
  "Pretty print the JSON file when it's opened."
  :group 'json-mode
  :type 'boolean)

(defcustom json-mode-fold-ellipsis "…"
  "Ellipsis displayed in place of folded content."
  :group 'json-mode
  :type 'string)

;;; variables
(defvar json-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'json-mode-fold)
    (define-key map (kbd "C-c C-u") #'json-mode-unfold-all)
    (define-key map (kbd "C-c C-m") #'json-mode-minify-buffer)
    (define-key map (kbd "C-c C-p") #'json-mode-pretty-print-buffer)
    (define-key map (kbd "C-c C-v") #'json-mode-validate-buffer)
    map)
  "Keymap for `json-mode'.")

;;;###autoload
(define-derived-mode json-mode js-mode "JSON"
  "A simple mode for JSON editing."
  (when json-mode-pretty-print-on-open
    (json-pretty-print-buffer)
    (goto-char (point-min))    ; this line is ignored in pretty print function
    (set-buffer-modified-p nil)))

;;; defuns
(defun json-mode-pretty-print-buffer ()
  "Pretty prints the buffer with JSON content.
Jumps to the beginning of it. Ignores errors."
  (interactive)
  (condition-case nil                   ; error variable is unused
      (json-pretty-print-buffer)
    (json-error (user-error "Invalid JSON")))
  (goto-char (point-min)))

(defun json-mode-minify-buffer ()
  "Minifies JSON."
  (interactive)
  (unless (json-mode-buffer-valid-p)
    (user-error "Invalid JSON"))
  (goto-char (point-max))
  (while (/= 1 (line-number-at-pos))    ; this respects narrowing
    (delete-indentation)))

(defun json-mode-fold ()
  "Folds or unfolds the Array or Object literal after point
without crossing enclosing boundaries of enclosing Object or
Array."
  (interactive)
  (save-excursion
    ;; get out of the string
    (when (json-mode-inside-string-p)
      (while                          ; equivalent of a do {…} while (…); loop
          (progn
            (backward-up-list 1 t t)
            (not (json-mode-at-string-beginning-p))))
      (forward-sexp 1))
    (json-mode-skip-label-colon)
    ;; skip things that aren't Objects and Arrays without getting out of
    ;; enclosing Objects or Array
    (condition-case nil
        (while (not (json-mode-before-object-or-array-p))
          (forward-sexp 1)
          ;; skip label colons
          (when (json-mode-at-string-end-p)
            (json-mode-skip-label-colon)))
      (scan-error))
    (if (json-mode-before-object-or-array-p)
        ;; get relevant region and hide or show it
        (let* ((beg (progn
                      (skip-chars-forward "^{[")
                      (1+ (point))))
               (end (progn
                      (forward-sexp 1)
                      (1- (point))))
               (overlays (overlays-at beg)))
          (if overlays
              (mapc #'delete-overlay overlays)
            (json-mode-hide-region beg end)))
      (user-error "Nothing to hide or show"))))

(defun json-mode-unfold-all ()
  "Unfolds the entire buffer."
  (interactive)
  (delete-all-overlays))

(defun json-mode-validate-buffer ()
  (interactive)
  (if (json-mode-buffer-valid-p)
      (message "Buffer contains a valid JSON")
    (message "Buffer doesn't contain a valid JSON")))

(defun json-mode-before-object-or-array-p ()
  (looking-at "[ \t\r\n]*[\\[{]"))

(defun json-mode-face-before-point ()
  (get-text-property (1- (point)) 'face))

(defun json-mode-at-string-beginning-p ()
  (and (eq (json-mode-face-before-point) nil)
       (eq (face-at-point) 'font-lock-string-face)))

(defun json-mode-at-string-end-p ()
  (and (eq (json-mode-face-before-point) 'font-lock-string-face)
       (eq (face-at-point) nil)))

(defun json-mode-inside-string-p ()
  (and (eq (face-at-point) 'font-lock-string-face)
       (eq (json-mode-face-before-point) 'font-lock-string-face)))

(defun json-mode-skip-label-colon ()
  (skip-chars-forward " \t\r\n:"))

(defun json-mode-hide-region (beg end)
  "Hides region from BEG to END with an overlay."
  (let ((overlay (make-overlay beg end)))
    ;; FIXME: show hidden content in isearch before it's finished
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'isearch-open-invisible #'delete-overlay)
    (overlay-put overlay 'display json-mode-fold-ellipsis)
    (overlay-put overlay 'evaporate t)))

(defun json-mode-buffer-valid-p ()
  (condition-case nil
      (progn
        (json-read-from-string (buffer-string))
        t)
    (json-error nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(provide 'json-mode)
