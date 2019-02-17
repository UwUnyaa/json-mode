;;; json-mode.el --- basic JSON editing mode -*- lexical-binding: t; coding: utf-8 -*-
;;; Version: 0.5.0

;; Author: DoMiNeLa10 (https://github.com/DoMiNeLa10)

;;; license: GPLv3 or newer

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file defines a major mode for editing JSON files. It provides an
;; option to pretty print JSON files when they're opened and provides a way to
;; fold Array and Object literals (bound to C-c C-f by default
;; (`json-mode-fold'.)) The entire buffer can be unfolded quickly with the
;; command bound to C-c C-u (`json-mode-unfold-all'.)
;;
;; Content can be pretty printed (with a command bound to C-c C-p by default
;; (`json-mode-pretty-print-buffer')) and minified (with a command bound to
;; C-c C-m by default (`json-mode-minify-buffer'.)) Both of these commands try
;; to validate JSON before doing their thing.
;;
;; Buffer is validated when Emacs is idle, and it can be also validated with a
;; command bound to C-c C-v by default (`json-mode-validate-buffer'.)
;;
;; Files with .json extension will be opened with this major mode by default.

(eval-when-compile
  (require 'cl))                        ; `cl-flet'
(require 'json)
(require 'subr-x)                       ; `string-trim'

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

(defcustom json-mode-timer-enable t
  "Enables idle validation displayed on mode line."
  :group 'json-mode
  :type 'boolean)

(defcustom json-mode-timer-delay 0.1
  "Delay before idle timer for validation starts."
  :group 'json-mode
  :type 'float)

(defcustom json-mode-path-format #'json-mode-format-path-js
  "Default path format for JSON paths.

The value should be a function that takes a list of path segments
and returns a string with a fromatted path. Each element of that
list is either a string (key for an Object) or a number (Array
index)."
  :group 'json-mode
  :type '(choice (const :tag
                        "JavaScript"
                        json-mode-format-path-js)
                 (const :tag
                        "Square brackets"
                        json-mode-format-path-brackets)
                 (const :tag
                        "JSON pointer (RFC 6901)"
                        json-mode-format-path-json-pointer)
                 (function :tag
                        "Custom function")))

;;; constants
(defconst json-mode-mode-name "JSON"
  "Mode name for `json-mode'.")

;;; variables
(defvar json-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'json-mode-fold)
    (define-key map (kbd "C-c C-u") #'json-mode-unfold-all)
    (define-key map (kbd "C-c C-m") #'json-mode-minify-buffer)
    (define-key map (kbd "C-c C-p") #'json-mode-pretty-print-buffer)
    (define-key map (kbd "C-c C-v") #'json-mode-validate-buffer)
    (define-key map (kbd "C-c C-l") #'json-mode-get-path-to-point)
    map)
  "Keymap for `json-mode'.")

(defvar json-mode-timer nil
  "Local variable storing a reference to a timer.")
(put 'json-mode-timer 'permanent-local t)

;;; Code:
;;;###autoload
(define-derived-mode json-mode js-mode json-mode-mode-name
  "A simple mode for JSON editing."
  (when (and json-mode-pretty-print-on-open (json-mode-buffer-valid-p))
    (json-mode-pretty-print-buffer)
    (goto-char (point-min))    ; this line is ignored in pretty print function
    (set-buffer-modified-p nil))
  (when json-mode-timer-enable
    (make-local-variable 'json-mode-timer)
    (let ((buffer (current-buffer)))
      (json-mode-mode-line-validate buffer t)
      (add-hook 'after-change-functions
                (lambda (&rest args)
                  (ignore args)
                  (json-mode-timer-set buffer))
                nil t)
      (cl-flet ((timer-cancel () (json-mode-timer-cancel buffer)))
        (add-hook 'kill-buffer-hook #'timer-cancel nil t)
        (add-hook 'change-major-mode-hook #'timer-cancel nil t)))))

;;; defuns
(defun json-mode-pretty-print-buffer ()
  "Pretty prints the buffer with JSON content.
Jumps to the beginning of it. Ignores errors."
  (interactive)
  (condition-case nil                   ; error variable is unused
      (json-pretty-print-buffer)
    (error (user-error "Invalid JSON")))
  (goto-char (point-min)))

(defun json-mode-minify-buffer ()
  "Minifies JSON."
  (interactive)
  (unless (json-mode-buffer-valid-p)
    (user-error "Invalid JSON"))
  (let ((json-encoding-pretty-print nil)
        (json-object-type 'alist)
        (buffer-text (delete-and-extract-region (point-min) (point-max))))
    (insert (json-encode (json-read-from-string buffer-text)))))

(defun json-mode-get-path-to-point (point &optional formatter)
  "Return the path to POINT.

When called interactively, the path is added to the kill ring.

When called non-interactively, path is formatted by FORMATTER or
value of `json-mode-path-format'."
  (interactive "d")
  (save-excursion
    (goto-char point)
    ;; Strings aren't relevant parts of the path, get out of them
    (when (json-mode-inside-string-p)
      (backward-up-list 1 t t))
    (let* ((path
            (nreverse
             (cl-remove-if
              #'null
              (cl-loop
               while (progn (skip-chars-backward " \t\r\n")
                            (/= (point) (point-min)))
               collect (let* ((start (point)))
                         (backward-up-list 1 t t)
                         (prog1
                             (cond
                              ((= (char-after) ?\[)
                               ;; move into the Array
                               (forward-char)
                               ;; move to the end of the first value
                               (when (condition-case nil
                                         (progn
                                           (forward-sexp)
                                           t)
                                       (scan-error nil))
                                 (let ((index 0))
                                   (while (< (point) start)
                                     (forward-sexp)
                                     (cl-incf index))
                                   index)))
                              ((= (char-after) ?\{)
                               ;; go back to start position
                               (goto-char start)
                               ;; try going back an expression if neccesary
                               (unless (= (char-before) ?,)
                                 (condition-case nil
                                     (backward-sexp)
                                   (scan-error nil)))
                               ;; skip whitespce
                               (skip-chars-backward " \t\r\n")
                               ;; go back another expression if point
                               ;; is after the label
                               (when (= (char-before) ?:)
                                 (backward-sexp))
                               ;; get the expression and trim the
                               ;; resulting string
                               (let* ((start (point))
                                      (end (progn (forward-sexp)
                                                  (point))))
                                 (string-trim
                                  (buffer-substring-no-properties
                                   start end)))))
                           ;; go up a level
                           (backward-up-list 1 t t)))))))
           (formatted-path (funcall (or formatter
                                        json-mode-path-format)
                                    path)))
      (when (called-interactively-p 'interactive)
        (if (> (length formatted-path) 0)
            (progn
              (kill-new formatted-path)
              (message "Copied to kill ring: %s" formatted-path))
          (message "Nothing to copy")))
      formatted-path)))

(defun json-mode-format-path-brackets (keys)
  "Format KEYS as bracket notation JSON path.

Intended for use in `json-mode-get-path-to-point'."
  (mapconcat
   (lambda (key)
     (format
      (if (numberp key)
          "[%d]"
        "[%s]")
      key))
   keys
   ""))

(defun json-mode-format-path-js (keys)
  "Format KEYS as JavaScript notation path.

Intended for use in `json-mode-get-path-to-point'."
  (mapconcat
   (lambda (key)
     (let ((parsed-key
            (condition-case nil
                (read key)
              (error nil))))
       (cond
        ((numberp key)
         (format "[%d]" key))
        ((and (stringp parsed-key)
              (string-match "^[0-9]\\|[^A-Za-z0-9$_]" parsed-key))
         (format "[%s]" key))
        (t
         (format ".%s" parsed-key)))))
   keys
   ""))

(defun json-mode-format-path-json-pointer (keys)
  "Format KEYS as JSON pointer (RFC 6901).

Intended for use in `json-mode-get-path-to-point'."
  (mapconcat
   (lambda (key)
     (let ((parsed-key
            (condition-case nil
                (read key)
              (error nil))))
       (cond
        ((numberp key)
         (format "/%d" key))
        (t
         (format "/%s"
                 (cl-reduce
                  (lambda (acc next)
                    (destructuring-bind (regexp . replacement) next
                      (replace-regexp-in-string regexp replacement acc)))
                  '(("~" . "~0")
                    ("/" . "~1"))
                  :initial-value parsed-key))))))
   keys
   ""))

(defun json-mode-fold ()
  "Fold or unfold the Array or Object literal after point.
Doesn't cross boundaries of enclosing Object or Array."
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
  "Validate a buffer and show result in minibuffer."
  (interactive)
  (if (json-mode-buffer-valid-p)
      (message "Buffer contains a valid JSON")
    (message "Buffer doesn't contain a valid JSON")))

(defun json-mode-before-object-or-array-p ()
  "Check if point is before opening of an Object or Array."
  (looking-at "[ \t\r\n]*[\\[{]"))

(defun json-mode-face-before-point ()
  "Get face of character before point."
  (get-text-property (1- (point)) 'face))

(defun json-mode-at-string-beginning-p ()
  "Check if point is at the beginning of a String."
  (and (eq (json-mode-face-before-point) nil)
       (eq (face-at-point) 'font-lock-string-face)))

(defun json-mode-at-string-end-p ()
  "Check if point is just after a String."
  (and (eq (json-mode-face-before-point) 'font-lock-string-face)
       (eq (face-at-point) nil)))

(defun json-mode-inside-string-p ()
  "Check if point is inside of a String."
  (and (eq (face-at-point) 'font-lock-string-face)
       (eq (json-mode-face-before-point) 'font-lock-string-face)))

(defun json-mode-skip-label-colon ()
  "Move point past a label colon."
  (skip-chars-forward " \t\r\n:"))

(defun json-mode-hide-region (beg end)
  "Hides region from BEG to END with an overlay."
  (let ((overlay (make-overlay beg end)))
    ;; FIXME: show hidden content in isearch before it's finished
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'display
                 (propertize
                  json-mode-fold-ellipsis
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "<mouse-2>")
                              (lambda ()
                                (interactive)
                                (delete-overlay overlay)))
                            (define-key map [follow-link] 'mouse-face)
                            map)))
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'help-echo "mouse-2: unfold")
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'isearch-open-invisible #'delete-overlay)))

(defun json-mode-buffer-valid-p ()
  "Check if buffer has a valid JSON inside."
  (condition-case nil
      (progn
        (json-read-from-string (buffer-string))
        t)
    (error nil)))

(defun json-mode-mode-line-validate (buffer &optional force)
  "Idle timer function to display JSON validity in mode line.

Only BUFFER will be validated when it's active or FORCE is t."
  (let ((current-buffer-p (eq (current-buffer) buffer)))
    ;; avoid validating when buffer isn't active
    (when (or force current-buffer-p)
      (setq mode-name (format "%s validating…" json-mode-mode-name))
      (let ((buffer-valid-p (with-current-buffer buffer
                              (json-mode-buffer-valid-p))))
        (setq mode-name (format "%s %s"
                                json-mode-mode-name
                                (if buffer-valid-p
                                    "valid"
                                  "invalid")))))
    ;; set a timer if buffer wasn't current
    (when (and (not force) (not current-buffer-p))
      (add-hook 'buffer-list-update-hook
                (lambda () (json-mode-timer-set buffer))))))

(defun json-mode-timer-set (target-buffer)
  "Set up a timer for validation.

TARGET-BUFFER should be the buffer for which the timer should be
set."
  (with-current-buffer target-buffer
    (let ((timer (timer-create)))
      (timer-set-function timer
                          #'json-mode-mode-line-validate
                          (list target-buffer))
      (timer-set-idle-time timer json-mode-timer-delay)
      (timer-activate-when-idle timer)
      (setq json-mode-timer timer))))

(defun json-mode-timer-cancel (buffer)
  "Cancel a timer in BUFFER."
  (with-current-buffer buffer
    (when (local-variable-p 'json-mode-timer)
      (when json-mode-timer
        (cancel-timer json-mode-timer))
      (kill-local-variable 'json-mode-timer))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(provide 'json-mode)
;;; json-mode.el ends here
