;;; long-lines.el --- Detect long lines -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Nikita Bloshchanevich

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

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
;; URL: https://github.com/nbfalcon/long-lines
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1.0

;;; Commentary:

;; Find long lines in the current buffer and emit warnings for them.

;;; Code:

;;; Core (library functions)

(require 'cl-lib)

(defcustom long-lines-column nil
  "Lines longer than this are considered long.
If nil, `fill-column' is used instead."
  :type '(choice (number :tag "Number of columns")
                 (const :tag "Use `fill-column'" nil))
  :group 'long-lines)

(defun long-lines-column ()
  "Resolve the variable `long-lines-column'.
Fall back to `fill-column'."
  (or long-lines-column fill-column))

(defun long-lines-in-buffer (&optional column)
  "Find lines longer than COLUMN in buffer.
COLUMN defaults to `fill-column'.

The result is a list ((LINE-NUM COLUMNS START END)...), where
LINE-NUM is the 1-based line number, COLUMNS is the number of
columns of that line, and (START, END) specify the bounds of the
line (without the trailing newline)."
  (or column (setq column (long-lines-column)))
  (save-excursion
    (goto-char (point-min))
    (cl-loop for line from 1 until (eobp)
             for start = (point) do (end-of-line)
             for end = (point)
             ;; Don't use `current-column', since it reports "2" for certain
             ;; emojis (e.g. #x1f379, "Tropical Drink"), and because it breaks
             ;; with `page-break-lines'. As such, we must handle tabs ourselves.
             for columns = (let ((tabs (cl-count ?\t
                                                 (buffer-substring-no-properties
                                                  start end))))
                             (+ (- end start) (* tabs (1- tab-width))))
             if (> columns column) collect (list line columns start end)
             do (forward-line))))


;;; interactive visualization (`package-lint'-style)

(defun long-lines--ensure ()
  "Ensure that we are in a `long-lines' buffer."
  (unless (derived-mode-p 'long-lines-view-mode)
    (user-error "Must be in a `long-lines' buffer")))

(defvar-local long-lines--buffer nil
  "Buffer `long-lines' was invoked on.")

(defun long-lines-goto ()
  "Jump to the long line at `point'.
Only works in the `long-lines' buffer."
  (interactive)
  (long-lines--ensure)
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (or (looking-at "^\\([[:digit:]]+\\):[[:digit:]]+: ")
          (user-error "Not on a long line")))
    (let ((line (string-to-number (match-string-no-properties 1)))
          (offset (max 0 (- (point) (match-end 0)))))
      (pop-to-buffer long-lines--buffer)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char offset)
      (recenter))))

(defvar long-lines-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'long-lines-goto)
    (define-key map (kbd "g") #'long-lines-update)
    map))

(define-derived-mode long-lines-view-mode special-mode "Long lines"
  "Minor mode for the `long-lines' buffer.")

;; `:inherit' taken from `compilation'
(defface long-lines-line-number-face '((t :inherit font-lock-keyword-face))
  "Face used for line numbers by `long-lines'."
  :group 'long-lines)

(defface long-lines-columns-face '((t :inherit font-lock-doc-face))
  "Face used for column counts by `long-lines'."
  :group 'long-lines)

(defface long-lines-count-face '((t :inherit long-lines-line-number-face))
  "Face used to display the count of long lines."
  :group 'long-lines)

(defface long-lines-column-face '((t :inherit long-lines-columns-face))
  "Face used to display the column count by `long-lines'."
  :group 'long-lines)

(defun long-lines--1 (column buffer-name &optional force)
  "Initialize the `long-lines' buffer.
COLUMN is as in `long-lines', BUFFER-NAME is the name to be used
for the buffer. If FORCE is non-nil, emit a `message' instead of
a `user-error' if there are no long lines."
  (let* ((lines (or (long-lines-in-buffer column)
                    (if force (ignore (message "No long lines"))
                      (user-error "No long lines"))))
         (text (cl-loop for (num columns start end) in lines
                        concat (format "%s:%s: %s\n"
                                       (propertize
                                        (number-to-string num)
                                        'face 'long-lines-line-number-face)
                                       (propertize
                                        (number-to-string columns)
                                        'face 'long-lines-columns-face)
                                       (buffer-substring start end))))
         (orig-buf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s long lines (> %s columns) found:\n\n"
                        (propertize (if lines (number-to-string (length lines))
                                      "No")
                                    'face 'long-lines-count-face)
                        (propertize (number-to-string column)
                                    'face 'long-lines-column-face)))
        (save-excursion (insert text)))
      (long-lines-view-mode)
      (view-mode 1)
      (setq long-lines--buffer orig-buf)
      (set (make-local-variable 'long-lines-column) column)
      (pop-to-buffer (current-buffer)))))

(defun long-lines--interactive ()
  "`interactive' function to read a column."
  (and current-prefix-arg (list (read-number "Column: "))))

(defun long-lines-update (&optional column)
  "Update the current `long-lines' buffer.
See `long-lines' for COLUMN."
  (interactive (long-lines--interactive))
  (or column (setq column (long-lines-column)))
  (long-lines--ensure)
  (let ((buf-name (buffer-name)))
    (with-current-buffer long-lines--buffer
      (long-lines--1 column buf-name t))))

(defun long-lines (&optional column)
  "List lines in the current buffer longer than COLUMN.
COLUMN defaults to `fill-column'."
  (interactive (long-lines--interactive))
  (long-lines--1 (or column (long-lines-column))
                 (format "*Long lines: %s*" (buffer-name (current-buffer)))))

(provide 'long-lines)
;;; long-lines.el ends here
