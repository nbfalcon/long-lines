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
             ;; `let'-bind `buffer-display-table' so that
             ;; `page-break-lines-mode' and `prettify-symbols-mode' don't affect
             ;; long lines.
             for columns = (let (buffer-display-table) (current-column))
             if (> columns column) collect (list line columns start end)
             do (forward-line))))

;;; Rendering

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

(defun long-lines-render (long-line)
  "Render a LONG-LINE to a pretty string.
LONG-LINE must have been acquired from `long-lines-in-buffer' and
this function must be invoked in the same buffer."
  (cl-destructuring-bind (num columns start end) long-line
    (format "%s:%s: %s"
            (propertize
             (number-to-string num)
             'face 'long-lines-line-number-face)
            (propertize
             (number-to-string columns)
             'face 'long-lines-columns-face)
            (buffer-substring start end))))

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

(defun long-lines--1 (column buffer-name &optional force)
  "Initialize the `long-lines' buffer.
COLUMN is as in `long-lines', BUFFER-NAME is the name to be used
for the buffer. If FORCE is non-nil, emit a `message' instead of
a `user-error' if there are no long lines."
  (let* ((lines (or (long-lines-in-buffer column)
                    (if force (ignore (message "No long lines"))
                      (user-error "No long lines"))))
         (text (mapconcat #'long-lines-render lines "\n"))
         (orig-buf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s long line%s (> %s columns) found:\n\n"
                        (propertize (if lines (number-to-string (length lines))
                                      "No")
                                    'face 'long-lines-count-face)
                        (if (cdr lines) "s" "")
                        (propertize (number-to-string column)
                                    'face 'long-lines-column-face)))
        (save-excursion (insert text "\n")))
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
  (let ((buf-name (buffer-name))
        (start (point)))
    (with-current-buffer long-lines--buffer
      (long-lines--1 column buf-name t))
    ;; Save `point' so that we don't jump too far. `save-excursion' can't be
    ;; used here, since we clear the entire buffer.
    (goto-char start)))

(defun long-lines (&optional column)
  "List lines in the current buffer longer than COLUMN.
COLUMN defaults to `fill-column'."
  (interactive (long-lines--interactive))
  (long-lines--1 (or column (long-lines-column))
                 (format "*Long lines: %s*" (buffer-name (current-buffer)))))

;;; Highlight too long parts of lines

(defun long-lines-goto-column (column)
  "Like `move-to-column', but skip trailing 0-width characters.
Goto COLUMN. Unlike `move-to-column', this function does not
return the column."
  (interactive (list (read-number "Column: ")))
  (let (buffer-display-table)
    (when (> (move-to-column (1+ column)) column)
      (forward-char -1))))

(defface long-lines-highlight-face '((t :inherit 'error))
  "Face used to highlight long line parts."
  :group 'long-line)

(defun long-lines-highlight-fontify (end)
  "FUNCTION `font-lock' rule to highlight long lines until END.
The part of a line that goes beyond function `long-lines-column'
is highlighted.

See command `long-lines-highlight-mode'."
  (when (<= (point) end)
    (long-lines-goto-column (long-lines-column))
    (let ((start (point)))
      (end-of-line)
      (set-match-data (list start (point))))
    (forward-line)
    t))

(define-minor-mode long-lines-highlight-mode
  "Highlight line parts going over `long-lines-column'."
  :init-value nil
  :lighter nil
  (let ((kw '((long-lines-highlight-fontify . 'long-lines-highlight-face))))
    (if long-lines-highlight-mode
        (font-lock-add-keywords nil kw)
      (font-lock-remove-keywords nil kw)))
  (font-lock-flush))

(defun long-lines-goto-long-column ()
  "Go to the part of the current line exceeding the long column."
  (declare (interactive-only long-lines-goto-column))
  (interactive)
  (long-lines-goto-column (long-lines-column)))

;;; `completing-read' interface (`swiper'-like)

(defun long-lines--candidates ()
  "Return a list of line candidates using `long-lines-render'."
  (let ((lines (or (long-lines-in-buffer)
                   (user-error "No long lines"))))
    (mapcar #'long-lines-render lines)))

(defun long-lines--action (cand)
  "Jump to CAND.
CAND must have been acquired using `long-lines--candidates'."
  (save-match-data
    (cl-assert (string-match "\\([[:digit:]]+\\):" cand))
    (let ((line (string-to-number (match-string-no-properties 1 cand))))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun long-lines-find ()
  "Select a long line using `completing-read'."
  (interactive)
  (let ((line (completing-read "Goto long line:" (long-lines--candidates))))
    (long-lines--action line)))

(defun long-lines-find-ivy ()
  "`long-lines-find' using `ivy'."
  (interactive)
  (require 'ivy)
  (declare-function ivy-read "ivy" (prompt collection &rest --cl-rest--))
  (ivy-read "Goto long line: " (long-lines--candidates)
            :action #'long-lines--action
            :caller 'long-lines-find-ivy))

(defun long-lines-find-helm ()
  "`long-lines-find' using `helm'."
  (interactive)
  (require 'helm)
  (declare-function helm "helm" (&rest plist))
  (declare-function helm-make-source "helm-source" (name class &rest args))
  (helm :sources (helm-make-source "long lines" 'helm-source-sync
                   :candidates (long-lines--candidates)
                   :action #'long-lines--action)
        :buffer "*helm long lines*"))

;;; `avy' integration

(defun long-lines--point (col start)
  "`long-lines-goto-column' COL from START.
Return the new `point'."
  (goto-char start)
  (long-lines-goto-column col)
  (point))

(defun long-lines-avy ()
  "Jump to long line parts using `avy'."
  (interactive)
  (require 'avy)
  (declare-function avy-process "avy" (candidates &optional overlay-fn
                                                  cleanup-fn))
  (let* ((lines (save-restriction (narrow-to-region (window-start)
                                                    (window-end nil t))
                                  (long-lines-in-buffer)))
         (candidates (save-excursion
                       (cl-loop with long-col = (long-lines-column)
                                for (_ _ start end) in lines
                                for pos = (long-lines--point long-col start)
                                collect (cons pos end)))))
    (avy-process candidates)))

;;; error-checker integration

(defun long-lines--format-diagnostic (long-col ncols)
  "Format a \"line too long\" diagnostic message.
LONG-COL is column after which lines are long (see function
`long-lines-column'), while NCOLS is the actual number of columns
of the long line."
  (format "Line too long (%d columns > %d)" ncols long-col))

;;; `flycheck'
;;;###autoload
(defun long-lines-flycheck-setup ()
  "Configure the `flycheck' checker."
  (require 'flycheck)
  (declare-function flycheck-define-generic-checker "flycheck"
                    (symbol docstring &rest properties))
  (declare-function flycheck-error-new-at "flycheck" (arg1 arg2 &rest rest))
  (flycheck-define-generic-checker 'long-lines
    "`long-lines' `flycheck' checker."
    :start (lambda (checker cb)
             (save-excursion
               (cl-loop with long-col = (long-lines-column)
                        with lines = (long-lines-in-buffer long-col)
                        for (line ncols start end) in lines
                        for off = (- (long-lines--point long-col start) start)
                        collect
                        (flycheck-error-new-at
                         line (1+ off) 'warning
                         (long-lines--format-diagnostic long-col ncols)
                         :checker checker
                         :end-column (1+ (- end start)))
                        into diagnostics finally do
                        (funcall cb 'finished diagnostics))))
    :modes t))

;;; `flymake'
(defun long-lines-flymake (cb &rest _args)
  "A `flymake' backend for long-line diagnostics.
For `flycheck', see `long-lines-flycheck-setup'.

CB is called to register the diagnostics."
  (declare-function flymake-make-diagnostic "flymake"
                    (buffer beg end type text
                            &optional data overlay-properties))
  (save-excursion
    (cl-loop with long-col = (long-lines-column)
             for (_line ncols start end) in (long-lines-in-buffer long-col)
             collect (flymake-make-diagnostic
                      (current-buffer) (long-lines--point long-col start) end
                      :warning (long-lines--format-diagnostic long-col ncols))
             into diagnostics finally do
             (funcall cb diagnostics))))

(provide 'long-lines)
;;; long-lines.el ends here
