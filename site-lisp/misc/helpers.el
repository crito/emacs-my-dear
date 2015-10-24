;; helpers.el --- A bunch of functions and macros used in my emacs configuration.

;; Copyright (C) 2015 crito <crito@cryptodrunks.net>

;; Author:      crito <crito@cryptodrunks.net>
;; Created:     2015-10-22

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information.

;;; Commentary:

;;; Code:
(require 'dash)
(require 'cl-lib)

(defmacro hook-λ (hook &rest body)
  "Shorten declaration of HOOK by adding BODY to it."
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(defmacro hook-modes (modes &rest body)
  "Add to a list of MODES the hook BODY."
  (declare (indent 1) (debug t))
  `(dolist (mode ,modes)
          (hook-λ (intern (format "%s-hook" mode)) ,@body)))

(defmacro make-transform-symbol-at-point-defun (func)
  "Not sure how FUNC gets transformed in this macro.
Taken from:
https://github.com/waymondo/hemacs/blob/93ee9ebc068ddffbc909c5b378b14cb93a3a73ec/lib/hemacs.el#L29-L39"
  (declare (indent 1) (debug t))
  (let ((defun-name (intern (format "%s-symbol-at-point" (symbol-name func)))))
    `(progn
       (defun ,defun-name ()
         (interactive)
         (save-excursion
           (er/mark-symbol)
           (let ((current-symbol (buffer-substring-no-properties (region-beginning) (region-end))))
             (call-interactively 'delete-region)
             (insert (funcall ',func current-symbol))))))))

;; Take from prelude. Those are a bunch of handy functions.
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun prelude-rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun prelude-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; Taken from https://github.com/waymondo/hemacs
(defun turn-on-comint-history (history-file)
  "Write comint history into HISTORY-FILE."
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

(defun hippie-expand-case-sensitive (orig-fun &rest args)
  "Disable case sensitive search before calling ORIG-FUN with ARGS."
  (let ((case-fold-search nil))
    (apply orig-fun args)))

(defun try-expand-dabbrev-matching-buffers (old)
  "Expand OLD from all buffers with the same major mode."
  (let ((matching-buffers (--filter
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (cl-flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(defun try-expand-dabbrev-other-buffers (old)
  "Expand OLD from all buffers that have not the same major mode."
  (let ((matching-buffers (--reject
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (cl-flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(defun find-file-maybe-make-directories (filename &optional wildcards)
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir :make-parents)))))

;; Take from
;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode.
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; And finally my own functions and macros.
;; Make window splitting bit more useful,
(defun vsplit-same-buffer ()
  "Split the window vertically and switch to it."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun vsplit-last-buffer ()
  "Split the window vertically and switch to next buffer."
  (interactive)
  (vsplit-same-buffer)
  (switch-to-next-buffer))

(defun hsplit-same-buffer ()
  "Split the window horizontally and switch to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(defun hsplit-last-buffer ()
  "Split the window horizontally and switch to next buffer."
  (interactive)
  (hsplit-same-buffer)
  (switch-to-next-buffer))

(defun auto-fill-comments ()
  "Auto fill comments."
  (set (make-local-variable 'comment-auto-fill-only-comments) +1)
  (auto-fill-mode +1))

(defun when-program-exists (program-name fn)
  "If PROGRAM-NAME executable is found apply FN."
  (when (executable-find program-name)
    (funcall fn)))

(defun minor-mode-p (mode)
  "Test if the minor mode MODE is enabled."
  (cl-letf ((predicate (lambda (m) (string= mode m))))
    (cl-some predicate minor-mode-list)))

(provide 'helpers)

;;; helpers.el ends here
