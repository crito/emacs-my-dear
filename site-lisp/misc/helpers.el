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
(defmacro hook-up (hook &rest body)
  "Shorten declaration of HOOK byr adding BODY to it."
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(defun packages-installed-p (pkgs)
  "Check if all PKGS are installed."
  (dolist (pkg pkgs)
    (package-installed-p pkg)))

(defun install-packages (pkgs)
  "Install all PKGS."
  (unless (packages-installed-p pkgs)
    (message "Emacs is refreshing it's package database ...")
    (package-refresh-contents)
    
    ;; install the missing packages.
    (dolist (pkg pkgs)
      (unless (package-installed-p pkg)
        (message "==> Bootstrapping %s ..." pkg)
        (package-install pkg)))))

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

;; Take from prelude.
(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun auto-fill-comments ()
  "Auto fill comments."
  (set (make-local-variable 'comment-auto-fill-only-comments) +1)
  (auto-fill-mode +1))

(defun when-program-exists (program-name fn)
  "If PROGRAM-NAME executable is found apply FN."
  (when (executable-find program-name)
    (funcall fn)))

(provide 'helpers)

;;; helpers.el ends here
