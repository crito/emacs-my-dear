;; crito-functions.el --- A collection of useful functions.

;; Copyright (C) 2015 crito <crito@cryptodrunks.net>

;; Author:      crito <crito@cryptodrunks.net>
;; Created:     2015-10-18

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
;; Variation of `zap-to-char'.

;; Taken from misc.el - http://repo.or.cz/emacs.git/blob_plain/HEAD:/lisp/misc.el
(defun zap-up-to-char (arg char)
    "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
    (interactive "p\ncZap up to char: ")
    (let ((direction (if (>= arg 0) 1 -1)))
      (kill-region (point)
                   (progn
                     (forward-char direction)
                     (unwind-protect
                         (search-forward (char-to-string char) nil nil arg)
                       (backward-char direction))
                     (point)))))

;; If we can correct our spelling, let's do that
(defun when-program-exists (program-name fn)
  "If PROGRAM-NAME executable is found apply FN."
  (when (executable-find program-name)
    (funcall fn)))

;; Take from prelude.
(defun crito-move-beginning-of-line (arg)
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

(defun crito-minor-mode-p (mode)
  "Test if the minor mode MODE is enabled."
  (cl-letf ((predicate (lambda (m) (string= mode m))))
    (cl-some predicate minor-mode-list)))

(provide 'crito-functions)

;;; crito-functions.el ends here














