;; crito-bindings.el --- List all bindings.

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
(require 'smartrep)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; map the window manipulation keys to control 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap

;; Map some handy functions.
(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)
(global-set-key (kbd "C-c r") 'prelude-rename-buffer-and-file)
(global-set-key (kbd "C-c d") 'prelude-delete-file-and-buffer)

;; join lines, as in vim.
(global-set-key (kbd "C-M-j") 'join-line)

;; Rebind newline-and-indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; Switch windows.
(global-set-key (kbd "M-p") 'ace-window)

;; C-j is normally bound to `newline-and-indent`. Let's use it for ace jump
;; mode instead.
;; (global-set-key (kbd "C-j") 'ace-jump-mode)
;; (global-set-key (kbd "C-j l") 'ace-jump-line-mode)
;; (global-set-key (kbd "C-j c") 'ace-jump-char-mode)
(smartrep-define-key global-map "M-g"
  '(("g" . goto-line)
    ("j" . ace-jump-mode)
    ("l" . ace-jump-line-mode)
    ("c" . ace-jump-char-mode)))

;; Helm
(smartrep-define-key global-map "C-c e"
  '(("f" . helm-find-files)
    ("y" . helm-show-kill-ring)
    ("b" . helm-mini)
    ("x" . helm-M-x)
    ("p" . helm-browse-project)
    ("s" . helm-ag-project-root)
    ("m" . helm-all-mark-rings)
    ("d" . helm-descbinds)))

;; I keep some helm shortcuts as duplicates, they are either shorter
;; or standard.
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; Mimic vim's `dt` key stroke.
(global-set-key (kbd "M-z") 'fastnav-zap-up-to-char-forward)
(global-set-key (kbd "M-Z") 'fastnav-zap-up-to-char-backward)

;; Anzu
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; Shortcuts for operate on number.
(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(provide 'crito-bindings)
;;; crito-bindings.el ends here
