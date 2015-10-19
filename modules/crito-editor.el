;; crito-editor.el --- Configure the basic behavior of the editor.

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
(require 'crito-packages)

(defvar crito-editor-packages
  '(ace-jump-mode
    ace-window
    anzu
    browse-kill-ring
    diff-hl
    diminish
    easy-kill
    exec-path-from-shell
    expand-region
    fastnav
    git-gutter
    helm
    helm-descbinds
    helm-ls-git
    operate-on-number
    projectile
    rainbow-delimiters
    smartparens
    undo-tree
    volatile-highlights)
  "A list of all packages that are installed from ELPA for the editor.")

(crito-require-packages crito-editor-packages)

;; Always ALWAYS use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; `C-n` inserts newlines if the point is at the end of the buffer
(setq next-line-add-newlines t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; Write backup and autosave files to own directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "bak" dotfiles-dir))))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name "auto-saves" dotfiles-dir) t)))
;; (setq auto-save-list-file-prefix dotfiles-dir)
(auto-save-mode -1)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

; line wrapping
(setq fill-column 78)

; show the end of the buffer
(setq-default indicate-empty-line t)

;; Take the environment from the shell
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "DBUS_SESSION_BUS_ADDRESS")

;; diminish keeps the modeline tidy
(require 'diminish)

;; ace products
(require 'ace-window)
(require 'ace-jump-mode)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
1(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" dotfiles-dir))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" dotfiles-dir))
(savehist-mode +1)

;; Helm
(require 'helm)
(require 'helm-descbinds)
(require 'helm-ls-git)

(setq
 helm-google-suggest-use-curl-p t
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-quick-update t ; do not display invisible candidates
 helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
 helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

 helm-split-window-default-side 'other ;; open helm buffer in another window
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-candidate-number-limit 200 ; limit the number of displayed canidates
 helm-M-x-requires-pattern 0     ; show all candidates when set to 0
 helm-boring-file-regexp-list
 '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source
                                        ; when reaching top or bottom of source.
 ido-use-virtual-buffers t      ; Needed in helm-buffers-list
 helm-mode-fuzzy-match t          ; fuzzy matching buffer names when non--nil
                                        ; useful in helm-mini that
                                        ; lists buffers
 helm-completion-in-region-fuzzy-match t)

(helm-descbinds-mode)
(helm-autoresize-mode)
(setq helm-autoresize-max-height 60)
(setq helm-autoresize-min-height 40)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

  ;;; save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; Set the spell checker program.
(require 'flyspell)
(setq ispell-program-name "aspell" ; Use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" dotfiles-dir)
      bookmark-save-flag 1)

;; projectile is a project management mode
(require 'projectile)
(require 'grizzl)
(setq projectile-cache-file (expand-file-name  "projectile.cache" dotfiles-dir))
(setq projectile-completion-system 'grizzl)
(projectile-global-mode t)

;; tramp for sudo and remote access
(require 'tramp)
(setenv "SSH_AUTH_SOCK" (concat "/run/user/" (number-to-string (user-real-uid)) "/keyring/ssh"))
(setq tramp-default-method "ssh")

;; anzu-mode enhances isearch by showing total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; Always show git-gutter
(require 'git-gutter)
(global-git-gutter-mode +1)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; operate-on-number
(require 'operate-on-number)

(provide 'crito-editor)
;;; crito-editor.el ends here
