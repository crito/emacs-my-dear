;;; init.el --- Everything has to start somewhere.
;;
;; Copyright (c) 2015 crito
;;
;; Author: crito <crito@cryptodrunks.net>

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;; Bootstrap
(defun add-subfolders-to-load-path (root-dir)
  "Add all sub directories of ROOT-DIR recursively to the load path."
  (dolist (f (directory-files root-dir))
    (let ((dir (expand-file-name f root-dir)))
      (when (and (file-directory-p dir)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path dir)
        (add-subfolders-to-load-path dir)))))

;; Configure various directory locations.
(defvar dotfiles-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration.")
(defvar site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir)
  "The site-lisp directory of this Emacs distribution.")
(defvar backup-dir (expand-file-name "bak" dotfiles-dir)
  "Write backup files to this directory.")

;; Add to the load path.
;;(add-to-list 'load-path (expand-file-name "site-lisp" dotfiles-dir))
(add-subfolders-to-load-path site-lisp-dir)

(require 'helpers)
(require 'package)

(defvar bootstrap-packages
  '(use-package
    diminish
    bind-key)
  "Those packages are required from bootstrap on.")

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(unless (packages-installed-p bootstrap-packages)
  (package-refresh-contents)
  (install-packages bootstrap-packages))

(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

(setq load-prefer-newer t      ; Always load newest byte code.
      require-final-newline t  ; Newline at the end of a file
      next-line-add-newlines t ; `C-n` inserts newlines if the point is at the
                               ; end of the buffer
      blink-matching-paren 'jump ; Jump to the opening parent when closing it.
      )

(setq-default indent-tabs-mode nil  ; Don't use tabs to indent
              tab-width 8           ; but maintain correct appearance
              fill-column 78        ; line wrapping
              indicate-empty-line t ; show the end of the buffer
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              bidi-display-reordering nil
              )

;; (use-package helpers
;;   :load-path "site-lisp/")

(use-package smartrep :ensure t)

;; Read custom.el before the vars below.
(use-package cus-edit
  :init
  (setq custom-file (expand-file-name "custom.el" dotfiles-dir))
  :config
  (load custom-file 'no-error 'no-message))

;; Some configurable variables.
(defvar env-vars-from-shell '("DBUS_SESSION_BUS_ADDRESS"
                              "SSH_AUTH_SOCK"
                              "HISTFILE")
  "Environment variables to load from shell.")

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (dolist (var env-vars-from-shell)
    (exec-path-from-shell-copy-env var)))

;;; Basic libraries
(use-package dash :ensure t :config (dash-enable-font-lock))

(use-package cl-lib :ensure t)

(use-package s
  :ensure t
  :commands
  (s-lower-camel-case s-upper-camel-case s-snake-case s-dashed-words)
  :config
  (make-transform-symbol-at-point-defun s-lower-camel-case)
  (make-transform-symbol-at-point-defun s-upper-camel-case)
  (make-transform-symbol-at-point-defun s-snake-case)
  (make-transform-symbol-at-point-defun s-dashed-words))

;;; UI elements
(use-package "subr" :init (defalias 'yes-or-no-p #'y-or-n-p))

(use-package startup
  :defer t
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        inhibit-startup-echo-area-message ""))

(use-package scrolling
  :defer t
  :init
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1))

(use-package tool-bar :defer t :config (tool-bar-mode -1))

(use-package scroll-bar :defer t :config (scroll-bar-mode -1))

(use-package menu-bar :defer t :config (menu-bar-mode -1))

(use-package line-number :defer t :config (line-number-mode))

(use-package column-number :defer t :config (column-number-mode))

(use-package size-indication :defer t :config (size-indication-mode))

(use-package novice :defer t :init (setq disabled-command-function nil))

;; Let's be hard on ourselves ...
(use-package guru-mode
  :ensure t
  :config
  (guru-mode))

(use-package hi-lock
  :ensure t
  :bind
  (("M-o l" . highlight-lines-matching-regexp)
   ("M-o r" . highlight-regexp)
   ("M-o w" . highlight-phrase)))

(use-package fringe :defer t :config (fringe-mode 4))

(use-package zenburn-theme :ensure t :init (load-theme 'zenburn t))

;;; Configure the editor
(use-package smartrep)

(use-package utf-8-support
  :defer t
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (load-library "iso-transl"))

(use-package delete-selection :defer t :init (delete-selection-mode t))

(use-package files
  :init (setq backup-directory-alist '((".*" . 'backup-dir))))

(use-package simple
  :config
  (auto-save-mode -1)
  (add-hook 'text-mode-hook #'auto-fill-mode))

(use-package auto-revert :defer t :config (global-auto-revert-mode))

(use-package hippie-expand
  :defer t
  :bind
  ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

;; ace products
(use-package ace-window :ensure t :bind ("M-p" . ace-window))
(use-package ace-jump-mode
  :ensure t
  :bind
  (("M-g j" . ace-jump-mode)
   ("M-g c" . ace-jump-char-mode)
   ("M-g l" . ace-jump-line-mode)))

;; Set meaningful names for buffers with the same name.
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ; rename after killing uniquified.
        uniquify-after-kill-buffer-p t
        ; don't muck with special buffers.
        uniquify-ignore-buffers-re "^\\*"))

;; Remember your location in a file when saving it.
(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "saveplace" dotfiles-dir)))

;; Keep track of mini buffer history.
(use-package savehist
  :init
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" dotfiles-dir))
  :config
  (savehist-mode))

;; Abbreviations.
(use-package abbrev
  :init
  (setq abbrev-file-name (expand-file-name "abbrev.defs" dotfiles-dir))
  :config
  (abbrev-mode))

;; Use Helm to navigate the minibuffer
(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-c h f" . helm-find-files)
   ("C-c h y" . helm-show-kill-ring)
   ("C-c h b" . helm-mini)
   ("C-c h x" . helm-M-x)
   ("C-c h p" . helm-browse-project)
   ("C-c h m" . helm-all-mark-rings))
  :init
  (setq helm-net-prefer-curl t
        ; scroll 4 lines other window using M-<next>/M-<prior>.
        helm-scroll-amount 4
        ; do not display invisible candidates.
        helm-quick-update t
        ; be idle for this many seconds, before updating in delayed sources.
        helm-idle-delay 0.01
        ; be idle for this many seconds, before updating candidate buffer.
        helm-input-idle-delay 0.01
        ; search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp t
        ; open helm buffer in another window.
        helm-split-window-default-side 'other
        ; open helm buffer inside current window, not occupy whole other window
        helm-split-window-in-side-p t
        ; limit the number of displayed candidates
        helm-candidate-number-limit 200
        ; show all candidates when set to 0
        helm-M-x-requires-pattern 0
        ; do not show these files in helm buffer
        helm-boring-file-regexp-list '("\\.git$"
                                       "\\.hg$"
                                       "\\.svn$"
                                       "\\.CVS$"
                                       "\\._darcs$"
                                       "\\.la$"
                                       "\\.o$"
                                       "\\.i$"
                                       "\\.elc$"
                                       "\\.pyc$")
        helm-ff-file-name-history-use-recentf t
        ; move to end or beginning of source when reaching top or bottom.
        helm-move-to-line-cycle-in-source t
        ; Needed in helm-buffers-list
        ido-use-virtual-buffers t
        ; fuzzy matching buffer names when non-nil useful in helm-mini that
        ; lists buffers
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        ; configure the size of the helm windows
        helm-autoresize-max-height 60
        helm-autoresize-min-height 40)
  :config
  (helm-autoresize-mode)
  ; save current position to mark ring.
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring))

(use-package helm-descbinds
  :ensure t
  :bind
  ("C-c h d" . helm-descbinds)
  :config
  (helm-descbinds-mode))

(use-package helm-ag
  :ensure t
  :bind
  ("C-c h s" . helm-ag-project-root))

(use-package helm-ls-git :ensure t)

;; Set the spell checker
(use-package flyspell
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  :config
  (dolist (hook '(prog-mode-hook text-mode-hook))
      (hook-up hook
        (when-program-exists ispell-program-name #'flyspell-prog-mode))))

;; Semantically expand a region.
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; bookmark
(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (expand-file-name "bookmarks" dotfiles-dir)
        bookmark-save-flag 1))

;; Projectile project management
(use-package grizzl :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-cache-file (expand-file-name "projectile.cache"
                                                dotfiles-dir)
        projectile-completion-system 'grizzl)
  :config
  (projectile-global-mode))

;; Tramp for sudo or ssh access.
(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

;; anzu enhances isearch by showing total matches and current match position
(use-package anzu
  :diminish anzu-mode
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; easy-kill/easy-mark
(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

;; ediff
(use-package ediff
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Smarter kill-ring navigation
(use-package browse-kill-ring
  :bind
  ("C-M-y" . browse-kill-ring)
  :init
  (setq browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item 'solid))

;; Highlight current line.
(use-package hl-line :config (global-hl-line-mode))

;; volatile highlights
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode))

;; sensible undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; Highlight uncommitted changes.
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;; Indicate the 80 columns limit.
(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-character-color "#262626"
        fci-rule-column 80
        fci-always-use-textual-rule t)
  :config
  (fci-mode))

;; Always show the git gutter
(use-package git-gutter
  :ensure t
  :init
  (setq git-gutter:update-interval 2
        git-gutter:ask-p nil)
  :config
  (global-git-gutter-mode)
  (smartrep-define-key global-map "C-c g"
    '(("n" . git-gutter:next-hunk)
      ("p" . git-gutter:previous-hunk)
      ("s" . git-gutter:stage-hunk)
      ("r" . git-gutter:revert-hunk)
      ("c" . git-gutter:clear))))

;; Fastnav, operate on the next/previous nth character.
(use-package fastnav
   :ensure t
   :bind
   (("M-z" . fastnav-zap-up-to-char-forward)
    ("C-M-Z" . fastnav-zap-up-to-char-backward)))

;; Shortcuts to operate on number at point.
(use-package operate-on-number
 :config
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
     ("'" . operate-on-number-at-point))))

;;; Mail
(use-package post
  :mode ("/tmp/mutt.*$" . post-mode))

;; (use-package "mail"
;;   :config
;;   (hook-up 'mail-mode-hook
;;     (auto-fill-mode)))

;;; Programming modes
;; Lucy
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Smart pairing.
(use-package smartparens
  :init
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  :config
  (hook-up 'prog-mode-hook
    (require 'smartparens-config)
    (sp-use-paredit-bindings)
    (show-smartparens-mode)
    (smartparens-mode)))

;; Code checking.
(use-package flycheck
  :ensure t
  :config
  (hook-up 'prog-mode-hook'
    (set-face-background 'flycheck-error "#660000")
    (set-face-foreground 'flycheck-error nil)
    (set-face-background 'flycheck-warning "#775500")
    (set-face-foreground 'flycheck-warning nil)
    (flycheck-mode)))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (hook-up 'flycheck-mode-hook
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; Autocomplete.
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 3
        company-tooltip-flip-when-above t)
  :config
  (hook-up 'prog-mode-hook
    (company-mode)))

;; Snippets
(use-package yasnippet
  :bind
  (("<tab>" . nil)
   ("TAB" . nill)
   ("M-TAB" . yas-expand))
  :init
  (setq yas-snippet-dirs (expand-file-name "snippets" dotfiles-dir)
        yas-indent-line nil)
  :config
  (yas-global-mode))

;; Some common prog mode stuff
(use-package "prog-mode"
  :config
  (hook-up 'prog-mode-hook
    (auto-fill-comments)
    (prelude-font-lock-comment-annotations)))

;;; Programming language specifics
;; Shell scripting
(use-package sh-script
  :mode "\\.zsh$"
  :config
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;; Web-mode/JSON/Javascript
(use-package js2-mode :ensure t)
(use-package json-mode :ensure t)
(use-package web-mode
  :ensure t
  :mode (("\\.js[x]?\\'" . web-mode)
         ("\\.json$" . web-mode)
         ("\\.css$" . web-mode)
         ("\\.html$" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
  :config
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Racket
(use-package racket-mode
  :config
  (dolist (hook '(racket-mode-hook racket-repl-mode-hook))
    (add-hook 'racket-mode-hook 'racket-unicode-input-method-enable)))

;;; Global keybindings
(bind-keys
 ("M-3" . split-window-horizontally)
 ("M-2" . split-window-vertically)
 ("M-1" . delete-other-windows)
 ("M-0" . delete-window)
 ("M-o" . other-window)
 ("C-M-j" . join-line)
 ("RET" . newline-and-indent)
 ("C-a" . prelude-move-beginning-of-line)
 ("C-c C-c r" . prelude-rename-buffer-and-file)
 ("C-c C-c d" . prelude-delete-file-and-buffer))

(provide 'init)
;;; init.el ends here
