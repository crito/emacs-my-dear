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

;; Configure various directory locations.
(defvar dotfiles-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration.")
(defvar site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir)
  "The site-lisp directory of this Emacs distribution.")
(defvar backup-dir (expand-file-name "bak" dotfiles-dir)
  "Write backup files to this directory.")

;; Add to the load path.
(add-subfolders-to-load-path site-lisp-dir)

(require 'package)

(defvar bootstrap-packages
  '(use-package
    diminish
    bind-key)
  "Those packages are required from bootstrap on.")

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(install-packages bootstrap-packages)

(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

(setq load-prefer-newer t
      require-final-newline t
      blink-matching-paren 'jump)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-column 78
              indicate-empty-line t
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              bidi-display-reordering nil)

(defvar indent-sensitive-modes
  '(coffee-mode slim-mode python-mode))
(defvar progish-modes
  '(prog-mode css-mode))
(defvar lispy-modes '(emacs-lisp-mode
                      clojure-mode
                      racket-mode
                      ielm-mode
                      eval-expression-minibuffer-setup))
(defvar ruby-modes
  '(ruby-mode slim-mode inf-ruby-mode))
(defvar shellish-modes
  '(comint-mode compilation-mode magit-process-mode))
(defvar writing-modes
  '(org-mode markdown-mode text-mode))

;; Alternative ways to issue commands to emacs.
(use-package use-package-chords
  :ensure)

(use-package smartrep
  :ensure)

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
  :ensure
  :config
  (exec-path-from-shell-initialize)
  (dolist (var env-vars-from-shell)
    (exec-path-from-shell-copy-env var)))

;;; Basic libraries
(use-package dash
  :ensure
  :config
  (dash-enable-font-lock))

(use-package cl-lib
  :ensure)

;; The helpers rely on dash and cl-lib.
(use-package helpers
  :load-path "site-lisp/misc")

(use-package s
  :ensure
  :commands
  (s-lower-camel-case s-upper-camel-case s-snake-case s-dashed-words)
  :config
  (make-transform-symbol-at-point-defun s-lower-camel-case)
  (make-transform-symbol-at-point-defun s-upper-camel-case)
  (make-transform-symbol-at-point-defun s-snake-case)
  (make-transform-symbol-at-point-defun s-dashed-words))

;;; UI elements
(use-package "subr"
  :init
  (defalias 'yes-or-no-p #'y-or-n-p))

(use-package startup
  :defer
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        inhibit-startup-echo-area-message ""))

(use-package scrolling
  :defer
  :init
  (setq scroll-margin 0
        scroll-conservatively 100000
        scroll-preserve-screen-position 1))

(use-package tool-bar
  :defer
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :defer
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :defer
  :config
  (menu-bar-mode -1))

(use-package novice
  :defer
  :init (setq disabled-command-function nil))

;; Let's be hard on ourselves ...
(use-package guru-mode
  :ensure
  :config
  (dolist (hook '(prog-mode-hook text-mode-hook))
      (hook-λ hook
        (guru-mode))))

(use-package winner
  :ensure
  :config
  (winner-mode))

(use-package hi-lock
  :ensure
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))

(use-package fringe
  :config
  (fringe-mode 4))

(use-package zenburn-theme
  :ensure
  :init
  (load-theme 'zenburn t))

;;; Configure the editor
(use-package utf-8-support
  :defer
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (load-library "iso-transl"))

;; Use EasyPG for ~/.authinfo.gpg and other encrypted files.
(use-package epa-file
  :config
  (epa-file-enable))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-max-saved-items 1000))

(use-package files
  :defer
  :config
  (setq require-final-newline t
        confirm-kill-emacs nil
        confirm-nonexistent-file-or-buffer nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (advice-add 'helm-find-file :before #'find-file-maybe-make-directories)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package simple
  :bind (("M-`" . jump-to-mark)
         ([remap set-mark-command] . push-mark-no-activate)
         ([remap exchange-point-and-mark] . exchange-point-and-mark-no-activate))
  :config
  (column-number-mode)
  (line-number-mode)
  (size-indication-mode)
  (auto-save-mode -1)
  ;; (advice-add 'jump-to-mark :after #'recenter-top-bottom)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (hook-λ 'prog-mode-hook
    (auto-fill-comments)
    (global-prettify-symbols-mode)
    (prelude-font-lock-comment-annotations)))

(use-package autorevert
  :config
  (global-auto-revert-mode)
  (setq global-auto-revert-non-file-buffers t))

(use-package "window"
  :config
  (smartrep-define-key global-map "C-x"
    '(("{" . shrink-window-horizontally)
      ("}" . enlarge-window-horizontally))))

;; Sometimes handy to visualize what I'm doing.
(use-package command-log-mode
  :ensure)

;; Pretty smart expanding.
(use-package hippie-expand
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
  ;; (bind-key "TAB" #'hippie-expand read-expression-map)
  ;; (bind-key "TAB" #'hippie-expand minibuffer-local-map)
  ;; (bind-key* "M-?" (make-hippie-expand-function '(try-expand-line) t))
  (setq hippie-expand-verbose nil
        hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                           try-expand-dabbrev
                                           try-expand-dabbrev-matching-buffers
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-dabbrev-other-buffers))
  (hook-modes lispy-modes
    (setq-local hippie-expand-try-functions-list
                (append '(try-complete-lisp-symbol-partially
                          try-complete-lisp-symbol)
                        hippie-expand-try-functions-list))))

;; Saner regex syntax
(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

;; ace products
(use-package ace-window
  :ensure
  :bind ("M-o" . ace-window))

(use-package ace-jump-buffer
  :ensure
  :chords ((";a" . ace-jump-buffer)
           (":A" . ace-jump-buffer-other-window)
           (";x" . ace-jump-shellish-buffers))
  :config
  (make-ace-jump-buffer-function "shellish"
    (with-current-buffer buffer
      (not (derived-mode-p 'comint-mode))))
  (setq ajb-home-row-keys t))

(use-package ace-jump-mode
  :demand
  :ensure
  :bind (("C-;" . ace-jump-mode))
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode))
  :config
  (ace-jump-mode-enable-mark-sync)
  (setq ace-jump-mode-case-fold nil
        ace-jump-mode-scope 'visible))

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
  (setq save-place-file (expand-file-name "saveplace" dotfiles-dir)
        save-place t))

;; Keep track of mini buffer history.
(use-package savehist
  :config
  (setq savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        comint-input-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" dotfiles-dir))
  (savehist-mode))

;; Handle camel case more gracefully.
(use-package subword
  :init
  (global-subword-mode))

;; smarter newline.
(use-package smart-newline
  :ensure
  :config
  (hook-modes progish-modes
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))))

;; Toggle "/' quotes.
(use-package toggle-quotes
  :ensure
  :bind ("C-c q" . toggle-quotes))

;; Abbreviations.
(use-package abbrev
  :init
  (setq abbrev-file-name (expand-file-name "abbrev.defs" dotfiles-dir))
  :config
  (abbrev-mode))

;; Use Helm to navigate the minibuffer
(use-package helm
  :ensure
  :demand
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-c h f" . helm-find-files)
         ("C-c h y" . helm-show-kill-ring)
         ("C-c h b" . helm-mini)
         ("C-c h x" . helm-M-x)
         ("C-c h m" . helm-all-mark-rings))
  :config
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
  (helm-autoresize-mode)

  (use-package helm-descbinds
    :ensure
    :bind (("C-c h d" . helm-descbinds))
    :config
    (helm-descbinds-mode))

  (use-package helm-ag
    :ensure
    :bind (("C-c h s" . helm-ag-project-root)))

  (use-package helm-ls-git
    :ensure
    :bind (("C-c h g" . helm-ls-git-ls)))

  (use-package helm-projectile
    :ensure
    :demand
    :bind (("C-c h p" . helm-projectile)))

  ; save current position to mark ring.
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring))

;; Set the spell checker
(use-package flyspell
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  :config
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;; Semantically expand a region.
(use-package expand-region
  :ensure
  :bind (("C-=" . er/expand-region)))

;; bookmark
(use-package bookmark
  :defer
  :init
  (setq bookmark-default-file (expand-file-name "bookmarks" dotfiles-dir)
        bookmark-save-flag 1))

;; Projectile project management
(use-package projectile
  :ensure
  :bind (([remap projectile-switch-project] . helm-projectile-switch-project)
         ([remap projectile-find-file] . helm-projectile-find-file-dwim)
         ([remap projectile-switch-to-buffer] . helm-projectile-switch-to-buffer))
  :init
  (setq projectile-cache-file (expand-file-name "projectile.cache"
                                                dotfiles-dir))
  :config
  (use-package projectile-rails
    :ensure
    :config
    (add-hook 'projectile-mode-hook #'projectile-rails-on))

  (projectile-global-mode)
  (projectile-cleanup-known-projects))

;; Tramp for sudo or ssh access.
(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

;; anzu enhances query-replace by showing total matches and position matches.
(use-package anzu
  :ensure
  :demand
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)
         ("C-c a" . anzu-replace-at-cursor-thing))
  :config
  (global-anzu-mode))

;; Overview while searching for a regex.
(use-package swiper
  :ensure
  :bind (([remap isearch-forward]  . swiper)
         ([remap isearch-backward] . swiper)
         ("C-c C-r" . ivy-resume))
  :config
  (advice-add 'swiper :after #'recenter-top-bottom))

;; easy-kill/easy-mark
(use-package easy-kill
  :ensure
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; ediff
(use-package ediff
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Smarter kill-ring navigation
(use-package browse-kill-ring
  :ensure
  :bind (("C-M-y" . browse-kill-ring))
  :init
  (setq browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item 'solid))

;; Highlight current line.
(use-package hl-line
  :config (global-hl-line-mode))

;; volatile highlights
(use-package volatile-highlights
  :ensure
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode))

;; sensible undo
(use-package undo-tree
  :ensure
  :diminish undo-tree-mode
  :bind (("C--" . undo-tree-undo)
         ("C-+" . undo-tree-redo))
  :config
  (global-undo-tree-mode))

;;(use-package newcomment
  ;;:bind
  ;;("C-/" . comment-or-uncomment-region)
  ;;:config
  ;;(advice-add 'comment-or-uncomment-region :before #'with-region-or-line)
  ;;)

;; Indicate the 80 columns limit.
(use-package fill-column-indicator
  :ensure
  :config
  (setq fci-rule-character-color "#262626"
        fci-rule-column 80
        fci-always-use-textual-rule t)

  (hook-λ 'prog-mode-hook (fci-mode 1)))

;; Always show the git gutter
(use-package git-gutter
  :ensure
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode)
  (custom-set-variables
   '(git-gutter:update-interval 2)
   '(git-gutter:unchanged-sign " "))
  (smartrep-define-key global-map "C-c g"
    '(("n" . git-gutter:next-hunk)
      ("p" . git-gutter:previous-hunk)
      ("s" . git-gutter:stage-hunk)
      ("r" . git-gutter:revert-hunk)
      ("c" . git-gutter:clear))))

;; Fastnav, operate on the next/previous nth character.
(use-package fastnav
   :ensure
   :bind (("M-z" . fastnav-zap-up-to-char-forward)
          ("C-M-Z" . fastnav-zap-up-to-char-backward)))

;; Shortcuts to operate on number at point.
(use-package operate-on-number
  :ensure
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

(use-package ag
  :ensure
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t))

;;; Mail
(use-package post
  :load-path "site-lisp/mail"
  :mode (("/tmp/mutt.*$" . post-mode)))

;;; Text modes
(use-package pandoc-mode
  :ensure
  :bind (("C-c C-e p" . pandoc-convert-to-pdf)))

(use-package markdown-mode
  :ensure
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("^README\\.md$" . gfm-mode))
  :config
  (hook-λ 'markdown-mode-hook
    (when-program-exists "pandoc" #'pandoc-mode)))

(use-package yaml-mode
  :ensure
  :mode (("\\.y[a]?ml$" . yaml-mode)))

;;; Programming modes
;; Lucy
(use-package rainbow-mode
  :ensure
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Smart pairing.
(use-package smartparens
  :ensure
  :init
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  :config
  (use-package smartparens-config)
  (hook-λ 'prog-mode-hook
    (sp-use-paredit-bindings)
    (show-smartparens-mode)
    (smartparens-mode)))

;; Code checking.
(use-package flycheck
  :ensure
  :config
  (hook-λ 'prog-mode-hook
    (set-face-background 'flycheck-error "#660000")
    (set-face-foreground 'flycheck-error nil)
    (set-face-background 'flycheck-warning "#775500")
    (set-face-foreground 'flycheck-warning nil)
    (flycheck-mode)))

(use-package flycheck-pos-tip
  :ensure
  :config
  (hook-λ 'flycheck-mode-hook
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; Autocomplete.
(use-package company
  :ensure
  :config
  (global-company-mode)
  (setq company-idle-delay 0.5
        company-tooltip-align-annotations t
        company-require-match nil
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 3
        company-tooltip-flip-when-above t
        company-occurrence-weight-function #'company-occurrence-prefer-any-closest
        company-continue-commands
        (append company-continue-commands '(comint-previous-matching-input-from-input
                                            comint-next-matching-input-from-input)))
  (use-package company-dabbrev
    :config
    (setq company-dabbrev-minimum-length 3))
  (use-package company-dabbrev-code
    :config
    (setq company-dabbrev-code-modes t
          company-dabbrev-code-everywhere t))
  (use-package readline-complete
    :ensure
    :config
    (push #'company-readline company-backends)))

;; FIXME: Can't get yasnippet to run.
;; Snippets
;; (use-package yasnippet
;;   :ensure
;;   :mode
;;   (("\\.snippet$" . snippet-mode))
;;   :config
;;   (setq yas-snippet-dirs (expand-file-name "snippets" dotfiles-dir)
;;         yas-indent-line nil)
;;   (hook-λ 'prog-mode-hook
;;     (bind-keys :map yas-minor-mode-map
;;                ("<tab>" . nil)
;;                ("TAB" . nil)
;;                ("M-TAB" . yas-expand))
;;     (yas-minor-mode 1))

;;   ;;(yas-global-mode 1)
;;   )

;; Maintain a REST calls in a text file
(use-package restclient
  :ensure
  :mode (("\\.restclient$" . restclient-mode)))

;;; Programming language specifics
;; Shell scripting.
(use-package sh-script
  :mode (("\\.zsh$" . shell-script-mode)
         ("\\.rc$" . shell-script-mode)
         ("^\\.aliases$" . shell-script-mode)
         ("^\\.bash_profile$" . shell-script-mode)
         ("^\\.bashrc$" . shell-script-mode))
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

;; Files with a shebang #! at the beginning.
(use-package executable
  :defer
  :config
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;; Web-mode/JSON/Javascript
(use-package js2-mode
  :ensure)
(use-package json-mode
  :ensure)
(use-package web-mode
  :ensure
  :mode (("\\.js[x]?\\'" . web-mode)
         ("\\.json$" . web-mode)
         ("\\.css$" . web-mode)
         ("\\.html$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Coffee-script
(use-package coffee-mode
  :ensure
  :mode (("\\.coffee$" . coffee-mode))
  :bind (("C-c C-c" . coffee-compile-region-or-buffer))
  :config
  (setq coffee-command "coffee"
        coffee-tab-width 2
        coffee-debug-mode t
        coffee-args-repl '("-i" "--nodejs"))
  (add-to-list 'coffee-args-compile "--no-header")
  (add-to-list 'coffee-args-compile "-c")
  (add-to-list 'coffee-args-compile "--bare")
  (hook-λ 'coffee-mode-hook
    (subword-mode))
  ;; (bind-keys :map coffee-mode-map
  ;;            ("<C-return>" . coffee-smarter-newline)
  ;;            ("C-c C-c" . coffee-compile-region))
  )

;; Slim
(use-package slim-mode
  :ensure
  :config
  (setq slim-backspace-backdents-nesting nil)
  (hook-λ 'slim-mode-hook (modify-syntax-entry ?\= "."))
  ;; (bind-keys :map slim-mode-map
  ;;            ("<C-return>" . slim-newline-dwim))
  )

;; Ruby
(use-package ruby-mode
  :ensure
  :mode (("\\.rake$" . ruby-mode)
         ("^Gemfile[\\.lock]*$" . ruby-mode))
  :config
  ;; (bind-keys :map ruby-mode-map
  ;;            (":"          . smart-ruby-colon)
  ;;            ("<C-return>" . ruby-newline-dwim))
  (use-package ruby-tools :ensure)
  (use-package rspec-mode :ensure)
  (use-package inf-ruby
    :ensure
    :init
    (hook-λ 'inf-ruby-mode-hook
      (turn-on-comint-history ".pry_history"))
    ;; (bind-key "M-TAB" #'comint-previous-matching-input-from-input inf-ruby-mode-map)
    ;; (bind-key "<M-S-tab>" #'comint-next-matching-input-from-input inf-ruby-mode-map)
    )
  (use-package bundler
    :ensure
    ;;:config
    ;; (bind-key "G" #'bundle-open projectile-rails-command-map)
    )
  (use-package ruby-hash-syntax
    :ensure
    ;;:init
    ;; (bind-key "C-c C-:" #'ruby-toggle-hash-syntax ruby-mode-map)
    ))

;; Racket
(use-package racket-mode
  :ensure
  :config
  (dolist (hook '(racket-mode-hook racket-repl-mode-hook))
    (add-hook 'racket-mode-hook 'racket-unicode-input-method-enable)))

;; Various languages
(defvar programming-languages-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; Setup the above programming language modes
;; (mapc
;;  (lambda (entry)
;;    (let ((extension (car entry))
;;          (package (cadr entry))
;;          (mode (cadr (cdr entry))))
;;      (use-package package
;;        :ensure
;;        :mode '((extension . mode)))))
;;  programming-languages-alist)

;; (mapc
;;  (lambdta (entry)
;;    (let ((extension (car entry))
;;          (package (cadr entry))
;;          (mode (cadr (cdr entry))))
;;      (use-package package
;;        :ensure
;;        :mode ((extension . mode)))))
;;  programming-languages-alist)

;;; Global keybindings
(use-package key-chord
  :defer
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.1)
  (hook-λ 'minibuffer-setup-hook
    (set (make-local-variable 'input-method-function) nil)))

(use-package which-key
  :ensure
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2.0)
  (which-key-mode))

(bind-keys
 ("M-0" . delete-window)
 ("M-1" . delete-other-windows)
 ("M-2" . vsplit-last-buffer)
 ("M-3" . hsplit-last-buffer)
 ("C-x 2" . vsplit-same-buffer)
 ("C-x 3" . hsplit-same-buffer)
 ("C-M-j" . join-line)
 ("RET" . newline-and-indent)
 ("C-a" . prelude-move-beginning-of-line)
 ("C-c C-c r" . prelude-rename-buffer-and-file)
 ("C-c C-c d" . prelude-delete-file-and-buffer))

(provide 'init)
;;; init.el ends here
