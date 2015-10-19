;; crito-programming.el --- I do a lot of programming.

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
;; For the YAML flychecker install js-yaml.
;;   npm install -g js-yaml
;;; Code:

(require 'crito-packages)

(defvar crito-programming-packages
  '(flycheck
    flycheck-pos-tip
    guru-mode
    company)
  "A list of all packages that are installed from ELPA for programming.")

(defvar crito-programming-modes
  '(clojure-mode
    coffee-mode
    csv-mode
    d-mode
    dart-mode
    elixir-mode
    erlang
    go-mode
    groovy-mode
    haml-mode
    auctex
    lua-mode
    puppet-mode
    php-mode
    pkgbuild-mode
    rust-mode
    scala-mode2
    swift-mode
    textile-mode
    yaml-mode
    dockerfile-mode)
  "A list of default programming modes.")

(crito-require-packages crito-programming-packages)
(crito-require-packages crito-programming-modes)

;; Take from prelude.
(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun crito-auto-fill-comments ()
  "Auto fill comments."
  (set (make-local-variable 'comment-auto-fill-only-comments) +1)
  (auto-fill-mode +1))

;; Lucy ...
(require 'rainbow-delimiters)
(require 'rainbow-mode)

;; smart pairing for all
(require 'smartparens)
(require 'smartparens-config)

(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

;; guru mode
(require 'guru-mode)

;; show the name of the current function definition in the modeline
(require 'which-func)
(add-to-list 'which-func-modes 'ruby-mode)
(which-function-mode 1)

;; configure code checking
(require 'flycheck)
(require 'flycheck-pos-tip)
(eval-after-load "flycheck"
  '(progn
     (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
     (set-face-background 'flycheck-error "#660000")
     (set-face-foreground 'flycheck-error nil)
     (set-face-background 'flycheck-warning "#775500")
     (set-face-foreground 'flycheck-warning nil)))

;; Enable auto completion with company mode
(require 'company)

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)

(defun crito-prog-mode-defaults ()
  "Default programming hook, useful with any programming language."
  (when-program-exists ispell-program-name '(lambda () (flyspell-prog-mode)))
  (guru-mode)
  (prelude-font-lock-comment-annotations)
  (flycheck-mode)
  (crito-auto-fill-comments)
  (company-mode)
  (rainbow-mode)
  (rainbow-delimiters-mode)
  (smartparens-mode)
  (fci-mode 1))

(add-hook 'prog-mode-hook 'crito-prog-mode-defaults)

;; yaml-mode is not derived from prog-mode.
;; (add-hook 'yaml-mode-hook 'crito-prog-mode-defaults)
(add-hook 'yaml-mode-hook '(lambda () (crito-prog-mode-defaults)))

(provide 'crito-programming)

;;; crito-programming.el ends here
