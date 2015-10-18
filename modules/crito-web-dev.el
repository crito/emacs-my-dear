;; crito-web-dev.el --- Everything related to web development.

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
;; The following nodejs packages must be installed (best globally):
;; - eslint babel-eslint eslint-plugin-react
;; See for reference:
;;  - http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;  - https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/

;;; Code:
(require 'crito-packages)

(defvar crito-web-dev-packages
  '(js2-mode
    json-mode
    web-mode)
  "A list of packages to install from ELPA for web development.")

(crito-require-packages crito-web-dev-packages)

;; (setq web-mode-content-types-alist
;;       '(("jsx" . "\\.js[x]?\\'")))

(require 'js2-mode)
(require 'json-mode)
(require 'web-mode)

;; use web-mode for the following files
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; Specify the format of the comments.
;; (setq web-mode-comment-formats
;;       '(("javascript" . "//")
;;         ("jsx" . "//")))

(add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
(add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(provide 'crito-web-dev)

;;; crito-web-dev.el ends here
