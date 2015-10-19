;; crito-markdown.el --- Configure the markdown mode

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
;; The pandoc library should be installed.

;;; Code:
(require 'crito-packages)

(defvar crito-markdown-packages
  '(markdown-mode
    pandoc-mode)
  "A list of all packages that are installed from ELPA for markdown.")

;; Install all packages.
(crito-install-packages crito-markdown-packages)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Github flavored markdown has a few slight differences. Most README
;; files will be written in GFM though.
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(defun crito-markdown-mode-defaults ()
  "Default markdown hook."
  (when-program-exists "pandoc" '(lambda () (pandoc-mode))))

(add-hook 'markdown-mode-hook 'crito-markdown-mode-defaults)

(provide 'crito-markdown)

;;; crito-markdown.el ends here
