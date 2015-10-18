;;; packages.el --- Handle packages for emacs.

;; Copyright (c) 2015 crito
;;
;; Author: crito <crito@cryptodrunks.net>

;;; Commentary:

;; Takes care of the automatic installation of all the packages required.

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
(require 'package)
(require 'cl)

(defun crito-require-package (pkg)
  "Install PKG only if it's not already installed."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun crito-require-packages (pkgs)
  "Install PKGS only if they are not already installed."
  (dolist (pkg pkgs)
    (crito-require-package pkg)))

(defun crito-packages-installed-p (pkgs)
  "Check if all PKGS are installed"
  (every #'package-installed-p pkgs))

(defun crito-install-packages (pkgs)
  "Install all PKGS."
  (unless (crito-packages-installed-p pkgs)
    (message "%s" "Emacs is refreshing it's package database ...")
    (package-refresh-contents)
    (message "%s" "done.")
    ;; install the missing packages.
    (crito-require-packages pkgs)))

(dolist (source '(("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(setq package-user-dir (expand-file-name "elpa" dotfiles-dir))

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(defvar crito-packages
  '(grizzl
    magit
    smartrep)
  "A list of all packages that are installed from ELPA.")

;; Install all base packages.
(crito-install-packages crito-packages)

(provide 'crito-packages)
;;; crito-packages.el ends here
