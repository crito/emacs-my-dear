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

(defun crito-add-subfolders-to-load-path (root-dir)
  "Add all sub directories of ROOT-DIR recursively to the load path."
  (dolist (f (directory-files root-dir))
    (let ((dir (expand-file-name f root-dir)))
      (when (and (file-directory-p dir)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path dir)
        (crito-add-subfolders-to-load-path dir)))))

;; Always load newest byte code.
(setq load-prefer-newer t)

;; Configure various directory locations.
(defvar dotfiles-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration.")
(defvar modules-dir (expand-file-name "modules" dotfiles-dir)
  "This directory contains all Emacs modules.")
(defvar site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir)
  "The site-lisp directory of this Emacs distribution.")

;; Add to the load path.
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path site-lisp-dir)
(crito-add-subfolders-to-load-path modules-dir)
(crito-add-subfolders-to-load-path site-lisp-dir)

;; Load all modules.
(defvar crito-all-modules
      '(crito-functions
        crito-packages
        crito-ui
        crito-editor
        crito-programming
        crito-text
        crito-web-dev
        crito-markdown
        crito-haskell
        crito-shell
        crito-snippets
        crito-bindings)
      "List all modules we want to start.")

(dolist (module crito-all-modules)
  (require module))

(provide 'init)

;;; init.el ends here
