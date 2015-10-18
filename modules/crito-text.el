;; crito-text.el --- Everything about human readable text.

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
(defun crito-text-mode-defaults ()
  "Default text mode hook, useful with any text."
  (when-program-exists ispell-program-name '(lambda () (flyspell-mode +1)))
  (abbrev-mode)
  (auto-fill-mode +1))

(add-hook 'text-mode-hook 'crito-text-mode-defaults)

(provide 'crito-text)

;;; crito-text.el ends here
