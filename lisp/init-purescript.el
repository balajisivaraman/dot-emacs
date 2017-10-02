;;; init-purescript.el --- Purescript configuration -*- lexical-binding: t -*-

;; Copyright (C) 2017  Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module initializes configurations for working with Purescript.

;;; Code:

(use-package purescript-mode
  :commands purescript-mode)

(use-package flycheck-purescript
  :after purescript-mode
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-purescript-setup))

(use-package psci
  :after purescript-mode)

(use-package psc-ide
  :commands psc-ide-server-start
  :after purescript-mode)

(defun balaji-purescript-mode-hook ()
  "Hooks for purescript mode."
  (psc-ide-mode)
  (company-mode)
  (flycheck-mode)
  (inferior-psci-mode)
  (turn-on-purescript-indentation)
  (diminish 'purescript-indentation-mode)
  (psc-ide-server-start (locate-dominating-file default-directory "bower.json")))

(add-hook 'purescript-mode-hook 'balaji-purescript-mode-hook)

(provide 'init-purescript)
;;; init-purescript.el ends here
