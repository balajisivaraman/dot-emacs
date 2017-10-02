;;; init-haskell.el --- Haskell Mode, Intero and assorted Haskell configurations. -*- lexical-binding: t -*-

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

;; This file configures Haskell Mode, Intero, Structured Haskell Mode and any other config for programming with Haskell.

;;; Code:

(package-require 'haskell-mode)
(load "haskell-mode-autoloads")

(defun balaji/haskell-mode-hook ()
  (haskell-indentation-mode)
  (interactive-haskell-mode)
  (intero-mode t)
  (company-mode t)
  (flycheck-mode))

(use-package haskell-mode
  :mode
  (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
   ("\\.lhs\\'" . literate-haskell-mode))
  :init
  (balaji/setup-unicode-conversions)
  (setq haskell-hoogle-command nil)
  :config
  (use-package flycheck-haskell
    :config
    (flycheck-haskell-setup)
    (bind-key "M-n" #'flycheck-next-error haskell-mode-map)
    (bind-key "M-p" #'flycheck-previous-error haskell-mode-map))
  (use-package haskell-interactive-mode
    :ensure nil)
  (use-package haskell-process
    :ensure nil
    :init
    (setq
     haskell-process-suggest-remove-import-lines t
     haskell-process-auto-import-loaded-modules t
     haskell-process-log t)))

(use-package intero
  :commands (intero-mode))

(add-hook 'haskell-mode-hook 'balaji/haskell-mode-hook)

(provide 'init-haskell)
;;; init-haskell.el ends here
