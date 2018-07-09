;;; init-syntax-checkers.el --- Flycheck, Flyspell and other essential syntax checkers. -*- lexical-binding: t -*-

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

;; Load `flycheck' and `flyspell' for enhanced syntax checking in Emacs.

;;; Code:

(use-package flycheck
  :defer 5
  :bind (("C-c e" . balaji-flycheck-errors/body)
         ("C-c t f" . flycheck-mode))
  :init
  (defhydra balaji-flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message"))
  (global-flycheck-mode)
  :config
  (setq
   flycheck-standard-error-navigation nil
   flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
   flycheck-scalastylerc "scalastyle_config.xml")
  :diminish (flycheck-mode . " Ⓢ"))

(use-package flycheck-proselint
  :ensure nil
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-proselint-setup))

(use-package flycheck-pos-tip
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode))

;; Flyspell Mode
(use-package flyspell
  :diminish (flyspell-mode . " Ⓕ")
  :ensure nil
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init
  (use-package ispell
    :ensure nil
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)))
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package simple
  :ensure nil
  :bind (("M-g n" . balaji-errors/next-error)
         ("M-g p" . balaji-errors/previous-error))
  :init
  (defhydra balaji-errors ()
    "Errors."
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("f" first-error "first")
    ("q" nil "Quit" :exit t )))

(provide 'init-syntax-checkers)
;;; init-syntax-checkers.el ends here
