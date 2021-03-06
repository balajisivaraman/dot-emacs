;;; init-syntax-checkers.el --- Flycheck, Flyspell and other essential syntax checkers. -*- lexical-binding: t -*-

;; Copyright (C) 2021  Balaji Sivaraman

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

;; Load `flymake' and `flyspell' for enhanced syntax checking in Emacs.

;;; Code:

(use-package flymake
  :ensure nil
  :bind (("M-g n" . bs/flymake-errors/flymake-goto-next-error)
         ("M-g p" . bs/flymake-errors/flymake-goto-prev-error)
         ("C-c t f" . flycheck-mode))
  :init
  (defhydra bs/flymake-errors ()
    "Flycheck errors."
    ("n" flymake-goto-next-error "next")
    ("p" flymake-goto-prev-error "previous")
    ("q" nil "Quit" :exit t )))

(use-package flymake-proselint
  :after flymake
  :hook (text-mode . bs/flymake-proselint-setup)
  :init
  (defun bs/flymake-proselint-setup ()
    "Setup Flymake Proselint."
    (flymake-mode +1)
    (flymake-proselint-setup)))

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

(provide 'init-syntax-checkers)
;;; init-syntax-checkers.el ends here
