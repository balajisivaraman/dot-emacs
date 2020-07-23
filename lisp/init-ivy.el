;;; init-ivy.el --- Loads Ivy mode, counsel and swiper -*- lexical-binding: t -*-

;; Copyright (C) 2020 Balaji Sivaraman

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

;; This file sets up Ivy as the default completion framework of choice for Emacs.

;;; Code:

(use-package ivy
  :diminish (ivy-mode)
  :init (ivy-mode t)
  :config
  (setq-default
   ivy-use-virtual-buffers t
   enable-recursive-minibuffers t
   ivy-virtual-abbreviate 'fullpath
   ivy-count-format ""
   projectile-completion-system 'ivy
   ivy-dynamic-exhibit-delay-ms 150
   ivy-initial-inputs-alist nil
   ivy-height 10))

(use-package counsel
  :after ivy
  :diminish (counsel-mode)
  :init (counsel-mode t)
  :config
  (setq-default counsel-mode-override-describe-bindings t))

(use-package counsel-projectile
  :after (counsel projectile)
  :bind
  (("C-c p h" . counsel-projectile-find-file)
   :map projectile-mode-map
   ("C-c p s r" . counsel-projectile-rg)))

(use-package swiper
  :after (ivy)
  :bind
  (:map global-map
        ("C-S-s" . swiper)))

(use-package amx
  :after ivy
  :config
  (setq
   amx-backend 'auto
   amx-save-file "~/.emacs.d/temp/amx-items"
   amx-history-length 50
   amx-show-key-bindings nil)
 (amx-mode t))

(provide 'init-ivy)
;;; init-ivy.el ends here
