;;; init-project.el --- Project Management in Emacs made easier. -*- lexical-binding: t -*-

;; Copyright (C) 2020  Balaji Sivaraman

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

;; This module loads Projectile, which is the be-all and end-all for Emacs project management.

;;; Code:

(use-package projectile
  :defer t
  :diminish projectile-mode
  :init
  (projectile-mode)
  (unbind-key "C-c p s r")
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq
   projectile-git-command "rg -0 --files --hidden --follow --glob '!.git/*'"
   projectile-generic-command "rg -0 --files --hidden --follow --glob '!.git/*'"))

(use-package ignoramus
  :config
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    (add-to-list 'ignoramus-file-basename-exact-names name))
  (ignoramus-setup))

(use-package bookmark
  :bind
  ("C-c b b" . bookmark-jump)
  ("C-c b m" . bookmark-set)
  ("C-c b l" . bookmark-bmenu-list))

(provide 'init-project)
;;; init-project.el ends here
