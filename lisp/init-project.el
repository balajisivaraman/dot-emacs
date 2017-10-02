;;; init-project.el --- Project Management in Emacs made easier. -*- lexical-binding: t -*-

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

;; This module loads Projectile, which is the be-all and end-all for Emacs project management.

;;; Code:

(use-package projectile
  :defer t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :bind
  ("C-c C-f" . helm-projectile)
  :config
  (setq projectile-switch-project-action 'helm-projectile))

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

(use-package org-projectile
  :config
  (setq org-projectile-projects-file "/media/backup/Owncloud/Personal Notes/projects.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (add-to-list
   'org-capture-templates
   (org-projectile-project-todo-entry
    :capture-character "l"
    :capture-template "* TODO %? %a\n"
    :capture-heading "Linked Project TODO"))
  (add-to-list
   'org-capture-templates
   (org-projectile-project-todo-entry
    :capture-character "p")))

(use-package org-projectile-helm
  :after org-projectile
  :bind (("C-c n p" . org-projectile-helm-template-or-project)))

(provide 'init-project)
;;; init-project.el ends here
