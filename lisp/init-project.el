;;; init-project.el --- Project Management in Emacs made easier. -*- lexical-binding: t -*-

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2017 Balaji Sivaraman

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

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
  (setq  org-projectile-projects-file "/media/backup/Owncloud/Personal Notes/projects.org")
  (add-to-list
   'org-capture-templates
   (org-projectile-project-todo-entry
    :capture-character "l"
    :capture-template "* TODO %? %a\n"
    :capture-heading "Linked Project TODO"))
  (add-to-list
   'org-capture-templates
   (org-projectile-project-todo-entry
    :capture-character "p"))
  (setq org-confirm-elisp-link-function nil)
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-projectile-helm
  :after org-projectile
  :bind (("C-c n p" . org-projectile-helm-template-or-project)))

(provide 'init-project)
;;; init-project.el ends here
