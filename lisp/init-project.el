;;; init-project.el --- Custom functions for project management. -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

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

;; This file sets up and configures custom functions for project
;; management. It is meant to be a light-weight alternative for
;; something like Projectile.

;;; Code:

(use-package project
  :ensure nil
  :bind
  (("C-x p C" . project-compile)
   ("C-x p p" . bs/open-my-projects))
  :init
  (defun bs/project-vterm ()
  "Opens a new vterm buffer at project root."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current)))
         (project-root-name (s-replace-all '(("." . "")) (f-filename default-directory)))
         (vterm-buffer-name (format "*%s-vterm*" project-root-name)))
    (vterm)))
  :config
  (defun bs/open-my-projects ()
  "Opens a project within ~/projects/costa."
  (interactive)
  (let* ((project--list (mapcar (lambda (path) (list (f-short path))) (f-directories "~/projects/"))))
    (call-interactively 'project-switch-project)))
  (setq project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir")
                                  (bs/project-vterm "Vterm"))))

(bind-key "C-x p t" 'bs/project-vterm)

(provide 'init-project)
;;; init-project.el ends here
