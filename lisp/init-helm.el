;;; init-helm.el --- Life cannot function without Helm. -*- lexical-binding: t -*-

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

;; This file contains initialization code for Helm and assorted packages.

;;; Code:

(use-package helm-config
  :ensure helm
  :demand t
  :bind
  (("M-x" . helm-M-x)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-c f f" . helm-find-files)
   ("C-c f F" . helm-multi-files)
   ("C-c r r" . helm-do-ag)
   ("C-c r b" . helm-do-ag-buffers)
   ("C-c r p" . helm-do-ag-project-root)
   ("C-c j t" . helm-imenu))
  :config
  (use-package helm-files
    :ensure nil)
  (use-package helm-buffers
    :ensure nil)
  (use-package helm-descbinds
    :bind
    ("C-h b" . helm-descbinds))
  (use-package helm-mode
    :ensure nil
    :diminish helm-mode
    :init
    (helm-mode 1))
  (use-package helm-flycheck
    :after flycheck)
  (use-package helm-projectile
    :bind
    ("C-c C-f" . helm-projectile)
    :config
    (setq projectile-switch-project-action 'helm-projectile))
  (helm-autoresize-mode 1)
  (setq-default
   helm-display-header-line nil
   helm-autoresize-min-height 10
   helm-autoresize-max-height 35
   helm-split-window-in-side-p t
   helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s"
   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-apropos-fuzzy-match t)
  ;; Helm Bindings
  (bind-key "C-k" 'helm-previous-line helm-map)
  (bind-key "C-j" 'helm-next-line helm-map)
  (bind-key "C-l" 'helm-execute-persistent-action helm-find-files-map)
  (bind-key "C-h" 'helm-find-files-up-one-level helm-find-files-map))

(provide 'init-helm)
;;; init-helm.el ends here
