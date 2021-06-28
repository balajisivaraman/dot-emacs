;;; init-version-control.el ---  Make Emacs the best Git experience in the world. -*- lexical-binding: t -*-

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

;; This module loads Magit, Git Gutter Mode, and other Git-related goodies.

;;; Code:

(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g m" . magit-branch)
   ("C-c g M" . magit-merge)
   ("C-c g S" . magit-stash)
   ("C-c g a" . magit-stash-apply)
   ("C-c g p" . magit-pull)
   ("C-c g r" . magit-reset-head)
   ("C-c g R" . magit-reset-hard)
   ("C-c g l" . magit-log-all)
   ("C-c g L" . magit-log)
   ("C-c g c" . magit-checkout))
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package magit-delta
  :after magit
  :hook ((magit-mode . (lambda () (magit-delta-mode +1))))
  :config
  (setq magit-delta-default-dark-theme "gruvbox-dark"))

(use-package gist
  :bind (("C-c g g l" . gist-list)
         ("C-c g g b" . gist-region-or-buffer)))

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)))

(provide 'init-version-control)
;;; init-version-control.el ends here
