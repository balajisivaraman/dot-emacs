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
  :commands
  (magit-status
   magit-branch
   magit-merge
   magit-stash
   magit-stash-apply
   magit-pull
   magit-reset-head
   magit-reset-hard
   magit-log-all
   magit-log
   magit-checkout)
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0")
  (bs/general-bindings
   "gs" 'magit-status
   "gb" 'magit-branch
   "gp" 'magit-pull
   "gr" 'magit-reset-head
   "gR" 'magit-reset-head-hard
   "gf" 'magit-fetch
   "gl" 'magit-log-all
   "gL" 'magit-log
   "gc" 'magit-checkout))

(use-package magit-delta
  :after magit
  :hook ((magit-mode . (lambda () (magit-delta-mode +1))))
  :config
  (setq magit-delta-default-dark-theme "gruvbox-dark"))

(use-package gist
  :commands (gist-list gist-region-or-buffer)
  :config
  (bs/general-bindings
   "ggl" 'gist-list
   "ggb" 'gist-region-or-buffer))

(use-package git-timemachine
  :commands (git-timemachine)
  :init
  (bs/general-bindings
   "gt" 'git-timemachine))

(provide 'init-version-control)
;;; init-version-control.el ends here
