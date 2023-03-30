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
  :hook (magit-mode
         . (lambda ()
             (unbind-key "C-j" magit-mode-map)))
  :bind
  (("C-x g" . magit-status)
   ("C-c g s" . magit-status)
   ("C-c g p" . magit-pull)
   ("C-c g l" . magit-log-all)
   ("C-c g L" . magit-log)
   ("C-c g c" . magit-checkout)))

(use-package magit-delta
  :after magit
  :hook ((magit-mode . (lambda () (magit-delta-mode +1))))
  :custom
  (magit-delta-default-dark-theme "Dracula"))

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)))

(provide 'init-version-control)
;;; init-version-control.el ends here
