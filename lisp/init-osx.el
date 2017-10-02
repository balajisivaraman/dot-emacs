;;; init-osx.el --- Make working on OSX easier. -*- lexical-binding: t -*-

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

;; Packages to make working in Emacs on OSX a lot easier.

;;; Code:

(use-package ns-win
  :defer t
  :ensure nil
  :if (eq system-type 'darwin)
  :config
  (setq
   ns-pop-up-frames nil
   mac-option-modifier 'meta
   mac-command-modifier 'meta))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :init (osx-trash-setup))

;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind
  (("C-c f J" . reveal-in-osx-finder)))

(provide 'init-osx)
;;; init-osx.el ends here
