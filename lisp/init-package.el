;;; init-package.el --- Initialize package.el and any dependencies. -*- lexical-binding: t -*-

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

;; This file contains initialization code for package.el and convenience packages like use-package and paradox.

;;; Code:

(add-to-list 'load-path (expand-file-name "use-package" balaji/site-lisp-dir))
(eval-when-compile
  (require 'use-package))
;; Always install packages from Melpa, Elpa
;; Over-ridden when not used by setting (:ensure nil) in use-package declarations
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish nil t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
;; (setq package-archives '(("elpa-mirror" . "~/.emacs.d/elpa-mirror/")))
(package-initialize)
;; package.el should not initialize our packages.
;; We're going to use use-package for that.
(setq package-enable-at-startup nil
      ;; don't add that `custom-set-variables' block to my initl!
      package--init-file-ensured t)

(use-package paradox
  :config
  (setq
   paradox-execute-asynchronously nil ; No async update, please
   paradox-spinner-type 'moon      ; Fancy spinner
   ;; Show all possible counts
   paradox-display-download-count t
   paradox-display-star-count t
   ;; Don't star automatically
   paradox-automatically-star nil
   ;; Hide download button, and wiki packages
   paradox-use-homepage-buttons nil ; Can type v instead
   paradox-hide-wiki-packages t))

(use-package use-package-chords
  :config (key-chord-mode 1))

(add-to-list 'load-path (expand-file-name "elpa-mirror" balaji/site-lisp-dir))
(use-package elpa-mirror
  :ensure nil
  :init
  (setq elpamr-default-output-directory "~/.emacs.d/elpa-mirror"))

(provide 'init-package)
;;; init-package.el ends here
