;;; -*- lexical-binding: t -*-
;;; init-ruby.el --- Make working with Ruby/Rails a breeze.

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

;; Loads Enhanced Ruby Mode, Robe, RVM and a bunch of other Ruby-editing Emacs packages.

;;; Code:

(use-package enh-ruby-mode
  :mode "\\(\\.rb\\|.erb\\|Gemfile\\)"
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package robe
  :diminish robe-mode
  :commands robe-mode)

(use-package rvm
  :commands rvm-activate-corresponding-ruby)

(use-package rspec-mode
  :diminish rspec-mode
  :commands rspec-mode
  :config
  (rspec-install-snippets))

(use-package rubocop
  :diminish rubocop-mode
  :commands rubocop-mode)

(use-package ruby-tools
  :diminish ruby-tools-mode
  :commands ruby-tools-mode)

(use-package bundler
  :bind
  (:map enh-ruby-mode-map
        ("C-c m b i" . bundle-install)
        ("C-c m b u" . bundle-update)
        ("C-c m b c" . bundle-check)))

(defun balaji-ruby-mode-hooks ()
  "Hooks for enhanced ruby mode."
  (robe-mode t)
  (add-to-list 'company-backends 'company-robe)
  (company-mode t)
  (rvm-activate-corresponding-ruby)
  (rspec-mode t)
  (rubocop-mode t)
  (ruby-tools-mode t))

(add-hook 'enh-ruby-mode-hook 'balaji-ruby-mode-hooks)

(provide 'init-ruby)
;;; init-ruby.el ends here
