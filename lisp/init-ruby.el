;;; -*- lexical-binding: t -*-
;;; init-ruby.el --- Make working with Ruby/Rails a breeze.

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
