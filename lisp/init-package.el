;;; init-package.el --- Initialize package.el and any dependencies. -*- lexical-binding: t -*-

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

;; This file contains initialization code for package.el and convenience packages like use-package and paradox.

;;; Code:

(add-to-list 'load-path (expand-file-name "use-package" balaji/site-lisp-dir))
(require 'use-package)
;; Always install packages from Melpa, Elpa
;; Over-ridden when not used by setting (:ensure nil) in use-package declarations
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish nil t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
(package-initialize)
;; package.el should not initialize our packages.
;; We're going to use use-package for that.
(setq package-enable-at-startup nil)

(defun package-require (package)
  "Install `PACKAGE` only if it is not already installed."
  (unless (package-installed-p package)
    (package-install package nil)))

(use-package paradox
  :bind (("C-c a p" . paradox-list-packages)
         ("C-c a P" . package-list-packages-no-fetch)
         ("C-c a u" . paradox-upgrade-packages))
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

(provide 'init-package)
;;; init-package.el ends here
