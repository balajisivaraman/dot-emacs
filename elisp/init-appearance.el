;;; -*- lexical-binding: t -*-
;;; init-appearance.el --- Themes and other assorted superficial configuration.

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2016 Balaji Sivaraman

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

;; This file installs the Solarized Themes and provides handy helper functions to cycle through them.

;;; Code:

(use-package solarized-theme
  :init
  (defvar my-color-themes (list '(solarized-dark) '(solarized-light)))
  (defvar my-current-theme nil)
  (defvar my-theme-list my-color-themes)

  (defun balaji/set-default-theme ()
    (interactive)
    (setq my-current-theme (car (car my-color-themes)))
    (setq my-theme-list (cdr my-color-themes))
    (load-theme my-current-theme t))

  (defun balaji/cycle-themes ()
    (interactive)
    (cond
     ((null my-theme-list)
      (setq my-current-theme (car (car my-color-themes)))
      (setq my-theme-list (cdr my-color-themes)))
     ((listp my-theme-list)
      (setq my-current-theme (car (car my-theme-list)))
      (setq my-theme-list (cdr my-theme-list)))
     ((t)
      (setq my-current-theme (car (car my-theme-list)))
      (setq my-theme-list (my-color-themes))))
    (load-theme my-current-theme t)
    (message "%S" my-current-theme))

  :bind
  ("C-c t" . balaji/cycle-themes))

(balaji/set-default-theme)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode)
  (spaceline-emacs-theme))

(provide 'init-appearance)
;;; init-appearance.el ends here;
