;;; -*- lexical-binding: t -*-
;;; init-haskell.el --- Haskell Mode, GHC-Mod and assorted Haskell configurations

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

;; This file configures Haskell Mode, GHC-Mod, Structured Haskell Mode and any other config for programming with Haskell.

;;; Code:

(require 'init-package)

(package-require 'haskell-mode)
(load "haskell-mode-autoloads")

(defun balaji/haskell-mode-hook ()
  (haskell-indentation-mode)
  (interactive-haskell-mode)
  (intero-mode t)
  (company-mode t)
  (flycheck-mode))

(use-package haskell-mode
  :mode
  (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
   ("\\.lhs\\'" . literate-haskell-mode))
  :init
  (require 'init-unicode-conversions)
  (balaji/setup-unicode-conversions)
  (setq haskell-hoogle-command nil)
  :config
  (use-package flycheck-haskell
    :config
    (flycheck-haskell-setup)
    (bind-key "M-n" #'flycheck-next-error haskell-mode-map)
    (bind-key "M-p" #'flycheck-previous-error haskell-mode-map))
  (use-package haskell-interactive-mode
    :ensure nil)
  (use-package haskell-process
    :ensure nil
    :init
    (setq
     haskell-process-suggest-remove-import-lines t
     haskell-process-auto-import-loaded-modules t
     haskell-process-log t)))

(add-hook 'haskell-mode-hook 'balaji/haskell-mode-hook)

(provide 'init-haskell)
;;; init-haskell.el ends here
