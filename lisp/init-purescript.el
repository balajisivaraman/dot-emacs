;;; init-purescript.el --- Purescript configuration -*- lexical-binding: t -*-

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

;; This module initializes configurations for working with Purescript.

;;; Code:

(use-package purescript-mode
  :commands purescript-mode)

(use-package flycheck-purescript
  :after purescript-mode
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-purescript-setup))

(use-package psci
  :after purescript-mode)

(use-package psc-ide
  :commands psc-ide-server-start
  :after purescript-mode)

(defun balaji-purescript-mode-hook ()
  "Hooks for purescript mode."
  (psc-ide-mode)
  (company-mode)
  (flycheck-mode)
  (inferior-psci-mode)
  (turn-on-purescript-indentation)
  (diminish 'purescript-indentation-mode)
  (psc-ide-server-start (locate-dominating-file default-directory "bower.json")))

(add-hook 'purescript-mode-hook 'balaji-purescript-mode-hook)

(provide 'init-purescript)
;;; init-purescript.el ends here
