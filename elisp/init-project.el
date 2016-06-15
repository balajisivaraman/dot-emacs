;;; -*- lexical-binding: t -*-
;;; init-project.el --- Project Management in Emacs made easier.

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

;; This module loads Projectile, which is the be-all and end-all for Emacs project management.

;;; Code:

(require 'init-package)

(use-package projectile
  :defer t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :bind
  ("C-c C-f" . helm-projectile))

(use-package ag)

(use-package helm-ag
  :bind
  (("C-c s a" . helm-ag)
   ("C-c s A" . helm-do-ag))
  :config
  (setq
   helm-ag-fuzzy-match t                   ; Fuzzy matching
   helm-ag-insert-at-point 'symbol         ; Default to symbol at point
   helm-ag-edit-save t                     ; save buffers after editing
   ))

(use-package ignoramus
  :config
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    (add-to-list 'ignoramus-file-basename-exact-names name))
  (ignoramus-setup))

(provide 'init-project)
;;; init-project.el ends here
