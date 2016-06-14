;;; -*- lexical-binding: t -*-
;;; init-helm.el --- Life cannot function without helm

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

;; This file contains initialization code for Helm and assorted packages.

;;; Code:

(require 'init-package)

(use-package helm-config
  :ensure helm
  :demand t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x f" . helm-multi-files)
   ("C-x C-o" . helm-imenu))
  :config
  (use-package helm-files
    :ensure nil)
  (use-package helm-buffers
    :ensure nil)
  (use-package helm-descbinds
    :ensure nil
    :bind
    ("C-h b" . helm-descbinds))
  (use-package helm-mode
    :ensure nil
    :diminish helm-mode
    :init
    (helm-mode 1))
  (helm-autoresize-mode 1)
  (setq-default
   helm-display-header-line nil
   helm-autoresize-min-height 10
   helm-autoresize-max-height 35
   helm-split-window-in-side-p t

   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-apropos-fuzzy-match t))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s s" . helm-swoop)
         ("C-c s S" . helm-multi-swoop)
         ("C-c s C-s" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-speed-or-color t
        ;; Split window like Helm does
        helm-swoop-split-window-function #'helm-default-display-buffer))

(provide 'init-helm)
