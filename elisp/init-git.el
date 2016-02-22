;;; -*- lexical-binding: t -*-
;;; init-git.el --- Makes Emacs the best Git experience in the world.

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

;; This module loads Magit, Git Gutter Mode, and other Git-related goodies.

;;; Code:

(require 'init-package)

(package-require 'magit)
(use-package magit
  :bind
  (("C-x g g" . magit-status)
   ("C-x g m" . magit-branch)
   ("C-x g M" . magit-merge)
   ("C-x g s" . magit-stash)
   ("C-x g a" . magit-stash-apply)
   ("C-x g p" . magit-pull)
   ("C-x g r" . magit-reset-head)
   ("C-x g R" . magit-reset-hard)
   ("C-x g l" . magit-log-all)
   ("C-x g L" . magit-log)
   ("C-x g c" . magit-checkout))
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

(provide 'init-git)
