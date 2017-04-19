;;; init-version-control.el ---  Make Emacs the best Git experience in the world. -*- lexical-binding: t -*-

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

;; This module loads Magit, Git Gutter Mode, and other Git-related goodies.

;;; Code:

(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g m" . magit-branch)
   ("C-c g M" . magit-merge)
   ("C-c g S" . magit-stash)
   ("C-c g a" . magit-stash-apply)
   ("C-c g p" . magit-pull)
   ("C-c g r" . magit-reset-head)
   ("C-c g R" . magit-reset-hard)
   ("C-c g l" . magit-log-all)
   ("C-c g L" . magit-log)
   ("C-c g c" . magit-checkout))
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0")
  (advice-add 'magit-status :after #'delete-other-windows))

(use-package gist
  :bind (("C-c g g l" . gist-list)
         ("C-c g g b" . gist-region-or-buffer)))

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)))

(provide 'init-version-control)
;;; init-version-control.el ends here
