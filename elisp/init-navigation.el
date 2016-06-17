;;; -*- lexical-binding: t -*-
;;; init-navigation.el --- Emacs navigation made much easier

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

;; This module loads Avy Mode, Ace Window Mode, Swiper, and other modes to make Emacs navigation a breeze.

;;; Code:

(use-package avy
  :bind
  (("C-c j c" . avy-goto-char)
   ("C-c j w" . avy-goto-word-1)
   ("C-c j b" . avy-pop-mark)
   ("C-c j l" . avy-goto-line)))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package golden-ratio
  :init
  (defun lunaryorn-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . lunaryorn-toggle-golden-ratio))
  :diminish (golden-ratio-mode . "ⓖ")
  :config
  (setq
   golden-ratio-extra-commands '(windmove-up
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 ace-window
                                 ace-delete-window
                                 ace-select-window
                                 ace-swap-window
                                 ace-maximize-window)
   golden-ratio-auto-scale nil
   golden-ratio-exclude-modes '(flycheck-error-list-mode
                                calc-mode
                                ediff-mode
                                eshell-mode
                                dired-mode)

   split-width-threshold nil
   golden-ratio-exclude-buffer-regexp
   `(,(rx bos "*" (any "h" "H") "elm*" eos)
     ,(rx bos "*which-key*" eos)
     ,(rx bos "*NeoTree*" eos))))

(provide 'init-navigation)
;;; init-navigation.el ends here
