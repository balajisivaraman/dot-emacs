;;; -*- lexical-binding: t -*-
;;; init-complete.el --- Sets up auto-completion using company mode

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

;; This module loads Company mode for providing auto-completion capabilities to Emacs.

;;; Code:

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-minimum-prefix-length 2
   company-selection-wrap-around t
   company-show-numbers t
   company-tooltip-align-annotations t
   company-require-match nil
   company-dabbrev-downcase nil
   company-dabbrev-code-ignore-case nil
   company-transformers '(company-sort-by-occurrence))
  (use-package company-quickhelp
    :init
    (setq company-quickhelp-delay 0.6)
    :config
    (company-quickhelp-mode t)))

(provide 'init-complete)
;;; init-complete.el ends here
