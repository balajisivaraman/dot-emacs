;;; -*- lexical-binding: t -*-
;;; init-codestyle.el --- Language-agnostic sane default code style settings.

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

;; This file loads any custom code style (indentation, whitespace etc.) settings I like to use in all languages.

;;; Code:

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-font-lock-mode t)
(setq-default js-indent-level 2)
(setq mode-require-final-newline nil
      require-final-newline nil)

;; Cleanup unnecessary whitespace
(use-package ethan-wspace
  :diminish ethan-wspace-mode
  :init
  (global-ethan-wspace-mode t)
  :bind
  ("C-c w c" . ethan-wspace-clean-all))

(provide 'init-codestyle)
;;; init-codestyle.el ends here
