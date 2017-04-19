;;; init-osx.el --- Make working on OSX easier. -*- lexical-binding: t -*-

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

;; Packages to make working in Emacs on OSX a lot easier.

;;; Code:

(use-package ns-win
  :defer t
  :ensure nil
  :if (eq system-type 'darwin)
  :config
  (setq
   ns-pop-up-frames nil
   mac-option-modifier 'meta
   mac-command-modifier 'meta))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :init (osx-trash-setup))

;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind
  (("C-c f J" . reveal-in-osx-finder)))

(provide 'init-osx)
;;; init-osx.el ends here
