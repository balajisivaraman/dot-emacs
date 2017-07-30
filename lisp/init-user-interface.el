;;; init-user-interface.el --- Better themes, modeline and usability for Emacs. -*- lexical-binding: t -*-

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

;; Loads Dracula Theme, removes extraneous user-interface elements and makes Emacs more usable.

;;; Code:

(load-theme 'dracula t)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode)
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-toggle-window-number-on)
  (setq spaceline-window-numbers-unicode t))

(use-package nyan-mode
  :init (nyan-mode))

(use-package which-func
  :init (which-function-mode)
  :config
  (setq
   which-func-unknown "⊥"))

(use-package restart-emacs
  :bind
  ("C-c q r" . restart-emacs)
  ("C-c q q" . save-buffers-kill-emacs))

(use-package face-remap
  :ensure nil
  :bind
  (("C-+" . balaji-font-scaling/text-scale-increase)
   ("C--" . balaji-font-scaling/text-scale-decrease))
  :init
  (defhydra balaji-font-scaling ()
    "Font scaling"
    ("+" text-scale-increase "Scale Up")
    ("-" text-scale-decrease "Scale Down")
    ("q" nil "Quit" :exit t )))

(use-package page-break-lines
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(provide 'init-user-interface)
;;; init-user-interface.el ends here
