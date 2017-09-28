;;; init-syntax-checkers.el --- Flycheck, Flyspell and other essential syntax checkers. -*- lexical-binding: t -*-

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

;; Load `flycheck' and `flyspell' for enhanced syntax checking in Emacs.

;;; Code:

(use-package flycheck
  :defer 5
  :bind (("C-c e" . balaji-flycheck-errors/body)
         ("C-c t f" . flycheck-mode))
  :init
  (defhydra balaji-flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message")
    ;; See `helm-flycheck' package below
    ("h" helm-flycheck "list with helm"))

  (global-flycheck-mode)
  :config
  (setq
   flycheck-standard-error-navigation nil
   flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
   flycheck-scalastylerc "scalastyle_config.xml")
  :diminish (flycheck-mode . " Ⓢ"))

(use-package helm-flycheck
  :after flycheck)

(use-package flycheck-pos-tip
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-pos-tip-mode))

;; Flyspell Mode
(use-package flyspell
  :diminish (flyspell-mode . " Ⓕ")
  :ensure nil
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init
  (use-package ispell
    :ensure nil
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)))
  :config
  (unbind-key "C-." flyspell-mode-map))

(use-package simple
  :ensure nil
  :bind (("M-g n" . balaji-errors/next-error)
         ("M-g p" . balaji-errors/previous-error))
  :init
  (defhydra balaji-errors ()
    "Errors."
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("f" first-error "first")
    ("q" nil "Quit" :exit t )))

(provide 'init-syntax-checkers)
;;; init-syntax-checkers.el ends here
