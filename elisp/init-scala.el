;;; -*- lexical-binding: t -*-
;;; init-scala.el --- Configurations for coding in Scala

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

;; This module loads Scala Mode, SBT Mode and Ensime to help with Scala development.

;;; Code:

(use-package scala-mode
  :mode
  ("\\.scala" . scala-mode)
  :config
  (setq
   scala-indent:default-run-on-strategy scala-indent:eager-strategy
   scala-indent:indent-value-expression t
   scala-indent:align-parameters t
   scala-indent:align-forms t))

(use-package ensime
  :commands ensime ensime-mode
  :after scala-mode
  :bind
  (:map scala-mode-map
        ("C-c m e" . ensime)
        ("C-c m s" . ensime-shutdown)
   :map ensime-mode-map
        ("C-c m E" . ensime-reload)
        ("M-n"     . nil)
        ("M-p"     . nil))
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-default-buffer-prefix "ENSIME-"
   ensime-prefer-noninteractive t
   ensime-refactor-enable-beta t
   ensime-refactor-preview t
   ensime-auto-connect 'always
   ensime-refactor-preview-override-hunk 10)
  :config
  (ensime-company-enable))

(use-package sbt-mode
  :commands sbt-start sbt-command)

(defcustom
  balaji/scala-mode-prettify-symbols
  '(("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("==" . ?≡)
    ("!=" . ?≠)
    ("+-" . ?±)
    ("::" . ?∷))
  "Prettify symbols for scala-mode.")

(defun balaji/scala-mode-hook ()
  (company-mode t)
  (ensime-mode t)
  (setq prettify-symbols-alist balaji/scala-mode-prettify-symbols))

(add-hook 'scala-mode-hook 'balaji/scala-mode-hook)

(provide 'init-scala)
;;; init-scala.el ends here
