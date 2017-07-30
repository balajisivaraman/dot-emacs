;;; init-rust.el --- Configurations for making Emacs and Rust play well together -*- lexical-binding: t -*-

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

;; This module loads and configures Rust mode

;;; Code:

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq
   rust-format-on-save t))

(use-package cargo
  :diminish cargo-minor-mode
  :commands cargo-minor-mode)

(use-package racer
  :diminish racer-mode
  :commands racer-mode
  :config
  (setq
   racer-cmd "~/.cargo/bin/racer"
   racer-rust-src-path "~/Projects/rust/rust-lang/src"))

(use-package flycheck-rust
  :commands flycheck-rust-setup)

(defun balaji/rust-mode-hook ()
  "Hooks for Rust Mode."
  (cargo-minor-mode)
  (racer-mode)
  (eldoc-mode)
  (company-mode)
  (flycheck-rust-setup))

(add-hook 'rust-mode-hook 'balaji/rust-mode-hook)

(provide 'init-rust)
;;; init-rust.el ends here
