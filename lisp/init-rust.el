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
  :commands cargo-minor-mode
  :bind
  (:map rust-mode-map
        ("C-c m d" . cargo-process-doc)
        ("C-c m f" . cargo-process-fmt)
        ("C-c m r r" . cargo-process-run)
        ("C-c m r b" . cargo-process-run-bin)
        ("C-c m r e" . cargo-process-run-example)
        ("C-c m n" . cargo-process-new)
        ("C-c m m" . cargo-process-mode)
        ("C-c m i" . cargo-process-init)
        ("C-c m t t" . cargo-process-test)
        ("C-c m t c" . cargo-process-current-test)
        ("C-c m t f" . cargo-process-current-file-tests)
        ("C-c m b" . cargo-process-build)
        ("C-c m c" . cargo-process-clean)
        ("C-c m k" . cargo-process-check)
        ("C-c m B" . cargo-process-bench)
        ("C-c m u" . cargo-process-update)
        ("C-c m R" . cargo-process-repeat)
        ("C-c m C" . cargo-process-clippy)
        ("C-c m s" . cargo-process-search)
        ("C-c m D" . cargo-process-doc-open))
  )

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
