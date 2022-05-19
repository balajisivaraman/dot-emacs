;;; init-rust.el --- Configurations for making Emacs and Rust play well together -*- lexical-binding: t -*-

;; Copyright (C) 2021  Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
  :commands
  (cargo-minor-mode
   cargo-process-doc
   cargo-process-fmt
   cargo-process-run
   cargo-process-run-bin
   cargo-process-run-example
   cargo-process-new
   cargo-process-mode
   cargo-process-init
   cargo-process-test
   cargo-process-current-test
   cargo-process-current-file-tests
   cargo-process-build
   cargo-process-clean
   cargo-process-check
   cargo-process-bench
   cargo-process-update
   cargo-process-repeat
   cargo-process-clippy
   cargo-process-search
   cargo-process-doc-open)
  :bind
  (:map rust-mode-map
        ("M-m d" . cargo-process-doc)
        ("M-m f" . cargo-process-fmt)
        ("M-m r r" . cargo-process-run)
        ("M-m r b" . cargo-process-run-bin)
        ("M-m r e" . cargo-process-run-example)
        ("M-m n" . cargo-process-new)
        ("M-m i" . cargo-process-init)
        ("M-m t t" . cargo-process-test)
        ("M-m t c" . cargo-process-current-test)
        ("M-m t f" . cargo-process-current-file-tests)
        ("M-m b" . cargo-process-build)
        ("M-m c" . cargo-process-clean)
        ("M-m k" . cargo-process-check)
        ("M-m B" . cargo-process-bench)
        ("M-m u" . cargo-process-update)
        ("M-m R" . cargo-process-repeat)
        ("M-m C" . cargo-process-clippy)
        ("M-m s" . cargo-process-search)
        ("M-m D" . cargo-process-doc-open))
  :init
  (which-key-add-major-mode-key-based-replacements 'rust-mode
    "M-m c" "cargo"))

(defun bs/rust-mode-hook ()
  "Hooks for Rust Mode."
  (eglot-ensure)
  (company-mode)
  (cargo-minor-mode)
  (flymake-mode)
  (eldoc-mode))

(add-hook 'rust-mode-hook 'bs/rust-mode-hook)

(provide 'init-rust)
;;; init-rust.el ends here
