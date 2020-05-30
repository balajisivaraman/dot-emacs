;;; init-rust.el --- Configurations for making Emacs and Rust play well together -*- lexical-binding: t -*-

;; Copyright (C) 2020  Balaji Sivaraman

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

(use-package flycheck-rust
  :commands flycheck-rust-setup)

(defun balaji/rust-mode-hook ()
  "Hooks for Rust Mode."
  (lsp)
  (company-mode)
  (cargo-minor-mode)
  (eldoc-mode))

(add-hook 'rust-mode-hook 'balaji/rust-mode-hook)

(provide 'init-rust)
;;; init-rust.el ends here
