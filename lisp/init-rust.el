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
  :commands cargo-minor-mode
  :bind
  (:map rust-mode-map
        ("C-, d" . cargo-process-doc)
        ("C-, f" . cargo-process-fmt)
        ("C-, r r" . cargo-process-run)
        ("C-, r b" . cargo-process-run-bin)
        ("C-, r e" . cargo-process-run-example)
        ("C-, n" . cargo-process-new)
        ("C-, m" . cargo-process-mode)
        ("C-, i" . cargo-process-init)
        ("C-, t t" . cargo-process-test)
        ("C-, t c" . cargo-process-current-test)
        ("C-, t f" . cargo-process-current-file-tests)
        ("C-, b" . cargo-process-build)
        ("C-, c" . cargo-process-clean)
        ("C-, k" . cargo-process-check)
        ("C-, B" . cargo-process-bench)
        ("C-, u" . cargo-process-update)
        ("C-, R" . cargo-process-repeat)
        ("C-, C" . cargo-process-clippy)
        ("C-, s" . cargo-process-search)
        ("C-, D" . cargo-process-doc-open))
  )

(defun bs/rust-mode-hook ()
  "Hooks for Rust Mode."
  (bs/initialize-chosen-lsp-server)
  (company-mode)
  (cargo-minor-mode)
  (flymake-mode)
  (eldoc-mode))

(add-hook 'rust-mode-hook 'bs/rust-mode-hook)

(provide 'init-rust)
;;; init-rust.el ends here
