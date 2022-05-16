;;; init-finance.el --- Setup ledger mode and customizations. -*- lexical-binding: t -*-

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

;; This module loads and configures ledger mode.

;;; Code:

(defun bs/insert-rupee-symbol ()
  "Insert Indian Rupee Symbol at point."
  (interactive)
  (insert "₹"))

(use-package ledger-mode
  :mode "\\.ledger$"
  :hook (ledger-mode . company-mode)
  :bind (:map ledger-mode-map
              ("M-m r" . bs/insert-rupee-symbol))
  :config
  (setq ledger-binary-path "hledger"
        ledger-mode-should-check-version nil
        ledger-init-file-name " "
        ledger-post-amount-alignment-column 75))

(defun bs/ledger-mode-hook ()
  "Hooks for Ledger Mode."
  (company-mode)
  (outline-minor-mode))

(add-hook 'ledger-mode-hook 'bs/ledger-mode-hook)

(provide 'init-finance)
;;; init-finance.el ends here
