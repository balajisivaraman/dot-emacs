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
  :init
  (bs/general-mode-specific-bindings 'ledger-mode-map
                                     "r" 'bs/insert-rupee-symbol
                                     "a" 'ledger-add-transaction
                                     "bp" 'ledger-display-balance-at-point)
  (which-key-add-major-mode-key-based-replacements 'ledger-mode
    "SPC m r" "rupee-symbol"
    "SPC m a" "add transaction"
    "SPC m b" "balance"))

(add-hook 'ledger-mode-hook 'company-mode)

(provide 'init-finance)
;;; init-finance.el ends here
