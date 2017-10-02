;;; init-company.el --- Sets up auto-completion using company mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017  Balaji Sivaraman

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

;; This module loads Company mode for providing auto-completion capabilities to Emacs.

;;; Code:

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-minimum-prefix-length 2
   company-selection-wrap-around t
   company-show-numbers t
   company-tooltip-align-annotations t
   company-require-match nil
   company-dabbrev-downcase nil
   company-dabbrev-code-ignore-case nil
   company-transformers '(company-sort-by-occurrence))
  (use-package company-quickhelp
    :init
    (setq company-quickhelp-delay 0.6)
    :config
    (company-quickhelp-mode t)))


(provide 'init-company)
;;; init-company.el ends here
