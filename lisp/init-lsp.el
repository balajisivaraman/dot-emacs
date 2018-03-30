;;; init-lsp.el --- Language Server Protocol Support -*- lexical-binding: t -*-

;; Copyright (C) 2018 Balaji Sivaraman

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

;; Basic LSP Mode Configuration

;;; Code:

(use-package lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :after (lsp-mode company)
  :config
  (push 'company-lsp company-backends))

(provide 'init-lsp)
;;; init-lsp.el ends here
