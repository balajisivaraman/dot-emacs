;;; init-eglot.el --- Language Server Protocol Support -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

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

;; Eglot Mode Configuration

;;; Code:

(defvar bs/node-version (s-trim (shell-command-to-string "node -v")))

(use-package eglot
  :commands (eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("~/bin/rust-analyzer-wrapper")))
  (add-to-list 'eglot-server-programs '(ng2-html-mode . ("~/bin/ng-langserver-wrapper")))
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(shell-script-mode . ("bash-language-server"))))

(use-package cc-mode
  :ensure nil
  :hook (c++-mode . eglot-ensure))

(provide 'init-eglot)
;;; init-eglot.el ends here
