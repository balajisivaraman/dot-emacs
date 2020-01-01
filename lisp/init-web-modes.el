;;; init-web-modes.el --- Initializes web (html, css, js, ts, restclient) modes -*- lexical-binding: t -*-

;; Copyright (C) 2019 Balaji Sivaraman

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

;; Initializes packages needed for working with REST APIs and front-end applications

;;; Code:

(use-package restclient
  :mode "\\.http\\$")

(use-package typescript-mode
  :commands (typescript-mode)
  :hook ((typescript-mode . lsp)
         (typescript-mode . company-mode))
  :init
  (setq typescript-indent-level 2))

(setq lsp-clients-angular-language-server-command
  '("node"
    "/home/balaji/.nvm/versions/node/v13.1.0/lib/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/home/balaji/.nvm/versions/node/v13.1.0/lib/node_modules/"
    "--tsProbeLocations"
    "/home/balaji/.nvm/versions/node/v13.1.0/lib/node_modules/"
    "--stdio"))

(use-package ng2-mode
  :commands (ng2-mode ng2-ts-mode ng2-html-mode)
  :hook (((ng2-mode ng2-html-mode ng2-ts-mode) . lsp)
         ((ng2-mode ng2-html-mode ng2-ts-mode) . company-mode)))

(provide 'init-web-modes)
;;; init-web-modes.el ends here
