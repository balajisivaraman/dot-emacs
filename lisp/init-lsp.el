;;; init-lsp.el --- Language Server Protocol Support -*- lexical-binding: t -*-

;; Copyright (C) 2020 Balaji Sivaraman

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

;; LSP Mode Configuration

;;; Code:

(defvar balaji/node-version (s-trim (shell-command-to-string "node -v")))

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-prefer-capf t
        lsp-idle-delay 0.500
        lsp-clients-angular-language-server-command
        `("node"
          ,(s-join "/" `(,(f-expand "~") ".nvm/versions/node" ,balaji/node-version "lib/node_modules/@angular/language-server"))
          "--ngProbeLocations"
          ,(s-join "/" `(,(f-expand "~") ".nvm/versions/node" ,balaji/node-version "lib/node_modules"))
          "--tsProbeLocations"
          ,(s-join "/" `(,(f-expand "~") ".nvm/versions/node" ,balaji/node-version "lib/node_modules"))
          "--stdio")))

(use-package lsp-clients
  :ensure nil
  :after lsp-mode)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
