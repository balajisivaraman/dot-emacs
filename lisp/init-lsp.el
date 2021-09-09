;;; init-lsp.el --- Language Server Protocol Support -*- lexical-binding: t -*-

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

;; LSP and Eglot Mode Configuration

;;; Code:

(defvar bs/node-version (s-trim (shell-command-to-string "node -v")))

(use-package lsp-mode
  :disabled t
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-'")
  :config
  (setq lsp-prefer-capf t
        lsp-idle-delay 0.500
        lsp-clients-angular-language-server-command
        `("node"
          ,(s-join "/" `(,(f-expand "~") ".fnm/node-versions" ,bs/node-version "installation/lib/node_modules/@angular/language-server"))
          "--ngProbeLocations"
          ,(s-join "/" `(,(f-expand "~") ".fnm/node-versions" ,bs/node-version "installation/lib/node_modules"))
          "--tsProbeLocations"
          ,(s-join "/" `(,(f-expand "~") ".fnm/node-versions" ,bs/node-version "installation/lib/node_modules"))
          "--stdio")))

(use-package eglot
  :commands (eglot-ensure)
  :bind (:map eglot-mode-map
         (("C-' a" . eglot-code-actions)
          ("C-' r" . eglot-rename)
          ("C-' w r" . eglot-reconnect)))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("~/bin/rust-analyzer-wrapper")))
  (add-to-list 'eglot-server-programs '(ng2-html-mode . ("~/bin/ng-langserver-wrapper")))
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(shell-script-mode . ("bash-language-server")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("ccls")))
  (setq eglot-confirm-server-initiated-edits nil))

(defvar bs/lsp-server-to-use nil)
(setq bs/lsp-server-to-use 'eglot)

(defun bs/initialize-chosen-lsp-server ()
  "Initializes either Eglot or Lsp Mode."
  (interactive)
  (if (eq bs/lsp-server-to-use 'lsp)
      (lsp-deferred)
    (eglot-ensure)))

(use-package cc-mode
  :ensure nil
  :hook (c++-mode . bs/initialize-chosen-lsp-server))

(provide 'init-lsp)
;;; init-lsp.el ends here
