;;; init-web-modes.el --- Make it easier to work through Hell. -*- lexical-binding: t -*-

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

;; Javscript is Hell, these packages make that Hell a lot more bearable.

;;; Code:

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("\\.es6\\'" . js2-mode)
         ("\\.ejs\\'" . js2-mode))
  :interpreter "node"
  :commands js2-mode
  :config
  (setq
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   js2-highlight-level 3
   js2-mode-indent-ignore-first-tab t
   js2-strict-inconsistent-return-warning nil
   js2-global-externs
   '("module" "require" "__dirname" "process" "console" "JSON" "$" "_"))
  (setq-default js2-basic-offset 2))

(use-package js2-refactor
  :diminish js2-refactor-mode
  :after js2-mode
  :commands (js2r-add-keybindings-with-prefix)
  :config
  (js2r-add-keybindings-with-prefix "C-c m"))

;; Use Tern for smarter JS.
(use-package tern
  :diminish (tern-mode . "Ⓣ")
  :config
  (add-to-list 'tern-command "--no-port-file" 'append))

(use-package company-tern
  :after company
  :commands company-tern
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package web-mode
  :mode ("\\.html$" . web-mode))

(use-package css-mode
  :mode ("\\.css$" . css-mode)
  :config (setq css-indent-offset 2))

(use-package web-beautify
  :after js2-mode
  :config
  (bind-key "C-c m b j" 'web-beautify-js js2-mode-map)
  ;; (bind-key "C-c m b j" 'web-beautify-js web-mode-map)
  ;; (bind-key "C-c m b h" 'web-beautify-html web-mode-map)
  ;; (bind-key "C-c m b c" 'web-beautify-css css-mode-map)
  )

(defun balaji-js2-mode-hooks ()
  "My hooks for web-modes modes."
  (company-mode t)
  (js2-refactor-mode t)
  (tern-mode t)
  (flycheck-mode t))

(add-hook 'js2-mode-hook 'balaji-js2-mode-hooks)

(provide 'init-web-modes)
;;; init-web-modes.el ends here
