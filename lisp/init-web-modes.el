;;; init-web-modes.el --- Make it easier to work through Hell. -*- lexical-binding: t -*-

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2016 Balaji Sivaraman

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

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
