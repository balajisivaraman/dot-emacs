;;; init-clojure.el --- Sets up Emacs for Clojure -*- lexical-binding: t -*-

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

;; Configures CIDER and other Clojure modes

;;; Code:

(use-package clojure-mode
  :hook
  (clojure-mode . company-mode)
  (clojure-mode . paredit-mode)
  :mode "\\.clj\\$")

(use-package clj-refactor
  :hook
  (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package cider
  :hook
  (cider-mode . eldoc-mode)
  (cider-mode . company-mode)
  ;; (cider-mode . flycheck-clojure-setup)
  (cider-repl-mode . company-mode)
  :config
  (setq
   cider-repl-history-file "~/.emacs.d/cider-history"
   ;; nice pretty printing
   cider-repl-use-pretty-printing t
   ;; nicer font lock in REPL
   cider-repl-use-clojure-font-lock t
   ;; result prefix for the REPL
   -repl-result-prefix ";; => "
   ;; never ending REPL history
   cider-repl-wrap-history t
   ;; looong history
   cider-repl-history-size 3000
   cider-show-error-buffer nil))

(provide 'init-clojure)
;;; init-clojure.el ends here
