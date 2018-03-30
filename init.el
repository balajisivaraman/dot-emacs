;;; init.el --- Emacs Configuration for Balaji Sivaraman -*- lexical-binding: t -*-

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

;; Emacs Configuration for Balaji Sivaraman that serves the following primary uses;
;;
;;  - Rust
;;  - Org Mode (Daily Agenda, Note Taking, Day Planner etc.)
;;  - Personal Finance (Ledger)
;;  - Javascript
;;  - Ruby
;;  - Markdown
;;  - Scala (Not Regularly Used)
;;  - Haskell (Not Regularly Used)
;;  - Emacs (and other) Lisp (Not Regularly Used)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Benchmarking Startup Begin

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst emacs-start-time (current-time))

;;; Initial Setup
(setq user-full-name "Balaji Sivaraman")
(setq user-mail-address "balaji@balajisivaraman.com")
(defvar balaji/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path balaji/site-lisp-dir)
(defvar balaji/lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path balaji/lisp-dir)

(require 'init-variables)
(require 'init-package)
(require 'init-lib)
(require 'init-functions)
(require 'init-defaults)
(require 'init-customizations)
(require 'init-exec-path)
(require 'init-keybindings)
;; (require 'init-evil)
(require 'init-navigation)
(require 'init-user-interface)
(require 'init-helm)
(require 'init-buffers)
(require 'init-files)
(require 'init-search)
(require 'init-basic-editing)
(require 'init-fontification)
(require 'init-codestyle)
(require 'init-company)
(require 'init-snippets)
(require 'init-syntax-checkers)
(require 'init-markup-languages)
(require 'init-finance)
(require 'init-lisp)
(require 'init-scala)
(require 'init-haskell)
(require 'init-idris)
;; (require 'init-erlang)
(require 'init-purescript)
(require 'init-org)
(require 'init-ruby)
(require 'init-rust)
(require 'init-recentf)
(require 'init-web-modes)
(require 'init-scripting-langs)
(require 'init-latex)
(require 'init-version-control)
(require 'init-project)
(require 'init-pragmata-pro)
;; (require 'init-evil-keybindings)

;; Initialize OS specific bindings
(cond
 ((eq system-type 'darwin) (require 'init-osx))
 ((eq system-type 'gnu/linux) (require 'init-linux)))

;;; Benchmarking Startup End
(when (window-system)
  (let ((elapsed-time (float-time (time-subtract (current-time)
                                                 emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed-time))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed-time (float-time (time-subtract (current-time)
                                                              emacs-start-time))))
                 (message "Loading %s...done (%.3fs)" ,load-file-name elapsed-time)))
            t))

;;; init.el ends here
