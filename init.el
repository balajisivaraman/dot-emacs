;;; init.el --- Emacs Configuration for Balaji Sivaraman -*- lexical-binding: t -*-

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

;; Emacs Configuration for Balaji Sivaraman that serves the following primary uses;
;;
;;  - Scala
;;  - Haskell
;;  - Emacs (and other) Lisp
;;  - Org Mode (Daily Agenda, Note Taking, Day Planner etc.)
;;  - Personal Finance (Ledger)
;;  - Javascript
;;  - Ruby
;;  - Markdown

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

(require 'init-package)
(require 'init-lib)
(require 'init-functions)
(require 'init-defaults)
(require 'init-customizations)
(require 'init-exec-path)
(require 'init-keybindings)
(require 'init-evil)
(require 'init-user-interface)
(require 'init-helm)
(require 'init-buffers)
(require 'init-files)
(require 'init-navigation)
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
(require 'init-erlang)
(require 'init-org)
(require 'init-ruby)
(require 'init-web-modes)
(require 'init-scripting-langs)
(require 'init-version-control)
(require 'init-project)
(require 'init-evil-keybindings)

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

