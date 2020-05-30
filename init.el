;;; init.el --- Emacs Configuration for Balaji Sivaraman -*- lexical-binding: t -*-

;; Copyright (C) 2020  Balaji Sivaraman

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

;; Emacs Configuration for Balaji Sivaraman

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Benchmarking Startup Begin

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; Startup Optimizations
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(defvar balaji--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defconst emacs-start-time (current-time))
(defvar enable-desktop-save)
(setq enable-desktop-save t)

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
(require 'init-navigation)
(require 'init-user-interface)
(require 'init-ivy)
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
(require 'init-lsp)
(require 'init-org)
(require 'init-clojure)
(require 'init-rust)
(require 'init-recentf)
(require 'init-scripting-langs)
(require 'init-latex)
(require 'init-version-control)
(require 'init-web-modes)
(require 'init-project)
(require 'init-pragmata-pro)

;; Initialize OS specific bindings
(when (eq system-type 'gnu/linux)
  (require 'init-linux))

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

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold 16777216
         gc-cons-percentage 0.1
         file-name-handler-alist balaji--file-name-handler-alist)))

;;; init.el ends here
