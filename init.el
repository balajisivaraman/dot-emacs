;;; init.el --- Emacs Configuration for Balaji Sivaraman -*- lexical-binding: t -*-

;; Copyright (C) 2021  Balaji Sivaraman

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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Startup Optimizations
(when (string-match ".Emacs 28." (emacs-version))
  (setq native-comp-deferred-compilation t))
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(defvar bs/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defconst emacs-start-time (current-time))
(defvar enable-desktop-save)
(setq enable-desktop-save t)

;;; Initial Setup
(setq user-full-name "Balaji Sivaraman")
(setq user-mail-address "balaji@balajisivaraman.com")
(defvar bs/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path bs/site-lisp-dir)
(defvar bs/lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path bs/lisp-dir)
(defvar bs/emacs-cache-directory)
(setq bs/emacs-cache-directory (concat user-emacs-directory ".cache/"))
(when (not (file-exists-p bs/emacs-cache-directory))
  (mkdir bs/emacs-cache-directory))

(require 'init-package)
(require 'init-lib)
(require 'init-defaults)
(require 'init-customizations)
(require 'init-functions)
(require 'init-exec-path)
(require 'init-keybindings)
(require 'init-navigation)
(require 'init-user-interface)
(require 'init-selections)
(require 'init-buffers)
(require 'init-files)
(require 'init-search)
(require 'init-basic-editing)
(require 'init-codestyle)
(require 'init-corfu)
(require 'init-snippets)
(require 'init-syntax-checkers)
(require 'init-markup-languages)
(require 'init-finance)
(require 'init-treesitter)
(require 'init-lisp)
(require 'init-lsp)
(require 'init-org)
(require 'init-note-taking)
(require 'init-clojure)
(require 'init-haskell)
(require 'init-python)
(require 'init-rust)
(require 'init-scripting-langs)
(require 'init-latex)
(require 'init-version-control)
(require 'init-web-modes)
(require 'init-project)
(require 'init-kubernetes)
(require 'init-mail)
(require 'init-os-customisations)

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
   (setq gc-cons-threshold 100000000
         gc-cons-percentage 0.1
         read-process-output-max (* 1024 1024)
         file-name-handler-alist bs/file-name-handler-alist)))

(if bs/at-work
    (progn
      (bs/ui-configuration)
      (server-start))
  (add-hook 'after-make-frame-functions #'bs/frame-functions))

;;; init.el ends here
