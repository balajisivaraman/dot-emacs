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
;; (package-initialize)

;; Startup Optimizations
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(defvar balaji--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defconst emacs-start-time (current-time))
(defconst balaji-evil-mode-enabled t)
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
(when balaji-evil-mode-enabled
  (require 'init-evil))
(require 'init-navigation)
(require 'init-user-interface)
(require 'init-helm)
;; (require 'init-ivy)
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
;; (require 'init-scala)
;; (require 'init-haskell)
;; (require 'init-idris)
;; (require 'init-erlang)
;; (require 'init-purescript)
(require 'init-org)
;; (require 'init-ruby)
(require 'init-rust)
(require 'init-recentf)
(require 'init-web-modes)
(require 'init-scripting-langs)
(require 'init-latex)
(require 'init-version-control)
(require 'init-project)
;; (require 'init-pragmata-pro)
(when balaji-evil-mode-enabled
  (require 'init-evil-keybindings))
(require 'init-general-keybindings)

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

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold 16777216
         gc-cons-percentage 0.1
         file-name-handler-alist balaji--file-name-handler-alist)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(evil-search-module (quote evil-search))
 '(package-selected-packages
   (quote
    (company-lsp lsp-ui emmet-mode pinentry general git-timemachine gist company-auctex auctex fish-mode restclient web-beautify web-mode company-tern tern js2-refactor js2-mode flycheck-rust cargo rust-mode org-bullets org-plus-contrib lsp-mode paredit ledger-mode toml-mode yaml-mode markdown-mode flycheck-pos-tip yasnippet-snippets yasnippet company-quickhelp company ethan-wspace rainbow-delimiters crux embrace easy-kill change-inner expand-region browse-kill-ring deadgrep hardhat sudo-edit focus-autosave-mode persistent-scratch ibuffer-vc helm-projectile helm-flycheck helm-descbinds helm unicode-fonts page-break-lines restart-emacs nyan-mode spaceline dracula-theme beginend golden-ratio window-numbering ace-window avy evil-collection evil-org linum-relative evil-rsi evil-multiedit evil-goggles evil-snipe evil-mc vi-tilde-fringe evil-magit evil-escape evil-surround evil-matchit evil-indent-textobject evil-nerd-commenter evil-visualstar evil-leader evil which-key exec-path-from-shell use-package-chords paradox)))
 '(paradox-github-token t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro" :foundry "FSD " :slant normal :weight normal :height 126 :width normal))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
