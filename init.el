;; Benchmarking Emacs Startup
(defconst emacs-start-time (current-time))

;;; Configure Use-Package
(defvar balaji/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path balaji/site-lisp-dir)
(add-to-list 'load-path (expand-file-name "use-package" balaji/site-lisp-dir))
(require 'use-package)
(require 'bind-key)
(require 'diminish nil t)

;;; Configure Libraries
(use-package s    :load-path "lib/s-el")
(use-package dash :load-path "lib/dash-el")

;;; Utility Functions and Macros
(defun balaji/network-connection-available-p ()
    "Check whether we have internet connectivity"
  (-any-p
   (lambda (interface) (s-starts-with-p "en" (car interface)))
   (network-interface-list)))

;;; Initialize package.el
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; package.el should not initialize our packages.
;; We're going to use use-package for that.
(setq package-enable-at-startup nil)

;;; Better Defaults
;; Hide the menu bar, tool bar and scroll bar
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Don't display the default start up message
;; If we're loading from a saved desktop, this setting becomes redundant.
(setq inhibit-startup-message t)
;; Highlight the current line on all buffers
(global-hl-line-mode t)
;; Who ever wants to fully type out YES and NO
(fset 'yes-or-no-p 'y-or-n-p)
 ;; This will help us open up buffers without confirmation.
(setq confirm-nonexistent-file-or-buffer nil)
;; Setting word wrap mode as default
(global-visual-line-mode 1)
(diminish 'visual-line-mode)
;; Highlight matching parens
(show-paren-mode t)
;; Show column number in bar
(column-number-mode t)
;; Typed text will replace the selection as in most modern editors
(delete-selection-mode t)
;; Display line numbers to the left of the buffer
(global-linum-mode t)
;; When I grew up, sentences always ended with a single space.
(setq sentence-end-double-space nil)
;; Prettify Symbols Mode in Emacs > 24.4 is awesome.
(setq-default prettify-symbols-mode t)
(global-prettify-symbols-mode t)
;; Electric Pair Mode
(electric-pair-mode t)

(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      visible-bell t
      load-prefer-newer t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(use-package iso-transl)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

(if (balaji/network-connection-available-p)
    (package-refresh-contents))

(defun package-require (package)
  "Install `package` only if it is not already installed"
  (unless (package-installed-p package)
    (package-install package nil)))

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
