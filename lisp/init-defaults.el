;;; init-defaults.el --- Sensible defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Emacs sensible defaults that don't require external packages.
;; Configures UI, behavior, file management, and editing preferences.

;;; Code:

;;; UI Configuration
;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Disable startup message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Silence bell
(setq ring-bell-function 'ignore)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Show column numbers
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Enable line numbers globally
(global-display-line-numbers-mode 1)

;; Disable line numbers in specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Scrolling Configuration
(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t)

;;; File Management
;; Create .cache subdirectories
(defvar user-backup-directory
  (expand-file-name "backups/" user-cache-directory)
  "Directory for backup files.")

(defvar user-auto-save-directory
  (expand-file-name "auto-save/" user-cache-directory)
  "Directory for auto-save files.")

(unless (file-exists-p user-backup-directory)
  (make-directory user-backup-directory t))

(unless (file-exists-p user-auto-save-directory)
  (make-directory user-auto-save-directory t))

;; Backup configuration
(setq make-backup-files t
      backup-directory-alist `(("." . ,user-backup-directory))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Auto-save configuration
(setq auto-save-default t
      auto-save-file-name-transforms `((".*" ,user-auto-save-directory t))
      auto-save-interval 200
      auto-save-timeout 20)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Require final newline
(setq require-final-newline t)

;; Use ibuffer instead of list-buffers
(defalias 'list-buffers 'ibuffer)

;;; Editing Defaults
;; Delete selection mode - typing replaces selection
(delete-selection-mode 1)

;; Electric pair mode - auto-close parens, quotes, etc.
(electric-pair-mode 1)

;; Modern sentence spacing (single space after period)
(setq sentence-end-double-space nil)

;; Fill column for text wrapping
(setq-default fill-column 80)

;; Line spacing for better readability
(setq-default line-spacing 0.2)

;;; History and Persistence
;; Save minibuffer history
(savehist-mode 1)
(setq savehist-file (expand-file-name "var/savehist" user-cache-directory))

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-save-file (expand-file-name "var/recentf" user-cache-directory))

;; Save place in files
(save-place-mode 1)
(setq save-place-file (expand-file-name "var/places" user-cache-directory))

;; Auto-save visited files
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 5)

;;; Coding Defaults
;; Use UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;;; Performance
;; Increase read process output max
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; Reduce rendering overhead
(setq fast-but-imprecise-scrolling t)

;;; Miscellaneous
;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't create new frames
(setq ns-pop-up-frames nil)

;; Pixelwise frame resizing
(setq frame-resize-pixelwise t)

;; Follow symlinks without prompting
(setq vc-follow-symlinks t)

;; Increase warning threshold
(setq warning-minimum-level :error)

(provide 'init-defaults)
;;; init-defaults.el ends here
