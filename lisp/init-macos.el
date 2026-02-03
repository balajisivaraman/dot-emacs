;;; init-macos.el --- macOS-specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures macOS-specific settings including PATH, keyboard modifiers,
;; and system integration.

;;; Code:

;;; PATH Configuration
;; Use exec-path-from-shell to sync PATH from shell
;; Note: Uses zsh instead of Fish due to exec-path-from-shell compatibility
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  (exec-path-from-shell-initialize))

;;; Keyboard Modifiers
;; Command and Option both work as Meta (for flexibility)
;; This allows both Cmd+b and Option+b to work as M-b
(setq mac-command-modifier 'meta
      mac-option-modifier 'meta)

;;; Window and Frame Behavior
;; Use native fullscreen
(setq ns-use-native-fullscreen t)

;; Don't create new frames for files
(setq ns-pop-up-frames nil)

;; Smooth window resizing
(setq frame-resize-pixelwise t)

;;; macOS Integration
;; Use macOS Trash
(setq delete-by-moving-to-trash t)

;; Open URLs with default browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;;; Dired macOS Compatibility
;; macOS ls doesn't support --dired flag
(setq dired-use-ls-dired nil)

;; Use GNU ls if available (better dired experience)
(when (executable-find "gls")
  (setq insert-directory-program "gls"
        dired-use-ls-dired t))

;;; Performance
;; Faster rendering on Retina displays
(setq frame-inhibit-implied-resize t)

(provide 'init-macos)
;;; init-macos.el ends here
