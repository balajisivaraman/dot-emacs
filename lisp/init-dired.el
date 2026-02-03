;;; init-dired.el --- Dired configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures Dired (directory editor) with modern enhancements.

;;; Code:

;;; Dired Built-in Settings
(require 'dired)
(require 'dired-x)

;; Human-readable file sizes
(setq dired-listing-switches "-alh")

;; Recursive operations
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always)

;; Guess target directory for copy/move
(setq dired-dwim-target t)

;; Kill dired buffer when opening new directory
(setq dired-kill-when-opening-new-directory-buffer t)

;; Auto-revert dired buffers
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Enable dired-x features
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

;; Hide dotfiles by default (toggle with . key)
(setq dired-omit-files "^\\.[^.]")

;; Jump to dired of current file with C-x C-j
(global-set-key (kbd "C-x C-j") 'dired-jump)

;;; Dired Enhancements
;; dired-subtree - Expand directories inline
(use-package dired-subtree
  :ensure t
  :after dired
  :general
  (:keymaps 'dired-mode-map
   "<tab>" 'dired-subtree-toggle
   "<backtab>" 'dired-subtree-cycle))

;; dired-hide-dotfiles - Toggle dotfiles visibility
(use-package dired-hide-dotfiles
  :ensure t
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :general
  (:keymaps 'dired-mode-map
   "." 'dired-hide-dotfiles-mode))

;; all-the-icons-dired - Icons in dired
(use-package all-the-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
