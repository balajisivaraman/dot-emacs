;;; init-version-control.el --- Version control configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures Magit for Git operations with delta for better diffs.

;;; Code:

;;; Transient - Required by Magit
;; Ensure we have a recent enough version
(use-package transient
  :ensure t
  :demand t)

;; Wait for transient to be fully loaded
(elpaca-wait)

;;; Magit - Git interface
(use-package magit
  :ensure t
  :config
  ;; Magit keybindings with M-g prefix
  (general-define-key
   :prefix "M-g"
   "g" '(magit-status :which-key "status")
   "f" '(magit-file-dispatch :which-key "file dispatch")
   "l" '(magit-log :which-key "log")
   "b" '(magit-branch :which-key "branch")
   "p" '(magit-pull :which-key "pull")
   "P" '(magit-push :which-key "push")
   "c" '(magit-commit :which-key "commit")
   "d" '(magit-diff :which-key "diff"))

  ;; Magit settings
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

;;; Magit Delta - Better diff highlighting
(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; Ignore global gitconfig to prevent rendering conflicts
  (add-to-list 'magit-delta-delta-args "--no-gitconfig")

  ;; Use delta themes that complement Modus themes
  ;; Modus Operandi (light) pairs well with Catppuccin Latte
  ;; Modus Vivendi (dark) pairs well with Catppuccin Mocha
  (setq magit-delta-default-dark-theme "Catppuccin Mocha"
        magit-delta-default-light-theme "Catppuccin Latte")

  ;; Hide +/- markers in fringe for cleaner look
  (setq magit-delta-hide-plus-minus-markers t))

(provide 'init-version-control)
;;; init-version-control.el ends here
