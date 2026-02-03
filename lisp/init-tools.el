;;; init-tools.el --- Development tools configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures development and productivity tools:
;; - Magit for Git operations
;; - Deft for searching notes
;; - Helpful for better help buffers

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
  ;; Use delta themes that complement Modus themes
  ;; Modus Operandi (light) pairs well with GitHub
  ;; Modus Vivendi (dark) pairs well with Monokai Extended
  (setq magit-delta-default-dark-theme "Catppuccin Mocha"
        magit-delta-default-light-theme "Catppuccin Latte")

  ;; Hide +/- markers in fringe for cleaner look
  (setq magit-delta-hide-plus-minus-markers t))

;;; Deft - Fast note searching
(use-package deft
  :ensure t
  :config
  ;; Deft directory (will be set for org notes)
  (setq deft-directory "~/Documents/notes"
        deft-extensions '("org" "txt" "md")
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval 0)

  ;; Deft keybinding with M-s prefix (search)
  (general-define-key
   :prefix "M-s"
   "d" '(deft :which-key "deft (search notes)")))

;;; Helpful - Better help buffers
(use-package helpful
  :ensure t
  :config
  ;; Replace default help commands with helpful
  (general-define-key
   "C-h f" 'helpful-callable
   "C-h v" 'helpful-variable
   "C-h k" 'helpful-key
   "C-h C" 'helpful-command
   "C-h F" 'helpful-function))

;;; GCMH - Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(provide 'init-tools)
;;; init-tools.el ends here
