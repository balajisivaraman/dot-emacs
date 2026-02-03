;;; init-keybindings.el --- Global keybindings with General -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures General.el and Which-Key for keybinding management.
;; Sets up prefix keys and their descriptions.
;; Package-specific keybindings remain in their respective configuration files.

;;; Code:

;;; General - Keybinding framework
(use-package general
  :ensure t
  :demand t  ;; Load immediately, don't defer
  :config
  (general-override-mode))

;; Wait for General to be fully loaded
(elpaca-wait)

;;; Which-Key - Show available keybindings
(use-package which-key
  :ensure t
  :demand t  ;; Load immediately
  :config
  ;; Delay before showing which-key popup
  (setq which-key-idle-delay 0.5)

  ;; Show which-key at the bottom
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)

  ;; Limit height
  (setq which-key-side-window-max-height 0.25)

  ;; Sort by key
  (setq which-key-sort-order 'which-key-key-order-alpha)

  ;; Enable which-key mode
  (which-key-mode 1))

;; Wait for Which-Key to be fully loaded
(elpaca-wait)

;;; Define prefix descriptions for which-key
;; These prefixes are used by various packages
(which-key-add-key-based-replacements
  "M-j" "avy/jump"
  "M-m" "org/notes"
  "M-g" "magit/git"
  "M-s" "search")

(provide 'init-keybindings)
;;; init-keybindings.el ends here
