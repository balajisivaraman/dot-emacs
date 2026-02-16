;;; init-ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures themes, modeline, and typography.
;; Uses Modus themes for colors and custom typography for fonts.

;;; Code:

;;; Theme Configuration - Modus Themes
(use-package modus-themes
  :ensure t
  :init
  ;; Pre-load both themes so auto-dark can switch between them
  ;; Third argument 't means load but don't enable
  (load-theme 'modus-operandi t 't)
  (load-theme 'modus-vivendi t 't)
  ;; Enable Operandi as the default light theme
  (enable-theme 'modus-operandi)
  :config
  ;; Colorful headings with rainbow colors
  (setq modus-themes-headings
        '((1 . (rainbow))
          (2 . (rainbow))
          (3 . (rainbow))
          (4 . (rainbow))
          (5 . (rainbow))
          (t . (rainbow))))

  ;; Org blocks with subtle background
  (setq modus-themes-org-blocks 'gray-background)

  ;; Better mixed font support
  (setq modus-themes-mixed-fonts t)

  ;; Italic and bold constructs
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  ;; Keep UI elements monospace
  (setq modus-themes-variable-pitch-ui nil)

  ;; Mark modus themes as safe
  (setq custom-safe-themes t))

;;; Auto Dark - Switch themes based on macOS system appearance
(use-package auto-dark
  :ensure t
  :after modus-themes
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  ;; Enable osascript for emacs-plus-app (cask version)
  (auto-dark-allow-osascript t)
  :init
  ;; Enable auto-dark mode to handle theme switching
  (auto-dark-mode 1))

;;; Modeline Configuration
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count t
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)))

;; All-the-icons (required by doom-modeline)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;;; Typography Configuration
;; Base font size in points (configurable)
(defvar bs/base-font-size 14
  "Base font size in points for all typography calculations.")

;; Minor Third scale ratio (1.067)
(defvar bs/font-scale-ratio 1.067
  "Typography scale ratio for heading sizes (Minor Third = 1.067).")

;; Font families
(defvar bs/heading-font "Literata"
  "Font family for headings.")

(defvar bs/variable-pitch-font "Literata"
  "Font family for variable-pitch text (body text).")

(defvar bs/monospace-font "Lilex Nerd Font"
  "Font family for monospace text (code).")

;; Calculate height for a given level using Minor Third scale
(defun bs/calculate-font-height (level)
  "Calculate font height for LEVEL using Minor Third scale.
Level 0 is base size, level 1 is 1.067x, level 2 is 1.138x, etc."
  (expt bs/font-scale-ratio level))

;; Set base typography (non-org faces)
(defun bs/set-base-typography ()
  "Configure base typography (default, fixed-pitch, variable-pitch faces)."
  (interactive)

  ;; Set default face (monospace)
  (set-face-attribute 'default nil
                      :font bs/monospace-font
                      :height (* bs/base-font-size 10)) ; Height is in 1/10pt

  ;; Set fixed-pitch face (monospace)
  (set-face-attribute 'fixed-pitch nil
                      :font bs/monospace-font
                      :height 1.0) ; Relative to default

  ;; Set variable-pitch face (body text)
  (set-face-attribute 'variable-pitch nil
                      :font bs/variable-pitch-font
                      :height 1.0)) ; Relative to default

;; Apply base typography on startup
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (bs/set-base-typography))))
  (bs/set-base-typography))

;; Update base font size function
(defun bs/update-base-font-size (size)
  "Update base font size to SIZE and recalculate all typography."
  (interactive "nBase font size (in points): ")
  (setq bs/base-font-size size)
  (bs/set-base-typography)
  (message "Base font size updated to %d pt" size))

;;; Golden Ratio - Automatic window resizing
(use-package golden-ratio
  :ensure t
  :config
  ;; Enable golden-ratio mode
  (golden-ratio-mode 1)

  ;; Exclude certain modes/commands from golden-ratio
  (setq golden-ratio-exclude-modes
        '("ediff-mode"
          "eshell-mode"
          "dired-mode"))

  ;; Don't resize minibuffer and which-key
  (setq golden-ratio-exclude-buffer-regexp
        '("^ \\*Minibuf.*\\*$"
          "^\\*which-key\\*$")))

(provide 'init-ui)
;;; init-ui.el ends here
