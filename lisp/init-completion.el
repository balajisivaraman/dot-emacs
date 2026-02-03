;;; init-completion.el --- Completion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures the modern completion stack:
;; - Vertico: Minibuffer completion UI
;; - Orderless: Flexible completion style
;; - Marginalia: Rich annotations
;; - Corfu: In-buffer completion popup
;; - Cape: Completion At Point Extensions
;; - Embark: Contextual actions
;; - Consult: Enhanced search and navigation commands

;;; Code:

;;; Vertico - Vertical minibuffer completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; Cycle through candidates
  (setq vertico-cycle t)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Resize vertico minibuffer
  (setq vertico-resize t))

;;; Orderless - Flexible completion style
(use-package orderless
  :ensure t
  :config
  ;; Use orderless for completion
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :config
  ;; Align annotations to right
  (setq marginalia-align 'right))

;;; Corfu - In-buffer completion popup
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  ;; Auto-show completion popup
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 2)

  ;; Cycle through candidates
  (setq corfu-cycle t)

  ;; Quit corfu on specific events
  (setq corfu-quit-no-match 'separator)
  (setq corfu-quit-at-boundary 'separator)

  ;; Preview current candidate
  (setq corfu-preview-current t)

  ;; Enable corfu in minibuffer
  (setq corfu-popupinfo-delay '(0.5 . 0.2))

  ;; Disable corfu in certain modes
  (when (boundp 'corfu-excluded-modes)
    (dolist (mode '(eshell-mode shell-mode))
      (add-to-list 'corfu-excluded-modes mode))))

;; Disable corfu in org-mode tables
(defun bs/corfu-disable-in-org-table ()
  "Disable corfu in org tables."
  (when (and (derived-mode-p 'org-mode)
             (org-at-table-p))
    (setq-local corfu-auto nil)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'post-command-hook #'bs/corfu-disable-in-org-table nil t)))

;;; Cape - Completion At Point Extensions
(use-package cape
  :ensure t
  :config
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;;; Embark - Contextual actions
(use-package embark
  :ensure t
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Keybindings
  (general-define-key
   "C-c e" 'embark-act
   "C-h B" 'embark-bindings))

;;; Embark-Consult integration
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Consult - Enhanced commands
(use-package consult
  :ensure t
  :config
  ;; Preview settings
  (setq consult-preview-key 'any)

  ;; Narrow key
  (setq consult-narrow-key "<")

  ;; Use ripgrep for consult-grep
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")

  ;; Project root function
  (setq consult-project-function #'consult--default-project-function)

  ;; Keybindings - C-x bindings
  (general-define-key
   "C-x b" 'consult-buffer
   "C-x 4 b" 'consult-buffer-other-window
   "C-x 5 b" 'consult-buffer-other-frame
   "C-x r b" 'consult-bookmark
   "M-y" 'consult-yank-pop)

  ;; M-s bindings (search)
  (general-define-key
   :prefix "M-s"
   "g" '(consult-ripgrep :which-key "ripgrep")
   "G" '(consult-git-grep :which-key "git grep")
   "i" '(consult-imenu :which-key "imenu")
   "I" '(consult-imenu-multi :which-key "imenu multi")
   "o" '(consult-outline :which-key "outline")
   "l" '(consult-line :which-key "line")
   "L" '(consult-line-multi :which-key "line multi"))

  ;; M-g bindings (goto)
  (general-define-key
   :prefix "M-g"
   "e" '(consult-compile-error :which-key "compile error")
   "m" '(consult-mark :which-key "mark")
   "k" '(consult-global-mark :which-key "global mark")))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Show recursion depth in minibuffer
(minibuffer-depth-indicate-mode 1)

(provide 'init-completion)
;;; init-completion.el ends here
