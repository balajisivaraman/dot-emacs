;;; bs-completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t -*-

;;; Minibuffer: vertico → orderless → marginalia → consult → embark

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  ;; Keep basic for tramp/remote paths where orderless doesn't apply well.
  (setq completion-category-overrides
        '((file (styles basic partial-completion))
          (tramp (styles basic)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;;; nerd-icons-completion — icons in vertico + marginalia minibuffer
(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :ensure t)

;; Search keybindings under C-c s (defined in bs-core via general).
;; C-s / C-r intentionally left as isearch-forward/isearch-backward.
(use-package general
  :ensure nil ; already loaded by bs-core
  :config
  (general-define-key
   "C-x b"   '(consult-buffer   :which-key "buffer/recent")
   "C-c s b" '(consult-buffer   :which-key "buffer")
   "C-c s l" '(consult-line     :which-key "line")
   "C-c s r" '(consult-ripgrep  :which-key "ripgrep")
   "C-c s f" '(consult-find     :which-key "find file")
   "C-c s o" '(consult-outline  :which-key "outline")))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; In-buffer completion: corfu + cape

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto  t)
  (setq corfu-cycle t)
  (setq tab-always-indent 'complete)
  (keymap-set corfu-map "TAB" #'corfu-complete)
  (keymap-set corfu-map "<tab>" #'corfu-complete)
  (global-corfu-mode 1))

;;; nerd-icons-corfu — file-type icons in corfu in-buffer popup
(use-package nerd-icons-corfu
  :ensure t
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :config
  ;; Add cape sources to the global completion-at-point-functions list.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

(provide 'bs-completion)
;;; bs-completion.el ends here
