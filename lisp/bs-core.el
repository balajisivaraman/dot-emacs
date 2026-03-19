;;; bs-core.el --- Core settings, UI, tools -*- lexical-binding: t -*-

;;; Cache / no-littering
;; Set cache dirs BEFORE requiring no-littering so it picks them up.
(setq no-littering-etc-directory (expand-file-name ".cache/etc/" user-emacs-directory))
(setq no-littering-var-directory (expand-file-name ".cache/var/" user-emacs-directory))

;; Explicit redirects (some are also handled by no-littering, belt + suspenders).
(setq backup-directory-alist
      `(("." . ,(expand-file-name ".cache/backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name ".cache/auto-save/" user-emacs-directory) t)))
(make-directory (expand-file-name ".cache/auto-save/" user-emacs-directory) t)
(setq recentf-save-file
      (expand-file-name ".cache/var/recentf" user-emacs-directory))
(setq savehist-file
      (expand-file-name ".cache/var/savehist" user-emacs-directory))
(setq save-place-file
      (expand-file-name ".cache/var/saveplace" user-emacs-directory))

(use-package no-littering
  :ensure t)

;; Redirect native-comp cache.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name ".cache/eln/" user-emacs-directory)))

;;; User identity
(setq user-full-name    "Balaji Sivaraman")
(setq user-mail-address "balaji@example.com") ; update as needed

;;; Baseline settings
(setq-default fill-column 80)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq make-backup-files t)
(setq version-control t)
(setq delete-old-versions t)

(delete-selection-mode 1)
(electric-pair-mode 1)

;; Persist recent files, minibuffer history, cursor positions.
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)

;; y/n instead of yes/no
(setopt use-short-answers t)

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)   ; also dired, ibuffer

;; Show column number in modeline
(column-number-mode 1)

;; Line numbers in all buffers (bs-writing-mode disables them for markdown)
(global-display-line-numbers-mode 1)

;; Pixel-precise smooth scrolling (Emacs 29+)
(pixel-scroll-precision-mode 1)

;; Keep point away from window edges when scrolling
(setq scroll-conservatively 101)
(setq scroll-margin 2)

;; Ensure files end with a newline
(setq require-final-newline t)

;;; Theme — auto-dark (switches modus-operandi/vivendi with macOS appearance)
(use-package auto-dark
  :ensure t
  :demand t
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-allow-osascript t)
  :init (auto-dark-mode))

;;; Fonts
;; Default monospace face.
(set-face-attribute 'default nil :family "Inconsolata Nerd Font" :height 160)
;; Fixed-pitch face matches default.
(set-face-attribute 'fixed-pitch nil :family "Inconsolata Nerd Font" :height 1.0)
;; Variable-pitch face for prose.
(set-face-attribute 'variable-pitch nil :family "Literata" :height 1.0)

;;; nerd-icons — required by doom-modeline (font already installed)
(use-package nerd-icons
  :ensure t)

;;; doom-modeline
(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (setq doom-modeline-height 28)
  (setq doom-modeline-icon t)
  (doom-modeline-mode 1))

;;; which-key — show available key continuations.
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode 1))

;;; exec-path-from-shell — import PATH from zsh on macOS
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; General.el — organised prefix key definitions.
(use-package general
  :ensure t
  :demand t
  :config
  ;; Define the major prefix namespaces so which-key shows meaningful labels.
  (general-define-key :prefix "C-c h" "" '(nil :which-key "Hugo"))
  (general-define-key :prefix "C-c s" "" '(nil :which-key "Search"))
  (general-define-key :prefix "C-x p" "" '(nil :which-key "Project"))
  (general-define-key :prefix "C-c q" "" '(nil :which-key "Quit"))
  (general-define-key :prefix "C-c e" "" '(nil :which-key "Emacs config"))
  (general-define-key "M-T" #'bs/vterm-scratch)
  (general-define-key
   "C-x p p" '(project-switch-project   :which-key "switch project")
   "C-x p f" '(project-find-file        :which-key "find file")
   "C-x p b" '(project-switch-to-buffer :which-key "switch buffer")
   "C-x p k" '(project-kill-buffers     :which-key "kill buffers")
   "C-x p g" '(project-find-regexp      :which-key "grep")
   "C-x p t" '(bs/vterm-project         :which-key "terminal")
   "C-x p P" '(bs/project-scan-code-dir :which-key "scan ~/code"))
  (general-define-key
   "C-c q q" '(save-buffers-kill-emacs :which-key "quit")
   "C-c q r" '(restart-emacs           :which-key "restart"))
  (general-define-key "C-x C-c" nil)  ; disable accidental quit
  (general-define-key
   "C-c e r" '(bs/reload-config :which-key "reload config")
   "C-c e e" '(bs/open-config   :which-key "open init.el")))

;;; helpful — better *Help* buffers.
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;;; transient — magit requires transient ≥ 0.12; override the old built-in.
(use-package transient :ensure t)

;;; magit
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

;;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name ".cache/undo-tree/" user-emacs-directory))))
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1)
  ;; Replace suspend-frame (C-z) with undo; add redo on C-S-z and C-S-/
  (global-set-key (kbd "C-z")   #'undo-tree-undo)
  (global-set-key (kbd "C-S-z") #'undo-tree-redo)
  (global-set-key (kbd "C-?")   #'undo-tree-redo))

;;; ibuffer — better buffer list, replacing list-buffers
(global-set-key [remap list-buffers] #'ibuffer)
(setq ibuffer-show-empty-filter-groups nil)

(use-package ibuffer-vc
  :ensure t
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

;;; dired — built-in file manager
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)                          ; smart copy/rename target in split windows
  (setq dired-kill-when-opening-new-dired-buffer t)   ; don't accumulate dired buffers
  (setq dired-auto-revert-buffer t))                  ; auto-refresh when revisiting

;; Use Emacs's own ls implementation so dirs-first works on macOS without gls.
(use-package ls-lisp
  :ensure nil
  :custom
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-dirs-first t))

;;; dired-x — jump to dired, omit dotfiles toggle, and more
(use-package dired-x
  :ensure nil
  :after dired
  :bind ("C-x C-j" . dired-jump)
  :config
  ;; Hide macOS cruft in addition to the default dot-file pattern.
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\.DS_Store$")))

;;; nerd-icons-dired — file-type icons in dired buffers
(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

;;; nerd-icons-ibuffer — file-type icons in ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;; vterm
(use-package vterm
  :ensure t)

(defun bs/vterm-scratch ()
  "Open a scratch vterm buffer."
  (interactive)
  (vterm "*scratch-terminal*"))

(defun bs/vterm-project ()
  "Open a vterm buffer rooted in the current project."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (vterm (format "*vterm-%s*" (project-name (project-current))))))

(defun bs/project-scan-code-dir ()
  "Scan ~/code recursively and register all git projects."
  (interactive)
  (let ((count 0))
    (cl-labels ((scan (dir)
                  (if (file-exists-p (expand-file-name ".git" dir))
                      (when-let ((proj (project-current nil dir)))
                        (project-remember-project proj)
                        (cl-incf count))
                    (dolist (entry (directory-files dir t "^[^.]"))
                      (when (file-directory-p entry)
                        (scan entry))))))
      (scan (expand-file-name "~/code")))
    (message "Registered %d projects from ~/code" count)))

;;; restart-emacs
(use-package restart-emacs
  :ensure t)

;;; Config helpers
(defun bs/reload-config ()
  "Reload init.el without restarting Emacs."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun bs/open-config ()
  "Open init.el for editing."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(provide 'bs-core)
;;; bs-core.el ends here
