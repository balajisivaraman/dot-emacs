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

;; macOS modifiers: keep Option as Alt, map Command to Meta.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;;; Theme — auto-dark (switches modus-operandi/vivendi with macOS appearance)
(use-package auto-dark
  :ensure t
  :demand t
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-allow-osascript t)
  :init (auto-dark-mode))

;; Feature modules can hook here to re-apply theme-sensitive face tweaks.
(defvar bs/theme-change-hook nil
  "Hook run after a theme is enabled.")

(defun bs/run-theme-change-hook (&rest _)
  "Run `bs/theme-change-hook' after theme changes."
  (dolist (fn bs/theme-change-hook)
    (condition-case err
        (funcall fn)
      (error
       (message "theme-change hook failed (%s): %s" fn (error-message-string err))))))

(advice-add 'enable-theme :after #'bs/run-theme-change-hook)

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

;;; envrc — per-buffer project environments via direnv (works well with mise)
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode 1))

(defun bs/reload-project-env ()
  "Reload direnv/mise environment for the current buffer."
  (interactive)
  (if (fboundp 'envrc-reload)
      (progn
        (envrc-reload)
        (message "Reloaded project environment"))
    (user-error "envrc is not available")))

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
   "C-x p s" '(bs/consult-ripgrep-project :which-key "ripgrep project")
   "C-x p t" '(bs/vterm-project         :which-key "terminal")
   "C-x p c" '(bs/copilot-in-project    :which-key "copilot")
   "C-x p e" '(bs/reload-project-env    :which-key "reload env")
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
         ("C-h x" . helpful-command))
  :config
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-command] #'helpful-command))

;;; transient — magit requires transient ≥ 0.12; override the old built-in.
(use-package transient :ensure t)

;;; magit
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
         ("C-x g" . magit-status)))

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

;;; crux — practical editing commands
(use-package crux
  :ensure t
  :bind (("C-a"   . crux-move-beginning-of-line)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c R" . crux-rename-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)))

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

(use-package golden-ratio
  :ensure t
  :config
  ;; Keep Copilot side terminal stable instead of letting it dominate layout.
  (add-to-list 'golden-ratio-inhibit-functions
               (lambda ()
                 (string-match-p "\\*copilot-" (buffer-name))))
  (golden-ratio-mode 1))

(defun bs/vterm--new-numbered-name (base-name)
  "Return next available numbered terminal buffer name from BASE-NAME."
  (let ((n 2))
    (while (get-buffer (format "%s-%d*" (substring base-name 0 -1) n))
      (setq n (1+ n)))
    (format "%s-%d*" (substring base-name 0 -1) n)))

(defun bs/vterm--open (base-name default-dir force-new)
  "Open vterm with BASE-NAME in DEFAULT-DIR.
Reuse existing base buffer unless FORCE-NEW is non-nil."
  (let* ((default-directory default-dir)
         (target-name (if force-new
                          (bs/vterm--new-numbered-name base-name)
                        base-name))
         (existing (get-buffer target-name)))
    (if existing
        (pop-to-buffer existing)
      (vterm target-name))))

(defun bs/vterm-scratch (arg)
  "Open a scratch vterm buffer in HOME.
With prefix ARG, force creation of a new numbered buffer."
  (interactive "P")
  (bs/vterm--open "*scratch-terminal*"
                  (expand-file-name "~")
                  arg))

(defun bs/vterm-project (arg)
  "Open a project-rooted vterm buffer.
With prefix ARG, force creation of a new numbered buffer."
  (interactive "P")
  (let ((project (project-current t)))
    (bs/vterm--open (format "*vterm-%s*" (project-name project))
                    (project-root project)
                    arg)))

(defun bs/copilot-in-project ()
  "Open Copilot CLI in a side vterm rooted at the current project."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (name (project-name project))
         (default-directory root)
         (buffer-name (format "*copilot-%s*" name))
         (buffer (get-buffer buffer-name)))
    (display-buffer-in-side-window
     (or buffer (get-buffer-create buffer-name))
     '((side . right)
       (slot . 1)
       (window-width . 0.33)))
    (select-window (get-buffer-window buffer-name))
    (unless (derived-mode-p 'vterm-mode)
      (vterm buffer-name))
    (when (bound-and-true-p golden-ratio-mode)
      (golden-ratio))
    (unless buffer
      (vterm-send-string "copilot")
      (vterm-send-return))))

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
