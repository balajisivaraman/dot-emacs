;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures vterm for terminal emulation within Emacs.
;; Provides global scratch terminal and per-project terminals.

;;; Code:

;;; Vterm - Terminal emulator
(use-package vterm
  :ensure t
  :config
  ;; Enable directory tracking (sync Emacs default-directory with shell pwd)
  (setq vterm-enable-manipulate-selection-data-by-osc52 t)

  ;; Kill buffer when shell exits
  (setq vterm-kill-buffer-on-exit t)

  ;; Scrollback buffer size
  (setq vterm-max-scrollback 10000))

;;; Global Scratch Terminal
(defun bs/scratch-terminal ()
  "Switch to global scratch terminal, creating it if it doesn't exist.
The terminal is identified by buffer name *scratch-terminal*."
  (interactive)
  (let ((buffer-name "*scratch-terminal*"))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name))))

;;; Project Terminal
(defun bs/project-terminal ()
  "Open a terminal at the current project root.
The terminal is named *vterm: <project-name>* and opens in the project root.
If the buffer already exists, switch to it."
  (interactive)
  (let* ((project (project-current t))
         (project-root (project-root project))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (buffer-name (format "*vterm: %s*" project-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((default-directory project-root))
        (vterm buffer-name)))))

;;; Keybindings
;; Global scratch terminal with M-S-t (Alt+Shift+t)
(global-unset-key (kbd "M-T"))
(general-define-key
 "M-T" '(bs/scratch-terminal :which-key "scratch terminal"))

;; Project terminal with C-x p t
(general-define-key
 :prefix "C-x p"
 "t" '(bs/project-terminal :which-key "project terminal"))

(provide 'init-terminal)
;;; init-terminal.el ends here
