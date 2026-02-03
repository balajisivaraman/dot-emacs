;;; init-utilities.el --- Utility packages and functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous utility packages and global helper functions.

;;; Code:

;;; Helper Functions
(defun bs/open-config ()
  "Open init.el in a new buffer."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun bs/reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Configuration reloaded!"))

;;; Restart Emacs
(use-package restart-emacs
  :ensure t
  :config
  ;; Keybindings for quit and restart
  (general-define-key
   "C-x C-c" 'save-buffers-kill-terminal  ;; Default quit (keep default)
   "C-c q q" '(save-buffers-kill-terminal :which-key "quit emacs")
   "C-c q r" '(restart-emacs :which-key "restart emacs"))

  ;; Which-key description for C-c q prefix
  (which-key-add-key-based-replacements
    "C-c q" "quit/restart"))

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

;;; Crux - Useful interactive commands
(use-package crux
  :ensure t
  :config
  ;; Unbind prefixes we want to use
  (global-unset-key (kbd "C-c f"))
  (global-unset-key (kbd "C-c b"))
  (global-unset-key (kbd "C-c e"))

  ;; File operations keybindings
  (general-define-key
   "C-c f d" '(crux-delete-file-and-buffer :which-key "delete file and buffer")
   "C-c f r" '(crux-rename-file-and-buffer :which-key "rename file and buffer")
   "C-c f c" '(crux-copy-file-preserve-attributes :which-key "copy file"))

  ;; Buffer operations
  (general-define-key
   "C-c b R" '(crux-revert-buffer-no-confirm :which-key "revert buffer")
   "C-c b k" '(crux-kill-other-buffers :which-key "kill other buffers"))

  ;; Editing shortcuts
  (general-define-key
   "C-c e d" '(crux-duplicate-current-line-or-region :which-key "duplicate line/region")
   "C-c e D" '(crux-duplicate-and-comment-current-line-or-region :which-key "duplicate and comment"))

  ;; Which-key prefix descriptions
  (which-key-add-key-based-replacements
    "C-c f" "file operations"
    "C-c b" "buffer operations"
    "C-c e" "editing"))

;;; Configuration Management Keybindings
(general-define-key
 :prefix "C-c c"
 "o" '(bs/open-config :which-key "open config")
 "r" '(bs/reload-config :which-key "reload config"))

;; Which-key description for C-c c prefix
(which-key-add-key-based-replacements
  "C-c c" "config")

(provide 'init-utilities)
;;; init-utilities.el ends here
