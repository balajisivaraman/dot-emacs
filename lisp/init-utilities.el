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
