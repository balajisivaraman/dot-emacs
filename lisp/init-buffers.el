;;; init-buffers.el --- Buffer management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures buffer management, ibuffer enhancements, and related utilities.

;;; Code:

;;; IBuffer Configuration
;; Enhanced buffer list with grouping and better display

;; Auto-update ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

;; Don't show empty groups
(setq ibuffer-show-empty-filter-groups nil)

;; Use human-readable file sizes
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 32 32 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; IBuffer grouping by project/mode
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*dashboard\\*$")))
         ("Org" (mode . org-mode))
         ("Programming" (or
                         (mode . python-mode)
                         (mode . js-mode)
                         (mode . typescript-mode)
                         (mode . rust-mode)
                         (mode . go-mode)
                         (mode . emacs-lisp-mode)))
         ("Dired" (mode . dired-mode))
         ("Magit" (name . "^magit"))
         ("Help" (or
                  (name . "^\\*Help\\*$")
                  (name . "^\\*info\\*$")
                  (name . "^\\*helpful")
                  (name . "^\\*Apropos\\*$"))))))

;; Apply default filter groups
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;; Buffer Navigation
;; Better buffer switching with consult-buffer (configured in init-completion.el)

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;; Auto-revert buffers
;; Automatically reload files when they change on disk
(global-auto-revert-mode 1)

;; Also revert dired buffers
(setq global-auto-revert-non-file-buffers t)

;; Be quiet about reverts
(setq auto-revert-verbose nil)

;;; Window Management Helpers
(defun bs/split-window-right-and-focus ()
  "Split window right and focus on the new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun bs/split-window-below-and-focus ()
  "Split window below and focus on the new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun bs/kill-this-buffer ()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

;; Keybindings for window/buffer management
(general-define-key
 "C-x k" 'bs/kill-this-buffer
 "C-x 2" 'bs/split-window-below-and-focus
 "C-x 3" 'bs/split-window-right-and-focus)

(provide 'init-buffers)
;;; init-buffers.el ends here
