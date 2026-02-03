;;; init-editing.el --- Editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures enhanced editing features including undo-tree, avy jump,
;; and spell checking with jinx.

;;; Code:

;;; Undo Tree
(use-package undo-tree
  :ensure t
  :init
  ;; Unbind C-z from its default (suspend-frame) before undo-tree loads
  (global-unset-key (kbd "C-z"))
  :config
  ;; Enable undo-tree globally
  (global-undo-tree-mode)

  ;; Store undo history in cache
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree/" user-cache-directory))))

  ;; Enable persistent undo history (but not for iCloud files)
  (setq undo-tree-auto-save-history t)

  ;; Don't save undo history for files in iCloud Drive
  ;; This prevents corruption from iCloud sync conflicts
  (defun bs/undo-tree-suppress-icloud-history ()
    "Disable undo-tree history for iCloud Drive files."
    (when (and buffer-file-name
               (string-match-p "Mobile Documents/com~apple~CloudDocs" buffer-file-name))
      (setq-local undo-tree-auto-save-history nil)))

  (add-hook 'find-file-hook #'bs/undo-tree-suppress-icloud-history)

  ;; Don't show undo tree in diff by default (can toggle with 'd')
  (setq undo-tree-visualizer-diff nil)

  ;; Prevent undo-tree from littering
  (setq undo-tree-visualizer-timestamps t)

  ;; Set keybindings directly in undo-tree-map
  (define-key undo-tree-map (kbd "C-z") 'undo-tree-undo)
  (define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "M-_") 'undo-tree-redo))

;;; Avy - Jump to visible text
(use-package avy
  :ensure t
  :config
  ;; Use all keys for avy targets (more options)
  (setq avy-keys (number-sequence ?a ?z))

  ;; Show avy overlays in all windows
  (setq avy-all-windows t)

  ;; Highlight first decision char in cyan
  (setq avy-background t)

  ;; Timeout for char input
  (setq avy-timeout-seconds 0.5)

  ;; First unbind M-j so we can use it as a prefix
  (global-unset-key (kbd "M-j"))

  ;; Keybindings with M-j prefix
  (general-define-key
   :prefix "M-j"
   "c" '(avy-goto-char-timer :which-key "char timer")
   "l" '(avy-goto-line :which-key "line")
   "w" '(avy-goto-word-1 :which-key "word")
   "j" '(avy-goto-char :which-key "char")))

;;; Jinx - Modern spell checking
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :config
  ;; Use standard faces for misspellings
  (setq jinx-delay 0.2)

  ;; Languages to check (add more as needed)
  (setq jinx-languages "en_US")

  ;; Keybindings
  (general-define-key
   "M-$" 'jinx-correct
   "C-M-$" 'jinx-languages))

;;; Visual Line Mode for writing
;; Enable visual-line-mode (word wrapping) for text modes
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(provide 'init-editing)
;;; init-editing.el ends here
