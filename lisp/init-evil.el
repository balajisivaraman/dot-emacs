;;; init-evil.el --- Evil Mode configuration.  -*- lexical-binding: t -*-

;; Copyright (C) 2017  Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file loads Evil Mode and all assorted Evil mode related plugins. Evil Keybindings are in a separate file.

;;; Code:

(defun balaji/init-evil-leader ()
  (use-package evil-leader
    :init
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")))

(defun balaji/init-linum-relative ()
  (use-package linum-relative
    :diminish linum-relative-mode
    :commands linum-relative-mode
    :init
    (linum-relative-mode)
    :config
    (setq linum-relative-current-symbol "")))

(defun balaji/init-evil-org ()
  (use-package evil-org
    :after org
    :diminish
    (evil-org-mode)
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))))

(defun balaji/init-evil-collection ()
  (use-package evil-collection
    :after evil
    :init
    (setq
     evil-collection-mode-list
     '(
       bookmark
       calc
       calendar
       company
       deadgrep
       diff-mode
       dired
       doc-view
       edebug
       ediff
       elisp-mode
       elisp-refs
       ert
       eshell
       flycheck
       git-timemachine
       grep
       helm
       help
       ibuffer
       info
       lsp-ui-imenu
       macrostep
       magit
       man
       outline
       term
       which-key
       woman
       ))
    :config
    (evil-collection-init)))

(defun balaji/init-evil-mode ()
  "initialize evil mode and all its plugins"
  (setq evil-default-cursor 'box)
  (balaji/init-evil-leader)
  (use-package evil-visualstar
    :config
    (setq evil-visualstar/persistent t))
  (use-package evil-nerd-commenter
    :init
    (evilnc-default-hotkeys nil t))
  (use-package evil-indent-textobject)
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1))
  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))
  (use-package evil-escape
    :diminish evil-escape-mode
    :config
    (setq evil-escape-key-sequence "fd"))
  (use-package evil-magit)
  (use-package vi-tilde-fringe
    :diminish vi-tilde-fringe-mode
    :init
    (global-vi-tilde-fringe-mode))
  (use-package evil-mc
    :diminish evil-mc-mode)
  (use-package evil-mc-extras
    :disabled t
    :after evil-mc)
  (use-package evil-snipe
    :diminish evil-snipe-local-mode
    :config
    (setq evil-snipe-scope 'buffer))
  (use-package evil-goggles
    :diminish (evil-goggles-mode)
    :config
    (evil-goggles-mode)
    (evil-goggles-use-diff-faces))
  (use-package evil-multiedit
    :config
    (evil-multiedit-default-keybinds)
    (define-key evil-insert-state-map (kbd "M-d") 'kill-word))
  (use-package evil-rsi
    :diminish (evil-rsi-mode)
    :init
    (evil-rsi-mode))
  (evil-mode t)
  (balaji/init-linum-relative)
  (balaji/init-evil-org)
  (evil-escape-mode)
  (global-evil-visualstar-mode)
  (add-hook 'prog-mode-hook 'evil-mc-mode)
  (add-hook 'text-mode-hook 'evil-mc-mode)
  (add-hook 'prog-mode-hook 'turn-on-evil-snipe-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-snipe-mode)
  (add-hook 'prog-mode-hook 'turn-on-evil-snipe-override-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-snipe-override-mode)
  (add-hook 'org-agenda-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'org-agenda-mode-hook 'turn-off-evil-snipe-override-mode)
  (balaji/init-evil-collection)
  (require 'init-evil-keybindings))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil)
  :config
  (balaji/init-evil-mode))

(provide 'init-evil)
;;; init-evil.el ends here
