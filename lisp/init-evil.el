;;; init-evil.el --- Evil Mode configuration.  -*- lexical-binding: t -*-

;; Copyright (C) 2021  Balaji Sivaraman

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

(defun bs/init-evil-leader ()
  (use-package evil-leader
    :init
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")))

(defun bs/init-linum-relative ()
  (use-package linum-relative
    :diminish linum-relative-mode
    :commands linum-relative-mode
    :init
    (linum-relative-mode)
    :config
    (setq linum-relative-current-symbol "")))

(defun bs/init-evil-org ()
  (use-package evil-org
    :after org
    :diminish (evil-org-mode)
    :hook (org-mode . evil-org-mode)
    :custom (evil-org-set-key-theme '(textobjects insert navigation additional shift heading))))

(defun bs/init-evil-collection ()
  (use-package evil-collection
    :after evil
    :custom
    (evil-collection-want-find-usages-bindings t)
    (evil-collection-term-sync-state-and-mode-p t)
    (evil-collection-setup-minibuffer t)
    (evil-collection-want-unimpaired-p t)
    (evil-collection-outline-bind-tab-p t)
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
       eglot
       elfeed
       elisp-mode
       elisp-refs
       embark
       ert
       eshell
       flymake
       git-timemachine
       grep
       help
       ibuffer
       imenu
       indent
       info
       macrostep
       magit
       man
       markdown-mode
       ;; minibuffer
       outline
       pdf
       restclient
       rg
       term
       typescript-mode
       unimpaired
       which-key
       woman
       xref
       ))
    :config
    (evil-collection-init)))

(defun bs/init-evil-mode ()
  "initialize evil mode and all its plugins"
  (setq evil-default-cursor 'box)
  (bs/init-evil-leader)
  (use-package evil-visualstar
    :config
    (setq evil-visualstar/persistent t))
  (use-package evil-commentary
    :init
    (evil-commentary-mode))
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
  (bs/init-linum-relative)
  (bs/init-evil-org)
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
  (bs/init-evil-collection)
  (evil-set-initial-state 'messages-mode 'normal)
  (require 'init-evil-keybindings))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-search-module 'evil-search)
  :config
  (bs/init-evil-mode))

(provide 'init-evil)
;;; init-evil.el ends here
