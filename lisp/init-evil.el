;;; -*- lexical-binding: t -*-
;;; init-evil.el --- Evil Mode configuration.

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2017 Balaji Sivaraman

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

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
    :config
    (linum-relative-mode)
    (setq linum-relative-current-symbol "")))

(defun balaji/init-evil-org ()
  (use-package evil-org
    :diminish
    (evil-org-mode)))

(defun balaji/init-evil-mode ()
  "initialize evil mode and all its plugins"
  (setq evil-default-cursor 'box)
  (balaji/init-evil-leader)
  (use-package evil-visualstar)
  (use-package evil-nerd-commenter
    :init
    (evilnc-default-hotkeys))
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
    (setq evil-escape-key-sequence "ue"))
  (use-package evil-magit)
  (use-package vi-tilde-fringe
    :diminish vi-tilde-fringe-mode
    :init
    (global-vi-tilde-fringe-mode))
  (use-package evil-mc
    :diminish evil-mc-mode)
  (use-package evil-mc-extras
    :after evil-mc)
  (use-package evil-snipe
    :diminish evil-snipe-local-mode
    :config
    (setq evil-snipe-scope 'buffer))
  (evil-mode t)
  (balaji/init-linum-relative)
  (balaji/init-evil-org)
  (evil-escape-mode)
  (global-evil-visualstar-mode)
  (global-evil-mc-mode 1)
  (global-evil-mc-extras-mode 1)
  (add-hook 'prog-mode 'evil-snipe-local-mode)
  (add-hook 'text-mode 'evil-snipe-local-mode)
  (add-hook 'prog-mode 'evil-snipe-override-local-mode)
  (add-hook 'text-mode 'evil-snipe-override-local-mode)
  (add-hook 'org-agenda-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'org-agenda-mode-hook 'turn-off-evil-snipe-override-mode)
  (require 'init-evil-keybindings))

(use-package evil
  :init
  (balaji/init-evil-mode))

(provide 'init-evil)
;;; init-evil.el ends here
