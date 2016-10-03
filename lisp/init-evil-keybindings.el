;;; -*- lexical-binding: t -*-
;;; init-evil-keybindings.el --- Loads all Evil related keybindings.

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2016 Balaji Sivaraman

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

;; This file contains all my evil-mode related keybindings. A lot of these are inspired by Spacemacs.

;;; Code:

;; Universal Argument
(evil-leader/set-key "u" 'universal-argument)

;; Shell Command
(evil-leader/set-key "!" 'shell-command)

(evil-leader/set-key "tr" 'linum-relative-mode)

(evil-leader/set-key
  "<SPC>" 'helm-M-x)

;; Buffer Keybindings
(evil-leader/set-key
  "bd" 'kill-buffer
  "bb" 'switch-to-buffer
  "bB" 'ibuffer
  "br" 'revert-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer)

;; General Keybindings
(evil-leader/set-key
  "ff" 'helm-find-files
  "Tn" 'balaji/cycle-themes
  "ir" 'indent-region)

;; Lisp Keybindings
(evil-leader/set-key
  "er" 'eval-region
  "eb" 'eval-buffer)

;; Evil Mode
(bind-key "\C-c" 'evil-force-normal-state evil-insert-state-map)
(bind-key "\C-c" 'evil-force-normal-state evil-replace-state-map)
(bind-key "\C-c" 'evil-force-normal-state evil-visual-state-map)
(evil-leader/set-key
  "jc" 'avy-goto-char
  "jj" 'avy-goto-char
  "jl" 'avy-goto-line
  "jw" 'avy-goto-word-1)

;; Projectile Mode
(evil-leader/set-key
  "ph" 'helm-projectile
  "pi" 'projectile-invalidate-cache
  "pk" 'projectile-kill-buffers
  "xo" 'helm-imenu)

;; Helm Ag
(evil-leader/set-key
  "/" 'helm-do-ag)

;; Magit Mode
(evil-leader/set-key
  "gg" 'magit-status
  "gb" 'magit-branch
  "gs" 'magit-stash
  "ga" 'magit-stash-apply
  "gp" 'magit-pull
  "gr" 'magit-reset-head
  "gR" 'magit-reset-head-hard
  "gf" 'magit-fetch
  "gl" 'magit-log-all
  "gL" 'magit-log
  "gc" 'magit-checkout)

;; Expand Region
(evil-leader/set-key
  "ce" 'er/expand-region
  "cc" 'er/contract-region)

;; Transpose Words
(evil-leader/set-key
  "Tc" 'transpose-chars
  "Tw" 'transpose-words
  "Tt" 'transpose-words
  "Tl" 'transpose-lines
  "Te" 'transpose-sexps
  "Ts" 'transpose-sentences
  "Tp" 'transpose-paragraphs)

;; Bookmarks
(evil-leader/set-key
  "Bb" 'bookmark-jump
  "Bj" 'bookmark-jump
  "Bs" 'bookmark-set
  "Bl" 'bookmark-bmenu-list)

;; Package Utils
(evil-leader/set-key
  "Pu" 'paradox-upgrade-packages
  "PP" 'package-list-packages-no-fetch
  "Pp" 'paradox-list-packages)

(evil-leader/set-key
    "qr" 'restart-emacs
    "qq" 'save-buffers-kill-emacs)

(provide 'init-evil-keybindings)
;;; init-evil-keybindings.el ends here
