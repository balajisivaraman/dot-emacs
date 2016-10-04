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

(evil-leader/set-key
  "tr" 'linum-relative-mode
  "tg" 'balaji-toggle-golden-ratio)

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

;; Ibuffer Bindings
(evil-define-key 'normal 'ibuffer-mode-map "A" 'ibuffer-do-view)
(evil-define-key 'normal 'ibuffer-mode-map "D" 'ibuffer-do-delete)
(evil-define-key 'normal 'ibuffer-mode-map "E" 'ibuffer-do-eval)
(evil-define-key 'normal 'ibuffer-mode-map "F" 'ibuffer-do-shell-command-file)
(evil-define-key 'normal 'ibuffer-mode-map "H" 'ibuffer-do-view-other-frame)
(evil-define-key 'normal 'ibuffer-mode-map "I" 'ibuffer-do-query-replace-regexp)
(evil-define-key 'normal 'ibuffer-mode-map "M" 'ibuffer-do-toggle-modified)
(evil-define-key 'normal 'ibuffer-mode-map "N" 'ibuffer-do-shell-command-pipe-replace)
(evil-define-key 'normal 'ibuffer-mode-map "O" 'ibuffer-do-occur)
(evil-define-key 'normal 'ibuffer-mode-map "P" 'ibuffer-do-print)
(evil-define-key 'normal 'ibuffer-mode-map "Q" 'ibuffer-do-query-replace)
(evil-define-key 'normal 'ibuffer-mode-map "R" 'ibuffer-do-rename-uniquely)
(evil-define-key 'normal 'ibuffer-mode-map "S" 'ibuffer-do-save)
(evil-define-key 'normal 'ibuffer-mode-map "T" 'ibuffer-do-toggle-read-only)
(evil-define-key 'normal 'ibuffer-mode-map "U" 'ibuffer-do-replace-regexp)
(evil-define-key 'normal 'ibuffer-mode-map "V" 'ibuffer-do-revert)
(evil-define-key 'normal 'ibuffer-mode-map "W" 'ibuffer-do-view-and-eval)
(evil-define-key 'normal 'ibuffer-mode-map "X" 'ibuffer-do-shell-command-pipe)
(evil-define-key 'normal 'ibuffer-mode-map "`" 'ibuffer-switch-format)
(evil-define-key 'normal 'ibuffer-mode-map "b" 'ibuffer-bury-buffer)
(evil-define-key 'normal 'ibuffer-mode-map "d" 'ibuffer-mark-for-delete)
(evil-define-key 'normal 'ibuffer-mode-map "g" 'ibuffer-update)
(evil-define-key 'normal 'ibuffer-mode-map "j" 'ibuffer-jump-to-buffer)
(evil-define-key 'normal 'ibuffer-mode-map "k" 'ibuffer-do-kill-lines)
(evil-define-key 'normal 'ibuffer-mode-map "l" 'ibuffer-redisplay)
(evil-define-key 'normal 'ibuffer-mode-map "m" 'ibuffer-mark-forward)
(evil-define-key 'normal 'ibuffer-mode-map "n" 'ibuffer-forward-line)
(evil-define-key 'normal 'ibuffer-mode-map "o" 'ibuffer-visit-buffer-other-window)
(evil-define-key 'normal 'ibuffer-mode-map "p" 'ibuffer-backward-line)
(evil-define-key 'normal 'ibuffer-mode-map "t" 'ibuffer-toggle-marks)
(evil-define-key 'normal 'ibuffer-mode-map "u" 'ibuffer-unmark-forward)
(evil-define-key 'normal 'ibuffer-mode-map "v" 'ibuffer-do-view)
(evil-define-key 'normal 'ibuffer-mode-map "w" 'ibuffer-copy-filename-as-kill)
(evil-define-key 'normal 'ibuffer-mode-map "x" 'ibuffer-do-kill-on-deletion-marks)

(provide 'init-evil-keybindings)
;;; init-evil-keybindings.el ends here
