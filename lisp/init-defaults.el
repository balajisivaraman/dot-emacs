;;; init-defaults.el --- Settings/packages that should ideally be default in Emacs -*- lexical-binding: t -*-

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

;; Everything in this file should ideally be as is when Emacs is loaded, but unfortunately aren't.

;;; Code:

;; Hide the menu bar, tool bar and scroll bar
;; For the menu bar, don't hide it in OSX, since the top bar is always visible anyway
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Don't display the default start up message
;; If we're loading from a saved desktop, this setting becomes redundant.
(setq inhibit-startup-message t)
;; Highlight the current line on all buffers
(global-hl-line-mode t)
;; Who ever wants to fully type out YES and NO
(fset 'yes-or-no-p 'y-or-n-p)
;; This will help us open up buffers without confirmation.
(setq confirm-nonexistent-file-or-buffer nil)
;; Setting word wrap mode as default
(global-visual-line-mode 1)
(diminish 'visual-line-mode)
;; Highlight matching parens
(show-paren-mode t)
;; Show column number in bar
(column-number-mode t)
;; Typed text will replace the selection as in most modern editors
(delete-selection-mode t)
;; Display line numbers to the left of the buffer
(global-linum-mode t)
;; When I grew up, sentences always ended with a single space.
(setq sentence-end-double-space nil)
;; Prettify Symbols Mode in Emacs > 24.4 is awesome.
(setq-default prettify-symbols-mode t)
(global-prettify-symbols-mode t)
;; Electric Pair Mode
(electric-pair-mode t)
(blink-cursor-mode -1)

(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      visible-bell t
      load-prefer-newer t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(use-package iso-transl :ensure nil)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

(provide 'init-defaults)
;;; init-defaults.el ends here
