;;; -*- lexical-binding: t -*-
;;; init-defaults.el --- Settings Emacs should come out of the box with.

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

;; This file contains settings that should really be default in Emacs.

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

(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      visible-bell t
      load-prefer-newer t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(use-package iso-transl
  :ensure nil)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Below functions are taken from Bodil Stokke's ohai-emacs.
;; It can be found here: https://github.com/bodil/ohai-emacs/blob/master/ohai/ohai-lib.el
(defun balaji/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (s-trim (shell-command-to-string command)))

(defun balaji/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun balaji/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun balaji/exec-if-exec (command args)
  "If `command' satisfies `balaji/is-exec', run it with `args' and return its
output as per `balaji/exec'. Otherwise, return nil."
  (when (balaji/is-exec command) (balaji/exec (s-concat command " " args))))

(defun balaji/getent (user)
  "Get the /etc/passwd entry for the user `user' as a list of strings,
or nil if there is no such user. Empty fields will be represented as nil,
as opposed to empty strings."
  (-let [ent (balaji/exec (s-concat "getent passwd " user))]
    (when (not (s-blank? ent))
      (-map (lambda (i) (if (s-blank? i) nil i))
            (s-split ":" ent)))))

(defun balaji/user-full-name ()
  "Guess the user's full name. Returns nil if no likely name could be found."
  (or (balaji/exec-if-exec "git" "config --get user.name")
      (elt (balaji/getent (getenv "USER")) 4)))
(setq user-full-name (balaji/user-full-name))

(defun balaji/user-email ()
  "Guess the user's email address. Returns nil if none could be found."
  (or (balaji/exec-if-exec "git" "config --get user.email")
      (getenv "EMAIL")))
(setq user-mail-address (balaji/user-email))

(provide 'init-defaults)
