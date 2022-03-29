;;; init-navigation.el --- Emacs navigation made much easier. -*- lexical-binding: t -*-

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

;; This module loads Avy Mode, Ace Window Mode, Swiper, and other modes to make Emacs navigation a breeze.

;;; Code:

(use-package avy
  :demand t
  :commands (avy-goto-char avy-goto-word-1 avy-pop-mark avy-goto-line)
  :init
  (bs/general-bindings
   "jc" 'avy-goto-char
   "jj" 'avy-goto-char
   "jl" 'avy-goto-line
   "jw" 'avy-goto-word-1
   "jt" 'consult-imenu
   )
  :config
  (defun bs/avy-goto-word-2 (char1 char2 &optional arg beg end symbol)
    "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched."
    (interactive (list (let ((c1 (read-char "char 1: " t)))
                         (if (memq c1 '(? ?\b))
                             (keyboard-quit)
                           c1))
                       (let ((c2 (read-char "char 2: " t)))
                         (cond ((eq c2 ?)
                                (keyboard-quit))
                               ((memq c2 avy-del-last-char-by)
                                (keyboard-escape-quit)
                                (call-interactively 'avy-goto-char-2))
                               (t
                                c2)))
                       current-prefix-arg
                       nil nil))
    (when (eq char1 ?)
      (setq char1 ?\n))
    (when (eq char2 ?)
      (setq char2 ?\n))
    (avy-with bs/avy-goto-word-2
      (let* ((str (string char1 char2))
             (regex (cond ((string= str ".")
                           "\\.")
                          ((and avy-word-punc-regexp
                                (string-match avy-word-punc-regexp str))
                           (regexp-quote str))
                          ((and (<= char1 26) (<= char2 26))
                           str)
                          (t
                           (concat
                            (if symbol "\\_<" "\\b")
                            str)))))
        (avy-jump regex
                  :window-flip arg
                  :beg beg
                  :end end))))

  (defun bs/avy-goto-word-2-above (char1 char2 &optional arg)
    "Jump to the currently visible CHAR1 followed by CHAR2 at a word
start. This is a scoped version of `bs/avy-goto-word-2', where the scope
is the visible part of the current buffer up to point. The window
scope is determined by `avy-all-windows'. When ARG is non-nil, do the
opposite of `avy-all-windows'."
    (interactive (list (read-char "char 1: " t)
                       (read-char "char 2: " t)
                       current-prefix-arg))
    (avy-with bs/avy-goto-word-2-above
      (bs/avy-goto-word-2 char1 char2 arg (window-start) (point))))

  (defun bs/avy-goto-word-2-below (char1 char2 &optional arg)
    "Jump to the currently visible CHAR1 followed by CHAR2. This is a
scoped version of `bs/avy-goto-word-2', where the scope is the visible
part of the current buffer following point. The window scope is
determined by `avy-all-windows'. When ARG is non-nil, do the opposite
of `avy-all-windows'."
    (interactive (list (read-char "char 1: " t)
                       (read-char "char 2: " t)
                       current-prefix-arg))
    (avy-with bs/avy-goto-word-2-below
      (bs/avy-goto-word-2
       char1 char2 arg
       (point) (window-end (selected-window) t))))

  (evil-define-avy-motion bs/avy-goto-word-2-below inclusive)
  (evil-define-avy-motion bs/avy-goto-word-2-above inclusive)

  (evil-define-key 'normal text-mode-map
    "s" 'evil-bs/avy-goto-word-2-below
    "S" 'evil-bs/avy-goto-word-2-above)
  (evil-define-key 'normal prog-mode-map
    "s" 'evil-bs/avy-goto-word-2-below
    "S" 'evil-bs/avy-goto-word-2-above))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package golden-ratio
  :init
  (defun bs/toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :diminish (golden-ratio-mode . " ⓖ")
  :init
  (bs/general-bindings
   "tg" 'bs/toggle-golden-ratio)
  :config
  (setq
   golden-ratio-extra-commands '(windmove-up
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 select-window-0
                                 select-window-1
                                 select-window-2
                                 select-window-3
                                 select-window-4
                                 select-window-5
                                 select-window-6
                                 select-window-7
                                 select-window-8
                                 select-window-9
                                 ace-window
                                 ace-delete-window
                                 ace-select-window
                                 ace-swap-window
                                 ace-maximize-window)
   golden-ratio-auto-scale nil
   golden-ratio-exclude-modes '(flycheck-error-list-mode
                                calc-mode
                                ediff-mode
                                eshell-mode
                                dired-mode)

   split-width-threshold nil
   golden-ratio-exclude-buffer-regexp
   `(,(rx bos "*" (any "h" "H") "elm*" eos)
     ,(rx bos "*which-key*" eos)
     ,(rx bos "*NeoTree*" eos))))

(use-package page                       ; Page navigation
  :ensure nil
  :bind (("C-x ]" . bs/pages/forward-page)
         ("C-x [" . bs/pages/backward-page))
  :init
  (defhydra bs/pages ()
    "Pages"
    ("[" backward-page "backward")
    ("]" forward-page "forward")
    ("n" narrow-to-page "narrow" :exit t)
    ("q" nil "quit" :exit t)))

(use-package outline
  :ensure nil
  :defer t
  :diminish outline-minor-mode)

(defhydra bs/outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave"))

(bs/general-bindings
 "O" 'bs/outline/body)

(use-package beginend
  :diminish
  (beginend-global-mode . "")
  :init
  (beginend-global-mode)
  (-each (-distinct (-map
                     (lambda (item) (cdr item))
                     beginend-modes))
    (lambda (item) (diminish item ""))))

(use-package bookmark
  :ensure nil
  :commands (bookmark-jump bookmark-set bookmark-bmenu-list)
  :init
  (bs/general-bindings
   "fb" 'bookmark-jump
   "Bs" 'bookmark-set
   "Bl" 'bookmark-bmenu-list)
  :config
  (setq bookmark-file (concat bs/emacs-cache-directory "bookmarks")))

(provide 'init-navigation)
;;; init-navigation.el ends here
