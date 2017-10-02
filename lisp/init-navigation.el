;;; init-navigation.el --- Emacs navigation made much easier. -*- lexical-binding: t -*-

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

;; This module loads Avy Mode, Ace Window Mode, Swiper, and other modes to make Emacs navigation a breeze.

;;; Code:

(use-package avy
  :bind
  (("C-c j j" . avy-goto-char)
   ("C-c j w" . avy-goto-word-1)
   ("C-c j b" . avy-pop-mark)
   ("C-c j l" . avy-goto-line)))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package window-numbering
  :config
  (setq window-numbering-auto-assign-0-to-minibuffer nil)
  (window-numbering-mode 1))

(use-package golden-ratio
  :init
  (defun balaji-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . balaji-toggle-golden-ratio))
  :diminish (golden-ratio-mode . "ⓖ")
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
  :bind (("C-x ]" . balaji-pages/forward-page)
         ("C-x [" . balaji-pages/backward-page))
  :init
  (defhydra balaji-pages ()
    "Pages"
    ("[" backward-page "backward")
    ("]" forward-page "forward")
    ("n" narrow-to-page "narrow" :exit t)
    ("q" nil "quit" :exit t)))

(use-package outline
  :ensure nil
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(defhydra balaji-outline (:color pink :hint nil)
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

(global-set-key (kbd "C-c O") 'balaji-outline/body) ; by example

(use-package iy-go-to-char
  :disabled
  :chords (("fg" . iy-go-to-char)
           ("fd" . iy-go-to-char-backward)))

(use-package beginend
  :diminish ((beginend-global-mode . "")
             (beginend-prog-mode . ""))
  :init
  (beginend-global-mode))

(provide 'init-navigation)
;;; init-navigation.el ends here
