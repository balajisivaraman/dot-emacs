;;; init-navigation.el --- Emacs navigation made much easier. -*- lexical-binding: t -*-

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

(provide 'init-navigation)
;;; init-navigation.el ends here
