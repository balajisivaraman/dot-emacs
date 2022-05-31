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

(defvar jump-map)
(define-prefix-command 'jump-map)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'jump-map)

(use-package avy
  :custom
  (avy-timeout-seconds 0.2)
  :bind
  ("C-j j". avy-goto-char-timer))

(use-package ctrlf
  :config
  (ctrlf-mode t))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package outline
  :straight nil
  :defer t
  :bind (("C-c O" . bs/outline/body))
  :config
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
    ("q" hide-sublevels)  ; Hide everything but the top-level headings
    ("t" hide-body)    ; Hide everything but headings (all body lines)
    ("o" hide-other)   ; Hide other branches
    ("c" hide-entry)   ; Hide this entry's body
    ("l" hide-leaves)  ; Hide body lines in this entry and sub-entries
    ("d" hide-subtree) ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" show-all)    ; Show (expand) everything
    ("e" show-entry)  ; Show this heading's body
    ("i" show-children) ; Show this heading's immediate child sub-headings
    ("k" show-branches) ; Show all sub-headings under this heading
    ("s" show-subtree) ; Show (expand) everything in this heading & below
    ;; Move
    ("u" outline-up-heading)             ; Up
    ("n" outline-next-visible-heading)   ; Next
    ("p" outline-previous-visible-heading) ; Previous
    ("f" outline-forward-same-level)       ; Forward - same level
    ("b" outline-backward-same-level)      ; Backward - same level
    ("z" nil "leave")))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package bookmark
  :straight nil
  :bind
  (("C-c B l" . bookmark-bmenu-list)
   ("C-c B s" . bookmark-set)
   ("C-j b"   . bookmark-jump))
  :custom
  (bookmark-file (concat bs/emacs-cache-directory "bookmarks")))

(provide 'init-navigation)
;;; init-navigation.el ends here
