;;; init-basic-editing.el --- Essential tools for working with anything. -*- lexical-binding: t -*-

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

;; Loads lots of essential packages to make editing any type of text in Emacs easier.

;;; Code:

;; Undo Tree
(use-package undo-tree
  :bind
  (("C-S-z" . undo-tree-redo)
   ("C-z" . undo-tree-undo))
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,(concat bs/emacs-cache-directory "undo-tree"))))
  :config
  (global-undo-tree-mode 1))

;; Rectangle Editing
(use-package rect
  :straight nil
  :bind
  (("C-x r i" . string-insert-rectangle)
   ("C-x r r" . replace-rectangle)))

;; Expand Region and Change Inner
(use-package expand-region
  :bind
  (("C-c C-e" . er/expand-region)
   ("C-c C-c" . er/contract-region)))

(use-package change-inner
  :bind
  (("C-c <C-i>" . change-inner)
   ("C-c C-o" . change-outer)))

(use-package selected
  :demand t
  :custom
  (selected-ignore-modes '(magit-status-mode))
  :bind (:map selected-keymap
              ("[" . align-code)
              ("f" . fill-region)
              ("U" . unfill-region)
              ("d" . downcase-region)
              ("u" . upcase-region)
              ("r" . reverse-region)
              ("s" . sort-lines)
              ("q" . selected-off))
  :config
  (selected-global-mode 1))

;; Multiple Cursors Code
(use-package multiple-cursors
  :bind (("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> `"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region)
         ("<C-m> s"     . set-rectangular-region-anchor)
         ("<C-m> %"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-x"   . reactivate-mark)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align))
  :bind (:map selected-keymap
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("C->" . mc/unmark-next-like-this)
              ("C-." . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              ("C-," . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)))

;; Easy killing and marking on C-w
(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

;; Enable disabled commands
(put 'downcase-region             'disabled nil)   ; Let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page              'disabled nil)   ; Let narrowing work
(put 'narrow-to-region            'disabled nil)   ; Let narrowing work
(put 'set-goal-column             'disabled nil)
(put 'upcase-region               'disabled nil)   ; Let upcasing work

(use-package align
  :bind (("C-c x a" . align)
         ("C-c x c" . align-current)
         ("C-c x r" . align-regexp)))

(use-package embrace
  :bind (("C-c y" . bs/embrace/body)
         ("C-c x e" . bs/embrace/body))
  :config
  (defhydra bs/embrace (:hint nil)
    "
    Add (_a_), change (_c_) or delete (_d_) a pair.  Quit with _q_.
    "
    ("a" embrace-add)
    ("c" embrace-change)
    ("d" embrace-delete)
    ("q" nil)))

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c f r" . crux-rename-buffer-and-file)))

(use-package iedit
  :bind (("C-;" . iedit-mode)))

(use-package shift-number
  :bind
  (("C-c +" . shift-number-up)
   ("C-c -" . shift-number-down)))

(provide 'init-basic-editing)
;;; init-basic-editing.el ends here
