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
  :diminish undo-tree-mode
  :bind
  (("C-S-z" . undo-tree-redo)
   ("C-z" . undo-tree-undo))
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist `(("." . ,(concat bs/emacs-cache-directory "undo-tree")))))

;; Rectangle Editing
(bind-key "C-x r i" 'string-insert-rectangle)

;; Expand Region and Change Inner
(use-package expand-region
  :bind
  (("C-c C-e" . er/expand-region)
   ("C-c C-c" . er/contract-region))
  :init
  (bs/general-bindings
   "ce" 'er/expand-region
   "cc" 'er/contract-region))

(use-package change-inner
  :bind
  (("C-c C-i" . change-inner)
   ("C-c C-o" . change-outer)))

;; Multiple Cursors Code
(use-package multiple-cursors
  :bind
  (
   ("C-c c <SPC>" . mc/vertical-align-with-space)
   ("C-c c a"     . mc/vertical-align)
   ("C-c c e"     . mc/mark-more-like-this-extended)
   ("C-c c h"     . mc/mark-all-like-this-dwim)
   ("C-c c l"     . mc/edit-lines)
   ("C-c c n"     . bs/multiple-cursors/body)
   ("C-c c r"     . vr/mc-mark)
   ("C-c c C-a"   . mc/edit-beginnings-of-lines)
   ("C-c c C-e"   . mc/edit-ends-of-lines)
   ("C-c c C-s"   . mc/mark-all-in-region))
  :init
  (defhydra bs/multiple-cursors ()
    "Multiple cursors."
    ("n" mc/mark-next-like-this "mark")
    ("N" mc/unmark-next-like-this "unmark")
    ("q" nil "Quit" :exit t )))

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
  :init
  (defhydra bs/embrace (:hint nil)
    "
    Add (_a_), change (_c_) or delete (_d_) a pair.  Quit with _q_.
    "
    ("a" embrace-add)
    ("c" embrace-change)
    ("d" embrace-delete)
    ("q" nil)))

(use-package crux
  :commands (crux-duplicate-current-line-or-region crux-delete-file-and-buffer crux-visit-term-buffer crux-rename-buffer-and-file)
  :bind
  (("C-a" . crux-move-beginning-of-line))
  :init
  (bs/general-bindings
   "d" 'crux-duplicate-current-line-or-region
   "D" 'crux-delete-file-and-buffer
   "T" 'crux-visit-term-buffer
   "fr" 'crux-rename-buffer-and-file))

(use-package iedit)

(provide 'init-basic-editing)
;;; init-basic-editing.el ends here
