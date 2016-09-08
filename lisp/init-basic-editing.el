;;; init-basic-editing.el --- Essential tools for working with anything. -*- lexical-binding: t -*-

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

;; Loads lots of essential packages to make editing any type of text in Emacs easier.

;;; Code:

;; Browse Kill Ring
(use-package browse-kill-ring
  :defer 10
  :commands browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

;; Undo Tree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (("C-S-z" . undo-tree-redo)
   ("C-z" . undo-tree-undo))
  :config
  (global-undo-tree-mode 1))

;; Rectangle Editing
(bind-key "C-x r i" 'string-insert-rectangle)

;; Expand Region and Change Inner
(use-package expand-region
  :bind
  (("C-c C-e" . er/expand-region)
   ("C-c C-c" . er/contract-region)))

(use-package change-inner
  :bind
  (("C-c C-i" . change-inner)
   ("C-c C-o" . change-outer)))

;; Multiple Cursors Code
(use-package multiple-cursors
  :bind
  (("C-c c <SPC>" . mc/vertical-align-with-space)
   ("C-c c a"     . mc/vertical-align)
   ("C-c c e"     . mc/mark-more-like-this-extended)
   ("C-c c h"     . mc/mark-all-like-this-dwim)
   ("C-c c l"     . mc/edit-lines)
   ("C-c c n"     . mc/mark-next-like-this)
   ("C-c c p"     . mc/mark-previous-like-this)
   ("C-c c r"     . vr/mc-mark)
   ("C-c c C-a"   . mc/edit-beginnings-of-lines)
   ("C-c c C-e"   . mc/edit-ends-of-lines)
   ("C-c c C-s" . mc/mark-all-in-region)))

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
  :bind (("C-c y" . balaji-embrace/body)
         ("C-c x e" . balaji-embrace/body))
  :init
  (defhydra balaji-embrace (:hint nil)
    "
    Add (_a_), change (_c_) or delete (_d_) a pair.  Quit with _q_.
    "
    ("a" embrace-add)
    ("c" embrace-change)
    ("d" embrace-delete)
    ("q" nil)))

(provide 'init-basic-editing)
;;; init-basic-editing.el ends here
