;;; -*- lexical-binding: t -*-
;;; init-editing.el --- Modules necessary for editing in Emacs, irrespective of language used.

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

;; This module loads packages that make text editing in Emacs such a joy!

;;; Code:

(require 'init-package)

;; Browse Kill Ring
(use-package browse-kill-ring
  :defer 10
  :commands browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

;;;; Undo Tree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (("C-S-z" . undo-tree-redo)
   ("C-z" . undo-tree-undo))
  :config
  (global-undo-tree-mode 1))

;;;; Flyspell Mode
(use-package flyspell
  :ensure nil
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init
  (use-package ispell
    :ensure nil
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)))
  :config
  (unbind-key "C-." flyspell-mode-map))

;;;; Rectangle Editing
(bind-key "C-x r i" 'string-insert-rectangle)

;;;; Expand Region and Change Inner
(use-package expand-region
  :bind
  (("C-c C-e" . er/expand-region)
   ("C-c C-c" . er/contract-region)))

(use-package change-inner
  :bind
  (("C-c C-i" . change-inner)
   ("C-c C-o" . change-outer)))

;;;; Multiple Cursors Code
(use-package multiple-cursors
  :bind
  ("C-C C-C" . mc/edit-lines))

;;;; Visual Regexp
(use-package visual-regexp
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

;;; Enable disabled commands
(put 'downcase-region             'disabled nil)   ; Let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page              'disabled nil)   ; Let narrowing work
(put 'narrow-to-region            'disabled nil)   ; Let narrowing work
(put 'set-goal-column             'disabled nil)
(put 'upcase-region               'disabled nil)   ; Let upcasing work

(provide 'init-editing)
;;; init-editing.el ends here
