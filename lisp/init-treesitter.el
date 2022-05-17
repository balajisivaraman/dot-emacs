;;; init-treesitter.el --- Loads treesitter and assorted configs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

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

;; Loads treesitter for better syntax highlighting

;;; Code:

(use-package tree-sitter
  :hook ((tree-sitter-mode . tree-sitter-hl-mode))
  :diminish (tree-sitter-mode)
  :init
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)

;; Taken from here: https://github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el#L2213
(use-package evil-textobj-tree-sitter
    :defer 1
    :after (evil tree-sitter)
    :config
    (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
                                                                                      '((python-mode . [(import_statement) @import])
                                                                                        (go-mode . [(import_spec) @import])
                                                                                        (rust-mode . [(use_declaration) @import]))))
    (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
    (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
    (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
    (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
    (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
    (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
    (define-key evil-outer-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
    (define-key evil-inner-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
    (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
    (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
    (define-key evil-inner-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
    (define-key evil-outer-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
    (defun bs/goto-and-recenter (group &optional previous end query)
      (interactive)
      (evil-textobj-tree-sitter-goto-textobj group previous end query)
      (recenter 7))
    (define-key evil-normal-state-map (kbd "]r") (lambda () (interactive) (bs/goto-and-recenter "parameter.inner")))
    (define-key evil-normal-state-map (kbd "[r") (lambda () (interactive) (bs/goto-and-recenter "parameter.inner" t)))
    (define-key evil-normal-state-map (kbd "]R") (lambda () (interactive) (bs/goto-and-recenter "parameter.inner" nil t)))
    (define-key evil-normal-state-map (kbd "[R") (lambda () (interactive) (bs/goto-and-recenter "parameter.inner" t t)))
    (define-key evil-normal-state-map (kbd "]a") (lambda () (interactive) (bs/goto-and-recenter "conditional.outer")))
    (define-key evil-normal-state-map (kbd "[a") (lambda () (interactive) (bs/goto-and-recenter "conditional.outer" t)))
    (define-key evil-normal-state-map (kbd "]A") (lambda () (interactive) (bs/goto-and-recenter "conditional.outer" nil t)))
    (define-key evil-normal-state-map (kbd "[A") (lambda () (interactive) (bs/goto-and-recenter "conditional.outer" t t)))
    (define-key evil-normal-state-map (kbd "]c") (lambda () (interactive) (bs/goto-and-recenter "class.outer")))
    (define-key evil-normal-state-map (kbd "[c") (lambda () (interactive) (bs/goto-and-recenter "class.outer" t)))
    (define-key evil-normal-state-map (kbd "]C") (lambda () (interactive) (bs/goto-and-recenter "class.outer" nil t)))
    (define-key evil-normal-state-map (kbd "[C") (lambda () (interactive) (bs/goto-and-recenter "class.outer" t t)))
    (define-key evil-normal-state-map (kbd "]f") (lambda () (interactive) (bs/goto-and-recenter "function.outer")))
    (define-key evil-normal-state-map (kbd "[f") (lambda () (interactive) (bs/goto-and-recenter "function.outer" t)))
    (define-key evil-normal-state-map (kbd "]F") (lambda () (interactive) (bs/goto-and-recenter "function.outer" nil t)))
    (define-key evil-normal-state-map (kbd "[F") (lambda () (interactive) (bs/goto-and-recenter "function.outer" t t))))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
