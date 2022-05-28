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
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after (tree-sitter))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
