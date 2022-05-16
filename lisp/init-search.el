;;; init-search.el --- Better tools for search/replace. -*- lexical-binding: t -*-

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

;; This module loads ISearch, Helm Swoop, Visual Regexp and Ag.

;;; Code:

(use-package visual-regexp
  :disabled t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package deadgrep
  :commands (deadgrep)
  :bind (("M-s r" . deadgrep)))

(use-package rg
  :commands (rg rg-literal)
  :init
  (rg-enable-default-bindings)
  :bind (("M-s s" . rg)))

(use-package wgrep
  :commands (wgrep))

(provide 'init-search)
;;; init-search.el ends here
