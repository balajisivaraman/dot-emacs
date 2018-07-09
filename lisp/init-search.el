;;; init-search.el --- Better tools for search/replace. -*- lexical-binding: t -*-

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

;; This module loads ISearch, Helm Swoop, Visual Regexp and Ag.

;;; Code:

(use-package "isearch"
  :defer t
  :ensure nil
  :bind (("C-s"   . isearch-forward-regexp)
         ("C-r"   . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward))
  :init
  (diminish 'isearch-mode))

(use-package visual-regexp
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package ripgrep)

(provide 'init-search)
;;; init-search.el ends here
