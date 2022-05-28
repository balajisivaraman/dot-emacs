;;; init-snippets.el --- Yasnippet, Custom Snippets and other stuff! -*- lexical-binding: t -*-

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

;; My custom configuration for the Yasnippet minor mode.

;;; Code:

(use-package yasnippet
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (prog-mode . yas-minor-mode)
  :bind ("C-<tab>" . yas-expand)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-snippets)
;;; init-snippets.el ends here
