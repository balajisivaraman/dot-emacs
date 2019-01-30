;;; init-markup-languages.el --- Markdown and Yaml. -*- lexical-binding: t -*-

;; Copyright (C) 2019  Balaji Sivaraman

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

;; Add Markdown and YAML editing capabilities.

;;; Code:

(use-package markdown-mode
  :mode
  ".md\\|.markdown"
  :bind (:map markdown-mode-map
              ("C-c i l" . markdown-insert-link)
              ("C-c i b" . markdown-insert-bold)
              ("C-c i i" . markdown-insert-italic))
  :init
  (add-hook 'markdown-mode-hook #'flycheck-mode))

(use-package yaml-mode
  :mode
  "\\(\\.yml\\|\\.yaml\\)")

(use-package toml-mode
  :mode
  "\\.toml")

(provide 'init-markup-languages)
;;; init-markup-languages.el ends here
