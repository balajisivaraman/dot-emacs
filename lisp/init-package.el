;;; init-package.el --- Initialize straight.el and use-package. -*- lexical-binding: t -*-

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

;; This file contains initialization code for straight.el and use-package.

;;; Code:

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))
;; Always install packages from Melpa, Elpa
;; Over-ridden when not used by setting (:ensure nil) in use-package declarations
(setq straight-use-package-by-default t)
(setq use-package-verbose t)
(require 'bind-key)

(bind-key "M-P u" 'straight-pull-all)
(bind-key "M-P c" 'straight-remove-unused-repos)

(provide 'init-package)
;;; init-package.el ends here
