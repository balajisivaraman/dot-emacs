;;; init-lib.el --- Essential Emacs Lisp Libraries. -*- lexical-binding: t -*-

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

;; Load essential ELisp libraries like `s-mode', `dash' and `f-mode'.

;;; Code:

(use-package s         :load-path "lib/s-el"         :ensure nil)
(use-package dash      :load-path "lib/dash-el"      :ensure nil)
(use-package f         :load-path "lib/f-el"         :ensure nil)

(provide 'init-lib)
;;; init-lib.el ends here
