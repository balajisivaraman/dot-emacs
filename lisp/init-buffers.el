;;; init-buffers.el --- Buffers, windows and frames. -*- lexical-binding: t -*-

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

;; Add `ibuffer' and `persistent-scratch' packages.

;;; Code:

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package persistent-scratch
  :custom
  (persistent-scratch-save-file (concat bs/emacs-cache-directory ".persistent-scratch"))
  :config
  (persistent-scratch-setup-default))

(provide 'init-buffers)
;;; init-buffers.el ends here
