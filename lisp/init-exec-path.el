;;; init-exec-path.el --- Set the executable search path from the user shell. -*- lexical-binding: t -*-

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

;; To ensure availability of command line tools, we try to get Emacs's
;; executable path from your shell configuration using the
;; `exec-path-from-shell' package.

;;; Code:

(use-package exec-path-from-shell
  :init
  (setenv "SHELL" "/bin/bash")
  (when (not (string-equal system-type "windows-nt"))
    (exec-path-from-shell-initialize)))

(use-package eshell
  :ensure nil
  :config
  (setq
   eshell-directory-name (concat bs/emacs-cache-directory "eshell")))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
