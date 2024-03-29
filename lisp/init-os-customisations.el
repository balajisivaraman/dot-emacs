;;; init-os-customisations.el --- Customizations for Linux -*- lexical-binding: t -*-

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

;; Customizations specific to Linux

;;; Code:

(use-package pinentry
  :if (string-equal system-type "gnu/linux")
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(use-package vterm
  :if (not (string-equal system-type "windows-nt"))
  :bind
  ("M-T" . bs/scratch-vterm-buffer)
  :config
  (defun bs/scratch-vterm-buffer ()
  "Open a *scratch* vterm buffer for misc operations."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((vterm-buffer-name "*scratch-term*")
         (default-directory "~"))
    (vterm))))

(provide 'init-os-customisations)
;;; init-os-customisations.el ends here
