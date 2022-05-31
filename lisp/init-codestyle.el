;;; -*- lexical-binding: t -*-
;;; init-codestyle.el --- Language-agnostic sane default code style settings.

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

;; This file loads any custom code style (indentation, whitespace
;; etc.) settings I like to use in all languages.

;;; Code:

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-font-lock-mode t)
(setq-default js-indent-level 2)
(setq mode-require-final-newline nil
      require-final-newline nil)

(use-package whitespace
  :straight nil
  :custom
  (whitespace-style '(face trailing space-before-tab empty missing-newline-at-eof))
  :hook
  ((text-mode . whitespace-mode)
   (prog-mode . whitespace-mode)))

(use-package whitespace-cleanup-mode
  :bind
  (("C-c f c" . whitespace-cleanup))
  :config
  (global-whitespace-cleanup-mode t))

(provide 'init-codestyle)
;;; init-codestyle.el ends here
