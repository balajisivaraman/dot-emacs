;;; init-erlang.el --- Configure Erlang and Elixir mode -*- lexical-binding: t -*-

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

;; This module configures Erlang and Elixir mode

;;; Code:

(use-package erlang
  :commands erlang-mode)

(use-package company-distel
  :after distel-completion-lib
  :init
  (add-to-list 'company-backends 'company-distel))

(defun balaji-erlang-mode-hook ()
    "Elixir mode hooks."
  (company-mode t))

(add-hook 'erlang-mode-hook 'balaji-erlang-mode-hook)

(use-package elixir-mode
  :commands elixir-mode)

(use-package alchemist
  :after elixir-mode)

(defun balaji-elixir-mode-hook ()
    "Elixir mode hooks."
  (company-mode t)
  (alchemist-mode t))

(add-hook 'elixir-mode-hook 'balaji-elixir-mode-hook)

(use-package elixir-yasnippets
  :after elixir-mode)

(use-package flycheck-elixir
  :after elixir-mode)

(provide 'init-erlang)
;;; init-erlang.el ends here
