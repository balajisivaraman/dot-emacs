;;; init-corfu.el --- Sets up auto-completion using Corfu mode. -*- lexical-binding: t -*-

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

;; This module loads Corfu mode for providing auto-completion capabilities to Emacs.

;;; Code:

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current nil)
  (corfu-preselect-first t)
  (corfu-on-exact-match nil)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5)
  (tab-always-indent 'complete))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))

(use-package corfu-quick
  :after (corfu)
  :ensure nil
  :bind (:map corfu-map
         ("\M-q" . corfu-quick-complete)
         ("\C-q" . corfu-quick-insert)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'init-corfu)
;;; init-corfu.el ends here
