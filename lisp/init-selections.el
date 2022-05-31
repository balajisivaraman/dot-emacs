;;; init-selections.el --- Loads Selectrum and assorted packages -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

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

;; This module loads selectrum and assorted packages and has functions to help with them.

;;; Code:

(use-package vertico
  :init
  (vertico-mode t))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package consult
  :bind
  (("C-c f b"  . consult-bookmark)
   ("C-x b"  . consult-buffer)
   ("M-y"    . consult-yank-pop)
   ("M-g g"    . consult-goto-line)
   ("M-g M-g"    . consult-goto-line)
   ("C-j t". consult-imenu))
  :config
  (consult-customize
   consult-buffer consult-ripgrep consult-bookmark
   consult-recent-file
   :preview-key (kbd "M-.")))

(use-package marginalia
  :config
  (marginalia-mode t))

(use-package embark
  :bind
  (("C-h B" . embark-bindings)
   ("C-." . embark-act))
  (:map minibuffer-local-map
   ("C-." . embark-act))
  :config
  (setq embark-action-indicator
      (lambda (map &optional _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator))

(provide 'init-selections)
;;; init-selections.el ends here
