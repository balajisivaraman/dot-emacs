;;; init-user-interface.el --- Better themes, modeline and usability for Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2020  Balaji Sivaraman

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

;; Loads One Dark Theme, removes extraneous user-interface elements and makes Emacs more usable.

;;; Code:

(defvar balaji/one-themes-path "~/code/emacs-one-themes/")
(use-package one-themes
  :load-path balaji/one-themes-path
  :init
  (add-to-list 'custom-theme-load-path balaji/one-themes-path)
  (load-theme 'one-dark t)
  :config
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))
(use-package nyan-mode
  :init (nyan-mode))

(use-package which-func
  :init (which-function-mode)
  :config
  (setq
   which-func-unknown "⊥"))

(use-package restart-emacs
  :bind
  ("C-c q r" . restart-emacs)
  ("C-c q q" . save-buffers-kill-emacs))

(use-package face-remap
  :ensure nil
  :bind
  (("C-+" . balaji-font-scaling/text-scale-increase)
   ("C--" . balaji-font-scaling/text-scale-decrease))
  :init
  (defhydra balaji-font-scaling ()
    "Font scaling"
    ("+" text-scale-increase "Scale Up")
    ("-" text-scale-decrease "Scale Down")
    ("q" nil "Quit" :exit t )))

(use-package page-break-lines
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-mode t)
  (setq dimmer-fraction 0.5))

(provide 'init-user-interface)
;;; init-user-interface.el ends here
