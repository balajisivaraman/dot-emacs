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

;; Loads Modus Operandi Theme, removes extraneous user-interface elements and makes Emacs more usable.

;;; Code:

(use-package modus-operandi-theme
  :init
  (setq
   modus-themes-syntax 'alt-syntax-yellow-comments
   modus-themes-fringes 'subtle
   modus-themes-headings '((t . no-bold))
   modus-themes-scale-headings t
   modus-themes-scale-1 1.15
   modus-themes-scale-2 1.3
   modus-themes-scale-3 1.45
   modus-themes-scale-4 1.6
   modus-themes-scale-5 1.75)
  :config
  (load-theme 'modus-operandi t))

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
