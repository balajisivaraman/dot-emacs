;;; init-user-interface.el --- Better themes, modeline and usability for Emacs. -*- lexical-binding: t -*-

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

;; Loads Gruvbox Theme, removes extraneous user-interface elements and makes Emacs more usable.

;;; Code:

(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-dark-hard t)
  :config
  (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
  (add-to-list 'default-frame-alist '(alpha . (98 . 98))))

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
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-mode t)
  (setq dimmer-fraction 0.5)
  (add-to-list 'dimmer-buffer-exclusion-regexps "^magit.*"))

(use-package rainbow-delimiters
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 40))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

(use-package beacon
  :custom
  (beacon-color "#fb4934")
  (beacon-push-mark 10)
  (beacon-blink-delay 0.3)
  (beacon-blink-duration 0.3)
  :config
  (beacon-mode))

(provide 'init-user-interface)
;;; init-user-interface.el ends here
