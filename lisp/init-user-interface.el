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

(use-package modus-themes
  :init
  (setq
   modus-themes-syntax '(alt-syntax yellow-comments)
   modus-themes-fringes 'subtle
   modus-themes-headings '((t . (rainbow no-bold)))
   modus-themes-mixed-fonts t
   modus-themes-org-agenda ; this is an alist: read the manual or its doc string
      '((header-block . (variable-pitch 1.3))
        (header-date . (workaholic bold-today 1.1))
        (event . (accented varied))
        (scheduled . rainbow)
        (habit . traffic-light)))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  (set-frame-parameter (selected-frame) 'alpha '(96 . 96))
  (add-to-list 'default-frame-alist '(alpha . (96 . 96))))

(use-package nyan-mode
  :init (nyan-mode))

(use-package restart-emacs
  :commands (restart-emacs save-buffers-kill-emacs)
  :bind
  ("C-c q r" . restart-emacs)
  ("C-c q q" . save-buffers-kill-emacs))

(use-package face-remap
  :ensure nil
  :diminish (buffer-face-mode)
  :bind
  (("C-+" . bs/font-scaling/text-scale-increase)
   ("C--" . bs/font-scaling/text-scale-decrease))
  :init
  (defhydra bs/font-scaling ()
    "Font scaling"
    ("+" text-scale-increase "Scale Up")
    ("-" text-scale-decrease "Scale Down")
    ("q" nil "Quit" :exit t )))

(use-package rainbow-delimiters
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 35))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

(provide 'init-user-interface)
;;; init-user-interface.el ends here
