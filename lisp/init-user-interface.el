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

(use-package restart-emacs
  :commands (restart-emacs save-buffers-kill-emacs)
  :init
  (defun bs/close-emacs ()
    (interactive)
    "Quit Emacs or Delete Frame depending on where we're running
      Emacs."
    (if bs/at-work
        (save-buffers-kill-emacs)
      (delete-frame)))
  (bs/general-bindings
   "qr" 'restart-emacs
   "qq" 'bs/close-emacs))

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

(provide 'init-user-interface)
;;; init-user-interface.el ends here
