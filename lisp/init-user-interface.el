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
   modus-themes-org-agenda '((header-block . (variable-pitch 1.3))
                             (header-date . (workaholic bold-today 1.1))
                             (event . (accented varied))
                             (scheduled . rainbow)
                             (habit . traffic-light))
   modus-themes-completions '((matches . (extrabold background))
                              (selection . (semibold accented))
                              (popup . (extrabold))))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  (set-frame-parameter (selected-frame) 'alpha '(96 . 96))
  (add-to-list 'default-frame-alist '(alpha . (96 . 96))))

(use-package nyan-mode
  :config (nyan-mode))

(use-package restart-emacs
  :bind
  ("C-c q r" . restart-emacs)
  ("C-c q q" . save-buffers-kill-emacs))

(use-package face-remap
  :ensure nil
  :bind
  (("C-+" . bs/font-scaling/text-scale-increase)
   ("C--" . bs/font-scaling/text-scale-decrease))
  :config
  (defhydra bs/font-scaling ()
    "Font scaling"
    ("+" text-scale-increase "Scale Up")
    ("-" text-scale-decrease "Scale Down")
    ("q" nil "Quit" :exit t )))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-height 40)
  :config (doom-modeline-mode 1))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

(use-package pulsar
  :config
  (pulsar-global-mode t)
  :custom
  (pulsar-pulse-functions
   ;; NOTE 2022-04-09: The commented out functions are from before
   ;; the introduction of `pulsar-pulse-on-window-change'.  Try that
   ;; instead.
   '(recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     ;; bookmark-jump
     ;; other-window
     ;; delete-window
     ;; delete-other-windows
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     ;; windmove-right
     ;; windmove-left
     ;; windmove-up
     ;; windmove-down
     ;; windmove-swap-states-right
     ;; windmove-swap-states-left
     ;; windmove-swap-states-up
     ;; windmove-swap-states-down
     ;; tab-new
     ;; tab-close
     ;; tab-next
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading))
  (pulsar-pulse-on-window-change t)
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-red)
  (pulsar-highlight-face 'pulsar-yellow))

(use-package lin
  :config
  (lin-global-mode 1)
  :custom
  (lin-face 'lin-blue)
  (lin-mode-hooks '(bongo-mode-hook
                    dired-mode-hook
                    elfeed-search-mode-hook
                    git-rebase-mode-hook
                    grep-mode-hook
                    ibuffer-mode-hook
                    ilist-mode-hook
                    ledger-report-mode-hook
                    log-view-mode-hook
                    magit-log-mode-hook
                    mu4e-headers-mode
                    notmuch-search-mode-hook
                    notmuch-tree-mode-hook
                    occur-mode-hook
                    org-agenda-mode-hook
                    proced-mode-hook
                    tabulated-list-mode-hook)))

(provide 'init-user-interface)
;;; init-user-interface.el ends here
