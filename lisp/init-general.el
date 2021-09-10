;;; init-general.el --- Defines keybindings using emacs general package. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Balaji Sivaraman

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

;; This module defines all my keybindings using Emacs General to be
;; compatible with both Emacs and Evil modes. This should be called at
;; the end after all other modules have been loaded.

;;; Code:

(use-package general)

(defconst my-emacs-leader "C-c")
(defconst my-emacs-local-leader "C-c m")
(defconst my-evil-leader "SPC")
(defconst my-evil-local-leader "SPC m")

(general-create-definer my-emacs-leader-def
  :prefix my-emacs-leader)

(general-create-definer my-emacs-local-leader-def
  :prefix my-emacs-local-leader)

(general-create-definer my-evil-leader-def
  :prefix my-evil-leader)

(general-create-definer my-evil-local-leader-def
  :prefix my-evil-local-leader)

(defmacro bs/general-mode-specific-bindings (major-mode-map &rest args)
  "Macro to bind ARGS for MAJOR-MODE-MAP."
  `(progn
     (my-emacs-local-leader-def
       :keymaps ,major-mode-map
       ,@args)
     (my-evil-local-leader-def
       :states '(normal visual)
       :keymaps ,major-mode-map
       ,@args)
     )
  )

(defmacro bs/general-bindings (&rest args)
  "Macro to bind ARGS globally in both Emacs and Evil."
  `(progn
     (my-emacs-leader-def
       ,@args)
     (my-evil-leader-def 'normal 'override
       ,@args)
     )
  )

;; General Bindings
(bs/general-bindings
 "fd" 'bs/dot-emacs
 "hA" 'about-emacs
 "ha" 'apropos-command
 "hc" 'describe-copying
 "hD" 'view-emacs-debugging
 "hd" 'apropos-documentation
 "hf" 'describe-function
 "hF" 'view-emacs-FAQ
 "hg" 'describe-gnu-project
 "hh" 'view-hello-file
 "hi" 'info
 "hk" 'describe-key
 "hm" 'describe-mode
 "ho" 'describe-symbol
 "hp" 'finder-by-keyword
 "hs" 'describe-syntax
 "ht" 'help-with-tutorial
 "hv" 'describe-variable
 "hw" 'describe-no-warranty
 "h<return>" 'view-order-manuals
 "h?" 'help-for-help
 "ib" 'bs/indent-region-or-buffer)

;; Package Bindings
(bs/general-bindings
 "Pu" 'paradox-upgrade-packages
 "Pm" 'elpamr-create-mirror-for-installed
 "PP" 'package-list-packages-no-fetch
 "Pp" 'paradox-list-packages)

(bs/general-bindings
 "tr" 'linum-relative-mode)

;; Window Bindings
(bs/general-bindings
 "wd" 'delete-window
 "wo" 'ace-window
 "wh" 'split-window-horizontally
 "wv" 'split-window-vertically
 "wm" 'delete-other-windows)

(bs/general-bindings
 "0" 'select-window-0
 "1" 'select-window-1
 "2" 'select-window-2
 "3" 'select-window-3
 "4" 'select-window-4
 "5" 'select-window-5
 "6" 'select-window-6
 "7" 'select-window-7
 "8" 'select-window-8
 "9" 'select-window-9)

(provide 'init-general)
;;; init-general.el ends here
