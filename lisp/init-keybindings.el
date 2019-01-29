;;; init-keybindings.el --- Custom keybindings and additional packages. -*- lexical-binding: t -*-

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

;; Loads some custom keybindings and `which-key' package for key discovery.

;;; Code:

;; (define-key key-translation-map "\C-t" "\C-x")

;; Newline should always indent by default.
(bind-key "<RET>" 'newline-and-indent)
(unbind-key "C-x C-c")

(defvar toggle-map)
(define-prefix-command 'toggle-map)
(bind-key "C-c t" #'toggle-map)

(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta))

;; Inspired by Spacemacs and Sebastian Wiesner's Config
;; The latter can be found here: https://github.com/lunaryorn/.emacs.d/blob/master/init.el#L317
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        ;; Let's go unicode :)
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Lambdas
          ("\\`\\?\\?\\'"   . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"       . "|=")
          ;; Drop/shorten package prefixes
          ("\\`balaji-"     . "b-")
          ("projectile-"    . "proj-")
          ("helm-"          . "h-")
          ("magit-"         . "ma-"))))

(which-key-declare-prefixes
  "C-c a" "applications"
  "C-c b" "bookmarks"
  "C-c B" "buffer"
  "C-c f" "files"
  "C-c g" "git"
  "C-c g g" "gist"
  "C-c h" "helm/help"
  "C-c j" "jump"
  "C-c m" "major mode"
  "C-c o" "org mode"
  "C-c O" "outline"
  "C-c p" "projects"
  "C-c p s" "projects/search"
  "C-c q" "quit/restart"
  "C-c s" "search"
  "C-c t" "toggle")

(which-key-declare-prefixes-for-mode 'emacs-lisp-mode
  "C-c m e" "eval"
  "C-c m f" "file"
  "C-c m d" "debug")

(which-key-declare-prefixes-for-mode 'rust-mode
  "C-c m t" "test"
  "C-c m r" "run")

(provide 'init-keybindings)
;;; init-keybindings.el ends here
