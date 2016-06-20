;;; -*- lexical-binding: t -*-
;;; init-keybindings.el --- General Keybindings for Emacs.

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2016 Balaji Sivaraman

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; This file contains custom keybindings for Emacs. Mode-specific keybindings are available in their respective init files.

;;; Code:

;; Newline should always indent by default.
(bind-key "<RET>" 'newline-and-indent)

;; Use `p` and `n` to go to previous and next buffers.
(bind-key "C-x p" 'previous-buffer)
(bind-key "C-x n" 'next-buffer)

(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

(bind-key "C-c r" 'eval-region)

(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

(bind-key "<key-4660>" 'ignore)
(bind-key "C-<key-4660>" 'ignore)

(bind-key "C-c q q" 'save-buffers-kill-emacs)

(use-package restart-emacs
  :bind "C-c q r")

(defvar toggle-map)
(define-prefix-command 'toggle-map)
(bind-key "C-c t" #'toggle-map)

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
          ("\\`lunaryorn-"  . "")
          ("projectile-"    . "proj-")
          ("helm-"          . "h-")
          ("magit-"         . "ma-"))))

(which-key-declare-prefixes
  "C-c a" "applications"
  "C-c b" "bookmarks"
  "C-c f" "files"
  "C-c g" "git"
  "C-c h" "helm/help"
  "C-c j" "jump"
  "C-c m" "major mode"
  "C-c o" "org mode"
  "C-c p" "projects"
  "C-c p s" "projects/search"
  "C-c q" "quit/restart"
  "C-c s" "search"
  "C-c t" "toggle")


(which-key-declare-prefixes-for-mode 'emacs-lisp-mode
  "C-c m e" "eval"
  "C-c m f" "file"
  "C-c m d" "debug")

(provide 'init-keybindings)
;;; init-keybindings.el ends here
