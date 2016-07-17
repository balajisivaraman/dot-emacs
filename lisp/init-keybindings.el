;;; init-keybindings.el --- Custom keybindings and additional packages. -*- lexical-binding: t -*-

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

;; Loads some custom keybindings and `which-key' package for key discovery.

;;; Code:

;; Newline should always indent by default.
(bind-key "<RET>" 'newline-and-indent)
(unbind-key "C-x C-c")

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

(which-key-declare-prefixes-for-mode 'js2-mode
  "C-c m 3" "if-refactor"
  "C-c m a" "add/args"
  "C-c m b" "barf"
  "C-c m c" "contract"
  "C-c m d" "debug"
  "C-c m i" "inject/introduce"
  "C-c m l" "localize"
  "C-c m r" "rename"
  "C-c m s" "slurp/split"
  "C-c m t" "toggle"
  "C-c m u" "unwrap"
  "C-c m v" "var"
  "C-c m w" "wrap"
  "C-c m e" "extract")

(provide 'init-keybindings)
;;; init-keybindings.el ends here
