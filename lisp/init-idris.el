;;; init-idris.el --- Configures Idris Mode -*- lexical-binding: t -*-

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2017 Balaji Sivaraman

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

;; This module loads and configures Idris Mode.

;;; Code:

(use-package idris-mode
  :mode ("\\.idr\\'" . idris-mode)
  :config
  (bind-keys
   :map idris-mode-map
   :prefix-map idris-map
   :prefix "C-c m"
   ;; Shorthands: rebind the standard evil-mode combinations to the local
   ;; leader for the keys not used as a prefix below.
   ("c" . idris-case-dwim)
   ("d" . idris-add-clause)
   ("l" . idris-make-lemma)
   ("p" . idris-proof-search)
   ("r" . idris-load-file)
   ("t" . idris-type-at-point)
   ("w" . idris-make-with-block)

   ;; ipkg.
   ("bc" . idris-ipkg-build)
   ("bC" . idris-ipkg-clean)
   ("bi" . idris-ipkg-install)
   ("bp" . idris-open-package-file)

   ;; Interactive editing.
   ("ia" . idris-proof-search)
   ("ic" . idris-case-dwim)
   ("ie" . idris-make-lemma)
   ("im" . idris-add-missing)
   ("ir" . idris-refine)
   ("is" . idris-add-clause)
   ("ip" . idris-case-split)
   ("iw" . idris-make-with-block)

   ;; Documentation.
   ("ha" . idris-apropos)
   ("hd" . idris-docs-at-point)
   ("hs" . idris-type-search)
   ("ht" . idris-type-at-point)

   ;; Active term manipulations.
   ("mn" . idris-normalise-term)
   ("mi" . idris-show-term-implicits)
   ("mh" . idris-hide-term-implicits)
   ("mc" . idris-show-core-term)

   ;; Errors
   ("en" . idris-next-error)
   ("ep" . idris-previous-error)

   ;; REPL
   ("'"  . idris-repl)
   ("sb" . idris-load-file)
   ;; "sB. spacemacs/idris-load-file-and-focus
   ("si" . idris-repl)
   ("sn" . idris-load-forward-line)
   ;; "sN. spacemacs/idris-load-forward-line-and-focus
   ("sp" . idris-load-backward-line)
   ;; "sP. spacemacs/idris-load-backward-line-and-focus
   ("ss" . idris-pop-to-repl)
   ("sq" . idris-quit))

  (bind-keys
   :map idris-prover-script-mode-map
   :prefix-map idris-prover-map
   :prefix "C-c m"
   ("n" . idris-prover-script-forward)
   ("p" . idris-prover-script-backward)
   ("k" . idris-prover-abandon)
   ("q" . idris-prover-script-qed)))

(defun balaji/idris-mode-hook ()
  "Hooks for Idris mode."
  (company-mode t)
  (flycheck-mode))

(which-key-declare-prefixes-for-mode 'idris-mode
  "C-c m b" "ipkg"
  "C-c m i" "interactive edit"
  "C-c m h" "doc"
  "C-c m m" "term manipulation"
  "C-c m s" "repl")

(add-hook 'idris-mode-hook 'balaji/idris-mode-hook)

(provide 'init-idris)
;;; init-idris.el ends here
