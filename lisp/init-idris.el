;;; init-idris.el --- Configures Idris Mode -*- lexical-binding: t -*-

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
