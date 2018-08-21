;;; init-general-keybindings.el --- Defines keybindings using emacs general package. -*- lexical-binding: t -*-

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

(defmacro balaji/general-mode-specific-bindings (major-mode-map &rest args)
  "Macro to bind ARGS for MAJOR-MODE-MAP."
  `(progn
     (my-emacs-local-leader-def
       :keymaps ,major-mode-map
       ,@args)
     (when balaji-evil-mode-enabled
       (my-evil-local-leader-def
         :states '(normal visual)
         :keymaps ,major-mode-map
         ,@args))
     )
  )

(defmacro balaji/general-bindings (&rest args)
  "Macro to bind ARGS globally in both Emacs and Evil."
  `(progn
     (my-emacs-leader-def
       ,@args)
     (when balaji-evil-mode-enabled
       (my-evil-leader-def 'normal 'override
         ,@args))
     )
  )

;; Avy Jump Bindings
(balaji/general-bindings
 "jc" 'avy-goto-char
 "jj" 'avy-goto-char
 "jl" 'avy-goto-line
 "jw" 'avy-goto-word-1
 "jt" 'helm-imenu
 )

;; Bookmarks
(balaji/general-bindings
 "fb" 'bookmark-jump
 "Bs" 'bookmark-set
 "Bl" 'bookmark-bmenu-list)

;; Buffer Keybindings
(balaji/general-bindings
 "bd" 'kill-buffer
 "bb" 'helm-mini
 "bI" 'ibuffer
 "br" 'revert-buffer
 "bp" 'previous-buffer
 "bn" 'next-buffer)

;; Emacs Lisp Bindings
(balaji/general-mode-specific-bindings 'emacs-lisp-mode-map
  "eb" 'do-eval-buffer
  "er" 'do-eval-region
  "es" 'eval-last-sexp
  "s"  'scratch
  "dc" 'cancel-debug-on-entry
  "de" 'debug-on-entry
  "dr" 'toggle-debug-on-error
  "fb" 'emacs-lisp-byte-compile-and-load
  "fl" 'find-library
  "L"  'elint-current-buffer)

(which-key-declare-prefixes-for-mode 'emacs-lisp-mode
  "SPC m e" "eval"
  "SPC m f" "file"
  "SPC m d" "debug")

;; Expand Region
(balaji/general-bindings
  "ce" 'er/expand-region
  "cc" 'er/contract-region)

;; File Keybindings
(balaji/general-bindings
  "fd" 'balaji-dot-emacs
  "ff" 'helm-find-files
  "fs" 'save-buffer
  "fr" 'crux-rename-buffer-and-file
  "fw" 'write-file)

;; Helm Ag
(balaji/general-bindings
  "/" 'helm-do-ag)

;; Help
(balaji/general-bindings
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
  "h?" 'help-for-help)

;; Idris Mode
(balaji/general-mode-specific-bindings 'idris-mode-map
 ;; Shorthands: rebind the standard evil-mode combinations to the local
 ;; leader for the keys not used as a prefix below.
 "c" 'idris-case-dwim
 "d" 'idris-add-clause
 "l" 'idris-make-lemma
 "p" 'idris-proof-search
 "r" 'idris-load-file
 "t" 'idris-type-at-point
 "w" 'idris-make-with-block

 ;; ipkg.
 "bc" 'idris-ipkg-build
 "bC" 'idris-ipkg-clean
 "bi" 'idris-ipkg-install
 "bp" 'idris-open-package-file

 ;; Interactive editing.
 "ia" 'idris-proof-search
 "ic" 'idris-case-dwim
 "ie" 'idris-make-lemma
 "im" 'idris-add-missing
 "ir" 'idris-refine
 "is" 'idris-add-clause
 "ip" 'idris-case-split
 "iw" 'idris-make-with-block

 ;; Documentation.
 "ha" 'idris-apropos
 "hd" 'idris-docs-at-point
 "hs" 'idris-type-search
 "ht" 'idris-type-at-point

 ;; Active term manipulations.
 "mn" 'idris-normalise-term
 "mi" 'idris-show-term-implicits
 "mh" 'idris-hide-term-implicits
 "mc" 'idris-show-core-term

 ;; Errors
 "en" 'idris-next-error
 "ep" 'idris-previous-error

 ;; REPL
 "'"  'idris-repl
 "sb" 'idris-load-file
 ;; "msB" 'spacemacs/idris-load-file-and-focus
 "si" 'idris-repl
 "sn" 'idris-load-forward-line
 ;; "msN" 'spacemacs/idris-load-forward-line-and-focus
 "sp" 'idris-load-backward-line
 ;; "msP" 'spacemacs/idris-load-backward-line-and-focus
 "ss" 'idris-pop-to-repl
 "sq" 'idris-quit)

(balaji/general-mode-specific-bindings 'idris-prover-script-mode-map
  "n" 'idris-prover-script-forward
  "p" 'idris-prover-script-backward
  "k" 'idris-prover-abandon
  "q" 'idris-prover-script-qed)

(which-key-declare-prefixes-for-mode 'idris-mode
  "SPC m b" "ipkg"
  "SPC m i" "interactive edit"
  "SPC m h" "doc"
  "SPC m m" "term manipulation"
  "SPC m s" "repl")

;; Ledger Mode
(balaji/general-mode-specific-bindings 'ledger-mode-map
  "r" 'balaji/insert-rupee-symbol
  "a" 'ledger-add-transaction
  "bp" 'ledger-display-balance-at-point)

(which-key-declare-prefixes-for-mode 'ledger-mode
  "SPC m r" "rupee-symbol"
  "SPC m a" "add transaction"
  "SPC m b" "balance")

;; Magit Mode
(balaji/general-bindings
  "gs" 'magit-status
  "gb" 'magit-branch
  "gp" 'magit-pull
  "gr" 'magit-reset-head
  "gR" 'magit-reset-head-hard
  "gf" 'magit-fetch
  "gl" 'magit-log-all
  "gL" 'magit-log
  "gc" 'magit-checkout)

;; Markdown Mode
(balaji/general-mode-specific-bindings 'markdown-mode-map
  "il" 'markdown-insert-link
  "ib" 'markdown-insert-bold
  "ii" 'markdown-insert-italic
  )

(which-key-declare-prefixes-for-mode 'markdown-mode
  "SPC m i" "insert"
  )

;; Org Bindings
(balaji/general-bindings
  "oa" 'org-agenda
  "ob" 'org-check-before-date
  "oc" 'org-capture
  "od" 'org-check-deadlines
  "oA" 'org-check-after-date
  "or" 'org-archive-subtree
  "ol" 'balaji/org-insert-prop-for-current-entry
  "os" 'org-agenda-list-stuck-projects)

;; Outline Mode
(balaji/general-bindings
  "O" 'balaji-outline/body)

;; Package Utils
(balaji/general-bindings
  "Pu" 'paradox-upgrade-packages
  "Pm" 'elpamr-create-mirror-for-installed
  "PP" 'package-list-packages-no-fetch
  "Pp" 'paradox-list-packages)

;; Projectile Bindings
(balaji/general-bindings
  "ph" 'helm-projectile
  "pi" 'projectile-invalidate-cache
  "pk" 'projectile-kill-buffers
  "p!" 'projectile-run-shell-command-in-root
  "p&" 'projectile-run-async-shell-command-in-root
  "pD" 'projectile-dired
  "pE" 'projectile-edit-dir-locals
  "pF" 'projectile-find-file-in-known-projects
  "pI" 'projectile-ibuffer
  "pP" 'projectile-test-project
  "pR" 'projectile-regenerate-tags
  "pS" 'projectile-save-project-buffers
  "pT" 'projectile-find-test-file
  "pa" 'projectile-find-other-file
  "pb" 'projectile-switch-to-buffer
  "pc" 'projectile-compile-project
  "pd" 'projectile-find-dir
  "pe" 'projectile-recentf
  "pf" 'projectile-find-file
  "pg" 'projectile-find-file-dwim
  "ph" 'helm-projectile
  "pi" 'projectile-invalidate-cache
  "pj" 'projectile-find-tag
  "pk" 'projectile-kill-buffers
  "pl" 'projectile-find-file-in-directory
  "pm" 'projectile-commander
  "po" 'projectile-multi-occur
  "pp" 'projectile-switch-project
  "pq" 'projectile-switch-open-project
  "pr" 'projectile-replace
  "pt" 'projectile-toggle-between-implementation-and-test
  "pu" 'projectile-run-project
  "pv" 'projectile-vc
  "pz" 'projectile-cache-current-file
  "pxe" 'projectile-run-eshell
  "pxs" 'projectile-run-shell
  "pxt" 'projectile-run-term
  "psg" 'projectile-grep
  "pss" 'projectile-ag
  "p4a" 'projectile-find-other-file-other-window
  "p4b" 'projectile-switch-to-buffer-other-window
  "p4d" 'projectile-find-dir-other-window
  "p4f" 'projectile-find-file-other-window
  "p4g" 'projectile-find-file-dwim-other-window
  "p4t" 'projectile-find-implementation-or-test-other-window)

;; Purescipt Bindings
(balaji/general-mode-specific-bindings 'purescript-mode-map
 "a" 'psc-ide-add-clause
 "b" 'psc-ide-rebuild
 "c" 'psc-ide-case-split
 "l" 'psc-ide-load-all
 "sq" 'psc-ide-server-quit
 "ss" 'psc-ide-server-start
 "t" 'psc-ide-show-type
 "L" 'psc-ide-load-module
 "g" 'psc-ide-goto-definition
 "b" 'psc-ide-goto-definition-impl
 "i" 'psc-ide-add-import
 "pr" 'psci)

(which-key-declare-prefixes-for-mode 'purescript-mode
  "SPC m p" "psci"
  "SPC m s" "server")

;; Scala Mode
(balaji/general-mode-specific-bindings 'scala-mode-map
  ;; "." 'ensime-edit-definition
  ;; "," 'ensime-pop-find-definition-stack
  "i" 'ensime-import-type-at-point
  "e" 'ensime
  "E" 'ensime-reload
  "s" 'ensime-shutdown
  "to" 'ensime-sbt-do-test-only-dwim
  "tt"  'ensime-sbt-do-test-dwim
 )

(which-key-declare-prefixes-for-mode 'purescript-mode
  "SPC m" "Ensime"
  "SPC m e" "Ensime Start"
  "SPC m E" "Ensime Reload"
  "SPC m s" "Ensime Shutdown"
  "SPC m i" "Import Type at Point"
  "SPC m t" "Test"
  "SPC m t o" "Run Current Test"
  "SPC m t t" "Run Tests")

;; Quit Bindings
(balaji/general-bindings
    "qr" 'restart-emacs
    "qq" 'save-buffers-kill-emacs)

;; Ripgrep
(balaji/general-bindings
 "rr" 'deadgrep
  )

;; Rust Bindings
(balaji/general-mode-specific-bindings 'rust-mode-map
  "d" 'cargo-process-doc
  "f" 'cargo-process-fmt
  "rr" 'cargo-process-run
  "rb" 'cargo-process-run-bin
  "re" 'cargo-process-run-example
  "n" 'cargo-process-new
  "m" 'cargo-process-mode
  "i" 'cargo-process-init
  "tt" 'cargo-process-test
  "tc" 'cargo-process-current-test
  "tf" 'cargo-process-current-file-tests
  "b" 'cargo-process-build
  "c" 'cargo-process-clean
  "k" 'cargo-process-check
  "B" 'cargo-process-bench
  "u" 'cargo-process-update
  "R" 'cargo-process-repeat
  "C" 'cargo-process-clippy
  "s" 'cargo-process-search
  "D" 'cargo-process-doc-open)

(which-key-declare-prefixes-for-mode 'rust-mode
  "SPC m c" "cargo")

;; Toggle Bindings
(balaji/general-bindings
  "tr" 'linum-relative-mode
  "tg" 'balaji-toggle-golden-ratio)

;; Transpose Words
;; (balaji/general-bindings
;;   "Tc" 'transpose-chars
;;   "Tw" 'transpose-words
;;   "Tt" 'transpose-words
;;   "Tl" 'transpose-lines
;;   "Te" 'transpose-sexps
;;   "Ts" 'transpose-sentences
;;   "Tp" 'transpose-paragraphs)

;; Version Control
(balaji/general-bindings
  "ggp" 'gist-list
  "ggb" 'gist-region-or-buffer)

;; Window Bindings
(balaji/general-bindings
  "wd" 'delete-window
  "wo" 'ace-window
  "wh" 'split-window-horizontally
  "wv" 'split-window-vertically
  "wm" 'delete-other-windows)

(balaji/general-bindings
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

(provide 'init-general-keybindings)
;;; init-general-keybindings.el ends here
