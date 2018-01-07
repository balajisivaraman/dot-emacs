;;; init-evil-keybindings.el --- Loads all Evil related keybindings. -*- lexical-binding: t -*-

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

;; This file contains all my evil-mode related keybindings. A lot of these are inspired by Spacemacs.

;;; Code:

;; Which Key Configuration

(which-key-declare-prefixes
  "SPC a"   "applications"
  "SPC b"   "buffer"
  "SPC B"   "bookmarks"
  "SPC c"   "ex/co region"
  "SPC f"   "files"
  "SPC g"   "git"
  "SPC g g" "gist"
  "SPC h"   "helm/help"
  "SPC i"   "indent"
  "SPC j"   "jump"
  "SPC m"   "major mode"
  "SPC o"   "org mode"
  "SPC O"   "outline"
  "SPC p"   "projects"
  "SPC p s" "projects/search"
  "SPC P"   "packages"
  "SPC q"   "quit/restart"
  "SPC s"   "search"
  "SPC t"   "toggle"
  "SPC T"   "transpose"
  "SPC w"   "window")

;; Universal Argument
(evil-leader/set-key "u" 'universal-argument)

;; Shell Command
(evil-leader/set-key "!" 'shell-command)

;; General Keybindings
(evil-leader/set-key
  "Tn" 'balaji/cycle-themes
  "ir" 'indent-region
  "en" 'balaji-flycheck-errors/body
  "." 'xref-find-definitions
  "," 'xref-pop-marker-stack)

;; Evil Mode
(bind-key "\C-c" 'evil-force-normal-state evil-insert-state-map)
(bind-key "\C-c" 'evil-force-normal-state evil-replace-state-map)
(bind-key "\C-c" 'evil-force-normal-state evil-visual-state-map)

(evil-leader/set-key
  "<SPC>" 'helm-M-x)

(defun balaji-clear-search-highlight (args)
  "Clear search highlight when Return is pressed."
  (interactive "P")
  (evil-ex-call-command "" "nohlsearch" ""))

(bind-key "RET" 'balaji-clear-search-highlight evil-normal-state-map)
(evil-leader/set-key
  "sc" 'balaji-clear-search-highlight)
(bind-key ";" 'evil-ex evil-normal-state-map)

;; Avy Jump Bindings
(evil-leader/set-key
  "jc" 'avy-goto-char
  "jj" 'avy-goto-char
  "jl" 'avy-goto-line
  "jw" 'avy-goto-word-1)

;; Bookmarks
(evil-leader/set-key
  "Bb" 'bookmark-jump
  "Bj" 'bookmark-jump
  "Bs" 'bookmark-set
  "Bl" 'bookmark-bmenu-list)

;; Buffer Keybindings
(evil-leader/set-key
  "bd" 'kill-buffer
  "bb" 'switch-to-buffer
  "bI" 'ibuffer
  "br" 'revert-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer)

;; Company Mode
(eval-after-load "company"
  (lambda ()
    (bind-key "C-j" 'company-select-next company-active-map)
    (bind-key "C-k" 'company-select-previous company-active-map)))

;; Emacs Lisp Bindings
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "meb" 'do-eval-buffer
  "mer" 'do-eval-region
  "mes" 'eval-last-sexp
  "ms"  'scratch
  "mdc" 'cancel-debug-on-entry
  "mde" 'debug-on-entry
  "mdr" 'toggle-debug-on-error
  "mfb" 'emacs-lisp-byte-compile-and-load
  "mfl" 'find-library
  "mL"  'elint-current-buffer)

(which-key-declare-prefixes-for-mode 'emacs-lisp-mode
  "SPC m e" "eval"
  "SPC m f" "file"
  "SPC m d" "debug")

;; Expand Region
(evil-leader/set-key
  "ce" 'er/expand-region
  "cc" 'er/contract-region)

;; File Keybindings
(evil-leader/set-key
  "fd" 'balaji-dot-emacs
  "ff" 'helm-find-files
  "fs" 'save-buffer
  "fw" 'write-file)

;; Helm Ag
(evil-leader/set-key
  "/" 'helm-do-ag)

;; Help
(evil-leader/set-key
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

;; Ibuffer Bindings
(evil-define-minor-mode-key 'normal 'ibuffer-mode "A" 'ibuffer-do-view)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "D" 'ibuffer-do-delete)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "E" 'ibuffer-do-eval)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "F" 'ibuffer-do-shell-command-file)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "H" 'ibuffer-do-view-other-frame)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "I" 'ibuffer-do-query-replace-regexp)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "M" 'ibuffer-do-toggle-modified)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "N" 'ibuffer-do-shell-command-pipe-replace)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "O" 'ibuffer-do-occur)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "P" 'ibuffer-do-print)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "Q" 'ibuffer-do-query-replace)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "R" 'ibuffer-do-rename-uniquely)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "S" 'ibuffer-do-save)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "T" 'ibuffer-do-toggle-read-only)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "U" 'ibuffer-do-replace-regexp)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "V" 'ibuffer-do-revert)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "W" 'ibuffer-do-view-and-eval)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "X" 'ibuffer-do-shell-command-pipe)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "`" 'ibuffer-switch-format)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "b" 'ibuffer-bury-buffer)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "d" 'ibuffer-mark-for-delete)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "g" 'ibuffer-update)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "j" 'ibuffer-jump-to-buffer)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "k" 'ibuffer-do-kill-lines)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "l" 'ibuffer-redisplay)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "m" 'ibuffer-mark-forward)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "n" 'ibuffer-forward-line)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "o" 'ibuffer-visit-buffer-other-window)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "p" 'ibuffer-backward-line)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "t" 'ibuffer-toggle-marks)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "u" 'ibuffer-unmark-forward)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "v" 'ibuffer-do-view)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "w" 'ibuffer-copy-filename-as-kill)
(evil-define-minor-mode-key 'normal 'ibuffer-mode "x" 'ibuffer-do-kill-on-deletion-marks)

;; Idris Mode
(evil-leader/set-key-for-mode 'idris-mode
 ;; Shorthands: rebind the standard evil-mode combinations to the local
 ;; leader for the keys not used as a prefix below.
 "mc" 'idris-case-dwim
 "md" 'idris-add-clause
 "ml" 'idris-make-lemma
 "mp" 'idris-proof-search
 "mr" 'idris-load-file
 "mt" 'idris-type-at-point
 "mw" 'idris-make-with-block

 ;; ipkg.
 "mbc" 'idris-ipkg-build
 "mbC" 'idris-ipkg-clean
 "mbi" 'idris-ipkg-install
 "mbp" 'idris-open-package-file

 ;; Interactive editing.
 "mia" 'idris-proof-search
 "mic" 'idris-case-dwim
 "mie" 'idris-make-lemma
 "mim" 'idris-add-missing
 "mir" 'idris-refine
 "mis" 'idris-add-clause
 "mip" 'idris-case-split
 "miw" 'idris-make-with-block

 ;; Documentation.
 "mha" 'idris-apropos
 "mhd" 'idris-docs-at-point
 "mhs" 'idris-type-search
 "mht" 'idris-type-at-point

 ;; Active term manipulations.
 "mmn" 'idris-normalise-term
 "mmi" 'idris-show-term-implicits
 "mmh" 'idris-hide-term-implicits
 "mmc" 'idris-show-core-term

 ;; Errors
 "men" 'idris-next-error
 "mep" 'idris-previous-error

 ;; REPL
 "m'"  'idris-repl
 "msb" 'idris-load-file
 ;; "msB" 'spacemacs/idris-load-file-and-focus
 "msi" 'idris-repl
 "msn" 'idris-load-forward-line
 ;; "msN" 'spacemacs/idris-load-forward-line-and-focus
 "msp" 'idris-load-backward-line
 ;; "msP" 'spacemacs/idris-load-backward-line-and-focus
 "mss" 'idris-pop-to-repl
 "msq" 'idris-quit)

(evil-leader/set-key-for-mode 'idris-prover-script-mode
  "mn" 'idris-prover-script-forward
  "mp" 'idris-prover-script-backward
  "mk" 'idris-prover-abandon
  "mq" 'idris-prover-script-qed)

(which-key-declare-prefixes-for-mode 'idris-mode
  "SPC m b" "ipkg"
  "SPC m i" "interactive edit"
  "SPC m h" "doc"
  "SPC m m" "term manipulation"
  "SPC m s" "repl")

;; Ledger Mode
(evil-leader/set-key-for-mode 'ledger-mode
  "mr" 'balaji/insert-rupee-symbol
  "ma" 'ledger-add-transaction
  "mbp" 'ledger-display-balance-at-point)

(which-key-declare-prefixes-for-mode 'ledger-mode
  "SPC m r" "rupee-symbol"
  "SPC m a" "add transaction"
  "SPC m b" "balance")

;; Magit Mode
(evil-leader/set-key
  "gs" 'magit-status
  "gb" 'magit-branch
  "gp" 'magit-pull
  "gr" 'magit-reset-head
  "gR" 'magit-reset-head-hard
  "gf" 'magit-fetch
  "gl" 'magit-log-all
  "gL" 'magit-log
  "gc" 'magit-checkout)

;; Org Bindings
(evil-leader/set-key
  "oa" 'org-agenda
  "od" 'org-check-deadlines
  "ob" 'org-check-before-date
  "oA" 'org-check-after-date
  "os" 'org-agenda-list-stuck-projects)

;; Org Agenda Bindings
(evil-define-key 'normal org-agenda-mode-map
  "!" 'org-agenda-toggle-deadlines
  "#" 'org-agenda-dim-blocked-tasks
  "$" 'org-agenda-archive
  "%" 'org-agenda-bulk-mark-regexp
  "*" 'org-agenda-bulk-mark-all
  "+" 'org-agenda-priority-up
  "," 'org-agenda-priority
  "-" 'org-agenda-priority-down
  "." 'org-agenda-goto-today
  "/" 'org-agenda-filter-by-tag
  ":" 'org-agenda-set-tags
  "<" 'org-agenda-filter-by-category
  "=" 'org-agenda-filter-by-regexp
  ">" 'org-agenda-date-prompt
  "?" 'org-agenda-show-the-flagging-note
  "A" 'org-agenda-append-agenda
  "B" 'org-agenda-bulk-action
  "C" 'org-agenda-convert-date
  "D" 'org-agenda-toggle-diary
  "E" 'org-agenda-entry-text-mode
  "F" 'org-agenda-follow-mode
  "G" 'org-agenda-toggle-time-grid
  "H" 'org-agenda-holidays
  "I" 'org-agenda-clock-in
  "L" 'org-agenda-recenter
  "M" 'org-agenda-phases-of-moon
  "N" 'org-agenda-next-item
  "O" 'org-agenda-clock-out
  "P" 'org-agenda-previous-item
  "Q" 'org-agenda-Quit
  "R" 'org-agenda-clockreport-mode
  "S" 'org-agenda-sunrise-sunset
  "T" 'org-agenda-show-tags
  "U" 'org-agenda-bulk-unmark-all
  "X" 'org-agenda-clock-cancel
  "[" 'org-agenda-manipulate-query-add
  "\\" 'org-agenda-filter-by-tag-refine
  "]" 'org-agenda-manipulate-query-subtract
  "^" 'org-agenda-filter-by-top-headline
  "_" 'org-agenda-filter-by-effort
  "a" 'org-agenda-archive-default-with-confirmation
  "b" 'org-agenda-earlier
  "c" 'org-agenda-goto-calendar
  "d" 'org-agenda-day-view
  "e" 'org-agenda-set-effort
  "f" 'org-agenda-later
  "h" 'org-agenda-holidays
  "i" 'org-agenda-diary-entry
  "J" 'org-agenda-goto-date
  "K" 'org-agenda-capture
  "l" 'org-agenda-log-mode
  "m" 'org-agenda-bulk-mark
  "n" 'org-agenda-next-line
  "p" 'org-agenda-previous-line
  "q" 'org-agenda-quit
  "r" 'org-agenda-redo
  "t" 'org-agenda-todo
  "u" 'org-agenda-bulk-unmark
  "v" 'org-agenda-view-mode-dispatch
  "w" 'org-agenda-week-view
  "x" 'org-agenda-exit
  "y" 'org-agenda-year-view
  "z" 'org-agenda-add-note
  "s" 'org-save-all-org-buffers
  "{" 'org-agenda-manipulate-query-add-re
  "|" 'org-agenda-filter-remove-all
  "}" 'org-agenda-manipulate-query-subtract-re
  "~" 'org-agenda-limit-interactively)

;; Outline Mode
(evil-leader/set-key
  "O" 'balaji-outline/body)

;; Package Utils
(evil-leader/set-key
  "Pu" 'paradox-upgrade-packages
  "PP" 'package-list-packages-no-fetch
  "Pp" 'paradox-list-packages)

;; Projectile Mode
(evil-leader/set-key
  "ph" 'helm-projectile
  "pi" 'projectile-invalidate-cache
  "pk" 'projectile-kill-buffers
  "xo" 'helm-imenu)

;; Projectile Bindings
(evil-leader/set-key
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
(evil-leader/set-key-for-mode 'purescript-mode
 "ma" 'psc-ide-add-clause
 "mb" 'psc-ide-rebuild
 "mc" 'psc-ide-case-split
 "ml" 'psc-ide-load-all
 "msq" 'psc-ide-server-quit
 "mss" 'psc-ide-server-start
 "mt" 'psc-ide-show-type
 "mL" 'psc-ide-load-module
 "mg" 'psc-ide-goto-definition
 "mb" 'psc-ide-goto-definition-impl
 "mi" 'psc-ide-add-import
 "mpp" 'psci)

(which-key-declare-prefixes-for-mode 'purescript-mode
  "SPC m p" "psci"
  "SPC m s" "server")

;; Rust Bindings
(evil-leader/set-key-for-mode 'rust-mode
  "md" 'cargo-process-doc
  "mf" 'cargo-process-fmt
  "mrr" 'cargo-process-run
  "mrb" 'cargo-process-run-bin
  "mre" 'cargo-process-run-example
  "mn" 'cargo-process-new
  "mm" 'cargo-process-mode
  "mi" 'cargo-process-init
  "mtt" 'cargo-process-test
  "mtc" 'cargo-process-current-test
  "mtf" 'cargo-process-current-file-tests
  "mb" 'cargo-process-build
  "mc" 'cargo-process-clean
  "mk" 'cargo-process-check
  "mB" 'cargo-process-bench
  "mu" 'cargo-process-update
  "mR" 'cargo-process-repeat
  "mC" 'cargo-process-clippy
  "ms" 'cargo-process-search
  "mD" 'cargo-process-doc-open)

(which-key-declare-prefixes-for-mode 'rust-mode
  "SPC m c" "cargo")

;; Quit Bindings
(evil-leader/set-key
    "qr" 'restart-emacs
    "qq" 'save-buffers-kill-emacs)

;; Scala Mode
(evil-leader/set-key-for-mode 'scala-mode
  "." 'ensime-edit-definition
  "," 'ensime-pop-find-definition-stack
  "mi" 'ensime-import-type-at-point
  "me" 'ensime
  "mE" 'ensime-reload
  "ms" 'ensime-shutdown
  "mto" 'ensime-sbt-do-test-only-dwim
  "mtt"  'ensime-sbt-do-test-dwim
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

;; Toggle Bindings
(evil-leader/set-key
  "tr" 'linum-relative-mode
  "tg" 'balaji-toggle-golden-ratio)

;; Transpose Words
(evil-leader/set-key
  "Tc" 'transpose-chars
  "Tw" 'transpose-words
  "Tt" 'transpose-words
  "Tl" 'transpose-lines
  "Te" 'transpose-sexps
  "Ts" 'transpose-sentences
  "Tp" 'transpose-paragraphs)

;; Version Control
(evil-leader/set-key
  "ggp" 'gist-list
  "ggb" 'gist-region-or-buffer)

;; Window Bindings
(evil-leader/set-key
  "wd" 'delete-window
  "wo" 'ace-window
  "wh" 'split-window-horizontally
  "wv" 'split-window-vertically
  "wm" 'delete-other-windows)

(evil-leader/set-key
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

(provide 'init-evil-keybindings)
;;; init-evil-keybindings.el ends here
