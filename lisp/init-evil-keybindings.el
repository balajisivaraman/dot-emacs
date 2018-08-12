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

;; Company Mode
(eval-after-load "company"
  (lambda ()
    (bind-key "C-j" 'company-select-next company-active-map)
    (bind-key "C-k" 'company-select-previous company-active-map)))


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

(evil-define-key 'normal deadgrep-mode-map
  (kbd "RET") 'deadgrep-visit-result
  "n" 'deadgrep-forward
  "p" 'deadgrep-backward
  "g" 'deadgrep-restart
  "q" 'quit-window
  (kbd "TAB") 'deadgrep-toggle-file-results)

(provide 'init-evil-keybindings)
;;; init-evil-keybindings.el ends here
