;;; init-org.el --- Org Mode Customizations. -*- lexical-binding: t -*-

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

;; My customizations and additional packages for Org Mode.

;;; Code:

(defvar bs/org-map)
(define-prefix-command 'bs/org-map)
(global-set-key (kbd "M-o") 'bs/org-map)

(use-package org
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . corfu-mode)
         (org-agenda-mode . (lambda () (setq-local line-spacing 3))))
  :bind
  (("M-o a" . org-agenda)
   ("M-o c" . org-capture)
   ("M-o r" . org-archive-subtree)
   ("M-o l" . org-store-link))
  :config
  (define-key org-mode-map (kbd "M-o") nil)
  (define-key org-mode-map (kbd "C-j") nil)
  ;; Basic Configuration
  (setq
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CXLD(x!)")
                       (sequence "WAIT(w)" "HOLD(h)" "|" "DONE(d!)" "CXLD(x!)" "BREAK" "MEETING" "PHONECALL" "INTERRUPTION"))
   org-deadline-warning-days 14
   org-reverse-note-order nil
   org-confirm-elisp-link-function nil
   org-log-done 'time
   org-archive-location (s-concat bs/tasks-path "archives.org::")
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets `((,(s-concat bs/tasks-path "life.org") :maxlevel . 2))
   org-global-properties '(("Effort_ALL" . "5min 10min 15min 30min 45min 1h 2h 3h 4h 5h 6h 7h 8h 12h 16h 24h"))
   org-ellipsis "  "
   org-use-speed-commands t
   org-hide-emphasis-markers t
   org-log-into-drawer "LOGBOOK-NOTES"
   org-special-ctrl-k t
   org-M-RET-may-split-line nil
   org-ctrl-k-protect-subtree t
   org-blank-before-new-entry (quote ((heading . auto)
                                      (plain-list-item . auto)))
   org-bookmark-names-plist nil
   org-catch-invisible-edits 'show-and-error
   org-cycle-separator-lines -1
   org-enforce-todo-dependencies t
   org-use-fast-todo-selection 'expert
   org-use-fast-tag-selection t
   org-fast-tag-selection-single-key 'expert
   org-auto-align-tags nil
   org-tags-column 0
   org-imenu-depth 3
   org-startup-with-inline-images t
   org-image-actual-width nil
   )
  ;; Clock Configuration
  (org-clock-persistence-insinuate)
  (setq
   org-clock-history-length 20
   org-clock-in-resume t
   org-clock-out-remove-zero-time-clocks t
   org-clock-out-when-done t
   org-clock-persist t
   org-clock-persist-file (s-concat bs/emacs-cache-directory "org-clock-save.el")
   org-clock-persist-query-resume nil
   org-clock-into-drawer "TIME_SPENT_LOG"
   ;; if there is an org clock out hook active, then org does not
   ;; resume clocking in the previous task, so we remove it
   org-clock-out-hook nil)
  ;; Agenda Configuration
  (setq
   org-agenda-files (list (s-concat bs/tasks-path "life.org") (s-concat bs/tasks-path "inbox.org"))
   org-agenda-ndays 21
   org-agenda-show-all-dates t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-agenda-tags-column -150
   org-agenda-window-setup 'only-window
   org-agenda-todo-ignore-scheduled t
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-deadline-leaders '("!D!: " "D%02d: ")
   org-agenda-use-time-grid nil
   org-agenda-scheduled-leaders '("" "S%d: ")
   org-agenda-sorting-strategy
   (quote ((agenda habit-down time-up effort-up category-keep)
           (todo effort-up category-up)
           (tags effort-up category-up)
           (search category-up)))
   org-agenda-block-separator 126
   org-agenda-custom-commands
   '(("p" "Projects" todo "NEXT"
      ((org-agenda-overriding-header "Project Next Actions")))
     ("i" "Inbox" tags-todo "CATEGORY=\"Inbox\"TODO=\"TODO\""
      ((org-agenda-overriding-header "To Refile")))
     (" " "Agenda"
      ((agenda "" ((org-agenda-span 1)))
       (tags-todo "todo=\"NEXT\""
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-skip-function #'bs/org-skip-learning-and-based-on-context)))
       (tags-todo "todo=\"NEXT\""
                  ((org-agenda-overriding-header "Active Learning List")
                   (org-agenda-skip-function (lambda () (bs/org-include-or-skip-learning-actions-only t))))))))
   org-agenda-prefix-format '((agenda . "  %i   %-20c %-5s  %-8(bs/format-entry-scheduled-deadline-time)")
                              (todo . "  %i   %-37c ")
                              (tags . "  %i   %-26c 鬒 %-8e")
                              (search . "  %i   %-37c "))
   org-agenda-remove-tags t)
  ;; Org Id Configuration
  (add-to-list 'org-modules 'org-id)
  (setq org-id-locations-file (concat bs/emacs-cache-directory ".org-id-locations"))
  ;; Look and Feel Configuration
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (add-to-list 'org-file-apps (cons "\\.xopp\\'" "xournalpp %s")))

(use-package org-contrib
  :after org
  :config
  (add-to-list 'org-modules 'org-depend))

(use-package org-bullets
  :after org
  :hook ((org-mode . org-bullets-mode)))

(use-package org-protocol
  :after org
  :ensure nil)

(use-package org-checklist
  :after org
  :ensure nil)

(use-package org-indent
  :after org
  :ensure nil
  :commands (org-indent-mode)
  :hook (org-mode . org-indent-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-capture
  :ensure nil
  :after org
  :bind
  (("M-o c" . org-capture))
  :config
  (setq
   org-capture-templates
   `(("r" "Inbox [Read Later]" entry
      (file ,(s-concat bs/tasks-path "inbox.org"))
      "* TODO Read: %:description\n:PROPERTIES:\n:CREATED:  %U\n:URL: %l\n:END:\n"
      :immediate-finish t)
     ("t" "Inbox [Task]" entry
      (file ,(s-concat bs/tasks-path "inbox.org"))
      "* TODO %i%?\n:PROPERTIES:\n:CREATED:  %U\n:END:" :prepend t)
     ("n" "Inbox [Note]" entry
      (file ,(s-concat bs/notes-path "0.Inbox/inbox.org"))
      "* %i%?\n:PROPERTIES:\n:ID:  %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:END:" :prepend t))))

(use-package org-download
  :after org
  :custom
  (org-download-method 'attach))

(use-package bs-org
  :ensure nil
  :commands (bs/format-entry-scheduled-deadline-time)
  :bind
  ("M-o R" . bs/org-archive-all-done-cxld-tasks)
  (:map org-mode-map ("C-c C-w" . bs/org-refile))
  :hook
  ((before-save . bs/org-set-last-modified)
   (org-agenda-mode . bs/set-org-agenda-category-icon-alist)))

(provide 'init-org)
;;; init-org.el ends here
