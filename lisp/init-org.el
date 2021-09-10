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

(use-package org
  :ensure org-plus-contrib
  :diminish
  (org-indent-mode)
  :bind
  (("C-. a" . org-agenda)
   ("C-. r" . org-archive-subtree)
   ("C-. l" . org-store-link)
   ("C-. p" . bs/punch-in)
   ("C-. o" . bs/punch-out)
   :map org-mode-map
   ("C-. i" . bs/org-insert-prop-for-current-entry)
   ("C-. i" . org-insert-link))
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-bullets-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . company-mode)
         (org-agenda-mode . (lambda () (setq-local line-spacing 3)))
         (before-save . bs/org-set-last-modified))
  :init
  (bs/general-bindings
   "A" 'org-agenda
   "oA" 'org-check-after-date
   "ob" 'org-check-before-date
   "oc" 'org-capture
   "od" 'org-check-deadlines
   "oi" 'org-insert-link
   "ol" 'org-store-link
   "op" 'bs/org-insert-prop-for-current-entry
   "or" 'org-archive-subtree
   "os" 'org-agenda-list-stuck-projects)
  :config
  ;; Basic Configuration
  (setq
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CXLD(x!)")
                       (sequence "WAIT(w)" "HOLD(h)" "|" "DONE(d!)" "CXLD(x!)" "BREAK" "MEETING" "PHONECALL" "INTERRUPTION"))
   org-tag-alist (quote ((:startgroup)
                         ("@home" . ?h)
                         ("@work" . ?w)
                         (:endgroup)))
   org-deadline-warning-days 14
   org-reverse-note-order nil
   org-confirm-elisp-link-function nil
   org-log-done 'time
   org-archive-location (s-concat bs/nextcloud-path "gtd/archives.org::")
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets '((org-agenda-files :maxlevel . 9))
   org-global-properties '(("Effort_ALL" . "5min 10min 15min 30min 45min 1h 2h 3h 4h 5h 6h 7h 8h"))
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
   org-catch-invisible-edits 'error
   org-cycle-separator-lines 0
   org-enforce-todo-dependencies t
   org-use-fast-todo-selection 'expert
   org-use-fast-tag-selection t
   org-fast-tag-selection-single-key 'expert
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
   org-agenda-files (list (s-concat bs/nextcloud-path "gtd/life.org"))
   org-agenda-ndays 21
   org-agenda-show-all-dates t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-agenda-tags-column -180
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
                  ((org-agenda-overriding-header "Learning List")
                   (org-agenda-skip-function (lambda () (bs/org-include-or-skip-learning-actions-only t))))))))
   org-agenda-prefix-format '((agenda . "  %i   %-12c   鬒%-6e   %-6s %-8(bs/format-entry-scheduled-deadline-time)")
                              (todo . "  %i   %-12c   鬒%-6e   ")
                              (tags . "  %i   %-12c   鬒%-6e   ")
                              (search . "  %i   %-12c   鬒%-6e   "))
   org-agenda-category-icon-alist `(("Account" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Amma" ,(s-concat bs/org-agenda-icons-path "family.png") nil nil :ascent center)
                                    ("Appa" ,(s-concat bs/org-agenda-icons-path "family.png") nil nil :ascent center)
                                    ("Articles" ,(s-concat bs/org-agenda-icons-path "reading.png") nil nil :ascent center)
                                    ("ATP" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Backstage" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Books" ,(s-concat bs/org-agenda-icons-path "reading.png") nil nil :ascent center)
                                    ("Blog" ,(s-concat bs/org-agenda-icons-path "programming.png") nil nil :ascent center)
                                    ("Divya" ,(s-concat bs/org-agenda-icons-path "family.png") nil nil :ascent center)
                                    ("Dora" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Emacs" ,(s-concat bs/org-agenda-icons-path "emacs.png") nil nil :ascent center)
                                    ("Habit" ,(s-concat bs/org-agenda-icons-path "habit.png") nil nil :ascent center)
                                    ("Home" ,(s-concat bs/org-agenda-icons-path "home.png") nil nil :ascent center)
                                    ("Inbox" ,(s-concat bs/org-agenda-icons-path "inbox.png") nil nil :ascent center)
                                    ("Leadership" ,(s-concat bs/org-agenda-icons-path "leadership.png") nil nil :ascent center)
                                    ("Learning" ,(s-concat bs/org-agenda-icons-path "learning.png") nil nil :ascent center)
                                    ("Linux" ,(s-concat bs/org-agenda-icons-path "linux.png") nil nil :ascent center)
                                    ("MarcoTL" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Metrics" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("OrgMode" ,(s-concat bs/org-agenda-icons-path "orgmode.png") nil nil :ascent center)
                                    ("Papers" ,(s-concat bs/org-agenda-icons-path "reading.png") nil nil :ascent center)
                                    ("Personal" ,(s-concat bs/org-agenda-icons-path "personal.png") nil nil :ascent center)
                                    ("PLConcepts" ,(s-concat bs/org-agenda-icons-path "writing.png") nil nil :ascent center)
                                    ("PolarisDeco" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Programming" ,(s-concat bs/org-agenda-icons-path "programming.png") nil nil :ascent center)
                                    ("Royale" ,(s-concat bs/org-agenda-icons-path "building.png") nil nil :ascent center)
                                    ("Reading" ,(s-concat bs/org-agenda-icons-path "reading.png") nil nil :ascent center)
                                    ("RLedger" ,(s-concat bs/org-agenda-icons-path "ledger.png") nil nil :ascent center)
                                    ("Slides" ,(s-concat bs/org-agenda-icons-path "reading.png") nil nil :ascent center)
                                    ("Team" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("TW" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Vacation" ,(s-concat bs/org-agenda-icons-path "vacation.png") nil nil :ascent center)
                                    ("Watching" ,(s-concat bs/org-agenda-icons-path "learning.png") nil nil :ascent center)
                                    ("Work" ,(s-concat bs/org-agenda-icons-path "tw.png") nil nil :ascent center)
                                    ("Writing" ,(s-concat bs/org-agenda-icons-path "writing.png") nil nil :ascent center)))
  ;; Org Id Configuration
  (add-to-list 'org-modules 'org-id)
  (add-to-list 'org-modules 'org-depend)
  (setq org-id-link-to-org-use-id t
        org-id-locations-file (concat bs/emacs-cache-directory ".org-id-locations"))
  ;; Look and Feel Configuration
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (custom-theme-set-faces
   'user
   ;; configure overall variable pitch and fixed pitch fonts
   '(variable-pitch ((t (:family "SF Pro Text" :height 130))))
   '(fixed-pitch ((t (:family "Monospace" :weight normal :height 109)))))
  (diminish 'buffer-face-mode))

(use-package org-habit
  :ensure nil
  :config
  (add-to-list 'org-modules 'org-habit)

  (setq org-habit-preceding-days 6
        org-habit-following-days 7)

  ;; Length of the habit graph
  (setq org-habit-graph-column 65))

(use-package org-bullets
  :after org)

(use-package org-protocol
  :after org
  :ensure nil)

(use-package org-checklist
  :after org
  :ensure nil)

(use-package org-capture
  :ensure nil
  :after org
  :bind
  (("C-. c" . org-capture))
  :config
  (setq
   org-capture-templates
   `(("t" "Todo [inbox]" entry
      (file+headline ,(s-concat bs/nextcloud-path "gtd/life.org") "Inbox")
      "* TODO %i%? \n:PROPERTIES:\n:CREATED:  %U\n:END:" :prepend t)
     ("c" "Org Protocol Capture" entry
      (file+headline ,(s-concat bs/nextcloud-path "gtd/life.org") "Reading")
      "* TODO Read: %:description \n:PROPERTIES:\n:CREATED:  %U\n:URL: %l\n:END:")
     ("b" "Break" entry
      (file+olp ,(s-concat bs/nextcloud-path "gtd/life.org") "Work" "Time Keeping" "Breaks")
      "* BREAK %? \n:PROPERTIES:\n:CREATED:  %U\n:END:" :clock-in t :clock-keep nil :clock-resume t :unnarrowed t)
     ("i" "Interruption" entry
      (file+olp ,(s-concat bs/nextcloud-path "gtd/life.org") "Work" "Time Keeping" "Interruptions")
      "* INTERRUPTION %? \n:PROPERTIES:\n:CREATED:  %U\n:END:" :clock-in t :clock-keep nil :clock-resume t :unnarrowed t)
     ("m" "Meeting" entry
      (file+olp ,(s-concat bs/nextcloud-path "gtd/life.org") "Work" "Time Keeping" "Meetings")
      "* MEETING %? \n:PROPERTIES:\n:CREATED:  %U\n:END:" :clock-in t :clock-keep nil :clock-resume t :unnarrowed t)
     ("p" "Phone call" entry
      (file+olp ,(s-concat bs/nextcloud-path "gtd/life.org") "Work" "Time Keeping" "Phone Calls")
      "* PHONECALL %? \n:PROPERTIES:\n:CREATED:  %U\n:END:" :clock-in t :clock-keep nil :clock-resume t :unnarrowed t))))

(defun bs/org-set-created-property ()
  "Set a property on the entry for creation time."
  (interactive)
  (let* ((created "CREATED")
         (now  (format-time-string (org-time-stamp-format t t))))
    (unless (org-entry-get (point) created) nil
            (org-set-property created now))))

;; Below three functions are taken from: https://github.com/zaeph/.emacs.d/blob/master/init.el
(defun bs/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun bs/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (bs/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun bs/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (bs/org-set-time-file-property "LAST_MODIFIED")))

(defun bs/org-capture-hook ()
  "My hooks for Org Capture."
  (org-id-get-create)
  (bs/org-set-created-property))

(defun bs/org-insert-props-for-all-entries ()
  "Insert my properties for all entries in the current file."
  (interactive)
  (org-map-entries 'bs/org-capture-hook))

(defun bs/org-insert-prop-for-current-entry ()
  "Insert ID and Created time for entry at point."
  (interactive)
  (save-excursion
    (bs/org-capture-hook)))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (and
   (string= "TODO" (org-get-todo-state))
   (not (org-agenda-skip-entry-if
         (quote scheduled) (quote deadline)
         (quote regexp) "\n]+>"))))

(defun bs/org-skip-learning-and-based-on-context ()
  "Skips all tasks in Learning category and based on context."
  (save-restriction
    (widen)
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (or (bs/org-include-or-skip-learning-actions-only nil)
              (bs/org-skip-based-on-context))
          next-headline
        nil))))

(defun bs/org-include-or-skip-learning-actions-only (include)
  "Includes only Learning actions when INCLUDE is t. Skips them if
INCLUDE is nil."
  (save-restriction
    (widen)
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (category (org-get-category))
           (learning-category-list '("Learning" "Books" "Papers" "Articles" "Slides" "Reading" "Watching")))
      (cond
       ((and include (-contains? learning-category-list category))
        nil)
       ((and include (not (-contains? learning-category-list category)))
        next-headline)
       ((and (not include) (not (-contains? learning-category-list category)))
        nil)
       ((and (not include) (-contains? learning-category-list category))
        next-headline)
       (t nil)))))

(defun bs/org-skip-based-on-context ()
  "Skips an entry based on whether I'm at work or home."
  (save-restriction
    (widen)
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (tags (mapcar (lambda (tag) (when tag (progn (remove-text-properties 0 (length tag) '(inherited) tag) tag))) (org-get-tags))))
      (when tags
        (cond
         ((and bs/at-work (-contains? tags "@work")) nil)
         ((and bs/at-work (-contains? tags "@home")) next-headline)
         ((and (not bs/at-work) (-contains? tags "@home")) nil)
         ((and (not bs/at-work) (-contains? tags "@work")) next-headline)
         (t nil))))))

;; Below ideas borrowed and modified from
;; http://doc.norang.ca/org-mode.html#Clocking

(setq bs/keep-clock-running nil)

(defun bs/punch-in ()
  (interactive)
  (setq bs/keep-clock-running t)
  (bs/clock-in-organization-task-as-default))

(defun bs/punch-out ()
  (interactive)
  (setq bs/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defvar bs/regular-work-task-id "459d3dcc-a976-4c10-8bc1-fa63ad1cdb73")
(defun bs/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bs/regular-work-task-id 'marker)
    (org-show-subtree)
    (let* ((today (format-time-string "%A, %B %e, %Y"))
           (next-headline (save-excursion (or (org-forward-heading-same-level 1 t) (point-max))))
           (today-clock-position
            (re-search-forward (format "^[\\*]* %s$" today) next-headline t)))
      (if today-clock-position
          (org-with-point-at today-clock-position
            (org-clock-in '(16)))
        (org-insert-heading-respect-content)
        (org-metaright)
        (insert today)
        (org-clock-in '(16))))))

(defun bs/clock-out-maybe ()
  (when (and bs/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (cond
     ((marker-buffer org-clock-interrupted-task)
      (org-with-point-at org-clock-interrupted-task
        (org-clock-in '(16))))
     ((marker-buffer org-clock-default-task)
      (bs/clock-in-organization-task-as-default))
     (t t))))

(add-hook 'org-clock-out-hook 'bs/clock-out-maybe 'append)

(defun bs/format-entry-scheduled-deadline-time ()
  "Formats scheduled/deadline time of current entry, if present, in
HH:MM format. Deadline time gets preference if it exists."
  (interactive)
  (let ((deadline (org-get-deadline-time (point)))
        (scheduled (org-get-scheduled-time (point))))
    (cond
     (scheduled (format-time-string "%H:%M" scheduled))
     (deadline (format-time-string "%H:%M" deadline))
     (t ""))))

(provide 'init-org)
;;; init-org.el ends here
