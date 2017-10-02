;;; init-org.el --- Org Mode Customizations. -*- lexical-binding: t -*-

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

;; My customizations and additional packages for Org Mode.

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o d" . org-check-deadlines)
   ("C-c o b" . org-check-before-date)
   ("C-c o A" . org-check-after-date)
   ("C-c o r" . org-archive-subtree)
   :map org-mode-map
   ("C-c m l" . org-metaleft)
   ("C-c m r" . org-metaright))
  :init
  (advice-add 'org-agenda :after #'delete-other-windows)
  :config
  ;; (evil-set-initial-state 'org-agenda-mode 'normal)
  (setq
   org-todo-keywords
   '((sequence "TODO(t@/!)"
               "WAITING(w@/!)"
               "APPT(a!)"
               "DELEGATED(l@/!)"
               "STARTED(s!)"
               "|"
               "FEEDBACK(f@/!)"
               "VERIFY(v@/!)"
               "DONE(d@/!)"
               "DEFERRED(r@/!)"
               "CANCELLED(x@/!)"))
   org-agenda-files (quote ("~/ownCloud/Personal Notes/todo.org"))
   org-archive-location "/Users/balajisivaraman/ownCloud/Personal Notes/archives.org::"
   org-default-notes-file "~/ownCloud/Personal Notes/notes.org"
   org-agenda-ndays 21
   ;; below setting lists all unscheduled tasks as stuck
   org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:")
   org-deadline-warning-days 14
   org-agenda-show-all-dates t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-reverse-note-order t
   org-confirm-elisp-link-function nil
   org-log-done 'note)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 100))

(defun balaji/org-mode-hook ()
  "My hooks for Org Mode."
  (org-bullets-mode t)
  (company-mode t))

(use-package org-bullets
  :after org)

(use-package org-capture
  :ensure nil
  :after org
  :diminish (org-capture-mode . "ⓡ")
  :bind
  (("C-c o c" . org-capture))
  :config
  (setq
   org-capture-templates
   '(("t" "Todo" entry (file+headline "/Users/balajisivaraman/ownCloud/Personal Notes/todo.org" "Tasks"))
     ("n" "Note" entry (file "/Users/balajisivaraman/ownCloud/Personal Notes/notes.org")))))

(add-hook 'org-mode-hook 'balaji/org-mode-hook)

(provide 'init-org)
;;; init-org.el ends here
