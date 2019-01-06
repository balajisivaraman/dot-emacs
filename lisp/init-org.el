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

(defvar balaji/gtd-files-path)
(setq balaji/gtd-files-path "/media/backup/Owncloud/gtd/")

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
  :config
  (when balaji-evil-mode-enabled
    (evil-set-initial-state 'org-agenda-mode 'normal))
  (setq
   org-todo-keywords
   '((sequence "TODO(t@/!)"
               "WAITING(w@/!)"
               "DELEGATED(D@/!)"
               "|"
               "DONE(d@/!)"
               "CANCELLED(x@/!)"))
   org-agenda-files (list (s-concat balaji/gtd-files-path "inbox.org")
                          (s-concat balaji/gtd-files-path "routines.org")
                          (s-concat balaji/gtd-files-path "projects.org")
                          (s-concat balaji/gtd-files-path "tickler.org"))
   org-archive-location (s-concat balaji/gtd-files-path "archives.org::")
   org-agenda-ndays 21
   ;; below setting lists all unscheduled tasks as stuck
   org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:")
   org-deadline-warning-days 14
   org-agenda-show-all-dates t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-reverse-note-order nil
   org-confirm-elisp-link-function nil
   org-log-done 'note
   org-agenda-custom-commands
   '(("w" "At Work" tags-todo "@work|@phone"
      ((Org-agenda-overriding-header "Work")
       (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
     ("h" "At Home" tags-todo "@home|@phone"
      ((org-agenda-overriding-header "Home")
       (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
     ("W" "Waiting For" todo "WAITING"
      ((org-agenda-overriding-header "Waiting For"))))
   org-tag-alist '(("@computer" . ?c)
                   ("@email" . ?e)
                   ("@home" . ?h)
                   ("@phone" . ?p)
                   ("@work" . ?w))
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps t
   org-refile-targets `((,(s-concat balaji/gtd-files-path "projects.org") :maxlevel . 3)
                        (,(s-concat balaji/gtd-files-path "someday.org") :level . 1)
                        (,(s-concat balaji/gtd-files-path "tickler.org") :maxlevel . 2))
   org-agenda-window-setup 'only-window)
  (add-to-list 'org-modules 'org-habit)
  (setq org-agenda-tags-column 110))

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
   `(("t" "Todo [inbox]" entry
      (file ,(s-concat balaji/gtd-files-path "inbox.org"))
      "* TODO %i%?")
     ("T" "Tickler" entry
      (file ,(s-concat balaji/gtd-files-path "tickler.org"))
      "* %i%?"))))

(defun balaji/org-mode-hook ()
  "My hooks for Org Mode."
  (org-bullets-mode t)
  (company-mode t))

(defun balaji/org-set-created-property ()
  "Set a property on the entry for creation time."
  (interactive)
  (let* ((created "CREATED")
         (now  (format-time-string (org-time-stamp-format t t))))
    (unless (org-entry-get (point) created) nil
           (org-set-property created now))))

(defun balaji/org-capture-hook ()
  "My hooks for Org Capture."
  ;; (org-id-get-create)
  (balaji/org-set-created-property))

(add-hook 'org-mode-hook 'balaji/org-mode-hook)
(add-hook 'org-capture-prepare-finalize-hook 'balaji/org-capture-hook)

(defun balaji/org-insert-props-for-all-entries ()
  "Insert my properties for all entries in the current file."
  (interactive)
  (org-map-entries 'balaji/org-capture-hook))

(defun balaji/org-insert-prop-for-current-entry ()
  "Insert ID and Created time for entry at point."
  (interactive)
  (save-excursion
    (balaji/org-capture-hook)))

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

(provide 'init-org)
;;; init-org.el ends here
