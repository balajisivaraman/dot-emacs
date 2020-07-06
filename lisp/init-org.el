;;; init-org.el --- Org Mode Customizations. -*- lexical-binding: t -*-

;; Copyright (C) 2020  Balaji Sivaraman

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

(defvar balaji/nextcloud-path)
(setq balaji/nextcloud-path "/media/backup/Nextcloud/")

(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-. a" . org-agenda)
   ("C-. r" . org-archive-subtree)
   :map org-mode-map
   ("C-. i" . balaji/org-insert-prop-for-current-entry))
  :config
  (setq
   org-todo-keywords
   '((sequence "TODO(t@/!)"
               "PROJECT(P@/!)"
               "|"
               "DONE(d@/!)"
               "CANCELLED(x@/!)"))
   org-agenda-files (list (s-concat balaji/nextcloud-path "gtd/inbox.org")
                          (s-concat balaji/nextcloud-path "gtd/routines.org")
                          (s-concat balaji/nextcloud-path "gtd/projects.org")
                          (s-concat balaji/nextcloud-path "gtd/family.org")
                          (s-concat balaji/nextcloud-path "gtd/tw.org"))
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
   '(("w" "At Work" tags-todo "@work+@next_actions|@phone|"
      ((org-agenda-overriding-header "Next Actions At Work")))
     ("h" "At Home" tags-todo "@home+@next_actions|@phone"
      ((org-agenda-overriding-header "Next Actions At Home"))))
   org-tag-persistent-alist '(("@home" . ?h)
                              ("@work" . ?w)
                              ("@next_actions" . ?n))
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets (quote ((org-agenda-files :todo . "PROJECT")))
   org-agenda-window-setup 'only-window
   org-agenda-todo-ignore-scheduled t
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-hide-tags-regexp "\\|@work\\|@home\\|@next_actions")
  (add-to-list 'org-modules 'org-id)
  (setq org-agenda-tags-column 110))

(use-package org-bullets
  :after org)

(use-package org-capture
  :ensure nil
  :after org
  :bind
  (("C-. c" . org-capture))
  :config
  (setq
   org-capture-templates
   `(("t" "Todo [inbox]" entry
      (file ,(s-concat balaji/nextcloud-path "gtd/inbox.org"))
      "* TODO %i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t))))

(use-package org-roam
  :init
  (add-hook 'after-init-hook 'org-roam-mode)
  :bind
  ("C-. i" . org-roam-insert)
  :config
  (setq
   org-roam-directory (s-concat balaji/nextcloud-path "notes/")
   org-roam-index-file "index.org"
   org-roam-completion-system 'ivy))

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
  (org-id-get-create)
  (balaji/org-set-created-property))

(add-hook 'org-mode-hook 'balaji/org-mode-hook)

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
