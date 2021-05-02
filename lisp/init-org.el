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

(defvar balaji/nextcloud-path)
(setq balaji/nextcloud-path "/media/backup/Nextcloud/")

(use-package org
  :ensure org-plus-contrib
  :diminish
  (org-indent-mode)
  :bind
  (("C-. a" . org-agenda)
   ("C-. r" . org-archive-subtree)
   ("C-. l" . org-store-link)
   :map org-mode-map
   ("C-. i" . balaji/org-insert-prop-for-current-entry)
   ("C-. i" . org-insert-link))
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-bullets-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . company-mode)
         (before-save . balaji/org-set-last-modified))
  :config
  (setq
   org-todo-keywords
   '((sequence "TODO(t@/!)"
               "PROJECT(P@/!)"
               "DELEGATED(D@/!)"
               "|"
               "DONE(d@/!)"
               "CANCELLED(x@/!)"))
   org-agenda-files (list (s-concat balaji/nextcloud-path "gtd/inbox.org")
                          (s-concat balaji/nextcloud-path "gtd/routines.org")
                          (s-concat balaji/nextcloud-path "gtd/projects.org")
                          (s-concat balaji/nextcloud-path "gtd/personal.org")
                          (s-concat balaji/nextcloud-path "gtd/work.org"))
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
   org-archive-location (s-concat balaji/nextcloud-path "gtd/archives.org::")
   org-agenda-custom-commands
   '(("w" "At Work" tags-todo "@work+@next|@phone|"
      ((org-agenda-overriding-header "Next Actions At Work")))
     ("h" "At Home" tags-todo "@home+@next|@phone"
      ((org-agenda-overriding-header "Next Actions At Home")))
     ("D" "Delegated Tasks" todo "DELEGATED"
      ((org-agenda-overriding-header "Delegated Tasks:")))
     ("i" "Inbox" todo "TODO"
      ((org-agenda-files (list (s-concat balaji/nextcloud-path "gtd/inbox.org")))
       (org-agenda-overriding-header "To Refile"))))
   org-tag-persistent-alist '(("@home" . ?h)
                              ("@work" . ?w)
                              ("@next" . ?n)
                              ("@reading" . ?r)
                              ("@quick" . ?q))
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets (quote ((org-agenda-files :todo . "PROJECT")))
   org-agenda-window-setup 'only-window
   org-agenda-todo-ignore-scheduled t
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-hide-tags-regexp "\\|@work\\|@home\\|@next")
  (add-to-list 'org-modules 'org-id)
  (setq org-agenda-tags-column 110
        org-ellipsis "  "
        org-id-link-to-org-use-id t
        org-use-speed-commands t
        org-hide-emphasis-markers t
        org-special-ctrl-k t
        org-M-RET-may-split-line nil
        org-ctrl-k-protect-subtree t
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-bookmark-names-plist nil)
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (custom-theme-set-faces
   'user
   ;; configure overall variable pitch and fixed pitch fonts
   '(variable-pitch ((t (:family "Gentium Book Basic" :height 130))))
   '(fixed-pitch ((t (:family "NotoSansMono" :height 116)))))
  (diminish 'buffer-face-mode))

(use-package org-bullets
  :after org)

(use-package org-protocol
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
      (file ,(s-concat balaji/nextcloud-path "gtd/inbox.org"))
      "* TODO %i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("p" "Project" entry
      (file ,(s-concat balaji/nextcloud-path "gtd/projects.org"))
      "* PROJECT %i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("c" "Org Protocol Capture" entry
      (file ,(s-concat balaji/nextcloud-path "gtd/inbox.org"))
      "* TODO Read: %:description    :@home@:@next:@reading:
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:URL: %l
:END:"))))

(use-package org-roam
  :diminish (org-roam-mode)
  :hook ((after-init . org-roam-mode))
  :bind
  ("C-. i" . org-roam-insert)
  ("C-. C" . org-roam-find-file)
  :config
  (setq
   org-roam-directory (s-concat balaji/nextcloud-path "Notes/")
   org-roam-db-location "~/.org-roam.db"
   org-roam-db-gc-threshold most-positive-fixnum
   org-roam-graph-exclude-matcher '("private")
   org-roam-index-file "index.org"
   org-roam-completion-system 'ivy
   org-roam-capture-templates
   `(("d" "default" plain
      (function org-roam-capture--get-point)
      "%?"
      :file-name "%<%Y%m%d%H%M%S>-${slug}"
      :head "#+title: ${title}\n#+id: %(shell-command-to-string \"uuidgen\")#+created: %U\n#+last_modified: %U\n\n"
      :unnarrowed t)
     ("b" "book" plain
      (function org-roam-capture--get-point)
      "%?"
      :file-name "books/${slug}"
      :head "#+title: ${title}\n#+id: %(shell-command-to-string \"uuidgen\")#+created: %U\n#+last_modified: %U\n\n"
      :unnarrowed t)
     ("t" "talk" plain
      (function org-roam-capture--get-point)
      "%?"
      :file-name "talks/${slug}"
      :head "#+title: ${title}\n#+id: %(shell-command-to-string \"uuidgen\")#+created: %U\n#+last_modified: %U\n\n"
      :unnarrowed t)
     ("p" "paper" plain
      (function org-roam-capture--get-point)
      "%?"
      :file-name "papers/${slug}"
      :head "#+title: ${title}\n#+id: %(shell-command-to-string \"uuidgen\")#+created: %U\n#+last_modified: %U\n\n"
      :unnarrowed t)
     ("p" "private" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "private/${slug}"
      :head "#+title: ${title}\n#+id: %(shell-command-to-string \"uuidgen\")#+created: %U\n#+last_modified: %U\n\n"
      :unnarrowed t))))

(use-package deft
  :after org
  :bind
  ("C-. s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (s-concat balaji/nextcloud-path "Notes/")))

(defun balaji/org-set-created-property ()
  "Set a property on the entry for creation time."
  (interactive)
  (let* ((created "CREATED")
         (now  (format-time-string (org-time-stamp-format t t))))
    (unless (org-entry-get (point) created) nil
            (org-set-property created now))))

;; Below three functions are taken from: https://github.com/zaeph/.emacs.d/blob/master/init.el
(defun balaji/org-find-time-file-property (property &optional anywhere)
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

(defun balaji/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (balaji/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun balaji/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (balaji/org-set-time-file-property "LAST_MODIFIED")))

(defun balaji/org-capture-hook ()
  "My hooks for Org Capture."
  (org-id-get-create)
  (balaji/org-set-created-property))

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
