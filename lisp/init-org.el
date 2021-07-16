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
   :map org-mode-map
   ("C-. i" . bs/org-insert-prop-for-current-entry)
   ("C-. i" . org-insert-link))
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-bullets-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . company-mode)
         (before-save . bs/org-set-last-modified))
  :config
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CXLD(x!)")
     (sequence "PROJ(p)" "|" "DONE(d!)"))
   org-agenda-files (list (s-concat bs/nextcloud-path "gtd/life.org"))
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
   org-log-done 'time
   org-archive-location (s-concat bs/nextcloud-path "gtd/archives.org::")
   org-agenda-custom-commands
   '(("p" "Projects" todo "NEXT"
      ((org-agenda-overriding-header "Project Next Actions")))
     ("i" "Inbox" tags-todo "CATEGORY=\"Inbox\"TODO=\"TODO\""
      ((org-agenda-overriding-header "To Refile"))))
   org-tag-persistent-alist '(("@home" . ?h)
                              ("@work" . ?w)
                              ("@next" . ?n)
                              ("@reading" . ?r)
                              ("@quick" . ?q))
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets '((org-agenda-files :maxlevel . 9))
   org-agenda-window-setup 'only-window
   org-agenda-todo-ignore-scheduled t
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-hide-tags-regexp "\\|@work\\|@home\\|@next")
  (add-to-list 'org-modules 'org-id)
  (setq org-agenda-tags-column 110
        org-ellipsis "  "
        org-id-link-to-org-use-id t
        org-id-locations-file (concat bs/emacs-cache-directory ".org-id-locations")
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
        org-agenda-deadline-leaders '("!D!: " "D%02d: ")
        org-agenda-use-time-grid nil
        org-agenda-scheduled-leaders '("" "S%d: ")
        org-enforce-todo-dependencies t
        org-use-fast-todo-selection 'expert
        org-use-fast-tag-selection t
        org-fast-tag-selection-single-key 'expert)
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (custom-theme-set-faces
   'user
   ;; configure overall variable pitch and fixed pitch fonts
   '(variable-pitch ((t (:family "SF Pro Text" :height 130))))
   '(fixed-pitch ((t (:family "Iosevka Custom" :weight normal :height 124))))

   ;; configure fonts for org headings and document title
   '(org-level-8 ((t (:inherit default))))
   '(org-level-7 ((t (:inherit default))))
   '(org-level-6 ((t (:inherit default))))
   '(org-level-5 ((t (:inherit default))))
   '(org-level-3 ((t (:inherit default :height 1.15))))
   '(org-level-2 ((t (:inherit default :height 1.3))))
   '(org-level-1 ((t (:inherit default :height 1.5))))
   '(org-document-title ((t (:inherit default :height 1.75 :underline nil))))

   ;; configure fonts for other org elements
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   )
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
      "* TODO %i%?
:PROPERTIES:
:CREATED:  %U
:END:" :prepend t)
     ("i" "Idea" entry
      (file+headline ,(s-concat bs/nextcloud-path "gtd/life.org") "Ideas")
      "* PROJECT %i%?
:PROPERTIES:
:CREATED:  %U
:END:" :prepend t)
     ("c" "Org Protocol Capture" entry
      (file+headline ,(s-concat bs/nextcloud-path "gtd/life.org") "Reading")
      "* TODO Read: %:description
:PROPERTIES:
:CREATED:  %U
:URL: %l
:END:"))))

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

(provide 'init-org)
;;; init-org.el ends here
