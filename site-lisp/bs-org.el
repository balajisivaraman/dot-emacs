;;; bs-org.el --- Balaji Sivaraman's Org Customizations. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Balaji Sivaraman

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

;; My Org Mode customisations.

;;; Code:

(require 'org)

;;;###autoload
(defun bs/org-set-created-property ()
  "Set a property on the entry for creation time."
  (interactive)
  (let* ((created "CREATED")
         (now  (format-time-string (org-time-stamp-format t t))))
    (unless (org-entry-get (point) created) nil
            (org-set-property created now))))

;; Below three functions are taken from: https://github.com/zaeph/.emacs.d/blob/master/init.el

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun bs/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (bs/org-set-time-file-property "LAST_MODIFIED")))

;;;###autoload
(defun bs/org-capture-hook ()
  "My hooks for Org Capture."
  (org-id-get-create)
  (bs/org-set-created-property))

;;;###autoload
(defun bs/org-insert-props-for-all-entries ()
  "Insert my properties for all entries in the current file."
  (interactive)
  (org-map-entries 'bs/org-capture-hook))

;;;###autoload
(defun bs/org-insert-prop-for-current-entry ()
  "Insert ID and Created time for entry at point."
  (interactive)
  (save-excursion
    (bs/org-capture-hook)))

;;;###autoload
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

;;;###autoload
(defun org-current-is-todo ()
  (and
   (string= "TODO" (org-get-todo-state))
   (not (org-agenda-skip-entry-if
         (quote scheduled) (quote deadline)
         (quote regexp) "\n]+>"))))

;;;###autoload
(defun bs/org-skip-learning-and-based-on-context ()
  "Skips all tasks in Learning category and based on context."
  (save-restriction
    (widen)
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (or (bs/org-include-or-skip-learning-actions-only nil)
              (bs/org-skip-based-on-context))
          next-headline
        nil))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun bs/punch-in ()
  (interactive)
  (setq bs/keep-clock-running t)
  (bs/clock-in-organization-task-as-default))

;;;###autoload
(defun bs/punch-out ()
  (interactive)
  (setq bs/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defvar bs/regular-work-task-id "459d3dcc-a976-4c10-8bc1-fa63ad1cdb73")
;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun bs/get-category-and-icon-at-point ()
  "Returns tuple of category and category-icon of the Org node at
point."
  (let* ((category-name (org-get-category))
         (category-icon-name (org-entry-get (point) "CATEGORY_ICON")))
    (when category-icon-name
      `(,category-name . ,category-icon-name))))

;;;###autoload
(defun bs/get-category-icons-alist (agenda-file)
  "This reads the list of categories and their icons from
AGENDA-FILE and returns an alist of categories and their icons.

Categories without icons are skipped by default."
  (save-window-excursion
    (let* ((buffer (find-file-noselect agenda-file))
           (category-names-with-icons (remove nil (with-current-buffer buffer
                                                    (goto-char (point-min))
                                                    (widen)
                                                    (cl-loop until (eobp) do
                                                             (outline-next-heading)
                                                             collect (bs/get-category-and-icon-at-point)))))
           (category-icon-alist (mapcar (lambda (pair) `(,(car pair) ,(s-concat bs/org-agenda-icons-path (cdr pair)) nil nil :ascent center)) category-names-with-icons)))
      category-icon-alist)))

;;;###autoload
(defun bs/set-org-agenda-category-icon-alist ()
  (setq org-agenda-category-icon-alist (append (cl-loop for agenda-file in (org-agenda-files)
                                                        append (bs/get-category-icons-alist agenda-file))
                                               `(("Routines" ,(s-concat bs/org-agenda-icons-path "habit.png") nil nil :ascent center)))))

(provide 'bs-org)
;;; bs-org.el ends here
