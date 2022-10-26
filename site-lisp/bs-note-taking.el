;;; bs-note-taking.el --- My Org Roam customisations. -*- lexical-binding: t -*-

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

;; This module contains my personal customisations for Org Roam and assorted packages.

;;; Code:

(require 'org-roam)

;;;###autoload
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE"
  (interactive)
  (cdr (assoc "TYPE" (org-roam-node-properties node))))

;;;###autoload
(cl-defmethod org-roam-node-project ((node org-roam-node))
  "Return the PROJECT of NODE"
  (interactive)
  (cdr (assoc "PROJECT" (org-roam-node-properties node))))

;;;###autoload
(defun bs/folderify-roam-node (node)
  "Sanitise a Roam node title for folder names."
  (string-replace ": " " - " (org-roam-node-title node)))

;;;###autoload
(defun bs/org-roam-filter-by-type (type)
  (lambda (node)
    (string-equal type (org-roam-node-type node))))

;;;###autoload
(defun bs/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

;;;###autoload
(defun bs/create-new-para-tasks-category (group title)
  "Create a new category subtree named TITLE under parent subtree
named GROUP."
  (save-window-excursion
    (let* ((buffer (find-file-noselect (concat bs/tasks-path "life.org"))))
      (with-current-buffer buffer
        (progn
          (goto-char (point-min))
          (widen)
          (re-search-forward (format "^* %s" group))
          (org-narrow-to-subtree)
          (goto-char (point-max))
          (insert "\n")
          (insert (concat "** " title "\n:PROPERTIES:\n:CATEGORY:   " title "\n:END:\n"))
          (widen))))))

;;;###autoload
(defun bs/capture-new-work-project (&optional arg)
  (interactive "P")
  (let* ((node (org-roam-node-read nil (bs/org-roam-filter-by-tag "Project") nil nil "Enter Project Title: "))
         (title (org-roam-node-title node)))
    (unless arg
      (org-roam-capture- :node node
                         :templates '(("p" "project" plain
                                       "%?"
                                       :target
                                       (file+head
                                        "${bs/folderify-roam-node}.org"
                                        ":PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):TYPE:       Project
:END:
#+title: ${title}
#+filetags: Project

* Objectives

* References

* Meetings
#+BEGIN_SRC emacs-lisp :hlines no
  (append '(hline (Meeting Date) hline) (bs/org-roam-project-meetings) '(hline))
#+END_SRC

* Notes
")
                                       :unnarrowed t
                                       :immediate-finish t))))
    (bs/create-new-para-tasks-category "PROJECTS" title)))

;;;###autoload
(defun bs/capture-new-area ()
  (interactive)
  (let* ((node (org-roam-node-read nil (bs/org-roam-filter-by-tag "Area") nil nil "Enter Area Title: "))
         (title (org-roam-node-title node))
         (path (concat bs/notes-path "2.Areas/" (bs/folderify-roam-node node) "/")))
    (unless (file-directory-p path)
      (dired-create-directory path))
    (org-roam-capture- :node node
                       :templates '(("a" "area" plain
                                     "%?"
                                     :target
                                     (file+head
                                      "${bs/folderify-roam-node}"
                                      ":PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):TYPE:       Area
:END:
#+title: ${title}
* References

* Notes

* Linked Projects
")
                                     :unnarrowed t
                                     :immediate-finish t)))
    (bs/create-new-para-tasks-category "AREAS" title)))

;;;###autoload
(defun bs/capture-new-meeting ()
  (interactive)
  (let* ((node (org-roam-node-read nil (bs/org-roam-filter-by-type "Meeting") nil nil "Enter Meeting Title: "))
         (title (org-roam-node-title node))
         (project (org-roam-node-read nil (bs/org-roam-filter-by-type "Project") nil nil "Select Project: "))
         (project-title (org-roam-node-title project))
         (project-id (org-roam-node-id project))
         (project-link (org-link-make-string (concat "id:" project-id) project-title)))
    (org-roam-capture- :node node
                       :templates '(("m" "meeting" plain
                                     "%?"
                                     :target
                                     (file+head
                                      "${bs/folderify-roam-node}.org"
                                      ":PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):TYPE:       Meeting
:PROJECT:       ${project-title}
:END:
#+title: ${title}

Project:: ${project-link}

")
                                     :unnarrowed t
                                     :immediate-finish t)))))

;;;###autoload
(defun bs/capture-new-resource ()
  (interactive)
  (let* ((node (org-roam-node-read nil (bs/org-roam-filter-by-tag "Resource") nil nil "Enter Resource Title: "))
         (title (org-roam-node-title node))
         (path (concat bs/notes-path "3.Resources/" (bs/folderify-roam-node node) "/")))
    (unless (file-directory-p path)
      (dired-create-directory path))
    (org-roam-capture- :node node
                       :templates '(("r" "resource" plain
                                     "%?"
                                     :target
                                     (file+head
                                      "${bs/folderify-roam-node}.org"
                                      "#+title: ${title}\n#+filetags: Resource\n\n* References\n\n* Notes\n")
                                     :unnarrowed t
                                     :immediate-finish t)))
    (bs/create-new-para-tasks-category "RESOURCES" title)))

;;;###autoload
(defun bs/capture-new-project-reference ()
  (interactive)
  (let* ((node (org-roam-node-read nil nil nil nil "Enter Reference Title: "))
         (project (org-roam-node-read nil (bs/org-roam-filter-by-tag "Project") nil nil "Select Project: "))
         (project-title (org-roam-node-title project)))
    (org-roam-capture- :node node
                       :templates `(("r" "reference" plain
                                     "%?"
                                     :target
                                     (file+head
                                      ,(format "1.Projects/%s/${slug}.org" project-title)
                                      "#+title: ${title}")
                                     :unnarrowed t
                                     :immediate-finish t)))
    (save-window-excursion
      (let* ((buffer (find-file-noselect (org-roam-node-file project)))
             (id (org-roam-node-id node))
             (title (org-roam-node-title node)))
        (with-current-buffer buffer
          (progn
            (goto-char (point-min))
            (widen)
            (re-search-forward "^* References")
            (org-narrow-to-subtree)
            (goto-char (point-max))
            (insert "- ")
            (insert (org-link-make-string
                     (concat "id:" id)
                     title))
            (insert "\n")
            (widen)))))))

;;;###autoload
(defun bs/org-roam-refile-node-under-project ()
  "Refiles the node at point as a reference of a project of user's
choosing."
  (interactive)
  (let* ((project-node (org-roam-node-read nil (bs/org-roam-filter-by-tag "Project") nil nil "Select Project: "))
         (current-node (org-roam-node-at-point)))
    (if (s-suffix? "inbox.org" (org-roam-node-file current-node))
        (print (format "Refiling headline node in inbox.org to project '%s'" (org-roam-node-title current-node) (org-roam-node-title project-node)))
      (let* ((new-file (concat (substring (org-roam-node-file project-node)
                                          0
                                          (s-index-of "/Index.org" (org-roam-node-file project-node)))
                               "/"
                               (substring (org-roam-node-file current-node)
                                          (+ 6 (s-index-of "Inbox/" (org-roam-node-file current-node)))
                                          (length (org-roam-node-file current-node))))))
        (print (format "Refiling file node in '%s' to project '%s'" (org-roam-node-title current-node) (org-roam-node-title project-node)))
        (rename-file (org-roam-node-file current-node) new-file)
        (kill-buffer (current-buffer))
        (find-file new-file)
        (save-window-excursion
          (let* ((buffer (find-file-noselect (org-roam-node-file project-node)))
                 (id (org-roam-node-id current-node))
                 (title (org-roam-node-title current-node)))
            (with-current-buffer buffer
              (progn
                (goto-char (point-min))
                (widen)
                (re-search-forward "^* References")
                (org-narrow-to-subtree)
                (goto-char (point-max))
                (insert "- ")
                (insert (org-link-make-string
                         (concat "id:" id)
                         title))
                (insert "\n")
                (widen)
                (save-buffer)))))))))

;;;###autoload
(defun bs/exclude-current-node ()
  "Exclude node at point."
  (interactive)
  (org-set-property "ROAM_EXCLUDE" "t"))

;;;###autoload
(defun bs/find-para-node (type)
  "Find P.A.R.A node of TYPE using `org-roam-node-find'."
  (interactive)
  (org-roam-node-find nil nil (lambda (node) (string-equal type (org-roam-node-type node))) nil))

;;;###autoload
(defun bs/find-para-project ()
  (interactive)
  (bs/find-para-node "Project"))

;;;###autoload
(defun bs/find-para-area ()
  (interactive)
  (bs/find-para-node "Area"))

;;;###autoload
(defun bs/find-para-resource ()
  (interactive)
  (bs/find-para-node "Resource"))

(provide 'bs-note-taking)
;;; bs-note-taking.el ends here
