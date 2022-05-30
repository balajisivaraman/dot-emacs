;;; init-note-taking.el --- Configuration for my note-taking -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

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

;; This file configures Org Roam, Org Roam Bibtex, Ivy Bibtex, Org
;; Noter and other note-taking utilities.

;;; Code:

(defvar bs/bibfile-path)
(setq bs/bibfile-path "~/.zotero/zotLib.bib")
(defvar bs/notes-path)
(setq bs/notes-path (s-concat bs/nextcloud-path "TheSacredTexts/6.BibNotes"))

(defvar bs/org-roam-map)
(define-prefix-command 'bs/org-roam-map)
(global-set-key (kbd "M-n") 'bs/org-roam-map)

(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package bibtex-completion
  :defer t
  :custom
  (bibtex-completion-notes-path bs/notes-path)
  (bibtex-completion-bibliography bs/bibfile-path)
  (bibtex-completion-pdf-field "file"))

(use-package oc
  :ensure nil
  :custom
  (org-cite-global-bibliography `(,bs/bibfile-path))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package citar
  :after oc
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  (citar-open-note-functions '(orb-citar-edit-note))
  (citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                     (note . "Notes on ${author editor}, ${title}")))
  (citar-notes-paths `(,bs/notes-path))
  (bibtex-dialect 'biblatex)
  (citar-symbols `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
                   (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
                   (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  :bind
  (("C-c b" . citar-insert-citation)
   (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
   :map minibuffer-local-map
   ("M-b" . citar-insert-preset)))

(use-package org-roam
  :init
  (setq
   org-roam-directory (s-concat bs/nextcloud-path "TheSacredTexts/")
   org-roam-db-location (s-concat bs/emacs-cache-directory ".org-roam.db")
   org-roam-db-gc-threshold most-positive-fixnum
   org-roam-capture-templates
   '(("n" "ref + noter" plain
      "%?"
      :target
      (file+head
       "6.BibNotes/${citekey}.org"
       "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${ref}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :URL: ${url}
  :AUTHOR: ${author}
  :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
  :NOTER_PAGE:
  :END:\n\n")
      :unnarrowed t
      :immediate-finish t))
   org-roam-capture-ref-templates
   '(("p" "protocol" plain "* ${body}"
      :target
      (file+head
       "0.Inbox/${slug}.org"
       "\n\n#+title: ${title}\n\n")
      :unnarrowed t)))
  :bind
  ("M-n c a" . bs/capture-new-area)
  ("M-n c p" . bs/capture-new-project)
  ("M-n c r" . bs/capture-new-resource)
  ("M-n c R" . bs/capture-new-project-reference)
  ("M-n i"   . org-roam-node-insert)
  ("M-n f a" . bs/find-para-area)
  ("M-n f n" . org-roam-node-find)
  ("M-n f p" . bs/find-para-project)
  ("M-n f r" . bs/find-para-resource)
  ("M-n r"   . org-roam-node-random)
  ("M-n x"   . bs/exclude-current-node)
  :config
  (org-roam-setup)
  (defun bs/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))
  (defun bs/create-new-para-tasks-category (group title)
    "Create a new category subtree named TITLE under parent subtree
named GROUP."
    (save-window-excursion
      (let* ((buffer (find-file-noselect (concat bs/nextcloud-path "TheSacredTexts/5.Tasks/PARA.org"))))
        (with-current-buffer buffer
          (progn
            (goto-char (point-min))
            (widen)
            (re-search-forward (format "^* %s" group))
            (org-narrow-to-subtree)
            (goto-char (point-max))
            (insert "\n")
            (insert (concat "** " title "\n:PROPERTIES:\n:Category:   " title "\n:END:\n"))
            (widen))))))
  (defun bs/capture-new-project ()
    (interactive)
    (let* ((node (org-roam-node-read nil (bs/org-roam-filter-by-tag "Project") nil nil "Enter Project Title: "))
           (title (org-roam-node-title node))
           (path (concat bs/nextcloud-path "TheSacredTexts/1.Projects/" title "/")))
      (unless (file-directory-p path)
        (dired-create-directory path))
      (org-roam-capture- :node node
                         :templates '(("p" "project" plain
                                       "%?"
                                       :target
                                       (file+head
                                        "1.Projects/${title}/Index.org"
                                        "#+title: ${title}\n#+filetags: Project\n\n* Objectives\n\n* References\n\n* Notes\n")
                                       :unnarrowed t
                                       :immediate-finish t)))
      (bs/create-new-para-tasks-category "PROJECTS" title)))
  (defun bs/capture-new-area ()
    (interactive)
    (let* ((node (org-roam-node-read nil (bs/org-roam-filter-by-tag "Area") nil nil "Enter Area Title: "))
           (title (org-roam-node-title node))
           (path (concat bs/nextcloud-path "TheSacredTexts/2.Areas/" title "/")))
      (unless (file-directory-p path)
        (dired-create-directory path))
      (org-roam-capture- :node node
                         :templates '(("a" "area" plain
                                       "%?"
                                       :target
                                       (file+head
                                        "2.Areas/${title}/Index.org"
                                        "#+title: ${title}\n#+filetags: Area\n\n* References\n\n* Notes\n\n* Linked Projects\n")
                                       :unnarrowed t
                                       :immediate-finish t)))
      (bs/create-new-para-tasks-category "AREAS" title)))
  (defun bs/capture-new-resource ()
    (interactive)
    (let* ((node (org-roam-node-read nil (bs/org-roam-filter-by-tag "Resource") nil nil "Enter Resource Title: "))
           (title (org-roam-node-title node))
           (path (concat bs/nextcloud-path "TheSacredTexts/3.Resources/" title "/")))
      (unless (file-directory-p path)
        (dired-create-directory path))
      (org-roam-capture- :node node
                         :templates '(("r" "resource" plain
                                       "%?"
                                       :target
                                       (file+head
                                        "3.Resources/${title}/Index.org"
                                        "#+title: ${title}\n#+filetags: Resource\n\n* References\n\n* Notes\n")
                                       :unnarrowed t
                                       :immediate-finish t)))
      (bs/create-new-para-tasks-category "RESOURCES" title)))
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
  (defun bs/exclude-current-node ()
    "Exclude node at point."
    (interactive)
    (org-set-property "ROAM_EXCLUDE" "t"))
  (defun bs/find-para-node (type)
    "Find P.A.R.A node of TYPE using `org-roam-node-find'."
    (interactive)
    (org-roam-node-find nil nil (bs/org-roam-filter-by-tag type) nil))
  (defun bs/find-para-project ()
    (interactive)
    (bs/find-para-node "Project"))
  (defun bs/find-para-area ()
    (interactive)
    (bs/find-para-node "Area"))
  (defun bs/find-para-resource ()
    (interactive)
    (bs/find-para-node "Resource")))

(use-package org-roam-protocol
  :ensure nil
  :defer 20)

(use-package deft
  :after org
  :bind
  ("M-n s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (s-concat bs/nextcloud-path "TheSacredTexts/")))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-roam-ref-format 'org-cite)
  :config
  (setq
   orb-preformat-keywords
   '("citekey" "entry-type" "date" "pdf?" "note?" "file" "author" "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev" "keywords" "url")))

(use-package org-noter
  :after (:any org pdf-view)
  :bind
  ("M-n n" . org-noter)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'horizontal-split
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list bs/notes-path)
   ;; Auto save location in PDF
   org-noter-auto-save-last-location t))

(provide 'init-note-taking)
;;; init-note-taking.el ends here
