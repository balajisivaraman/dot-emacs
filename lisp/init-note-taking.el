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
(defvar bs/bibnotes-path)
(setq bs/bibnotes-path (s-concat bs/nextcloud-path "ThePlainTextLife/notes"))

(defvar bs/org-roam-map)
(define-prefix-command 'bs/org-roam-map)
(global-set-key (kbd "M-n") 'bs/org-roam-map)

(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package bibtex-completion
  :defer t
  :custom
  (bibtex-completion-notes-path bs/bibnotes-path)
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
  (citar-notes-paths `(,bs/bibnotes-path))
  (bibtex-dialect 'biblatex)
  (citar-symbols `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
                   (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
                   (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  :bind
  (("M-n f b" . citar-open)
   ("C-c b" . citar-insert-citation)
   (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package org-roam
  :init
  (setq
   org-roam-directory bs/notes-path
   org-roam-db-location (s-concat bs/emacs-cache-directory ".org-roam.db")
   org-roam-db-gc-threshold most-positive-fixnum
   org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target
      (file+head
       "${slug}.org"
       "#+TITLE: ${title}
#+CREATED: %U
#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)
     ("n" "ref + noter" plain
      "%?"
      :target
      (file+head
       "references/${citekey}.org"
       "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${ref}
#+CREATED: %U
#+LAST_MODIFIED: %U

* ${title}
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :URL: ${url}
  :AUTHOR: ${author}
  :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
  :NOTER_PAGE:
  :END:\n\n")
      :unnarrowed t)
     ("r" "ref" plain
      "%?"
      :target
      (file+head
       "references/${citekey}.org"
       "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${ref}
#+CREATED: %U
#+LAST_MODIFIED: %U

* ${title}
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :URL: ${url}
  :AUTHOR: ${author}
  :END:\n\n")
      :unnarrowed t)))
  :bind
  ("M-n i"   . org-roam-node-insert)
  ("M-n f n" . org-roam-node-find)
  ("M-n r"   . org-roam-node-random)
  ("M-n x"   . bs/exclude-current-node)
  ("M-n f r"   . org-roam-node-random)
  :commands
  (bs/org-roam-refile-node-under-project)
  :config
  (org-roam-setup)
  (defun bs/exclude-current-node ()
    "Exclude node at point."
    (interactive)
    (org-set-property "ROAM_EXCLUDE" "t")))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package org-roam-protocol
  :ensure nil
  :defer 5)

(use-package deft
  :after org
  :bind
  ("M-n s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory bs/notes-path))

(use-package org-roam-bibtex
  :after (org-roam)
  :init
  (org-roam-bibtex-mode)
  :config
  (setq
   orb-preformat-keywords
   '("citekey" "entry-type" "date" "pdf?" "note?" "file" "author" "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev" "keywords" "url")))

(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config
  (citar-org-roam-mode)
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam Notes"
                           :category 'org-roam-node
                           :items #'citar-org-roam--get-candidates
                           :hasitems #'citar-org-roam-has-notes
                           :open #'citar-org-roam-open-note
                           :create #'orb-citar-edit-note
                           :annotate #'citar-org-roam--annotate))
  (setq citar-notes-source 'orb-citar-source))

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
   org-noter-notes-search-path (list bs/bibnotes-path)
   ;; Auto save location in PDF
   org-noter-auto-save-last-location t))

(use-package bs-note-taking
  :disabled t
  :ensure nil
  :bind
  ("M-n c a" . bs/capture-new-area)
  ("M-n c m" . bs/capture-new-meeting)
  ("M-n c p" . bs/capture-new-work-project)
  ("M-n c r" . bs/capture-new-resource)
  ("M-n c R" . bs/capture-new-project-reference)
  ("M-n f a" . bs/find-para-area)
  ("M-n f p" . bs/find-para-project)
  ("M-n f r" . bs/find-para-resource)
  ("M-n x"   . bs/exclude-current-node))

(provide 'init-note-taking)
;;; init-note-taking.el ends here
