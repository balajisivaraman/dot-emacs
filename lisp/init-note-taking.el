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
(setq bs/notes-path (s-concat bs/nextcloud-path "Notes/bibliography"))

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

(use-package citar
  :custom
  (org-cite-global-bibliography `(bs/bibfile-path))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
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
  :diminish (org-roam-mode)
  :init
  (setq org-roam-v2-ack t)
  (setq
   org-roam-directory (s-concat bs/nextcloud-path "Notes/")
   org-roam-db-location "~/.org-roam.db"
   org-roam-db-gc-threshold most-positive-fixnum
   org-roam-graph-exclude-matcher '("private")
   org-roam-index-file "index.org"
   org-roam-completion-system 'ivy
   org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :if-new
      (file+head
       "%<%Y%m%d%H%M%S>-${slug}.org"
       ":PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):END:
#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)
     ("t" "talk" plain
      "%?"
      :if-new
      (file+head
       "talks/${slug}.org"
       ":PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):END:
#+TITLE: Talk: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)
     ("n" "ref + noter" plain
      "%?"
      :if-new
      (file+head
       "bibliography/${citekey}.org"
       ":PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):ROAM_REFS: ${ref}
:END:
#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n

- tags ::
- keywords :: ${keywords}

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
      :if-new
      (file+head
       "bibliography/${citekey}.org"
       ":PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):ROAM_REFS: ${ref}
:END:
#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n

- tags ::
- keywords :: ${keywords}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :URL: ${url}
  :AUTHOR: ${author}
  :END:\n\n")
      :unnarrowed t)
     ("p" "private" plain
      "%?"
      :if-new
      (file+head
       "private/${slug}.org"
       ":PROPERTIES:
:ID: %(shell-command-to-string \"uuidgen\"):END:
#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
      :unnarrowed t)))
  :bind
  ("M-n c" . org-roam-capture)
  ("M-n i" . org-roam-node-insert)
  ("M-n f" . org-roam-node-find)
  ("M-n r" . org-roam-node-random)
  :config
  (org-roam-setup))

(use-package deft
  :after org
  :bind
  ("M-n s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (s-concat bs/nextcloud-path "Notes/")))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :diminish (org-roam-bibtex-mode)
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
