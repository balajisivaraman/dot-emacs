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

(defvar balaji/bibfile-path)
(setq balaji/bibfile-path "~/.zotero/zotLib.bib")
(defvar balaji/notes-path)
(setq balaji/notes-path (s-concat balaji/nextcloud-path "Notes/bibliography"))

(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package ivy-bibtex
  :bind
  ("C-. b" . ivy-bibtex)
  :config
  (setq
   bibtex-completion-notes-path balaji/notes-path
   bibtex-completion-bibliography balaji/bibfile-path
   bibtex-completion-pdf-field "file"))

(use-package org-ref
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename
   org-ref-default-bibliography (list balaji/bibfile-path)
   org-ref-bibliography-notes (s-concat balaji/notes-path "/bibnotes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory balaji/notes-path
   org-ref-notes-function 'orb-edit-notes
   ))

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
     ("t" "talk" plain
      (function org-roam-capture--get-point)
      "%?"
      :file-name "talks/${slug}"
      :head "#+title: Talk: ${title}\n#+id: %(shell-command-to-string \"uuidgen\")#+created: %U\n#+last_modified: %U\n\n"
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

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq
   orb-preformat-keywords
   '("citekey" "entry-type" "date" "pdf?" "note?" "file" "author" "editor" "author-abbrev" "editor-abbrev" "author-or-editor-abbrev" "keywords" "url")
   orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "bibliography/${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n

- tags ::
- keywords :: ${keywords}

* ${title}
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :URL: ${url}
  :AUTHOR: ${author}
  :NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
  :NOTER_PAGE:
  :END:\n\n"
         :unnarrowed t))))

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list balaji/notes-path)
   )
  )

(provide 'init-note-taking)
;;; init-note-taking.el ends here
