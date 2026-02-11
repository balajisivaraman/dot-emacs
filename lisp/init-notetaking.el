;;; init-notetaking.el --- Note-taking configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures Org-mode and note-taking with:
;; - Vulpea for note management and linking
;; - Vulpea-journal for daily notes
;; - Vulpea-ui for sidebar and widgets
;; - Deft for fast note searching
;; - org-modern and org-superstar for visual enhancements
;; - Custom capture templates for notes and journal

;;; Code:

;;; Org Configuration Variables
(defvar bs/org-directory
  (expand-file-name "~/Library/Mobile Documents/com~apple~CloudDocs/ThePlainTextLife")
  "Main directory for org notes and files.")

(defvar bs/org-journal-directory
  (expand-file-name "journal/" bs/org-directory)
  "Directory for daily journal entries.")

(defvar bs/org-books-directory
  (expand-file-name "books/" bs/org-directory)
  "Directory for book notes.")

;;; Org Mode Basic Setup
(use-package org
  :ensure t
  :config
  ;; Set org directory
  (setq org-directory bs/org-directory)

  ;; Create journal directory if it doesn't exist
  (unless (file-directory-p bs/org-journal-directory)
    (make-directory bs/org-journal-directory t))

  ;; Create books directory if it doesn't exist
  (unless (file-directory-p bs/org-books-directory)
    (make-directory bs/org-books-directory t))

  ;; Startup settings
  (setq org-startup-indented t              ;; Indent content under headings
        org-startup-folded 'content         ;; Start with content visible
        org-hide-emphasis-markers t         ;; Hide markup markers
        org-pretty-entities t               ;; Show UTF-8 characters
        org-ellipsis " ▾"                   ;; Custom ellipsis for folded content
        org-startup-with-inline-images t)   ;; Show images on startup

  ;; Editing settings
  (setq org-special-ctrl-a/e t              ;; Smart beginning/end of line
        org-special-ctrl-k t                ;; Smart kill line
        org-return-follows-link t)          ;; RET follows links

  ;; Logging
  (setq org-log-done 'time                  ;; Log completion time
        org-log-into-drawer t))

;;; Mixed Pitch - Intelligent variable/fixed pitch mixing
(use-package mixed-pitch
  :ensure t
  :hook (org-mode . mixed-pitch-mode)
  :config
  ;; Don't make org metadata smaller - keep at normal size
  (setq mixed-pitch-set-height nil)

  ;; Set the variable-pitch face that mixed-pitch will use
  (set-face-attribute 'variable-pitch nil
                     :family bs/variable-pitch-font
                     :height (round (* bs/base-font-size 10 1.2))))

;;; Writeroom Mode - Distraction-free writing
(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 100)
  (setq writeroom-fullscreen-effect 'maximized)  ;; Maximize, not true fullscreen
  (setq writeroom-mode-line t)                   ;; Keep mode line visible

  ;; Hide line numbers in writeroom mode
  (add-hook 'writeroom-mode-hook
            (lambda ()
              (if writeroom-mode
                  (setq-local display-line-numbers nil)
                (setq-local display-line-numbers t))))

  ;; Keybinding to toggle
  (general-define-key
   "C-c w" '(writeroom-mode :which-key "writeroom mode")))

;;; Org ID - UUID generation for notes
(use-package org-id
  :after org
  :config
  ;; Use uuidgen for generating IDs
  (setq org-id-method 'uuid
        org-id-uuid-program "uuidgen")

  ;; Store ID locations in cache directory
  (setq org-id-track-globally t
        org-id-locations-file
        (expand-file-name "org-id-locations"
                         (expand-file-name ".cache/" user-emacs-directory)))

  ;; Ensure cache directory exists
  (let ((cache-dir (file-name-directory org-id-locations-file)))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))))

;;; Org Modern - Modern org-mode styling
(use-package org-modern
  :ensure t
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  ;; What to modernize
  (setq org-modern-keyword t                ;; Prettify keywords
        org-modern-checkbox nil             ;; Keep standard checkboxes
        org-modern-table t                  ;; Modernize tables
        org-modern-tag t                    ;; Modernize tags
        org-modern-timestamp t              ;; Modernize timestamps
        org-modern-priority t               ;; Modernize priorities
        org-modern-horizontal-rule t        ;; Modernize horizontal rules
        org-modern-todo t                   ;; Modernize TODO keywords
        org-modern-block-name t)            ;; Modernize block names

  ;; Keep star bullets the same size as the base font
  (setq org-modern-star 'replace)
  (setq org-modern-hide-stars nil)

  ;; Remove extra spacing around headings
  (setq org-modern-block-fringe nil))

;;; Org Superstar - Fancy heading bullets
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Use UTF-8 bullets for headings
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t)

  ;; Don't scale bullets - keep them at base font size
  (setq org-superstar-prettify-item-bullets nil))

;;; Org Appear - Show hidden emphasis markers when cursor is on them
(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t              ;; Show link markup on hover
        org-appear-autosubmarkers t         ;; Show sub/superscript markers
        org-appear-autoentities t           ;; Show entity markup
        org-appear-autokeywords t           ;; Show keyword markup
        org-appear-inside-latex t))         ;; Show latex markup

;;; Org Tidy - Clean up org buffers
(use-package org-tidy
  :ensure t
  :after org
  :hook (org-mode . org-tidy-mode)
  :config
  ;; Fully hide property drawers (not even fringe indicator)
  (setq org-tidy-properties-style 'invisible))

;;; Org Sticky Header - Show current heading in header line
(use-package org-sticky-header
  :ensure t
  :after org
  :hook (org-mode . org-sticky-header-mode)
  :config
  ;; Show full outline path in header
  (setq org-sticky-header-full-path 'full)
  ;; Don't show in agenda
  (setq org-sticky-header-always-show-header nil))

;;; Org Typography - Apply typographic scale to headings
(defun bs/set-org-typography ()
  "Apply typographic scale to org headings.
Uses bs/calculate-font-height for consistent scaling."
  (interactive)

  ;; Set org-level faces with typographic scale (absolute sizes, not relative)
  ;; Level 1 is largest (level 5), down to level 8 (level 1)
  (dolist (level-config '((org-level-1 . 5)
                         (org-level-2 . 4)
                         (org-level-3 . 3)
                         (org-level-4 . 2)
                         (org-level-5 . 1)
                         (org-level-6 . 1)
                         (org-level-7 . 1)
                         (org-level-8 . 1)))
    (let ((face (car level-config))
          (scale-level (cdr level-config)))
      (set-face-attribute face nil
                         :family bs/heading-font
                         :weight 'bold
                         ;; Use absolute size (in 1/10 points) instead of relative
                         :height (round (* bs/base-font-size 10 (bs/calculate-font-height scale-level)))))))

;; Apply typography on org-mode hook
(add-hook 'org-mode-hook #'bs/set-org-typography)

;; Reapply when base font size changes
(advice-add 'bs/update-base-font-size :after
            (lambda (&rest _)
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (when (eq major-mode 'org-mode)
                    (bs/set-org-typography))))))

;;; Vulpea - Note management and database
(use-package vulpea
  :ensure t
  :demand t
  :config
  ;; Set main directory for vulpea
  (setq vulpea-directory bs/org-directory)

  ;; Set directories to index
  (setq vulpea-db-sync-directories (list bs/org-directory))

  ;; Database location in cache
  (setq vulpea-db-file
        (expand-file-name "vulpea/vulpea.db"
                         (expand-file-name ".cache/" user-emacs-directory)))

  ;; Ensure database directory exists
  (let ((db-dir (file-name-directory vulpea-db-file)))
    (unless (file-directory-p db-dir)
      (make-directory db-dir t)))

  ;; Enable auto-sync mode
  (vulpea-db-autosync-mode +1)

  ;; Unbind M-m before using it as prefix
  (global-unset-key (kbd "M-m"))

  ;; Keybindings with M-m prefix
  (general-define-key
   :prefix "M-m"
   "f" '(vulpea-find :which-key "find note")
   "i" '(vulpea-insert :which-key "insert link")
   "c" '(org-capture :which-key "capture")))

;;; Vulpea Journal - Daily notes
(use-package vulpea-journal
  :ensure t
  :after vulpea
  :config
  ;; Configure journal template with comma in title and filename
  (setq vulpea-journal-default-template
        '(:file-name "journal/%Y-%m-%d, %A.org"
          :title "%Y-%m-%d, %A"
          :tags ("journal")
          :head "#+created: %<[%Y-%m-%d]>"))

  ;; Keybindings
  (general-define-key
   :prefix "M-m"
   "j" '(vulpea-journal-today :which-key "journal today")
   "J" '(vulpea-journal-date :which-key "journal date")))

;;; Vulpea UI - Sidebar and widgets
(use-package vulpea-ui
  :ensure t
  :after vulpea
  :config
  ;; Keybinding to toggle sidebar
  (general-define-key
   :prefix "M-m"
   "s" '(vulpea-ui-sidebar-toggle :which-key "toggle sidebar")))

;;; Org Capture Templates
(use-package org-capture
  :after (org vulpea)
  :config
  ;; Store the last entered title for reuse
  (defvar bs/last-capture-title nil
    "Store the last entered title for org-capture.")

  (setq org-capture-templates
        '(("n" "Plain Note" plain
           (file (lambda ()
                   (setq bs/last-capture-title (read-string "Note title: "))
                   (expand-file-name
                    (format "%s.org" bs/last-capture-title)
                    bs/org-directory)))
           "#+TITLE: %(identity bs/last-capture-title)\n#+FILETAGS: %^{Tags}\n#+DATE: %u\n\n%?"
           :empty-lines 1
           :unnarrowed t
           :hook (org-id-get-create))

          ("b" "Book Note" plain
           (file (lambda ()
                   (setq bs/last-capture-title (read-string "Book title: "))
                   (expand-file-name
                    (format "%s.org" bs/last-capture-title)
                    bs/org-books-directory)))
           "#+TITLE: %(identity bs/last-capture-title)\n#+AUTHOR: %^{Author}\n#+FILETAGS: :book:\n#+DATE: %u\n\n%?"
           :empty-lines 1
           :unnarrowed t
           :hook (org-id-get-create))

          ("j" "Journal Entry" plain
           (file vulpea-journal-today-file)
           "* %U\n\n%?"
           :empty-lines 1
           :unnarrowed t
           :hook (org-id-get-create)))))

;;; Deft - Fast note searching
(use-package deft
  :ensure t
  :config
  ;; Set deft directory to match org directory
  (setq deft-directory bs/org-directory
        deft-extensions '("org" "txt" "md")
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval 0)

  ;; Deft keybinding with M-s prefix (search)
  (general-define-key
   :prefix "M-s"
   "d" '(deft :which-key "deft (search notes)")))

(provide 'init-notetaking)
;;; init-notetaking.el ends here
