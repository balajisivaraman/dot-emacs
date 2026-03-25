;;; bs-pkm.el --- PKM and Tech Radar system -*- lexical-binding: t -*-

;;; ─── Forward declarations (suppress native-compiler warnings) ───────────────

(declare-function bs-writing-mode              "bs-writing")
(declare-function vulpea-create                "vulpea")
(declare-function vulpea-note-path             "vulpea")
(declare-function vulpea-visit                 "vulpea")
(declare-function vulpea-db-autosync-mode      "vulpea-db")
(declare-function vulpea-db-query-by-tags-every "vulpea-db")
(declare-function vulpea-note-id               "vulpea")
(declare-function vulpea-note-title            "vulpea")
(declare-function vulpea-note-tags             "vulpea")
(declare-function vulpea-buffer-meta-set       "vulpea-meta")
(declare-function vulpea-buffer-tags-add       "vulpea")

;;; ─── Variables and helpers ──────────────────────────────────────────────────

(defvar bs/pkm-directory (expand-file-name "~/Documents/notes/")
  "Root directory for the PKM vault.")

(defun bs/pkm--slugify (title)
  "Convert TITLE to a hyphenated lowercase slug.
Example: \"Ungoverned AI Rollouts\" → \"ungoverned-ai-rollouts\"."
  (string-trim
   (replace-regexp-in-string "-+" "-"
    (replace-regexp-in-string "[^a-z0-9]+" "-"
     (downcase title)))
   "-" "-"))

(defun bs/pkm--is-pkm-file-p ()
  "Return non-nil if current buffer visits an org file under the PKM vault."
  (and buffer-file-name
       (string-prefix-p (expand-file-name bs/pkm-directory)
                        (expand-file-name buffer-file-name))
       (string= (file-name-extension buffer-file-name) "org")))

(defun bs/pkm--new-id ()
  "Generate a fresh UUID suitable for an org :ID: property."
  (org-id-new))

;;; ─── Vulpea setup ────────────────────────────────────────────────────────────

;; Ensure vault directory exists before autosync starts.
(make-directory bs/pkm-directory t)

(use-package vulpea
  :ensure t
  :demand t
  :init
  ;; Must be set before vulpea-db-autosync-mode is enabled.
  (setq vulpea-db-sync-directories (list bs/pkm-directory))
  (setq vulpea-db-location
        (expand-file-name ".cache/var/vulpea.db" user-emacs-directory))
  (setq vulpea-default-notes-directory bs/pkm-directory)
  :config
  ;; On first install run: M-x vulpea-db-sync-full-scan
  (vulpea-db-autosync-mode 1))

;;; ─── Org-mode sensible defaults (note-taking, not task management) ──────────

(use-package org
  :ensure nil
  :config
  ;; Visual polish
  (setq org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-tags-column 0
        org-auto-align-tags nil
        org-cycle-separator-lines 0
        org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-folded 'content
        org-startup-indented t)
  ;; Editing comfort
  (setq org-special-ctrl-a/e t
        org-fold-catch-invisible-edits 'show-and-error
        org-insert-heading-respect-content t
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-list-allow-alphabetical t)
  ;; Source blocks
  (setq org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0)
  ;; Links and IDs
  (setq org-id-method 'uuid
        org-id-link-to-org-use-id t
        org-link-file-path-type 'relative
        org-return-follows-link t
        org-id-locations-file
        (expand-file-name ".cache/var/org-id-locations" user-emacs-directory))
  ;; Images
  (setq org-image-actual-width nil
        org-startup-with-inline-images nil)
  ;; Fast tag selector: single-key shortcuts for PKM tags.
  (setq org-tag-alist
        '(("note"       . ?n)
          ("journal"    . ?j)
          ("tech-radar" . ?t)
          ("ai-radar"   . ?a)
          ("case-study" . ?s))))

;;; ─── org-modern ─────────────────────────────────────────────────────────────

(use-package org-modern
  :ensure t
  :custom
  (org-modern-todo nil)
  (org-modern-priority nil)
  (org-modern-timestamp t)
  (org-modern-table t)
  :hook (org-mode . org-modern-mode))

;;; ─── org-appear ─────────────────────────────────────────────────────────────

(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
  :custom
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

;;; ─── Capture constants ───────────────────────────────────────────────────────

(defconst bs/pkm--tech-radar-quadrants
  '("languages-and-frameworks" "platforms" "tools" "techniques")
  "Valid quadrant values for Tech Radar entries.")

(defconst bs/pkm--ai-radar-quadrants
  '("layer-0-data" "layer-1-models" "layer-2-orchestration"
    "layer-3-control-plane" "layer-4-patterns")
  "Valid quadrant values for Applied AI Radar entries.")

(defconst bs/pkm--rings
  '("assess" "trial" "adopt" "caution")
  "Valid ring values for radar entries.")

;;; ─── Capture ─────────────────────────────────────────────────────────────────

(defun bs/pkm--capture (title file-name &rest create-args)
  "Create a PKM note with TITLE at FILE-NAME under vault, then visit it.
FILE-NAME is relative to `bs/pkm-directory'.
CREATE-ARGS are forwarded to `vulpea-create'."
  ;; Ensure subdirectory exists.
  (let ((subdir (file-name-directory
                 (expand-file-name file-name bs/pkm-directory))))
    (make-directory subdir t))
  (let ((note (apply #'vulpea-create title file-name create-args)))
    (vulpea-visit note)))

(defun bs/pkm-capture-note (title)
  "Capture a new General Note with TITLE."
  (interactive "sTitle: ")
  (bs/pkm--capture title
                   (format "general/%s.org" (bs/pkm--slugify title))
                   :id (bs/pkm--new-id)
                   :tags '("note")
                   :head "#+DATE: %<[%Y-%m-%d]>"
                   :meta '(("state" . "blip"))))

(defun bs/pkm-capture-journal ()
  "Capture a new Journal Entry for today, or open today's entry if it exists."
  (interactive)
  (let* ((title     (format-time-string "%Y-%m-%d, %A"))
         (file-name (format "journal/%s.org" title))
         (file-path (expand-file-name file-name bs/pkm-directory)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (bs/pkm--capture title file-name
                       :id (bs/pkm--new-id)
                       :tags '("journal")
                       :head "#+DATE: %<[%Y-%m-%d]>"))))

(defun bs/pkm-capture-tech-radar (title)
  "Capture a new Tech Radar entry with TITLE, prompting for quadrant and ring."
  (interactive "sTitle: ")
  (let ((quadrant (completing-read "Quadrant: " bs/pkm--tech-radar-quadrants nil t))
        (ring     (completing-read "Ring: "     bs/pkm--rings nil t)))
    (bs/pkm--capture title
                     (format "radar/%s.org" (bs/pkm--slugify title))
                     :id (bs/pkm--new-id)
                     :tags '("tech-radar")
                     :head "#+DATE: %<[%Y-%m-%d]>"
                     :meta `(("quadrant" . ,quadrant)
                             ("ring"     . ,ring)
                             ("state"    . "blip")))))

(defun bs/pkm-capture-ai-radar (title)
  "Capture a new Applied AI Radar entry with TITLE.
Prompts for quadrant and ring interactively."
  (interactive "sTitle: ")
  (let ((quadrant (completing-read "Quadrant: " bs/pkm--ai-radar-quadrants nil t))
        (ring     (completing-read "Ring: "     bs/pkm--rings nil t)))
    (bs/pkm--capture title
                     (format "radar/%s.org" (bs/pkm--slugify title))
                     :id (bs/pkm--new-id)
                     :tags '("ai-radar")
                     :head "#+DATE: %<[%Y-%m-%d]>"
                     :meta `(("ai-quadrant" . ,quadrant)
                             ("ai-ring"     . ,ring)
                             ("state"       . "blip")))))

(defun bs/pkm-capture-dual-radar (title)
  "Capture an entry that belongs on both the Tech and Applied AI Radars.
Prompts for independent quadrant and ring placements for each radar."
  (interactive "sTitle: ")
  (let ((tech-quadrant (completing-read "Tech quadrant: " bs/pkm--tech-radar-quadrants nil t))
        (tech-ring     (completing-read "Tech ring: "     bs/pkm--rings nil t))
        (ai-quadrant   (completing-read "AI quadrant: "   bs/pkm--ai-radar-quadrants nil t))
        (ai-ring       (completing-read "AI ring: "       bs/pkm--rings nil t)))
    (bs/pkm--capture title
                     (format "radar/%s.org" (bs/pkm--slugify title))
                     :id (bs/pkm--new-id)
                     :tags '("tech-radar" "ai-radar")
                     :head "#+DATE: %<[%Y-%m-%d]>"
                     :meta `(("quadrant"    . ,tech-quadrant)
                             ("ring"        . ,tech-ring)
                             ("ai-quadrant" . ,ai-quadrant)
                             ("ai-ring"     . ,ai-ring)
                             ("state"       . "blip")))))

(defun bs/pkm-capture-case-study (title)
  "Capture a new Case Study with TITLE."
  (interactive "sTitle: ")
  (bs/pkm--capture title
                   (format "case-studies/%s.org" (bs/pkm--slugify title))
                   :id (bs/pkm--new-id)
                   :tags '("case-study")
                   :head "#+DATE: %<[%Y-%m-%d]>"
                   :meta '(("state" . "blip"))))

;;; ─── Find / search ───────────────────────────────────────────────────────────

(defun bs/pkm--select-by-tag (prompt tag)
  "Prompt user to select a note tagged TAG using PROMPT.
Returns the selected `vulpea-note', or nil if none found or cancelled."
  (let* ((notes (vulpea-db-query-by-tags-every (list tag))))
    (if (null notes)
        (user-error "No notes found with tag: %s" tag)
      (let* ((choices (mapcar (lambda (n) (cons (vulpea-note-title n) n)) notes))
             (title   (completing-read prompt (mapcar #'car choices) nil t)))
        (cdr (assoc title choices))))))

(defun bs/pkm-find-notes ()
  "Find and open a General Note."
  (interactive)
  (when-let ((note (bs/pkm--select-by-tag "Note: " "note")))
    (vulpea-visit note)))

(defun bs/pkm-find-journal ()
  "Find and open a Journal Entry."
  (interactive)
  (when-let ((note (bs/pkm--select-by-tag "Journal: " "journal")))
    (vulpea-visit note)))

(defun bs/pkm-find-tech-radar ()
  "Find and open a Tech Radar entry."
  (interactive)
  (when-let ((note (bs/pkm--select-by-tag "Tech Radar: " "tech-radar")))
    (vulpea-visit note)))

(defun bs/pkm-find-ai-radar ()
  "Find and open an Applied AI Radar entry."
  (interactive)
  (when-let ((note (bs/pkm--select-by-tag "AI Radar: " "ai-radar")))
    (vulpea-visit note)))

(defun bs/pkm-find-case-studies ()
  "Find and open a Case Study."
  (interactive)
  (when-let ((note (bs/pkm--select-by-tag "Case Study: " "case-study")))
    (vulpea-visit note)))

;;; ─── State management ────────────────────────────────────────────────────────

(defun bs/pkm-promote-to-tech-radar ()
  "Promote the current AI Radar note to also appear on the Tech Radar.
Adds the tech-radar filetag and prompts for quadrant and ring placement."
  (interactive)
  (unless (bs/pkm--is-pkm-file-p)
    (user-error "Not in a PKM file"))
  (let ((quadrant (completing-read "Tech quadrant: " bs/pkm--tech-radar-quadrants nil t))
        (ring     (completing-read "Tech ring: "     bs/pkm--rings nil t)))
    (vulpea-buffer-tags-add "tech-radar")
    (vulpea-buffer-meta-set "quadrant" quadrant)
    (vulpea-buffer-meta-set "ring"     ring)))

(defun bs/pkm-promote-to-ai-radar ()
  "Promote the current Tech Radar note to also appear on the Applied AI Radar.
Adds the ai-radar filetag and prompts for quadrant and ring placement."
  (interactive)
  (unless (bs/pkm--is-pkm-file-p)
    (user-error "Not in a PKM file"))
  (let ((quadrant (completing-read "AI quadrant: " bs/pkm--ai-radar-quadrants nil t))
        (ring     (completing-read "AI ring: "     bs/pkm--rings nil t)))
    (vulpea-buffer-tags-add "ai-radar")
    (vulpea-buffer-meta-set "ai-quadrant" quadrant)
    (vulpea-buffer-meta-set "ai-ring"     ring)))

(defun bs/pkm-blip ()
  "Mark the current PKM note as Blip (newly captured, not yet scanned)."
  (interactive)
  (vulpea-buffer-meta-set "state" "blip"))

(defun bs/pkm-scanning ()
  "Mark the current PKM note as Scanning (actively being worked on)."
  (interactive)
  (vulpea-buffer-meta-set "state" "scanning"))

(defun bs/pkm-clear-state ()
  "Remove the state metadata from the current PKM note.
Absence of state means the note is fully scanned and ready for development."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^- state :: .*\n?" nil t)
      (replace-match ""))))

;;; ─── Radar link insertion ────────────────────────────────────────────────────

(defun bs/pkm-insert-radar-link ()
  "Insert an [[id:UUID][Title]] link to a Tech or AI Radar entry at point."
  (interactive)
  (let* ((notes (append (vulpea-db-query-by-tags-every '("tech-radar"))
                        (vulpea-db-query-by-tags-every '("ai-radar"))))
         (choices (mapcar (lambda (n) (cons (vulpea-note-title n) n)) notes))
         (title (completing-read "Radar entry: " (mapcar #'car choices) nil t))
         (note  (cdr (assoc title choices))))
    (when note
      (insert (format "[[id:%s][%s]]"
                      (vulpea-note-id note)
                      (vulpea-note-title note))))))

;;; ─── Writing mode auto-activation ───────────────────────────────────────────

(defun bs/pkm--maybe-activate-writing-mode ()
  "Enable `bs-writing-mode' and `jinx-mode' for org files under the PKM vault."
  (when (bs/pkm--is-pkm-file-p)
    (bs-writing-mode 1)
    (when (fboundp 'jinx-mode)
      (jinx-mode 1))))

(add-hook 'org-mode-hook #'bs/pkm--maybe-activate-writing-mode)

;;; ─── Keybindings (M-n prefix, override priority so it works in any buffer) ───

(use-package general
  :ensure nil
  :config
  (general-define-key :prefix "M-n c" "" '(nil :which-key "Capture"))
  (general-define-key :prefix "M-n f" "" '(nil :which-key "Find"))
  (general-define-key :keymaps 'override
   "M-n c n" '(bs/pkm-capture-note       :which-key "note")
   "M-n c j" '(bs/pkm-capture-journal    :which-key "journal")
   "M-n c t" '(bs/pkm-capture-tech-radar :which-key "tech radar")
   "M-n c a" '(bs/pkm-capture-ai-radar   :which-key "ai radar")
   "M-n c d" '(bs/pkm-capture-dual-radar :which-key "dual radar")
   "M-n c s" '(bs/pkm-capture-case-study :which-key "case study"))
  (general-define-key :keymaps 'override
   "M-n f n" '(bs/pkm-find-notes         :which-key "notes")
   "M-n f j" '(bs/pkm-find-journal       :which-key "journal")
   "M-n f t" '(bs/pkm-find-tech-radar    :which-key "tech radar")
   "M-n f a" '(bs/pkm-find-ai-radar      :which-key "ai radar")
   "M-n f s" '(bs/pkm-find-case-studies  :which-key "case studies"))
  (general-define-key :keymaps 'override
   "M-n b" '(bs/pkm-blip              :which-key "mark blip")
   "M-n w" '(bs/pkm-scanning          :which-key "mark scanning")
   "M-n x" '(bs/pkm-clear-state       :which-key "clear state")
   "M-n l" '(bs/pkm-insert-radar-link :which-key "insert radar link"))
  (general-define-key :prefix "M-n p" "" '(nil :which-key "Promote"))
  (general-define-key :keymaps 'override
   "M-n p t" '(bs/pkm-promote-to-tech-radar :which-key "to tech radar")
   "M-n p a" '(bs/pkm-promote-to-ai-radar   :which-key "to ai radar")))

(provide 'bs-pkm)
;;; bs-pkm.el ends here
