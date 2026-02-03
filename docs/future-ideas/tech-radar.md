# Tech Radar Implementation on Vulpea

## Overview

Build a personal Technology Radar system on top of Vulpea, inspired by d12frosted's Vino package. This would allow tracking technologies (tools, techniques, platforms, languages, frameworks) with metadata and querying capabilities.

## Concept

Each technology is represented as an Org note with metadata fields. The system provides:
- Structured capture templates for adding new technologies
- Query functions to filter by category, ring, experience level, etc.
- Reporting functions to generate useful views
- Maintenance functions for updating status and tracking reviews

## Use Cases

1. **Track technology adoption**: Maintain a living record of technologies used, evaluating, or archived
2. **Decision making**: Query "What tools are we using for X?" or "What's in Trial phase?"
3. **Onboarding**: Show team members the tech stack with context
4. **Regular reviews**: Identify technologies that haven't been reviewed in a while
5. **Historical record**: Track technology decisions over time with dates and rationale

## Metadata Schema

Each tech radar note would include:

```org
#+TITLE: Technology Name
#+CATEGORY: Tool | Technique | Platform | Language | Framework
#+RING: Adopt | Trial | Assess | Hold
#+REVIEWED: [2025-01-15]
#+EXPERIENCE: None | Basic | Intermediate | Expert
#+STATUS: Active | Deprecated | Evaluating | Archived
#+FILETAGS: :radar:cloud:backend:  (example tags)

* Overview
Brief description of the technology

* Use Cases
Where and why we use/would use this

* Experience Notes
Personal experiences, gotchas, tips

* Projects Using This
- [[id:project1][Project Name]]
- [[id:project2][Another Project]]

* Review History
** [2025-01-15] Review
Notes from latest review
```

## Implementation Plan

### Phase 1: Core Module (tech-radar.el)

Create `lisp/tech-radar.el` with these components:

#### 1.1 Configuration Variables

```elisp
(defvar tech-radar-directory
  (expand-file-name "tech-radar/" bs/org-directory)
  "Directory for tech radar items.")

(defvar tech-radar-categories
  '("Tool" "Technique" "Platform" "Language" "Framework")
  "Valid categories for tech radar items.")

(defvar tech-radar-rings
  '("Adopt" "Trial" "Assess" "Hold")
  "Tech radar rings following ThoughtWorks convention.")

(defvar tech-radar-experience-levels
  '("None" "Basic" "Intermediate" "Expert")
  "Experience levels with a technology.")

(defvar tech-radar-statuses
  '("Active" "Evaluating" "Deprecated" "Archived")
  "Status of technology in our context.")
```

#### 1.2 Helper Functions

```elisp
;; Check if a note is a tech radar item
(defun tech-radar-note-p (note)
  "Return non-nil if NOTE is a tech radar item."
  (let ((tags (vulpea-note-tags note)))
    (seq-contains-p tags "radar")))

;; Get all tech radar items
(defun tech-radar-get-all ()
  "Return all tech radar items."
  (vulpea-db-query
   (lambda (note)
     (tech-radar-note-p note))))

;; Get metadata helpers
(defun tech-radar-get-category (note)
  "Get category of tech radar NOTE."
  (vulpea-meta-get note "category" 'string))

(defun tech-radar-get-ring (note)
  "Get ring of tech radar NOTE."
  (vulpea-meta-get note "ring" 'string))

(defun tech-radar-get-experience (note)
  "Get experience level with tech radar NOTE."
  (vulpea-meta-get note "experience" 'string))

(defun tech-radar-get-status (note)
  "Get status of tech radar NOTE."
  (vulpea-meta-get note "status" 'string))

(defun tech-radar-get-reviewed (note)
  "Get last reviewed date of tech radar NOTE."
  (vulpea-meta-get note "reviewed" 'string))
```

#### 1.3 Query Functions

```elisp
(defun tech-radar-by-category (category)
  "Return all tech radar items in CATEGORY."
  (seq-filter
   (lambda (note)
     (string= category (tech-radar-get-category note)))
   (tech-radar-get-all)))

(defun tech-radar-by-ring (ring)
  "Return all tech radar items in RING."
  (seq-filter
   (lambda (note)
     (string= ring (tech-radar-get-ring note)))
   (tech-radar-get-all)))

(defun tech-radar-by-status (status)
  "Return all tech radar items with STATUS."
  (seq-filter
   (lambda (note)
     (string= status (tech-radar-get-status note)))
   (tech-radar-get-all)))

(defun tech-radar-needs-review (&optional months)
  "Return items not reviewed in MONTHS (default 6)."
  (let ((months (or months 6))
        (cutoff-date (time-subtract (current-time)
                                   (days-to-time (* months 30)))))
    (seq-filter
     (lambda (note)
       (let ((reviewed (tech-radar-get-reviewed note)))
         (when reviewed
           (time-less-p (org-read-date nil t reviewed)
                       cutoff-date))))
     (tech-radar-get-all))))
```

#### 1.4 Interactive Commands

```elisp
(defun tech-radar-find ()
  "Find and open a tech radar item."
  (interactive)
  (let* ((items (tech-radar-get-all))
         (note (vulpea-select-from "Tech Radar" items)))
    (when note
      (find-file (vulpea-note-path note)))))

(defun tech-radar-insert-link ()
  "Insert a link to a tech radar item."
  (interactive)
  (let* ((items (tech-radar-get-all))
         (note (vulpea-select-from "Tech Radar" items)))
    (when note
      (insert (org-link-make-string
               (concat "id:" (vulpea-note-id note))
               (vulpea-note-title note))))))

(defun tech-radar-mark-reviewed ()
  "Mark the current tech radar item as reviewed today."
  (interactive)
  (when (tech-radar-note-p (vulpea-buffer-note))
    (vulpea-meta-set "reviewed" (format-time-string "[%Y-%m-%d]"))
    (save-buffer)
    (message "Marked as reviewed today")))

(defun tech-radar-change-ring ()
  "Change the ring of the current tech radar item."
  (interactive)
  (when (tech-radar-note-p (vulpea-buffer-note))
    (let ((new-ring (completing-read "New ring: " tech-radar-rings nil t)))
      (vulpea-meta-set "ring" new-ring)
      (save-buffer)
      (message "Moved to %s ring" new-ring))))
```

#### 1.5 Reporting Functions

```elisp
(defun tech-radar-report-by-ring ()
  "Generate a report grouped by ring."
  (interactive)
  (let ((buf (get-buffer-create "*Tech Radar: By Ring*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Tech Radar - By Ring\n")
      (insert "#+STARTUP: overview\n\n")
      (dolist (ring tech-radar-rings)
        (let ((items (tech-radar-by-ring ring)))
          (insert (format "* %s (%d)\n\n" ring (length items)))
          (dolist (item items)
            (insert (format "- [[id:%s][%s]] /%s/\n"
                          (vulpea-note-id item)
                          (vulpea-note-title item)
                          (tech-radar-get-category item))))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun tech-radar-report-by-category ()
  "Generate a report grouped by category."
  (interactive)
  (let ((buf (get-buffer-create "*Tech Radar: By Category*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Tech Radar - By Category\n")
      (insert "#+STARTUP: overview\n\n")
      (dolist (category tech-radar-categories)
        (let ((items (tech-radar-by-category category)))
          (insert (format "* %s (%d)\n\n" category (length items)))
          (dolist (item items)
            (insert (format "- [[id:%s][%s]] /%s/\n"
                          (vulpea-note-id item)
                          (vulpea-note-title item)
                          (tech-radar-get-ring item))))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun tech-radar-report-needs-review ()
  "Generate a report of items needing review."
  (interactive)
  (let ((items (tech-radar-needs-review 6))
        (buf (get-buffer-create "*Tech Radar: Needs Review*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Tech Radar - Needs Review\n")
      (insert "#+SUBTITLE: Items not reviewed in 6+ months\n\n")
      (if (null items)
          (insert "No items need review!\n")
        (insert (format "Found %d items:\n\n" (length items)))
        (dolist (item items)
          (insert (format "- [[id:%s][%s]] /%s - %s/ (Last: %s)\n"
                        (vulpea-note-id item)
                        (vulpea-note-title item)
                        (tech-radar-get-category item)
                        (tech-radar-get-ring item)
                        (or (tech-radar-get-reviewed item) "Never")))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))
```

### Phase 2: Capture Templates

Add to `init-notetaking.el`:

```elisp
(add-to-list 'org-capture-templates
  '("r" "Tech Radar Item" plain
    (file (lambda ()
            (expand-file-name
             (format "%s.org"
                     (read-string "Technology name: "))
             tech-radar-directory)))
    "#+TITLE: %^{Technology Name}
#+CATEGORY: %^{Category|Tool|Technique|Platform|Language|Framework}
#+RING: %^{Ring|Assess|Trial|Adopt|Hold}
#+REVIEWED: %u
#+EXPERIENCE: %^{Experience|None|Basic|Intermediate|Expert}
#+STATUS: %^{Status|Evaluating|Active|Deprecated|Archived}
#+FILETAGS: :radar:%^{Tags}:

* Overview
%?

* Use Cases

* Experience Notes

* Projects Using This

* Review History
** %u Initial Entry
"
    :empty-lines 1
    :unnarrowed t
    :hook (org-id-get-create)))
```

### Phase 3: Keybindings

Add to tech-radar.el or init-notetaking.el:

```elisp
;; Tech Radar keybindings under M-r prefix
(global-unset-key (kbd "M-r"))

(general-define-key
 :prefix "M-r"
 "f" '(tech-radar-find :which-key "find tech")
 "i" '(tech-radar-insert-link :which-key "insert link")
 "c" '((lambda () (interactive) (org-capture nil "r")) :which-key "capture tech")
 "r" '(tech-radar-mark-reviewed :which-key "mark reviewed")
 "m" '(tech-radar-change-ring :which-key "move ring")
 "v r" '(tech-radar-report-by-ring :which-key "view by ring")
 "v c" '(tech-radar-report-by-category :which-key "view by category")
 "v n" '(tech-radar-report-needs-review :which-key "view needs review"))

(which-key-add-key-based-replacements
  "M-r" "tech radar"
  "M-r v" "view reports")
```

### Phase 4: Integration

Add to init.el:

```elisp
(require 'tech-radar)        ;; Tech Radar system
```

Add to init-notetaking.el after vulpea setup:

```elisp
;; Ensure tech-radar directory exists
(unless (file-directory-p tech-radar-directory)
  (make-directory tech-radar-directory t))
```

## Future Enhancements

Once the basic system is working, potential additions:

1. **Export to HTML/JSON**: Generate ThoughtWorks-style radar visualizations
2. **Comparison Reports**: See how the radar has changed over time
3. **Integration with Projects**: Auto-detect technologies used in project notes
4. **Scoring System**: Rate satisfaction, maturity, or other dimensions
5. **Timeline View**: See when technologies were adopted/deprecated
6. **Recommendation Engine**: Suggest items for review based on activity
7. **Export for Team Sharing**: Generate public-facing tech radar pages

## Estimated Implementation Time

- **Phase 1 (Core Module)**: 3-4 hours
- **Phase 2 (Capture Templates)**: 30 minutes
- **Phase 3 (Keybindings)**: 30 minutes
- **Phase 4 (Integration)**: 30 minutes
- **Testing & Refinement**: 1-2 hours

**Total**: 6-8 hours with Claude Code assistance

## References

- **Vino by d12frosted**: https://github.com/d12frosted/vino (wine tracking on Vulpea)
- **ThoughtWorks Tech Radar**: https://www.thoughtworks.com/radar (original concept)
- **Vulpea Documentation**: https://github.com/d12frosted/vulpea

## Implementation Notes

When implementing:
1. Create the tech-radar directory structure first
2. Implement and test core functions before adding UI
3. Start with basic queries, add complex ones as needed
4. Test with a few sample technologies before full migration
5. Consider adding this to CLAUDE.md under "Extensions" once implemented
