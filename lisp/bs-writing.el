;;; bs-writing.el --- Markdown, distraction-free writing, Hugo helpers -*- lexical-binding: t -*-

;;; markdown-mode
(defun bs/apply-markdown-heading-faces ()
  "Apply custom markdown heading typography."
  (dolist (spec '((markdown-header-face-1 outline-1 2.441)
                  (markdown-header-face-2 outline-2 1.953)
                  (markdown-header-face-3 outline-3 1.563)
                  (markdown-header-face-4 outline-4 1.250)
                  (markdown-header-face-5 outline-5 1.0)))
    (let ((face (nth 0 spec)))
      (when (facep face)
        (set-face-attribute face nil
                            :family "Alegreya SC"
                            :height (nth 2 spec)
                            :inherit (nth 1 spec))))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-hide-markup t)
  (setq markdown-header-scaling t)
  (setq markdown-header-scaling-values '(2.441 1.953 1.563 1.250 1.0 1.0))
  (bs/apply-markdown-heading-faces))

;; Reapply custom markdown heading faces after auto-dark theme switches.
(add-hook 'bs/theme-change-hook #'bs/apply-markdown-heading-faces)

;;; olivetti — centered writing column
(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 80))

;;; mixed-pitch — variable-pitch prose with fixed-pitch code
(use-package mixed-pitch
  :ensure t)

;;; bs-writing-mode — distraction-free minor mode
(defun bs/writing-hide-fringes ()
  "Set fringes to 0 for all windows showing the current buffer."
  (dolist (win (get-buffer-window-list nil nil t))
    (set-window-fringes win 0 0)))

(define-minor-mode bs-writing-mode
  "Distraction-free writing environment."
  :lighter " Write"
  (if bs-writing-mode
      (progn
        (olivetti-mode 1)
        (mixed-pitch-mode 1)
        (visual-line-mode 1)
        (display-line-numbers-mode -1)
        (hl-line-mode -1)
        ;; Hook fires after every window recalculation (including olivetti's),
        ;; keeping fringes at 0 even after olivetti-reset-window runs.
        (add-hook 'window-configuration-change-hook
                  #'bs/writing-hide-fringes nil :local)
        (bs/writing-hide-fringes))
    (olivetti-mode -1)
    (mixed-pitch-mode -1)
    (visual-line-mode -1)
    (remove-hook 'window-configuration-change-hook
                 #'bs/writing-hide-fringes :local)
    (dolist (win (get-buffer-window-list nil nil t))
      (set-window-fringes win nil nil))))

;; Auto-activate in markdown buffers.
(add-hook 'markdown-mode-hook #'bs-writing-mode)

;;; jinx — spell checking
(use-package jinx
  :ensure t
  :custom
  (jinx-languages "en_GB")
  :hook (markdown-mode . jinx-mode)
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-correct-all)))

;;; tempel — lightweight snippet system
(use-package tempel
  :ensure t
  :bind (("M-+"   . tempel-complete)
         ("C-c t i" . tempel-insert))
  :config
  (setq tempel-path
        (expand-file-name "templates/tempel" user-emacs-directory))
  ;; Navigate fields with TAB / S-TAB.
  (define-key tempel-map (kbd "TAB")   #'tempel-next)
  (define-key tempel-map (kbd "<backtab>") #'tempel-prev))

;; Add tempel to completion-at-point-functions in markdown buffers.
(defun bs/tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-complete completion-at-point-functions)))
(add-hook 'markdown-mode-hook #'bs/tempel-setup-capf)

;;; Hugo helpers

(defun bs/hugo--slugify (title)
  "Convert TITLE to a URL slug: lowercase, non-alphanumeric runs → hyphens."
  (string-trim
   (replace-regexp-in-string "-+" "-"
    (replace-regexp-in-string "[^a-z0-9]" "-"
     (downcase title)))
   "-" "-"))

(defun bs/hugo-new-post (title)
  "Create a new Hugo post directory + index.md with TOML front matter."
  (interactive "sPost title: ")
  (let* ((root     (project-root (project-current t)))
         (slug     (bs/hugo--slugify title))
         (date     (format-time-string "%Y-%m-%d"))
         (author   user-full-name)
         (post-dir (expand-file-name (format "content/posts/%s" slug) root))
         (file     (expand-file-name "index.md" post-dir)))
    (make-directory post-dir t)
    (make-directory (expand-file-name "images" post-dir) t)
    (find-file file)
    (insert (format "\
+++
title = \"%s\"
author = [\"%s\"]
description = \"\"
date = \"%s\"
toc = true
share = true
categories = []
tags = []
type = \"post\"
draft = true
+++

" title author date))
    (goto-char (point-max))))

(defun bs/hugo-rename-post (new-title)
  "Rename current Hugo post: update title in front matter and rename slug dir."
  (interactive "sNew post title: ")
  (unless (string-match "content/posts/\\([^/]+\\)/index\\.md"
                        (or (buffer-file-name) ""))
    (user-error "Not visiting a Hugo post index.md"))
  (let* ((old-dir  (file-name-directory (buffer-file-name)))
         (root     (project-root (project-current t)))
         (new-slug (bs/hugo--slugify new-title))
         (new-dir  (expand-file-name
                    (format "content/posts/%s/" new-slug) root))
         (new-file (expand-file-name "index.md" new-dir)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^title = \".*\"$" nil t)
        (replace-match (format "title = \"%s\"" new-title))))
    (save-buffer)
    (rename-file old-dir new-dir)
    (find-file new-file)
    (message "Renamed post to '%s' (slug: %s)" new-title new-slug)))

(defun bs/hugo-toggle-draft ()
  "Toggle draft = true/false in the current buffer's TOML front matter."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^draft = \\(true\\|false\\)$" nil t)
        (let ((current (match-string 1)))
          (replace-match
           (if (string= current "true") "draft = false" "draft = true"))
          (message "draft is now %s" (if (string= current "true") "false" "true")))
      (user-error "No 'draft' key found in front matter"))))

(defun bs/hugo-open-posts-dir ()
  "Open content/posts/ in Dired for the current project."
  (interactive)
  (let ((posts-dir (expand-file-name "content/posts/"
                                     (project-root (project-current t)))))
    (if (file-directory-p posts-dir)
        (dired posts-dir)
      (user-error "Directory not found: %s" posts-dir))))

;;; Hugo keybindings under C-c h (prefix declared in bs-core)
(use-package general
  :ensure nil
  :config
  (general-define-key
   "C-c h n" '(bs/hugo-new-post      :which-key "new post")
   "C-c h r" '(bs/hugo-rename-post   :which-key "rename post")
   "C-c h p" '(bs/hugo-open-posts-dir :which-key "posts dir")
   "C-c h d" '(bs/hugo-toggle-draft  :which-key "toggle draft")))

(provide 'bs-writing)
;;; bs-writing.el ends here
