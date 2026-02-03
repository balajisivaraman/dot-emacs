;;; init-project.el --- Project management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures Emacs' built-in project.el for project management.
;; Automatically discovers projects in configured directories.

;;; Code:

;;; Project Configuration Variables
(defvar bs/project-search-path "~/code"
  "Root directory to search for projects.
All immediate subdirectories will be added as projects.")

;;; Project.el Setup
;; Built-in project management (Emacs 28+)
(require 'project)

;; Add which-key description for C-x p prefix
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-x p" "project"))

;; Automatically discover projects in configured directory
(defun bs/discover-projects ()
  "Discover and add all Git repositories in `bs/project-search-path' as projects.
Searches recursively through all subdirectories."
  (interactive)
  (when (file-directory-p bs/project-search-path)
    (let ((count 0))
      (message "Searching for projects in %s..." bs/project-search-path)
      ;; Use find to recursively locate all .git directories
      (dolist (git-dir (directory-files-recursively
                        bs/project-search-path
                        "^\\.git$"
                        t  ;; Include directories
                        (lambda (dir)
                          ;; Skip hidden directories except .git
                          (not (string-match-p "/\\.[^./]" dir)))))
        (when (file-directory-p git-dir)
          ;; The project root is the parent of .git
          (let* ((project-root (file-name-directory (directory-file-name git-dir)))
                 (proj (project-current nil project-root)))
            (when proj
              (project-remember-project proj)
              (setq count (1+ count))))))
      (message "Discovered %d projects in %s" count bs/project-search-path))))

;; Discover projects on startup
(add-hook 'emacs-startup-hook #'bs/discover-projects)

;; Keybinding to manually rediscover projects
(general-define-key
 :prefix "C-c c"
 "p" '(bs/discover-projects :which-key "discover projects"))

;; Project settings
(setq project-switch-commands
      '((project-find-file "Find file")
        (project-find-regexp "Find regexp")
        (project-find-dir "Find directory")
        (project-dired "Dired")
        (project-eshell "Eshell")
        (magit-project-status "Magit" ?m)))

;; Use project.el with consult
(with-eval-after-load 'consult
  (setq consult-project-function #'consult--default-project-function))

(provide 'init-project)
;;; init-project.el ends here
