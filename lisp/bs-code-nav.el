;;; bs-code-nav.el --- Monorepo navigation helpers -*- lexical-binding: t -*-

;;; Project + xref navigation baseline (no external language servers required)

(defun bs/tags--generated-p (tags-file)
  "Return non-nil when TAGS-FILE exists and is non-empty."
  (and (file-exists-p tags-file)
       (> (file-attribute-size (file-attributes tags-file)) 0)))

(defun bs/tags--run (program args)
  "Run PROGRAM with ARGS, returning its numeric exit code."
  (let ((buf (get-buffer-create "*bs-tags*")))
    (with-current-buffer buf
      (erase-buffer))
    (apply #'call-process program nil buf t args)))

(defun bs/tags--chunks (items size)
  "Split ITEMS into sublists of SIZE."
  (let (result)
    (while items
      (push (seq-take items size) result)
      (setq items (seq-drop items size)))
    (nreverse result)))

(defun bs/tags--run-etags-chunked (etags tags-file files)
  "Generate TAGS-FILE using ETAGS over FILES in chunks."
  (let ((chunks (bs/tags--chunks files 200))
        (ok t)
        (first t))
    (dolist (chunk chunks)
      (let* ((base-args (if first
                            (list "-o" tags-file)
                          (list "-a" "-o" tags-file)))
             (args (append base-args chunk))
             (code (bs/tags--run etags args)))
        (unless (and (numberp code) (zerop code))
          (setq ok nil))
        (setq first nil)))
    ok))

(defconst bs/tags-ignore-dirs
  '(".git" "node_modules" ".venv" "dist" "build")
  "Directory names to exclude from TAGS generation.")

(defun bs/tags--ignored-file-p (file)
  "Return non-nil when FILE is under an ignored directory."
  (let ((case-fold-search nil))
    (seq-some (lambda (dir)
                (string-match-p (format "/%s/" (regexp-quote dir)) file))
              bs/tags-ignore-dirs)))

(defun bs/project-generate-tags ()
  "Generate a TAGS file at the current project root using ctags."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (default-directory root)
         (tags-file (expand-file-name "TAGS" root))
         (ctags (executable-find "ctags"))
         (etags (executable-find "etags"))
         (generated nil)
         (errors '()))
    (when (file-exists-p tags-file)
      (delete-file tags-file))
    (when ctags
      (let* ((exclude-args (mapcar (lambda (dir) (format "--exclude=%s" dir))
                                   bs/tags-ignore-dirs))
             (args (append (list "-e" "-R" "-f" tags-file) exclude-args (list ".")))
             (code (bs/tags--run ctags args)))
        (if (and (numberp code) (zerop code) (bs/tags--generated-p tags-file))
            (setq generated t)
          (push (format "ctags failed (exit %s)" code) errors))))
    (unless generated
      (when (file-exists-p tags-file)
        (delete-file tags-file))
      (when etags
        (let* ((files (seq-remove #'bs/tags--ignored-file-p
                                  (project-files (project-current t))))
               (ok (and files
                        (bs/tags--run-etags-chunked etags tags-file files))))
          (if (and ok (bs/tags--generated-p tags-file))
              (setq generated t)
            (push "etags failed while processing file chunks" errors)))))
    (if generated
        (progn
          (visit-tags-table tags-file)
          (message "Generated and loaded %s" tags-file))
      (let ((why (if errors (string-join (nreverse errors) "; ")
                   "no tag generator available")))
        (user-error
         "Could not generate TAGS in %s (%s). Install universal-ctags or ensure etags works"
         root why)))))

(defun bs/project-visit-tags ()
  "Visit TAGS file at the current project root."
  (interactive)
  (let* ((root (project-root (project-current t)))
         (tags-file (expand-file-name "TAGS" root)))
    (if (file-exists-p tags-file)
        (progn
          (visit-tags-table tags-file)
          (message "Loaded %s" tags-file))
      (user-error "No TAGS file at %s (run bs/project-generate-tags)" root))))

;;; Optional Eglot layer (quiet fallback when servers are unavailable)

(defvar bs/eglot-server-specs
  '(((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
    ((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode)
     . ("typescript-language-server" "--stdio"))
    ((terraform-mode terraform-ts-mode) . ("terraform-ls" "serve"))
    ((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio")))
  "Mode-to-server mapping used by `bs/eglot-maybe'.")

(defun bs/eglot--spec-for-current-mode ()
  "Return matching entry from `bs/eglot-server-specs' for current major mode."
  (seq-find (lambda (entry)
              (apply #'derived-mode-p (car entry)))
            bs/eglot-server-specs))

(defun bs/eglot-maybe ()
  "Start Eglot only when a suitable server is available; otherwise stay quiet."
  (interactive)
  (if (not (require 'eglot nil t))
      (message "Eglot not available in this Emacs build")
    (let* ((spec (bs/eglot--spec-for-current-mode))
           (server (cdr spec))
           (bin (car server)))
      (cond
       ((not spec)
        (message "No Eglot server mapping for %s" major-mode))
       ((not (executable-find bin))
        (message "No %s in PATH; using xref/consult/tags navigation" bin))
       (t
        (add-to-list 'eglot-server-programs (cons major-mode server))
        (eglot-ensure))))))

(use-package general
  :ensure nil ; loaded in bs-core
  :config
  (general-define-key
   "C-x p T" '(bs/project-generate-tags :which-key "generate TAGS")
   "C-x p V" '(bs/project-visit-tags    :which-key "visit TAGS")
   "C-x p X" '(bs/eglot-maybe           :which-key "eglot maybe")))

(provide 'bs-code-nav)
;;; bs-code-nav.el ends here
