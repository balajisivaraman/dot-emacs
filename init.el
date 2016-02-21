;; Benchmarking Emacs Startup
(defconst emacs-start-time (current-time))

(defvar balaji/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path balaji/site-lisp-dir)
(dolist (site-lisp-subdir-path (directory-files balaji/site-lisp-dir t "[^\s.]$"))
  (add-to-list 'load-path site-lisp-subdir-path))
(require 'use-package)
(require 'bind-key)

(when (window-system)
  (let ((elapsed-time (float-time (time-subtract (current-time)
						 emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed-time))

  (add-hook 'after-init-hook
	    `(lambda ()
	       (let ((elapsed-time (float-time (time-subtract (current-time)
							      emacs-start-time))))
		 (message "Loading %s...done (%.3fs)" ,load-file-name elapsed-time)))
	    t))
