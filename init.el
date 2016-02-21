;; Benchmarking Emacs Startup
(defconst emacs-start-time (current-time))

;;; Configure Use-Package
(defvar balaji/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path balaji/site-lisp-dir)
(add-to-list 'load-path (expand-file-name "use-package" balaji/site-lisp-dir))
(require 'use-package)
(require 'bind-key)

;;; Configure Libraries
(use-package s    :defer t :load-path "lib/s-el")
(use-package dash :defer t :load-path "lib/dash-el")

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
