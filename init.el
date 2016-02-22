;; Benchmarking Emacs Startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defconst emacs-start-time (current-time))

;;; Initial Setup
(defvar balaji/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path balaji/site-lisp-dir)
(defvar balaji/elisp-dir (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path balaji/elisp-dir)

;;; Configure Use-Package
(add-to-list 'load-path (expand-file-name "use-package" balaji/site-lisp-dir))
(require 'use-package)
(require 'bind-key)
(require 'diminish nil t)

;;; Configure Libraries
(use-package s    :load-path "lib/s-el")
(use-package dash :load-path "lib/dash-el")
(use-package f    :load-path "lib/f-el")

(require 'init-functions)
(require 'init-package)
(require 'init-defaults)
(require 'init-customizations)
(require 'init-editing)

(require 'init-ibuffer)
(require 'init-git)
(require 'init-helm)

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
