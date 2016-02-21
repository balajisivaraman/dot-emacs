;; Benchmarking Emacs Startup
(defconst emacs-start-time (current-time))

;;; Configure Use-Package
(defvar balaji/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path balaji/site-lisp-dir)
(add-to-list 'load-path (expand-file-name "use-package" balaji/site-lisp-dir))
(require 'use-package)
(require 'bind-key)
(require 'diminish nil t)

;;; Configure Libraries
(use-package s    :load-path "lib/s-el")
(use-package dash :load-path "lib/dash-el")

;;; Utility Functions and Macros
(defun balaji/network-connection-available-p ()
    "Check whether we have internet connectivity"
  (-any-p
   (lambda (interface) (s-starts-with-p "en" (car interface)))
   (network-interface-list)))

;;; Initialize package.el
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; package.el should not initialize our packages.
;; We're going to use use-package for that.
(setq package-enable-at-startup nil)

(if (balaji/network-connection-available-p)
    (package-refresh-contents))

(defun package-require (package)
  "Install `package` only if it is not already installed"
  (unless (package-installed-p package)
    (package-install package nil)))

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
