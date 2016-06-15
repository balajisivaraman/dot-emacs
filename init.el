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
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish nil t)

;;; Configure Libraries
(use-package s    :load-path "lib/s-el"    :ensure nil)
(use-package dash :load-path "lib/dash-el" :ensure nil)
(use-package f    :load-path "lib/f-el"    :ensure nil)

(require 'init-functions)
(require 'init-package)
(require 'init-defaults)
(require 'init-customizations)
(require 'init-editing)

(require 'init-appearance)
(require 'init-codestyle)
(require 'init-complete)
(require 'init-dired)
(require 'init-finance)
(require 'init-flycheck)
(require 'init-git)
(require 'init-haskell)
(require 'init-helm)
(require 'init-ibuffer)
(require 'init-keybindings)
(require 'init-lisp)
(require 'init-navigation)
(require 'init-project)
(require 'init-scala)
(require 'init-snippets)
(require 'init-unicode-conversions)

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
