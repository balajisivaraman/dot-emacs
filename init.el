;;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Main entry point for Emacs configuration.
;; This file loads modular configuration files from the lisp/ directory.

;;; Code:

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Setup .cache directory for all temporary files
(defvar user-cache-directory
  (expand-file-name ".cache/" user-emacs-directory)
  "Directory for all Emacs-generated temporary and cache files.")

;; Create cache directory if it doesn't exist
(unless (file-exists-p user-cache-directory)
  (make-directory user-cache-directory t))

;; Load configuration modules in order
(require 'init-elpaca)      ;; Package manager
(require 'init-defaults)    ;; Sensible defaults

;; Measure startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; init.el ends here
