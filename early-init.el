;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file runs before package.el and GUI initialization.
;; Used for performance optimizations and disabling package.el (we use Elpaca).

;;; Code:

;; Disable package.el in favor of Elpaca
(setq package-enable-at-startup nil)

;; Increase garbage collection threshold during startup for faster loading
;; gcmh will manage this after init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable unnecessary UI elements early for cleaner startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent frame resize when fonts load (smoother startup)
(setq frame-inhibit-implied-resize t)

;; Native compilation settings (suppress warnings, enable deferred compilation)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;;; early-init.el ends here
