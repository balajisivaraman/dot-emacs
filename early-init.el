;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Increase GC threshold during startup for faster load; reset after.
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 800 1024))))

;; Disable package.el — we use Elpaca instead.
(setq package-enable-at-startup nil)

;; Suppress native-comp stderr warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; Prevent frame resizing on font/UI changes during startup.
(setq frame-inhibit-implied-resize t)

;; Disable UI chrome before frames are created.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;;; early-init.el ends here
