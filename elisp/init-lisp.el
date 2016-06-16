;;; -*- lexical-binding: t -*-
;;; init-lisp.el --- Makes Emacs a better Lisp editor, if that is even possible.

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2016 Balaji Sivaraman

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; This module primarily loads Paredit, but also other Lisp-editing goodness.

;;; Code:

(require 'init-package)
(require 'init-complete)

(defvar lisp-modes '(emacs-lisp-mode
                     lisp-interaction-mode
                     inferior-lisp-mode
                     inferior-emacs-lisp-mode
                     lisp-mode))

(defun do-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
    (funcall current-mode))))

(bind-keys :map emacs-lisp-mode-map
           ("C-c m e E" . elint-current-buffer)
           ("C-c m e b" . do-eval-buffer)
           ("C-c m e c" . cancel-debug-on-entry)
           ("C-c m e d" . debug-on-entry)
           ("C-c m e e" . toggle-debug-on-error)
           ("C-c m e f" . emacs-lisp-byte-compile-and-load)
           ("C-c m e j" . emacs-lisp-mode)
           ("C-c m e l" . find-library)
           ("C-c m e r" . do-eval-region)
           ("C-c m e s" . scratch)
           ("C-c m e z" . byte-recompile-directory))

(defvar lisp-mode-hooks
  (--map (intern
      (concat (symbol-name it) "-hook"))
     lisp-modes))

(use-package paredit
  :diminish paredit-mode
  :commands paredit-mode
  :config
  (bind-key "M-p" 'paredit-splice-sexp-killing-backward emacs-lisp-mode-map)
  (bind-key "M-n" 'paredit-splice-sexp-killing-forward emacs-lisp-mode-map))

(defun balaji/lisp-mode-hook ()
  "Functions to be called when entering Lisp mode"
  (paredit-mode t)
  (company-mode t)
  (use-package eldoc
    :ensure nil
    :diminish eldoc-mode
    :commands eldoc-mode))

(defadvice emacs-lisp-mode
    (after elisp-rename-modeline activate)
  (setq mode-name "ELisp"))

(apply #'hook-into-modes 'balaji/lisp-mode-hook lisp-mode-hooks)

(provide 'init-lisp)
;;; init-lisp.el ends here
