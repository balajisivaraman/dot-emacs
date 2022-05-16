;;; init-lisp.el --- Makes Emacs a better Lisp editor, if that is even possible. -*- lexical-binding: t -*-

;; Copyright (C) 2021  Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module primarily loads Paredit, but also other Lisp-editing goodness.

;;; Code:

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
           ("M-m e b" . do-eval-buffer)
           ("M-m e r" . do-eval-region)
           ("M-m s" . scratch)
           ("M-m d c" . cancel-debug-on-entry)
           ("M-m d e" . debug-on-entry)
           ("M-m d r" . toggle-debug-on-error)
           ("M-m f b" . emacs-lisp-byte-compile-and-load)
           ("M-m f l" . find-library)
           ("M-m L" . elint-current-buffer))

(which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
  "M-m e" "eval"
  "M-m f" "file"
  "M-m d" "debug")

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

(defun bs/lisp-mode-hook ()
  "Functions to be called when entering Lisp mode."
  (company-mode t)
  (use-package eldoc
    :ensure nil
    :diminish eldoc-mode
    :commands eldoc-mode))

(defadvice emacs-lisp-mode
    (after elisp-rename-modeline activate)
  (setq mode-name "ELisp"))

(apply #'hook-into-modes 'bs/lisp-mode-hook lisp-mode-hooks)

(provide 'init-lisp)
;;; init-lisp-modes.el ends here
