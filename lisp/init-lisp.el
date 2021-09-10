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

(bs/general-mode-specific-bindings 'emacs-lisp-mode-map
                                   "eb" 'do-eval-buffer
                                   "er" 'do-eval-region
                                   "es" 'eval-last-sexp
                                   "s"  'scratch
                                   "dc" 'cancel-debug-on-entry
                                   "de" 'debug-on-entry
                                   "dr" 'toggle-debug-on-error
                                   "fb" 'emacs-lisp-byte-compile-and-load
                                   "fl" 'find-library
                                   "L"  'elint-current-buffer)

(which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
  "SPC m e" "eval"
  "SPC m f" "file"
  "SPC m d" "debug")

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
  (paredit-mode t)
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
