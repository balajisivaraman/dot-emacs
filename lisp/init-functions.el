;;; init-functions.el --- Utility Macros and Functions -*- lexical-binding: t -*-

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

;; This file contains utility functions and macros that I wrote or pilfered from the web like any self-respecting Emacs user.

;;; Code:

;;;###autoload
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;;;###autoload
(defun bs/dot-emacs ()
  "Go directly to .emacs, do not pass Go, do not collect $200."
  (interactive)
  (message "Stop procrastinating and do some work!")
  (find-file (s-concat user-emacs-directory "init.el")))
(bind-key "C-c f d" 'bs/dot-emacs)

;;;###autoload
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(bind-key "M-Q" 'unfill-paragraph global-map)

;;;###autoload
(defun bs/ui-configuration ()
  "Configure custom fonts."
  (custom-theme-set-faces
   'user
   ;; configure overall variable pitch and fixed pitch fonts
   '(default ((t (:family "Monospace" :weight normal :height 125))))
   '(variable-pitch ((t (:family "SF Pro Text" :height 130))))
   '(fixed-pitch ((t (:family "Monospace" :weight normal :height 125))))

   ;; configure fonts for org headings and document title
   '(org-level-5 ((t (:height 1.1))))
   '(org-level-4 ((t (:height 1.15))))
   '(org-level-3 ((t (:height 1.21))))
   '(org-level-2 ((t (:height 1.27))))
   '(org-level-1 ((t (:height 1.33)))))
  (modus-themes-load-vivendi))

;;;###autoload
(defun bs/frame-functions (frame)
  "Configure custom settings given initial non-daemon FRAME. Intended
for `after-make-frame-functions'."
  (bs/ui-configuration))

(provide 'init-functions)
;;; init-functions.el ends here
