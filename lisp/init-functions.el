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

(defun bs/network-connection-available-p ()
  "Check whether we have internet connectivity."
  (-any-p
   (lambda (interface) (s-starts-with-p "en" (car interface)))
   (network-interface-list)))

(defun bs/duplicate-line-or-region (&optional begin end)
  "If region is active, duplicates it. Othewise duplicates the current line."
  (interactive "r")
  (save-excursion
    (let ((reg-begin begin)
          (reg-end end)
          (line-text))
      (if (not (region-active-p))
          (progn
            (bs/smarter-move-beginning-of-line nil)
            (setq reg-begin (point))
            (move-end-of-line nil)
            (setq reg-end (point))))
      (setq line-text (buffer-substring reg-begin reg-end))
      (goto-char reg-end)
      (move-end-of-line nil)
      (newline)
      (insert line-text))))
(bind-key "C-c C-d" 'bs/duplicate-line-or-region)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun bs/dot-emacs ()
  "Go directly to .emacs, do not pass Go, do not collect $200."
  (interactive)
  (message "Stop procrastinating and do some work!")
  (find-file (s-concat user-emacs-directory "init.el")))
(bind-key "C-c f d" 'bs/dot-emacs)

(defun bs/indent-region-or-buffer (&optional begin end)
  "Indent a region or the whole file.

If called after a region is marked, indents the region between BEGIN and END.

Otherwise indents the whole buffer, i.e. everything between `point-min' and `point-max'"
  (interactive "r")
  (save-excursion
    (delete-trailing-whitespace)
    (if (region-active-p)
        (indent-region begin end nil)
      (indent-region (point-min) (point-max) nil))))
(bind-key "C-c B i" 'bs/indent-region-or-buffer)

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(bind-key "M-Q" 'unfill-paragraph global-map)

(defun bs/font-configuration ()
  "Configure custom fonts."
  (custom-theme-set-faces
   'user
   ;; configure overall variable pitch and fixed pitch fonts
   '(default ((t (:family "Monospace" :weight normal :height 101))))
   '(variable-pitch ((t (:family "SF Pro Text" :height 160))))
   '(fixed-pitch ((t (:family "Monospace" :weight normal :height 101))))))

(defun bs/frame-functions (frame)
  "Configure custom settings given initial non-daemon FRAME. Intended
for `after-make-frame-functions'."
  (bs/font-configuration))

(if bs/at-work
    (bs/font-configuration)
  (add-hook 'after-make-frame-functions #'bs/frame-functions))

(provide 'init-functions)
;;; init-functions.el ends here
