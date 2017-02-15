;;; init-functions.el --- Utility Macros and Functions -*- lexical-binding: t -*-

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

;; This file contains utility functions and macros that I wrote or pilfered from the web like any self-respecting Emacs user.

;;; Code:

(defun balaji/network-connection-available-p ()
  "Check whether we have internet connectivity."
  (-any-p
   (lambda (interface) (s-starts-with-p "en" (car interface)))
   (network-interface-list)))

(defun balaji/duplicate-line-or-region (&optional begin end)
  "If region is active, duplicates it. Othewise duplicates the current line."
  (interactive "r")
  (save-excursion
    (let ((reg-begin begin)
          (reg-end end)
          (line-text))
      (if (not (region-active-p))
          (progn
            (balaji/smarter-move-beginning-of-line nil)
            (setq reg-begin (point))
            (move-end-of-line nil)
            (setq reg-end (point))))
      (setq line-text (buffer-substring reg-begin reg-end))
      (goto-char reg-end)
      (move-end-of-line nil)
      (newline)
      (insert line-text))))
(bind-key "C-c C-d" 'balaji/duplicate-line-or-region)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun balaji-dot-emacs ()
  "Go directly to .emacs, do not pass Go, do not collect $200."
  (interactive)
  (message "Stop procrastinating and do some work!")
  (find-file (s-concat user-emacs-directory "init.el")))
(bind-key "C-c f d" 'balaji-dot-emacs)

(defun balaji-indent-region-or-buffer (&optional begin end)
  "Indent a region or the whole file.

If called after a region is marked, indents the region between BEGIN and END.

Otherwise indents the whole buffer, i.e. everything between `point-min' and `point-max'"
  (interactive "r")
  (save-excursion
    (delete-trailing-whitespace)
    (if (region-active-p)
        (indent-region begin end nil)
      (indent-region (point-min) (point-max) nil))))
(bind-key "C-c B i" 'balaji-indent-region-or-buffer)

(provide 'init-functions)
;;; init-functions.el ends here
