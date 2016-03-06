;;; -*- lexical-binding: t -*-
;;; init-functions.el --- Utility functions and macros

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

;; This file contains utility functions and macros that I wrote or picked from the web.

;;; Code:

(defun balaji/network-connection-available-p ()
    "Check whether we have internet connectivity"
  (-any-p
   (lambda (interface) (s-starts-with-p "en" (car interface)))
   (network-interface-list)))

;; Taken from Bozhidor Batsov's blog: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun balaji/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(bind-key "C-a" 'balaji/smarter-move-beginning-of-line)

(defun balaji/duplicate-line-or-region (mark point)
  "If region is active, duplicates it. Othewise duplicates the current line."
  (interactive "r")
  (save-excursion
    (let ((reg-begin mark)
          (reg-end  point))
      (if (not (region-active-p))
          (progn
            (balaji/smarter-move-beginning-of-line nil)
            (setq reg-begin (point))
            (move-end-of-line nil)
            (setq reg-end (point))))
      (kill-ring-save reg-begin reg-end)
      (goto-char reg-end)
      (move-end-of-line nil)
      (newline-and-indent)
      (yank))))
(bind-key "C-c C-d" 'balaji/duplicate-line-or-region)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(provide 'init-functions)
