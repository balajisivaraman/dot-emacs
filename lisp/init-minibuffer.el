;;; init-minibuffer.el --- Configures Minibuffer and Completions -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

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

;; This file configures minibuffer to my liking along with the completions buffer.

;;; Code:

(use-package minibuffer
  :ensure nil
  :config
  (setq completion-styles
        '(substring initials flex partial-completion orderless))
  (setq completion-category-overrides
        '((file (styles . (partial-completion orderless)))))
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold 2)
  (setq completion-pcm-complete-word-inserts-delimiters nil)
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  (setq completions-format 'one-column)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)
  (file-name-shadow-mode t)
  (minibuffer-electric-default-mode t))

;; Most of the following is taken directly or adapted from
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-minibuffer.el
(defgroup bs/minibuffer ()
  "Extensions for the minibuffer."
  :group 'minibuffer)

(defcustom bs/minibuffer-live-update-delay 0.3
  "Delay with which *Completions* buffer must be updated."
  :type 'number
  :group 'bs/minibuffer)

(defcustom bs/minibuffer-minimum-input 3
  "Minimum number of characters after which we must populate completion buffer."
  :type 'number
  :group 'bs/minibuffer)

(defun bs/minibuffer--honor-inhibit-message (fn &rest args)
  "Skip applying FN to ARGS if `inhibit-message' is t.
Meant as `:around' advice for `minibuffer-message', which does
not honor minibuffer message."
  (unless inhibit-message
    (apply fn args)))

(advice-add #'minibuffer-message :around #'bs/minibuffer--honor-inhibit-message)

(defun bs/get-minibuffer-contents ()
  "Gets the current contents of the minibuffer."
  (buffer-substring-no-properties (minibuffer-prompt-end) (point-max)))

(defun bs/check-minibuffer-minimum-input ()
  "Check whether minibuffer has at lesat `bs/minibuffer-minimum-input' characters."
  (>= (length (bs/get-minibuffer-contents)) bs/minibuffer-minimum-input))

(defun bs/minibuffer--live-completions (&rest _)
  "Update the *Completions* buffer."
  (when (minibufferp)
    (let ((while-no-input-ignore-events '(selection-request)))
      (while-no-input
        (if (bs/check-minibuffer-minimum-input)
            (condition-case nil
                (save-match-data
                  (save-excursion
                    (let ((inhibit-message t)
                          (ring-bell-function #'ignore))
                      (minibuffer-completion-help))))
              (quit (abort-recursive-edit)))
          (minibuffer-hide-completions))))))

(defun bs/minibuffer--live-completions-with-delay (&rest _)
  "Update Minibuffer Completions with `bs/minibuffer-live-update-delay'."
  (let ((delay bs/minibuffer-live-update-delay))
    (when (>= delay 0)
      (run-with-idle-timer delay nil #'bs/minibuffer--live-completions))))

(defun bs/minibuffer--setup-completions ()
  "Setup the completions buffer."
  (add-hook 'after-change-functions #'bs/minibuffer--live-completions-with-delay))

(add-hook 'minibuffer-setup-hook #'bs/minibuffer--setup-completions)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
