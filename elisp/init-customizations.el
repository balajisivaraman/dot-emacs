;;; -*- lexical-binding: t -*-
;;; init-customizations.el --- Settings that I prefer Emacs to have out of the box.

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

;; This file loads and initializes options and packages that I prefer my Emacs to have out of the box.

;;; Code:

(require 'init-package)

;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)

;; Backup Directory Configuration
(set-variable 'temporary-file-directory (s-concat user-emacs-directory "temp"))
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save minibuffer history across sessions
(setq savehist-file (s-concat user-emacs-directory ".savehist"))
(savehist-mode 1)

;; Enable Desktop Save Mode
(desktop-save-mode 1)
;; Restore 5 files eagerly, and the rest lazily, when Emacs idles.
(setq desktop-restore-eager 5)
;; Load the saved desktop always, even if it is locked.
(setq desktop-load-locked-desktop t)
;; Set the location to save/load default desktop
(setq desktop-dirname user-emacs-directory)
;; Delete files by moving them to trash
(setq delete-by-moving-to-trash t)

;; Saveplace Mode - Saves Cursor Position Within Files
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (s-concat user-emacs-directory ".saveplace"))
  (setq-default save-place t))

;; Exec Path From Shell is always necessary
;; So I'll go ahead and add it here for now
(use-package exec-path-from-shell
  :init
  (when (not (string-equal system-type "windows-nt"))
    (exec-path-from-shell-initialize)))

(use-package ns-win
  :defer t
  :ensure nil
  :if (eq system-type 'darwin)
  :config
  (setq
   ns-pop-up-frames nil
   mac-option-modifier 'meta
   mac-command-modifier 'meta))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :init (osx-trash-setup))

(provide 'init-customizations)
;;; init-customizations.el ends here
