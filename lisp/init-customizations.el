;;; init-customizations.el --- Settings that I prefer Emacs to have out of the box. -*- lexical-binding: t -*-

;; Copyright (C) 2017  Balaji Sivaraman

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

;; This file loads and initializes options and packages that I prefer my Emacs to have out of the box.

;;; Code:

;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)

;; Backup Directory Configuration
(set-variable 'temporary-file-directory user-temp-directory)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save minibuffer history across sessions
(setq savehist-file (s-concat user-temp-directory ".savehist"))
(savehist-mode 1)

(require 'desktop)
;; Set the location to save/load default desktop
(setq desktop-path
  (-snoc (-drop 2 desktop-path) user-temp-directory)
  )
(setq desktop-dirname user-temp-directory)
;; Restore 5 files eagerly, and the rest lazily, when Emacs idles.
(setq desktop-restore-eager 5)
;; Load the saved desktop always, even if it is locked.
(setq desktop-load-locked-desktop t)
;; Delete files by moving them to trash
(setq delete-by-moving-to-trash t)
;; Enable Desktop Save Mode
(desktop-save-mode 1)

;; Saveplace Mode - Saves Cursor Position Within Files
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (s-concat user-temp-directory ".saveplace"))
  (setq-default save-place t))

(provide 'init-customizations)
;;; init-customizations.el ends here
