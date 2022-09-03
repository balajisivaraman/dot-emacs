;;; init-customizations.el --- Settings that I prefer Emacs to have out of the box. -*- lexical-binding: t -*-

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

;; This file loads and initializes options and packages that I prefer my Emacs to have out of the box.

;;; Code:

;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)

;; Backup Directory Configuration
(set-variable 'temporary-file-directory (concat bs/emacs-cache-directory "temp"))
(setq auto-save-list-file-prefix (concat bs/emacs-cache-directory "auto-save-list/.saves-"))
(when (not (file-exists-p temporary-file-directory))
  (mkdir temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save minibuffer history across sessions
(setq savehist-file (concat bs/emacs-cache-directory ".savehist"))
(savehist-mode 1)

(setq delete-by-moving-to-trash t)
(desktop-save-mode 0)

;; Saveplace Mode - Saves Cursor Position Within Files
(use-package saveplace
  :straight nil
  :custom
  (save-place-file (concat bs/emacs-cache-directory ".saveplace"))
  :config
  (save-place-mode t))

(defvar bs/at-work)
(setq bs/at-work (or (string-equal (downcase (system-name)) "alphacentauri")
                     (string-equal (downcase (system-name)) "tatooine")
                     (string-equal (downcase (system-name)) "korriban.local")))

(defvar bs/nextcloud-path)
(setq bs/nextcloud-path
      (cond
       ((and bs/at-work (eq system-type 'windows-nt)) "C:/Users/Balaji Sivaraman/Nextcloud/")
       ((string-equal (downcase (system-name)) "tatooine") "/home/balaji/Nextcloud/")
       ((string-equal (downcase (system-name)) "korriban.local") "/Users/balaji/Nextcloud/")
       (bs/at-work "/mnt/c/Users/Balaji Sivaraman/Nextcloud/")
       (t "/media/backup/Nextcloud/")))
(defvar bs/tasks-path)
(setq bs/tasks-path (s-concat bs/nextcloud-path "ThePlainTextLife/tasks/"))
(defvar bs/notes-path)
(setq bs/notes-path (s-concat bs/nextcloud-path "ThePlainTextLife/notes/"))

(defvar bs/org-agenda-icons-path)
(setq bs/org-agenda-icons-path (s-concat user-emacs-directory "icons/"))

(provide 'init-customizations)
;;; init-customizations.el ends here
