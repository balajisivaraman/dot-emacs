;;; init-files.el --- Some customizations for working with files. -*- lexical-binding: t -*-

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

;; Make working with files in Emacs easier than before.

;;; Code:

(use-package files
  :straight nil
  :bind
  (("C-c f f" . find-file)
   ("C-c f s" . save-buffer)
   ("C-c f w" . write-file)
   ("C-c f z" . revert-buffer)
   ("C-c f /" . revert-buffer))
  :custom
  (revert-without-query '("")))

(use-package focus-autosave-mode
  :config (focus-autosave-mode))

(use-package dired
  :straight nil
  :defer t
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alhF")
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  :config
  (unbind-key "M-g" dired-mode-map))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Edit files as root, through Tramp
(use-package sudo-edit
  :commands (sudo-edit sudo-edit-current-file)
  :bind
  (("C-c f s" . sudo-edit)
   ("C-c f S" . sudo-edit-current-file)))

;; Hardhat prevents us from editing user-protected files
(use-package hardhat
  :custom (hardhat-mode-lighter "")
  :config (global-hardhat-mode))

(use-package recentf
  :straight nil
  :hook
  ((after-init . (lambda () (recentf-mode 1))))
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude '("/tmp/" "/ssh:"))
  (recentf-save-file (concat bs/emacs-cache-directory "recentf")))

(use-package ignoramus
  :config
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    (add-to-list 'ignoramus-file-basename-exact-names name))
  (ignoramus-setup))

(provide 'init-files)
;;; init-files.el ends here
