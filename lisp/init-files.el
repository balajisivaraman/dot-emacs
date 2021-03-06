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
  :ensure nil
  :bind (("C-c f z" . revert-buffer)
         ("C-c f /" . revert-buffer))
  :config
  (setq
   revert-without-query '("")))

(use-package focus-autosave-mode
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package dired
  :diminish (dired-omit-mode)
  :ensure nil
  :defer t
  :config
  (setq
   dired-auto-revert-buffer t
   dired-listing-switches "-alhF"
   dired-ls-F-marks-symlinks t
   dired-recursive-copies 'always
   dired-dwim-target t)
  (unbind-key "M-g" dired-mode-map))

(use-package dired+
  :disabled t
  :ensure nil
  :after dired)

(use-package dired-x
  :after dired
  :ensure nil
  :bind
  (("C-c f j" . dired-jump)
   ("C-x C-j" . dired-jump)))

;; Edit files as root, through Tramp
(use-package sudo-edit
  :bind
  (("C-c f s" . sudo-edit)
   ("C-c f S" . sudo-edit-current-file)))

;; Hardhat prevents us from editing user-protected files
(use-package hardhat
  :init (global-hardhat-mode)
  :config (setq hardhat-mode-lighter " Ⓗ"))

(use-package recentf
  :ensure nil
  :hook
  ((after-init . (lambda () (recentf-mode 1))))
  :init
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude '("/tmp/" "/ssh:"))
  (setq
   recentf-save-file (concat bs/emacs-cache-directory "recentf")))

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
