;;; init-files.el --- Some customizations for working with files. -*- lexical-binding: t -*-

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

;; Make working with files in emacs easier than before.

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
  :diminish (dired-omit-mode . "Ⓞ")
  :ensure nil
  :defer t
  :config
  (setq
   dired-auto-revert-buffer t
   dired-listing-switches "-alhF"
   dired-ls-F-marks-symlinks t
   dired-recursive-copies 'always
   diredp-hide-details-initially-flag nil
   dired-dwim-target t)
  (unbind-key "M-g" dired-mode-map))

(use-package dired+
  :after dired)

(use-package dired-x
  :after dired
  :ensure nil
  :bind
  (("C-c f j" . dired-jump)
   ("C-x C-j" . dired-jump)))

;; Edit files as root, through Tramp
(use-package sudo-edit
  :defer t
  :bind
  (("C-c f s" . sudo-edit)
   ("C-c f S" . sudo-edit-current-file)))

;; Hardhat prevents us from editing user-protected files
(use-package hardhat
  :init (global-hardhat-mode)
  :config (setq hardhat-mode-lighter " Ⓗ"))

(provide 'init-files)
;;; init-files.el ends here
