;;; -*- lexical-binding: t -*-
;;; init-files.el --- Some customizations for working with files

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

(defun balaji/revert-buffer ()
  "Revert buffers without confirming first"
  (interactive)
  (revert-buffer nil t t))

(use-package files
  :ensure nil
  :bind (("C-c f z" . balaji/revert-buffer)
         ("C-c f /" . balaji/revert-buffer)))

(use-package focus-autosave-mode
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package dired
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

;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind
  (("C-c f J" . reveal-in-osx-finder)))

(provide 'init-files)
;;; init-files.el ends here
