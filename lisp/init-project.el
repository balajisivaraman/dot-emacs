;;; init-project.el --- Custom functions for project management. -*- lexical-binding: t -*-

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

;; This file sets up and configures custom functions for project
;; management. It is meant to be a light-weight alternative for
;; something like Projectile.

;;; Code:

(defvar bs/project-map)
(define-prefix-command 'bs/project-map)
(global-unset-key (kbd "C-c p"))
(global-set-key (kbd "C-c p") 'bs/project-map)

(use-package find-file-in-project
  :commands (find-file-in-project)
  :bind
  ("C-c p f" . find-file-in-project)
  :config
  (setq ffip-use-rust-fd t))

(defun bs/kill-all-project-buffers ()
  "Kill all open buffers for the current project."
  (interactive)
  (let* ((project-root (f-filename (f-long (ffip-project-root))))
         (open-buffers (buffer-list))
         (project-file-names (ffip-project-search "" nil))
         (open-buffers-for-project
          (-filter (lambda (buffer)
                     (let ((buffer-file-name (buffer-file-name buffer))
                           (buffer-name (buffer-name buffer)))
                       (or (and (s-present? buffer-file-name)
                                (s-contains? project-root buffer-file-name))
                           (and (s-present? buffer-name)
                                (s-contains? project-root buffer-name))))) open-buffers)))
    (when (and (not (seq-empty-p open-buffers-for-project))
               (yes-or-no-p
                (format
                 "Are you sure you want to kill %d open buffers for project %s?"
                 (seq-length open-buffers-for-project)
                 project-root)))
      (-map 'kill-buffer open-buffers-for-project))))

(defun bs/open-project (base-path)
  "Opens a project within BASE-PATH and provides embark actions also."
  (interactive)
  (let* ((find-command (format "fd '' -t d --base-directory %s -d 1" base-path))
         (find-command-output (shell-command-to-string find-command))
         (project-list (-list (s-split "\n" find-command-output)))
         (selected-project (completing-read "Open project: " project-list)))
    (dired (format "%s/%s" base-path selected-project))))

(defun bs/open-my-projects ()
  "Opens a project within ~/projects."
  (interactive)
  (bs/open-project "~/projects"))

(bind-key "C-c p p" 'bs/open-my-projects)
(bind-key "C-c p k" 'bs/kill-all-project-buffers)

(provide 'init-project)
;;; init-project.el ends here
