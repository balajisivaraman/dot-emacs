;;; init-scala.el --- Configurations for coding in Scala. -*- lexical-binding: t -*-

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

;; This module loads Scala Mode, SBT Mode and Ensime to help with Scala development.

;;; Code:

(use-package scala-mode
  :mode
  ("\\.scala" . scala-mode)
  :config
  (setq
   scala-indent:default-run-on-strategy scala-indent:eager-strategy
   scala-indent:indent-value-expression t
   scala-indent:align-parameters t
   scala-indent:align-forms t
   ensime-startup-snapshot-notification nil))

(use-package ensime
  :pin melpa-stable
  :commands ensime ensime-mode
  :after scala-mode
  :bind
  (:map scala-mode-map
        ("C-c m e" . ensime)
        ("C-c m s" . ensime-shutdown)
   :map ensime-mode-map
        ("C-c m E" . ensime-reload))
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-default-buffer-prefix "ENSIME-"
   ensime-prefer-noninteractive t
   ensime-refactor-enable-beta t
   ensime-refactor-preview t
   ensime-startup-notification nil
   ensime-auto-connect 'always
   ensime-refactor-preview-override-hunk 10)
  :config
  (ensime-company-enable))

(use-package sbt-mode
  :commands sbt-start sbt-command)

(defcustom
  balaji/scala-mode-prettify-symbols
  '(("+-" . ?±))
  "Prettify symbols for scala-mode.")

(defun balaji/scala-mode-hook ()
  "Hooks for Scala Mode."
  (setq prettify-symbols-alist balaji/scala-mode-prettify-symbols)
  (company-mode t)
  (ensime-mode t)
  (ensime))

(add-hook 'scala-mode-hook 'balaji/scala-mode-hook)

(provide 'init-scala)
;;; init-scala.el ends here
