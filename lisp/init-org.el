;;; init-org.el --- Org Mode Customizations. -*- lexical-binding: t -*-

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>

;; The MIT License (MIT)

;; Copyright (C) 2017 Balaji Sivaraman

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

;; My customizations and additional packages for Org Mode.

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o d" . org-check-deadlines)
   ("C-c o b" . org-check-before-date)
   ("C-c o A" . org-check-after-date)
   ("C-c o r" . org-archive-subtree)
   :map org-mode-map
   ("C-c m l" . org-metaleft)
   ("C-c m r" . org-metaright))
  :init
  (advice-add 'org-agenda :after #'delete-other-windows)
  :config
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (setq
   org-todo-keywords
   '((sequence "TODO(t@/!)"
               "WAITING(w@/!)"
               "APPT(a!)"
               "DELEGATED(l@/!)"
               "STARTED(s!)"
               "|"
               "FEEDBACK(f@/!)"
               "VERIFY(v@/!)"
               "DONE(d@/!)"
               "DEFERRED(r@/!)"
               "CANCELLED(x@/!)"))
   org-agenda-files (quote ("~/ownCloud/Personal Notes/todo.org"))
   org-archive-location "/Users/balajisivaraman/ownCloud/Personal Notes/archives.org::"
   org-default-notes-file "~/ownCloud/Personal Notes/notes.org"
   org-agenda-ndays 21
   org-deadline-warning-days 14
   org-agenda-show-all-dates t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-reverse-note-order t
   org-log-done 'note))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package org-capture
  :ensure nil
  :after org
  :diminish (org-capture-mode . "ⓡ")
  :bind
  (("C-c o c" . org-capture))
  :config
  (setq
   org-capture-templates
   '(("t" "Todo" entry (file+headline "/Users/balajisivaraman/ownCloud/Personal Notes/todo.org" "Tasks"))
     ("n" "Note" entry (file "/Users/balajisivaraman/ownCloud/Personal Notes/notes.org")))))

(provide 'init-org)
;;; init-org.el ends here
