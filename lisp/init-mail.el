;;; init-mail.el --- Loads Mu4e and other assorted goodies -*- lexical-binding: t -*-

;; Copyright (C) 2022 Balaji Sivaraman

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

;; This library sets up Mu4e and other assorted goodies.

;;; Code:

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands (mu4e)
  :config
  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
        mu4e-update-interval (* 10 60) ; check mail 10 minutes
        mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "/media/backup/mail/"
        mu4e-completing-read-function 'completing-read
        mu4e-compose-format-flowed t)

  (setq mu4e-contexts
        (list
         ;; TW account
         (make-mu4e-context
          :name "TW"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/tw" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address     . "balajis@thoughtworks.com")
                  (user-full-name        . "Balaji Sivaraman")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder    . "/tw/[Gmail]/Drafts")
                  (mu4e-sent-folder      . "/tw/[Gmail]/Sent Mail")
                  (mu4e-refile-folder    . "/tw/[Gmail]/All Mail")
                  (mu4e-trash-folder     . "/tw/[Gmail]/Trash")))

         ;; Proton account
         (make-mu4e-context
          :name "Proton"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address       . "balaji@balajisivaraman.com")
                  (user-full-name          . "Balaji Sivaraman")
                  (auth-sources            . '("~/.authinfo"))
                  (smtpmail-smtp-server    . "127.0.0.1")
                  (smtpmail-smtp-service   . 1025)
                  (starttls-gnutls-program . "gnutls-cli")
                  (starttls-use-gnutls     . t)
                  (smtpmail-stream-type    . 'starttls)
                  (mu4e-drafts-folder      . "/personal/Drafts")
                  (mu4e-sent-folder        . "/personal/Sent")
                  (mu4e-refile-folder      . "/personal/All Mail")
                  (mu4e-trash-folder       . "/personal/Trash")))))

  (setq mu4e-maildir-shortcuts
        '(("/personal/inbox"     . ?i)
          ("/personal/Sent"      . ?s)
          ("/personal/Trash"     . ?t)
          ("/personal/Drafts"    . ?d)
          ("/personal/All Mail"  . ?a)))

  (setq message-send-mail-function 'smtpmail-send-it
        mail-user-agent 'mu4e-user-agent))

(use-package org-mime
  :commands (org-mime-htmlize))

(provide 'init-mail)
;;; init-mail.el ends here
