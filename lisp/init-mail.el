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
  :straight nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :bind (("C-c m m" . mu4e))
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
                  (mu4e-trash-folder     . "/tw/[Gmail]/Trash")
                  (org-msg-signature       . "

Thanks & Regards,

#+begin_signature
*Balaji Sivaraman*
/Lead Consultant Developer/
#+ATTR_HTML: :border 0 :rules all :frame border
|              |                                                 |
| Pronouns     | *{{{color(#F7617A,He/Him)}}}*                   |
| Email        | *{{{color(#F7617A,balajis@thoughtworks.com)}}}* |
| Telephone    | *{{{color(#F7617A,+91 98848 33476)}}}*          |
#+ATTR_HTML: :width 300px
[[http://www.thoughtworks.com/?utm_campaign=balaji-sivaraman-signature&utm_medium=email&utm_source=thoughtworks-email-signature-generator][https://gentle-reef-2837.herokuapp.com/images/thoughtworks_logo.519d4211.png]]
[[https://www.thoughtworks.com?utm_source=email-signature&utm_medium=email&utm_campaign=brand][https://gentle-reef-2837.herokuapp.com/images/carrers-white-blue.jpg]]
\"The list is an absolute good. The list is life. All around its margins lies the gulf.\"
 #+end_signature
")))

         ;; Proton account
         (make-mu4e-context
          :name "Proton"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address       . "balaji@balajisivaraman.com")
                  (user-full-name          . "Balaji Sivaraman")
                  (smtpmail-smtp-server    . "127.0.0.1")
                  (smtpmail-smtp-service   . 1025)
                  (starttls-gnutls-program . "gnutls-cli")
                  (starttls-use-gnutls     . t)
                  (smtpmail-stream-type    . starttls)
                  (mu4e-drafts-folder      . "/personal/Drafts")
                  (mu4e-sent-folder        . "/personal/Sent")
                  (mu4e-refile-folder      . "/personal/All Mail")
                  (mu4e-trash-folder       . "/personal/Trash")
                  (org-msg-signature       . "

Thanks & Regards,

#+begin_signature
Balaji Sivaraman
 #+end_signature
")))))

  (setq mu4e-maildir-shortcuts
        '(("/personal/inbox"     . ?i)
          ("/personal/Sent"      . ?s)
          ("/personal/Trash"     . ?t)
          ("/personal/Drafts"    . ?d)))

  (setq message-send-mail-function 'smtpmail-send-it
        mail-user-agent 'mu4e-user-agent))

(use-package org-msg
  :after (mu4e)
  :config
  (setq
   org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
   org-msg-startup "hidestars indent inlineimages"
   org-msg-greeting-fmt "\nHi%s,\n\n"
   org-msg-recipient-names '(("balajis@thoughtworks.com" . "Balaji Sivaraman"))
   org-msg-greeting-name-limit 3
   org-msg-default-alternatives '((new       . (text html))
                                  (reply-to-html . (text html))
                                  (reply-to-text . (text)))
   org-export-global-macros  '(("color" . "@@html:<span style=\"color: $1\">$2</span>@@@@latex:\textcolor{$1}{$2}@@@@odt:<text:span text:style-name=\"$1\">$2</text:span>@@"))
   org-msg-convert-citation t
   org-msg-enforce-css
   '((del nil
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (color . "grey")
           (border-left . "none")
           (text-decoration . "line-through")
           (margin-bottom . "0px")
           (margin-top . "10px")
           (line-height . "11pt")))
     (a nil
        ((color . "#0071c5")))
     (a reply-header
        ((color . "black")
         (text-decoration . "none")))
     (div reply-header
          ((padding . "3.0pt 0in 0in 0in")
           (border-top . "solid #e1e1e1 1.0pt")
           (margin-bottom . "20px")))
     (span underline
           ((text-decoration . "underline")))
     (li nil
         ((font-family . "\"Arial\"")
          (font-size . "10pt")
          (line-height . "10pt")
          (margin-bottom . "0px")
          (margin-top . "2px")))
     (nil org-ul
          ((list-style-type . "square")))
     (nil org-ol
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (line-height . "10pt")
           (margin-bottom . "0px")
           (margin-top . "0px")
           (margin-left . "30px")
           (padding-top . "0px")
           (padding-left . "5px")))
     (nil signature
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (margin-bottom . "20px")))
     (blockquote quote0
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (border-left . "3px solid #ccc")))
     (blockquote quote1
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#324e72")
                  (border-left . "3px solid #557fb4")))
     (blockquote quote2
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#6a3a4c")
                  (border-left . "3px solid #a8617c")))
     (blockquote quote3
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#7a4900")
                  (border-left . "3px solid #e08600")))
     (blockquote quote4
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#ff34ff")
                  (border-left . "3px solid #ff9afe")))
     (blockquote quote5
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#ff4a46")
                  (border-left . "3px solid #ffadab")))
     (blockquote quote6
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#008941")
                  (border-left . "3px solid #00ef71")))
     (blockquote quote7
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#006fa6")
                  (border-left . "3px solid #0daefe")))
     (blockquote quote8
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#a30059")
                  (border-left . "3px solid #ff098f")))
     (blockquote quote9
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#ffdbe5")
                  (border-left . "3px solid #ffffff")))
     (blockquote quote10
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#000000")
                  (border-left . "3px solid #333333")))
     (blockquote quote11
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#0000a6")
                  (border-left . "3px solid #0d0dfe")))
     (blockquote quote12
                 ((padding-left . "5px")
                  (margin-left . "10px")
                  (margin-top . "10px")
                  (margin-bottom . "0")
                  (font-style . "italic")
                  (background . "#f9f9f9")
                  (color . "#63ffac")
                  (border-left . "3px solid #c8ffe2")))
     (code nil
           ((font-size . "10pt")
            (font-family . "monospace")
            (background . "#f9f9f9")))
     (code src\ src-asl
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-c
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-c++
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-conf
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-cpp
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-csv
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-diff
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-ditaa
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-emacs-lisp
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-fundamental
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-ini
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-json
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-makefile
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-man
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-org
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-plantuml
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-python
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-sh
           ((color . "#ffffff")
            (background-color . "#000000")))
     (code src\ src-xml
           ((color . "#ffffff")
            (background-color . "#000000")))
     (nil linenr
          ((padding-right . "1em")
           (color . "black")
           (background-color . "#aaaaaa")))
     (pre nil
          ((line-height . "12pt")
           (color . "#ffffff")
           (background-color . "#000000")
           (margin . "0px")
           (font-size . "9pt")
           (font-family . "monospace")))
     (div org-src-container
          ((margin-top . "10px")))
     (nil figure-number
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (color . "#0071c5")
           (font-weight . "bold")
           (text-align . "left")))
     (nil table-number)
     (caption nil
              ((text-align . "left")
               (background . "#0071c5")
               (color . "white")
               (font-weight . "bold")))
     (nil t-above
          ((caption-side . "top")))
     (nil t-bottom
          ((caption-side . "bottom")))
     (nil listing-number
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (color . "#0071c5")
           (font-weight . "bold")
           (text-align . "left")))
     (nil figure
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (color . "#0071c5")
           (font-weight . "bold")
           (text-align . "left")))
     (nil org-src-name
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (color . "#0071c5")
           (font-weight . "bold")
           (text-align . "left")))
     (table nil
            ((font-family . "\"Arial\"")
             (font-size . "10pt")
             (margin-top . "0px")
             (line-height . "10pt")
             (border-collapse . "collapse")))
     (th nil
         ((border . "1px solid white")
          (background-color . "#0071c5")
          (color . "white")
          (padding-left . "10px")
          (padding-right . "10px")))
     (td nil
         ((font-family . "\"Arial\"")
          (font-size . "10pt")
          (margin-top . "0px")
          (padding-left . "0px")
          (padding-right . "20px")
          (border . "1px solid white")))
     (td org-left
         ((text-align . "left")))
     (td org-right
         ((text-align . "right")))
     (td org-center
         ((text-align . "center")))
     (div outline-text-4
          ((margin-left . "15px")))
     (div outline-4
          ((margin-left . "10px")))
     (h4 nil
         ((margin-bottom . "0px")
          (font-size . "11pt")
          (font-family . "\"Arial\"")))
     (h3 nil
         ((margin-bottom . "0px")
          (text-decoration . "underline")
          (color . "#0071c5")
          (font-size . "12pt")
          (font-family . "\"Arial\"")))
     (h2 nil
         ((margin-top . "20px")
          (margin-bottom . "20px")
          (font-style . "italic")
          (color . "#0071c5")
          (font-size . "13pt")
          (font-family . "\"Arial\"")))
     (h1 nil
         ((margin-top . "20px")
          (margin-bottom . "0px")
          (color . "#0071c5")
          (font-size . "12pt")
          (font-family . "\"Arial\"")))
     (p nil
        ((text-decoration . "none")
         (margin-bottom . "0px")
         (margin-top . "10px")
         (line-height . "11pt")
         (font-size . "10pt")
         (font-family . "\"Arial\"")))
     (div nil
          ((font-family . "\"Arial\"")
           (font-size . "10pt")
           (line-height . "11pt")))))
  (org-msg-mode))

(use-package org-mime
  :commands (org-mime-htmlize org-mime-edit-mail-in-org-mode))

(provide 'init-mail)
;;; init-mail.el ends here
