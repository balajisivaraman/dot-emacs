;;; init-feed.el --- Set up Elfeed -*- lexical-binding: t -*-

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

;; This file sets up Elfeed, my feeds and custom actions.

;;; Code:

(use-package elfeed
  :commands (elfeed)
  :bind
  ("C-x w" . elfeed)
  :config
  (setq
   elfeed-db-directory (s-concat bs/nextcloud-path "Feeds"))
  (add-hook 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 weeks ago"
                                                       :remove 'unread))
  (setq-default elfeed-search-filter "@1-month-ago +unread +dev"))

;; All below functions are taken from:
;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
(defvar youtube-feed-format
  '(("^UC" . "https://www.youtube.com/feeds/videos.xml?channel_id=%s")
    ("^PL" . "https://www.youtube.com/feeds/videos.xml?playlist_id=%s")
    (""    . "https://www.youtube.com/feeds/videos.xml?user=%s")))

(defun elfeed--expand (listing)
  "Expand feed LISTING depending on their tags."
  (cl-destructuring-bind (url . tags) listing
    (cond
     ((member 'youtube tags)
      (let* ((case-fold-search nil)
             (test (lambda (s r) (string-match-p r s)))
             (format (cl-assoc url youtube-feed-format :test test)))
        (cons (format (cdr format) url) tags)))
     (listing))))

(defmacro elfeed-config (&rest feeds)
  "Minimizes FEEDS listing indentation without being weird about it."
  (declare (indent 0))
  `(setf elfeed-feeds (mapcar #'elfeed--expand ',feeds)))

(elfeed-config
 ;; Individual Blogs
 ("https://charity.wtf/feed/" dev blog)
 ("https://medium.com/feed/@copyconstruct" dev blog)
 ("https://martinfowler.com/feed.atom" dev blog)
 ("http://dtrace.org/blogs/bmc/feed/" dev blog)
 ("https://www.allthingsdistributed.com/atom.xml" dev blog)

 ;; Company Engineering Blogs
 ("https://engineering.linkedin.com/blog.rss.html" dev blog)
 ("https://engineering.atspotify.com/feed" dev blog)
 ("https://www.thoughtworks.com/rss/insights.xml" dev blog)
 ("https://cacm.acm.org/blogs/blog-cacm.rss" )

 ;; Company News Feeds
 ("https://cacm.acm.org/magazine.rss" dev release)

 ;; Youtube
 ("UC_QIfHvN9auy2CoOdSfMWDw" youtube) ;; Strange Loop
 ("UCs_tLP3AiwYKwdUHpltJPuA" youtube) ;; Goto Conferences
 )

(provide 'init-feed)
;;; init-feed.el ends here
