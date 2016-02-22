;;; -*- lexical-binding: t -*-
;;; init-keybindings.el --- General Keybindings for Emacs.

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

;; This file contains custom keybindings for Emacs. Mode-specific keybindings are available in their respective init files.

;;; Code:

;; Newline should always indent by default.
(bind-key "<RET>" 'newline-and-indent)

;; Use `p` and `n` to go to previous and next buffers.
(bind-key "C-x p" 'previous-buffer)
(bind-key "C-x n" 'next-buffer)

(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

(bind-key "C-c r" 'eval-region)

(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; Use Command Key as Meta on Macs
(if (string-equal system-type "darwin")
    (setq-default mac-command-modifier 'meta))

(bind-key "<key-4660>" 'ignore)
(bind-key "C-<key-4660>" 'ignore)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
