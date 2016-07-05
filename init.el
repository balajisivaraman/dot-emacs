;;; init.el --- Emacs Configuration for Balaji Sivaraman -*- lexical-binding: t -*-

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

;; Emacs Configuration for Balaji Sivaraman that serves the following primary uses;
;;
;;  - Scala
;;  - Haskell
;;  - Emacs (and other) Lisp
;;  - Org Mode (Daily Agenda, Note Taking, Day Planner etc.)
;;  - Personal Finance (Ledger)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Benchmarking Startup Begin
(defconst emacs-start-time (current-time))


;;; Initial Setup
(defvar balaji/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path balaji/site-lisp-dir)


;;; Package Management
(add-to-list 'load-path (expand-file-name "use-package" balaji/site-lisp-dir))
(require 'use-package)
;; Always install packages from Melpa, Elpa
;; Over-ridden when not used by setting (:ensure nil) in use-package declarations
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish nil t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
(package-initialize)
;; package.el should not initialize our packages.
;; We're going to use use-package for that.
(setq package-enable-at-startup nil)

(defun package-require (package)
  "Install `PACKAGE` only if it is not already installed."
  (unless (package-installed-p package)
    (package-install package nil)))

(use-package paradox
  :bind (("C-c a p" . paradox-list-packages)
         ("C-c a P" . package-list-packages-no-fetch)
         ("C-c a u" . paradox-upgrade-packages))
  :config
  (setq
   paradox-execute-asynchronously nil ; No async update, please
   paradox-spinner-type 'moon      ; Fancy spinner
   ;; Show all possible counts
   paradox-display-download-count t
   paradox-display-star-count t
   ;; Don't star automatically
   paradox-automatically-star nil
   ;; Hide download button, and wiki packages
   paradox-use-homepage-buttons nil ; Can type v instead
   paradox-hide-wiki-packages t))


;;; Configure Libraries
(use-package s    :load-path "lib/s-el"    :ensure nil)
(use-package dash :load-path "lib/dash-el" :ensure nil)
(use-package f    :load-path "lib/f-el"    :ensure nil)


;;; Utility Macros and Functions
(defun balaji/network-connection-available-p ()
  "Check whether we have internet connectivity."
  (-any-p
   (lambda (interface) (s-starts-with-p "en" (car interface)))
   (network-interface-list)))

;; Taken from Bozhidor Batsov's blog: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun balaji/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(bind-key "C-a" 'balaji/smarter-move-beginning-of-line)

(defun balaji/duplicate-line-or-region (&optional begin end)
  "If region is active, duplicates it. Othewise duplicates the current line."
  (interactive "r")
  (save-excursion
    (let ((reg-begin begin)
          (reg-end end)
          (line-text))
      (if (not (region-active-p))
          (progn
            (balaji/smarter-move-beginning-of-line nil)
            (setq reg-begin (point))
            (move-end-of-line nil)
            (setq reg-end (point))))
      (setq line-text (buffer-substring reg-begin reg-end))
      (goto-char reg-end)
      (move-end-of-line nil)
      (newline)
      (insert line-text))))
(bind-key "C-c C-d" 'balaji/duplicate-line-or-region)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;; Below functions are taken from Bodil Stokke's ohai-emacs.
;; It can be found here: https://github.com/bodil/ohai-emacs/blob/master/ohai/ohai-lib.el
(defun balaji/exec (command)
  "Run a shell command and return its output as a string, whitespace trimmed."
  (s-trim (shell-command-to-string command)))

(defun balaji/is-exec (command)
  "Returns true if `command' is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun balaji/resolve-exec (command)
  "If `command' is an executable on the system search path, return its absolute path.
Otherwise, return nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun balaji/exec-if-exec (command args)
  "If `command' satisfies `balaji/is-exec', run it with `args' and return its
output as per `balaji/exec'. Otherwise, return nil."
  (when (balaji/is-exec command) (balaji/exec (s-concat command " " args))))

(defun balaji/getent (user)
  "Get the /etc/passwd entry for the user `user' as a list of strings,
or nil if there is no such user. Empty fields will be represented as nil,
as opposed to empty strings."
  (-let [ent (balaji/exec (s-concat "getent passwd " user))]
    (when (not (s-blank? ent))
      (-map (lambda (i) (if (s-blank? i) nil i))
            (s-split ":" ent)))))

(defun balaji/user-full-name ()
  "Guess the user's full name. Returns nil if no likely name could be found."
  (or (balaji/exec-if-exec "git" "config --get user.name")
      (elt (balaji/getent (getenv "USER")) 4)))
(setq user-full-name (balaji/user-full-name))

(defun balaji/user-email ()
  "Guess the user's email address. Returns nil if none could be found."
  (or (balaji/exec-if-exec "git" "config --get user.email")
      (getenv "EMAIL")))
(setq user-mail-address (balaji/user-email))

(defun balaji-dot-emacs ()
  "Go directly to .emacs, do not pass Go, do not collect $200."
  (interactive)
  (message "Stop procrastinating and do some work!")
  (find-file (s-concat user-emacs-directory "init.el")))
(bind-key "C-c f d" 'balaji-dot-emacs)

(defun balaji-indent-region-or-buffer (&optional begin end)
  "Indent a region or the whole file.

If called after a region is marked, indents the region between BEGIN and END.

Otherwise indents the whole buffer, i.e. everything between `point-min' and `point-max'"
  (interactive "r")
  (save-excursion
    (delete-trailing-whitespace)
    (if (region-active-p)
        (indent-region begin end nil)
      (indent-region (point-min) (point-max) nil))))
(bind-key "C-c B i" 'balaji-indent-region-or-buffer)


;;; Better Defaults
;; Hide the menu bar, tool bar and scroll bar
;; For the menu bar, don't hide it in OSX, since the top bar is always visible anyway
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Don't display the default start up message
;; If we're loading from a saved desktop, this setting becomes redundant.
(setq inhibit-startup-message t)
;; Highlight the current line on all buffers
(global-hl-line-mode t)
;; Who ever wants to fully type out YES and NO
(fset 'yes-or-no-p 'y-or-n-p)
;; This will help us open up buffers without confirmation.
(setq confirm-nonexistent-file-or-buffer nil)
;; Setting word wrap mode as default
(global-visual-line-mode 1)
(diminish 'visual-line-mode)
;; Highlight matching parens
(show-paren-mode t)
;; Show column number in bar
(column-number-mode t)
;; Typed text will replace the selection as in most modern editors
(delete-selection-mode t)
;; Display line numbers to the left of the buffer
(global-linum-mode t)
;; When I grew up, sentences always ended with a single space.
(setq sentence-end-double-space nil)
;; Prettify Symbols Mode in Emacs > 24.4 is awesome.
(setq-default prettify-symbols-mode t)
(global-prettify-symbols-mode t)
;; Electric Pair Mode
(electric-pair-mode t)
(blink-cursor-mode -1)

(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      visible-bell t
      load-prefer-newer t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(use-package iso-transl :ensure nil)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)


;;; Customizations
;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)

;; Backup Directory Configuration
(set-variable 'temporary-file-directory (s-concat user-emacs-directory "temp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; save minibuffer history across sessions
(setq savehist-file (s-concat user-emacs-directory ".savehist"))
(savehist-mode 1)

;; Enable Desktop Save Mode
(desktop-save-mode 1)
;; Restore 5 files eagerly, and the rest lazily, when Emacs idles.
(setq desktop-restore-eager 5)
;; Load the saved desktop always, even if it is locked.
(setq desktop-load-locked-desktop t)
;; Set the location to save/load default desktop
(setq desktop-dirname user-emacs-directory)
;; Delete files by moving them to trash
(setq delete-by-moving-to-trash t)

;; Saveplace Mode - Saves Cursor Position Within Files
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (s-concat user-emacs-directory ".saveplace"))
  (setq-default save-place t))


;;; Environment Fixup
;; Exec Path From Shell is always necessary
;; So I'll go ahead and add it here for now
(use-package exec-path-from-shell
  :init
  (when (not (string-equal system-type "windows-nt"))
    (exec-path-from-shell-initialize)))


;;; OSX Support
(use-package ns-win
  :defer t
  :ensure nil
  :if (eq system-type 'darwin)
  :config
  (setq
   ns-pop-up-frames nil
   mac-option-modifier 'meta
   mac-command-modifier 'meta))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :init (osx-trash-setup))

;; Reveal current buffer in finder
(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  ;; Bind analogous to `dired-jump' at C-c f j
  :bind
  (("C-c f J" . reveal-in-osx-finder)))


;;; Keys and Keybindings
;; Newline should always indent by default.
(bind-key "<RET>" 'newline-and-indent)
(unbind-key "C-x C-c")

(defvar toggle-map)
(define-prefix-command 'toggle-map)
(bind-key "C-c t" #'toggle-map)

;; Inspired by Spacemacs and Sebastian Wiesner's Config
;; The latter can be found here: https://github.com/lunaryorn/.emacs.d/blob/master/init.el#L317
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        ;; Let's go unicode :)
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Lambdas
          ("\\`\\?\\?\\'"   . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"       . "|=")
          ;; Drop/shorten package prefixes
          ("\\`balaji-"     . "b-")
          ("projectile-"    . "proj-")
          ("helm-"          . "h-")
          ("magit-"         . "ma-"))))

(which-key-declare-prefixes
  "C-c a" "applications"
  "C-c b" "bookmarks"
  "C-c B" "buffer"
  "C-c f" "files"
  "C-c g" "git"
  "C-c g g" "gist"
  "C-c h" "helm/help"
  "C-c j" "jump"
  "C-c m" "major mode"
  "C-c o" "org mode"
  "C-c O" "outline"
  "C-c p" "projects"
  "C-c p s" "projects/search"
  "C-c q" "quit/restart"
  "C-c s" "search"
  "C-c t" "toggle")

(which-key-declare-prefixes-for-mode 'emacs-lisp-mode
  "C-c m e" "eval"
  "C-c m f" "file"
  "C-c m d" "debug")


;;; User Interface
(use-package solarized-theme
  :init
  (defvar my-color-themes (list '(solarized-dark) '(solarized-light)))
  (defvar my-current-theme nil)
  (defvar my-theme-list my-color-themes)

  (defun balaji/set-default-theme ()
    (interactive)
    (setq my-current-theme (car (car my-color-themes)))
    (setq my-theme-list (cdr my-color-themes))
    (load-theme my-current-theme t))

  (setq solarized-use-variable-pitch nil
        ;; Prefer italics over bold
        solarized-use-less-bold t
        solarized-use-more-italic t
        solarized-distinct-doc-face t)

  (defun balaji/cycle-themes ()
    (interactive)
    (cond
     ((null my-theme-list)
      (setq my-current-theme (car (car my-color-themes)))
      (setq my-theme-list (cdr my-color-themes)))
     ((listp my-theme-list)
      (setq my-current-theme (car (car my-theme-list)))
      (setq my-theme-list (cdr my-theme-list)))
     ((t)
      (setq my-current-theme (car (car my-theme-list)))
      (setq my-theme-list (my-color-themes))))
    (load-theme my-current-theme t)
    (spaceline-spacemacs-theme)
    (message "%S" my-current-theme))

  :bind
  ("C-c t t" . balaji/cycle-themes))

(balaji/set-default-theme)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode)
  (spaceline-spacemacs-theme))

(use-package nyan-mode
  :init (nyan-mode))

(use-package which-func
  :init (which-function-mode)
  :config
  (setq
   which-func-unknown "⊥"))

(use-package restart-emacs
  :bind
  ("C-c q r" . restart-emacs)
  ("C-c q q" . save-buffers-kill-emacs))

(use-package face-remap
  :ensure nil
  :bind
  (("C-+" . balaji-font-scaling/text-scale-increase)
   ("C--" . balaji-font-scaling/text-scale-decrease))
  :init
  (defhydra balaji-font-scaling ()
    "Font scaling"
    ("+" text-scale-increase "Scale Up")
    ("-" text-scale-decrease "Scale Down")
    ("q" nil "Quit" :exit t )))

(use-package page-break-lines
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)


;;; Helm
(use-package helm-config
  :ensure helm
  :demand t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-c f f" . helm-find-files)
   ("C-c f F" . helm-multi-files)
   ("C-c j t" . helm-imenu))
  :config
  (use-package helm-files
    :ensure nil)
  (use-package helm-buffers
    :ensure nil)
  (use-package helm-descbinds
    :bind
    ("C-h b" . helm-descbinds))
  (use-package helm-mode
    :ensure nil
    :diminish helm-mode
    :init
    (helm-mode 1))
  (helm-autoresize-mode 1)
  (setq-default
   helm-display-header-line nil
   helm-autoresize-min-height 10
   helm-autoresize-max-height 35
   helm-split-window-in-side-p t

   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-apropos-fuzzy-match t))



;;; Buffer, Windows and Frames
(use-package ibuffer-vc
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (ibuffer-vc-set-filter-groups-by-vc-root)
     (unless (eq ibuffer-sorting-mode 'alphabetic)
       (ibuffer-do-sort-by-alphabetic)))))
(bind-key "C-x C-b" 'ibuffer)

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))


;;; File Handling
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

;; Hardhat prevents us from editing user-protected files
(use-package hardhat
  :init (global-hardhat-mode)
  :config (setq hardhat-mode-lighter " Ⓗ"))


;;; Navigation and Scrolling
(use-package avy
  :bind
  (("C-c j j" . avy-goto-char)
   ("C-c j w" . avy-goto-word-1)
   ("C-c j b" . avy-pop-mark)
   ("C-c j l" . avy-goto-line)))

(use-package ace-window
  :bind
  ("C-x o" . ace-window))

(use-package golden-ratio
  :init
  (defun balaji-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . balaji-toggle-golden-ratio))
  :diminish (golden-ratio-mode . "ⓖ")
  :config
  (setq
   golden-ratio-extra-commands '(windmove-up
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 ace-window
                                 ace-delete-window
                                 ace-select-window
                                 ace-swap-window
                                 ace-maximize-window)
   golden-ratio-auto-scale nil
   golden-ratio-exclude-modes '(flycheck-error-list-mode
                                calc-mode
                                ediff-mode
                                eshell-mode
                                dired-mode)

   split-width-threshold nil
   golden-ratio-exclude-buffer-regexp
   `(,(rx bos "*" (any "h" "H") "elm*" eos)
     ,(rx bos "*which-key*" eos)
     ,(rx bos "*NeoTree*" eos))))

(use-package page                       ; Page navigation
  :ensure nil
  :bind (("C-x ]" . balaji-pages/forward-page)
         ("C-x [" . balaji-pages/backward-page))
  :init
  (defhydra balaji-pages ()
    "Pages"
    ("[" backward-page "backward")
    ("]" forward-page "forward")
    ("n" narrow-to-page "narrow" :exit t)
    ("q" nil "quit" :exit t)))

(use-package outline
  :ensure nil
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(defhydra balaji-outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave"))

(global-set-key (kbd "C-c O") 'balaji-outline/body) ; by example


;;; Search
(use-package "isearch"
  :defer t
  :ensure nil
  :bind (("C-s"   . isearch-forward-regexp)
         ("C-r"   . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward))
  :init
  (diminish 'isearch-mode))

(use-package visual-regexp
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s s" . helm-swoop)
         ("C-c s S" . helm-multi-swoop)
         ("C-c s C-s" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-speed-or-color t
        ;; Split window like Helm does
        helm-swoop-split-window-function #'helm-default-display-buffer))

(use-package ag)

(use-package helm-ag
  :bind
  (("C-c s a" . helm-ag)
   ("C-c s A" . helm-do-ag))
  :config
  (setq
   helm-ag-fuzzy-match t                   ; Fuzzy matching
   helm-ag-insert-at-point 'symbol         ; Default to symbol at point
   helm-ag-edit-save t                     ; save buffers after editing
   ))


;;; Basic Editing
;; Browse Kill Ring
(use-package browse-kill-ring
  :defer 10
  :commands browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

;; Undo Tree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind
  (("C-S-z" . undo-tree-redo)
   ("C-z" . undo-tree-undo))
  :config
  (global-undo-tree-mode 1))

;; Rectangle Editing
(bind-key "C-x r i" 'string-insert-rectangle)

;; Expand Region and Change Inner
(use-package expand-region
  :bind
  (("C-c C-e" . er/expand-region)
   ("C-c C-c" . er/contract-region)))

(use-package change-inner
  :bind
  (("C-c C-i" . change-inner)
   ("C-c C-o" . change-outer)))

;; Multiple Cursors Code
(use-package multiple-cursors
  :bind
  ("C-C C-C" . mc/edit-lines))

;; Easy killing and marking on C-w
(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

;; Enable disabled commands
(put 'downcase-region             'disabled nil)   ; Let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page              'disabled nil)   ; Let narrowing work
(put 'narrow-to-region            'disabled nil)   ; Let narrowing work
(put 'set-goal-column             'disabled nil)
(put 'upcase-region               'disabled nil)   ; Let upcasing work

(use-package align
  :bind (("C-c x a" . align)
         ("C-c x c" . align-current)
         ("C-c x r" . align-regexp)))


;;; Highlights and Fontification
(defconst balaji/unicode-conversions
  '(("[ (]\\(->\\)[) \n]"     . ?→)
    ("[ (]\\(/=\\)[) ]"       . ?≠)
    ;;("[ (]\\(<=\\)[) ]"       . ?≤)
    ;;("[ (]\\(>=\\)[) ]"       . ?≥)
    ;;("[ (]\\(=\\)[) ]"        . ?≡)
    ("[ (]\\(\\.\\)[) ]"      . ?∘)
    ("[ (]\\(&&\\)[) ]"       . ?∧)
    ("[ (]\\(||\\)[) ]"       . ?∨)
    ("[ (]\\(\\*\\)[) ]"      . ?×)
    ("[ (]\\(\\\\\\)[(_a-z]"  . ?λ)
    (" \\(<-\\)[ \n]"         . ?←)
    ;; (" \\(-<\\) "             . ?↢)
    ;; (" \\(>-\\) "             . ?↣)
    (" \\(=>\\)[ \n]"         . ?⇒)
    ;;(" \\(>=>\\) "           . ?↣)
    ;;(" \\(<=<\\) "           . ?↢)
    ;;(" \\(>>=\\) "           . ?↦)
    ;;(" \\(=<<\\) "           . ?↤)
    ("[ (]\\(\\<not\\>\\)[ )]" . ?¬)
    ;;("[ (]\\(<<<\\)[ )]"      . ?⋘)
    ;;("[ (]\\(>>>\\)[ )]"      . ?⋙)
    (" \\(::\\) "             . ?∷)
    ("\\(`union`\\)"          . ?⋃)
    ("\\(`intersect`\\)"      . ?⋂)
    ("\\(`elem`\\)"           . ?∈)
    ("\\(`notElem`\\)"        . ?∉)
    ;;("\\<\\(mempty\\)\\>"    . ??)
    ;; ("\\(`mappend`\\)"        . ?⨂)
    ;; ("\\(`msum`\\)"           . ?⨁)
    ;; ("\\(\\<True\\>\\)"       . "𝗧𝗿𝘂𝗲")
    ;; ("\\(\\<False\\>\\)"      . "𝗙𝗮𝗹𝘀𝗲")
    ("\\(\\<undefined\\>\\)"  . ?⊥)
    ("\\<\\(forall \\)\\>"   . ?∀)))

(defun balaji/setup-unicode-conversions ()
  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           (append (--map
                    `(,(car it)
                      ,(if (characterp (cdr it))
                           `(0 (ignore
                                (compose-region (match-beginning 1)
                                                (match-end 1)
                                                ,(cdr it))))
                         `(0 ,(cdr it))))
                    balaji/unicode-conversions)
                   '(("(\\|)" . 'esk-paren-face)))))
        '(haskell-mode literate-haskell-mode)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))


;;; Codestyle
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-font-lock-mode t)

;; Cleanup unnecessary whitespace
(use-package ethan-wspace
  :diminish ethan-wspace-mode
  :init
  (global-ethan-wspace-mode t)
  :bind
  ("C-c c" . ethan-wspace-clean-all)
  :config
  (setq mode-require-final-newline nil
        require-final-newline nil))


;;; Skeletons, Code Completion and Expansion
(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-minimum-prefix-length 2
   company-selection-wrap-around t
   company-show-numbers t
   company-tooltip-align-annotations t
   company-require-match nil
   company-dabbrev-downcase nil
   company-dabbrev-code-ignore-case nil
   company-transformers '(company-sort-by-occurrence))
  (use-package company-quickhelp
    :init
    (setq company-quickhelp-delay 0.6)
    :config
    (company-quickhelp-mode t)))

(use-package yasnippet
  :commands (yas-expand yas-minor-mode)
  :diminish (yas-minor-mode . " Ⓨ")
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (yas-global-mode 1))


;;; Spelling and Syntax Checking
(use-package flycheck
  :defer 5
  :bind (("C-c e" . balaji-flycheck-errors/body)
         ("C-c t f" . flycheck-mode))
  :init
  (defhydra balaji-flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message")
    ;; See `helm-flycheck' package below
    ("h" helm-flycheck "list with helm"))

  (global-flycheck-mode)
  :config
  (setq
   flycheck-standard-error-navigation nil
   flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
   flycheck-scalastylerc "scalastyle_config.xml")
  :diminish (flycheck-mode . " Ⓢ"))

(use-package helm-flycheck
  :after flycheck)

;; Flyspell Mode
(use-package flyspell
  :ensure nil
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init
  (use-package ispell
    :ensure nil
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)))
  :config
  (unbind-key "C-." flyspell-mode-map))


;;; Markup Languages
(use-package markdown-mode
  :mode
  ".md\\|.markdown")

(use-package yaml-mode
  :mode
  "\\(\\.yml\\|\\.yaml\\)")


;;; Finance
(defun balaji/insert-rupee-symbol ()
  "Insert Indian Rupee Symbol at point."
  (interactive)
  (insert "₹"))

(use-package ledger-mode
  :bind (:map ledger-mode-map
              ("C-c m r" . balaji/insert-rupee-symbol))
  :mode
  "\\.ledger$")

(add-hook 'ledger-mode-hook 'company-mode)

;;; Lisp Modes
(defvar lisp-modes '(emacs-lisp-mode
                     lisp-interaction-mode
                     inferior-lisp-mode
                     inferior-emacs-lisp-mode
                     lisp-mode))

(defun do-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))

(bind-keys :map emacs-lisp-mode-map
           ("C-c m e b" . do-eval-buffer)
           ("C-c m e r" . do-eval-region)
           ("C-c m s" . scratch)
           ("C-c m d c" . cancel-debug-on-entry)
           ("C-c m d e" . debug-on-entry)
           ("C-c m d r" . toggle-debug-on-error)
           ("C-c m f b" . emacs-lisp-byte-compile-and-load)
           ("C-c m f l" . find-library)
           ("C-c m L" . elint-current-buffer))

(defvar lisp-mode-hooks
  (--map (intern
          (concat (symbol-name it) "-hook"))
         lisp-modes))

(use-package paredit
  :diminish paredit-mode
  :commands paredit-mode
  :config
  (bind-key "M-p" 'paredit-splice-sexp-killing-backward emacs-lisp-mode-map)
  (bind-key "M-n" 'paredit-splice-sexp-killing-forward emacs-lisp-mode-map))

(defun balaji/lisp-mode-hook ()
  "Functions to be called when entering Lisp mode"
  (paredit-mode t)
  (company-mode t)
  (use-package eldoc
    :ensure nil
    :diminish eldoc-mode
    :commands eldoc-mode))

(defadvice emacs-lisp-mode
    (after elisp-rename-modeline activate)
  (setq mode-name "ELisp"))

(apply #'hook-into-modes 'balaji/lisp-mode-hook lisp-mode-hooks)

;;; Scala
(use-package scala-mode
  :mode
  ("\\.scala" . scala-mode)
  :config
  (setq
   scala-indent:default-run-on-strategy scala-indent:eager-strategy
   scala-indent:indent-value-expression t
   scala-indent:align-parameters t
   scala-indent:align-forms t))

(use-package ensime
  :commands ensime ensime-mode
  :after scala-mode
  :bind
  (:map scala-mode-map
        ("C-c m e" . ensime)
        ("C-c m s" . ensime-shutdown)
   :map ensime-mode-map
        ("C-c m E" . ensime-reload)
        ("M-n"     . nil)
        ("M-p"     . nil))
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-default-buffer-prefix "ENSIME-"
   ensime-prefer-noninteractive t
   ensime-refactor-enable-beta t
   ensime-refactor-preview t
   ensime-auto-connect 'always
   ensime-refactor-preview-override-hunk 10)
  :config
  (ensime-company-enable))

(use-package sbt-mode
  :commands sbt-start sbt-command)

(defcustom
  balaji/scala-mode-prettify-symbols
  '(("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("==" . ?≡)
    ("!=" . ?≠)
    ("+-" . ?±)
    ("::" . ?∷))
  "Prettify symbols for scala-mode.")

(defun balaji/scala-mode-hook ()
  (setq prettify-symbols-alist balaji/scala-mode-prettify-symbols)
  (company-mode t)
  (ensime-mode t)
  (ensime))

(add-hook 'scala-mode-hook 'balaji/scala-mode-hook)


;;; Haskell
(package-require 'haskell-mode)
(load "haskell-mode-autoloads")

(defun balaji/haskell-mode-hook ()
  (haskell-indentation-mode)
  (interactive-haskell-mode)
  (intero-mode t)
  (company-mode t)
  (flycheck-mode))

(use-package haskell-mode
  :mode
  (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
   ("\\.lhs\\'" . literate-haskell-mode))
  :init
  (balaji/setup-unicode-conversions)
  (setq haskell-hoogle-command nil)
  :config
  (use-package flycheck-haskell
    :config
    (flycheck-haskell-setup)
    (bind-key "M-n" #'flycheck-next-error haskell-mode-map)
    (bind-key "M-p" #'flycheck-previous-error haskell-mode-map))
  (use-package haskell-interactive-mode
    :ensure nil)
  (use-package haskell-process
    :ensure nil
    :init
    (setq
     haskell-process-suggest-remove-import-lines t
     haskell-process-auto-import-loaded-modules t
     haskell-process-log t)))

(use-package intero
  :commands (intero-mode))

(add-hook 'haskell-mode-hook 'balaji/haskell-mode-hook)


;;; Org
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


;;; Ruby
(use-package enh-ruby-mode
  :mode "\\(\\.rb\\|.erb\\|Gemfile\\)"
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package robe
  :diminish robe-mode
  :commands robe-mode)

(use-package rvm
  :commands rvm-activate-corresponding-ruby)

(use-package rspec-mode
  :diminish rspec-mode
  :commands rspec-mode
  :config
  (rspec-install-snippets))

(use-package rubocop
  :diminish rubocop-mode
  :commands rubocop-mode)

(use-package ruby-tools
  :diminish ruby-tools-mode
  :commands ruby-tools-mode)

(use-package bundler
  :bind
  (:map enh-ruby-mode-map
        ("C-c m b i" . bundle-install)
        ("C-c m b u" . bundle-update)
        ("C-c m b c" . bundle-check)))

(defun balaji-ruby-mode-hooks ()
  "Hooks for enhanced ruby mode."
  (robe-mode t)
  (add-to-list 'company-backends 'company-robe)
  (company-mode t)
  (rvm-activate-corresponding-ruby)
  (rspec-mode t)
  (rubocop-mode t)
  (ruby-tools-mode t))

(add-hook 'enh-ruby-mode-hook 'balaji-ruby-mode-hooks)


;;; Scripting Languages
(use-package shell-script-mode
  :ensure nil
  :mode
  "\\(.sh\\|.bash\\|.zsh\\)\\(_history\\|_profile\\|rc\\)")

(use-package fish-mode
  :mode
  "\\(.fish\\|.load\\)")


;;; Version Control
(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g m" . magit-branch)
   ("C-c g M" . magit-merge)
   ("C-c g S" . magit-stash)
   ("C-c g a" . magit-stash-apply)
   ("C-c g p" . magit-pull)
   ("C-c g r" . magit-reset-head)
   ("C-c g R" . magit-reset-hard)
   ("C-c g l" . magit-log-all)
   ("C-c g L" . magit-log)
   ("C-c g c" . magit-checkout))
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0")
  (advice-add 'magit-status :after #'delete-other-windows))

(use-package gist
  :bind (("C-c g g l" . gist-list)
         ("C-c g g b" . gist-region-or-buffer)))


;;; Project Management
(use-package projectile
  :defer t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :bind
  ("C-c C-f" . helm-projectile))

(use-package ignoramus
  :config
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    (add-to-list 'ignoramus-file-basename-exact-names name))
  (ignoramus-setup))

(use-package bookmark
  :bind
  ("C-c b b" . bookmark-jump)
  ("C-c b m" . bookmark-set)
  ("C-c b l" . bookmark-bmenu-list))


;;; Benchmarking Startup End
(when (window-system)
  (let ((elapsed-time (float-time (time-subtract (current-time)
                                                 emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed-time))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed-time (float-time (time-subtract (current-time)
                                                              emacs-start-time))))
                 (message "Loading %s...done (%.3fs)" ,load-file-name elapsed-time)))
            t))

;;; init.el ends here

