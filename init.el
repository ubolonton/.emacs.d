;;; For browsing Emacs's C source. This must be set early.
(setq source-directory "~/Programming/Tools/emacs")

(add-to-list 'load-path "~/.emacs.d/config")
(require 'ublt-util)

;;; Emacs is not a text editor, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
;;; Some packages mess up `package-archives'. This fixes that.
(defvar ublt/package-archives package-archives)
(add-hook 'after-init-hook (lambda () (setq package-archives ublt/package-archives)))
(package-initialize)

;;; Required packages
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar ublt/packages
  '(smex auto-complete yasnippet
         org textmate undo-tree whole-line-or-region
         ace-jump-mode htmlize twittering-mode keyfreq
         adaptive-wrap
         ido-ubiquitous                 ; List-narrowing UI
         helm helm-cmd-t helm-swoop
         number-font-lock-mode          ; Color numbers in code
         powerline                      ; Util helping configure mode-line
         ;; TODO: Use & combine with eproject
         projectile                     ; Project management
         emms                           ; Music
         paredit                        ; Structural editing with ()[]{}
         scpaste                        ; Publish highlighted code fragments
         exec-path-from-shell           ; Uhm, f*ck shell
         anzu                           ; Match count for search
         info+
         pabbrev                        ; TODO: Find better alternative
         ;; git
         magit magit-svn
         ;; Vim emulation
         evil surround
         ;; Appearance
         color-theme rainbow-mode page-break-lines  ;; whitespace
         diminish                       ; Mode names => symbols
         highlight highlight-symbol highlight-parentheses hl-line+ idle-highlight-mode volatile-highlights
         ;; Don't actually use these themes, just to learn some ideas
         color-theme-solarized zenburn
         ;; Dired
         dired-details dired-details+
         dired+
         ;; Code folding
         fold-dwim fold-dwim-org hideshowvis
         ;; Languages
         flymake
         haskell-mode quack
         adoc-mode
         markdown-mode yaml-mode
         less-css-mode scss-mode
         clojure-mode clojurescript-mode durendal cider
         elisp-slime-nav
         js2-mode flymake-jshint
         php-mode php-boris flymake-php
         rvm flymake-ruby
         elpy                           ;python
         web-mode
         emmet-mode                          ; html/css editing
         ;; TODO: Remove starter kit dependency
         starter-kit))
(dolist (p ublt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; XXX: Some starter-kit packages are broken
(defalias 'run-coding-hook 'esk-prog-mode-hook)
(defalias 'esk-run-coding-hook 'esk-prog-mode-hook)

;;; NOTE: As my stuffs may depend on packages loaded after
;;; starter-kit, it does not make sense to let starter-kit load my
;;; stuffs. Thus my config is in ~/.emacs.d/init.el, not
;;; ~/.emacs.d/ubolonton/init.el. And don't ever choose "elpa" as your
;;; user name =))



;;; Path to stuffs that come from single files
(ublt/add-path "single-file-modes")

;; (ublt/set-up 'auto-async-byte-compile
;;   (setq auto-async-byte-compile-display-function 'bury-buffer)
;;   (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

(ublt/set-up 'exec-path-from-shell
  (exec-path-from-shell-initialize))

;;; General usability
(require 'ublt-misc)
(require 'ublt-dvorak)
(require 'ublt-appearance)
(require 'ublt-navigation)
(require 'ublt-editing)

;;; Personal stuffs
(ublt/add-path "org2blog/")
(ublt/add-path "o-blog")
(ublt/add-path "o-blog/lisp")
(ublt/add-path "org-html-slideshow")
(ublt/set-up 'ublt-communication)
(when window-system (ublt/set-up 'ublt-entertainment))
(ublt/set-up 'ublt-organization)

;;; Might grow into a project on its own, adding more project
;;; management stuffs
(ublt/add-path "eproject")
(setq eproject-completing-read-function 'eproject--ido-completing-read
      eproject-todo-expressions '("TODO" "XXX" "FIX" "FIXME" "HACK" "NTA"))
(require 'eproject-ido-imenu)

(ublt/set-up 'projectile
  (projectile-global-mode +1))

(require 'ublt-evil)
(require 'ublt-dired)
(require 'ublt-ido)
;; (ublt/add-path "helm")
(require 'ublt-helm)

(require 'ublt-git)
(require 'ublt-python)
(require 'ublt-js)
(require 'ublt-flymake)


;;; Languages support ------------------------------------------------

(require 'ublt-factor)
(require 'ublt-erlang)
(require 'ublt-haskell)
(require 'ublt-sql)
(require 'ublt-web)
(require 'ublt-markups)
(ublt/set-up 'octave-mod
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))


;;; Lisp, Clojure --------------------------------------

(require 'ublt-lisp)


(setq custom-file "~/.emacs.d/custom.el")
(condition-case err
    (load custom-file)
  (error (message "Error loading custom file")))


;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (server-start)
  (error (message "Could not start server")))
