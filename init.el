;;; For browsing Emacs's C source. This must be set early.
(setq source-directory "~/Programming/Tools/emacs")

(add-to-list 'load-path "~/.emacs.d/config")
(require 'ublt-util)

;;; Emacs is not a text editor, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")
                  ("elpy" . "http://jorgenschaefer.github.io/packages/")
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
  '(smex company yasnippet
         textmate undo-tree whole-line-or-region
         ace-jump-mode htmlize twittering-mode keyfreq
         expand-region
         move-text                      ; move selected/current lines up/down
         linum-relative                 ; relative line number
         nyan-mode                      ; buffer position as nyan cat
         org org-bullets
         ox-reveal                      ; reveal.js slides from org-mode
         adaptive-wrap
         ido-ubiquitous                 ; List-narrowing UI
         flx-ido                        ; Improved flex matching for ido
         helm helm-cmd-t helm-swoop
         ;; number-font-lock-mode          ; Color numbers in code
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
         zeal-at-point                  ; Doc search
         ;; git
         magit magit-svn
         ;; Vim emulation
         evil
         evil-surround
         evil-args
         evil-visualstar
         evil-numbers
         evil-nerd-commenter
         evil-matchit
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
         flycheck
         flycheck-pyflakes
         haskell-mode quack
         adoc-mode
         markdown-mode yaml-mode
         less-css-mode scss-mode
         clojure-mode clojurescript-mode cider
         elisp-slime-nav
         js2-mode
         php-mode php-boris
         rvm
         elpy                           ;python
         web-mode
         emmet-mode                          ; html/css editing
         go-mode
         inf-mongo
         ;; TODO: Remove starter kit dependency
         starter-kit))
(dolist (p ublt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; XXX TODO: Remove this
(ublt/set-up 'starter-kit)

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

(condition-case err
    (when (y-or-n-p "Load secrets?")
      (require 'ublt-secrets "ublt-secrets.el.gpg"))
  (error (message "(ubolonton) Failed to load secrets!")))

;;; Personal stuff
(ublt/add-path "org2blog/")
(ublt/add-path "o-blog")
(ublt/add-path "org-html-slideshow")
(ublt/set-up 'ublt-communication)
(when window-system
  (ublt/set-up 'ublt-entertainment))
(ublt/set-up 'ublt-organization)


;;; More stuff

;;; Vim emulation
(require 'ublt-evil)
;;; File management
(require 'ublt-dired)
;;; Completion/narrowing interfaces
(require 'ublt-ido)
(require 'ublt-helm)
;;; git
(require 'ublt-git)
;;; Code linters.
(require 'ublt-flycheck)

;;; https://github.com/politza/pdf-tools
(ublt/set-up 'pdf-tools
  (pdf-tools-install))


;;; Languages support ------------------------------------------------

(require 'ublt-factor)
(require 'ublt-erlang)
(require 'ublt-haskell)
(require 'ublt-sql)
(require 'ublt-web)
(require 'ublt-markups)
(require 'ublt-lisp)
(require 'ublt-python)
(require 'ublt-js)


(setq custom-file "~/.emacs.d/custom.el")
(condition-case err
    (load custom-file)
  (error (message "Error loading custom file")))


;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (unless (server-running-p)
      (server-start))
  (error (message "Could not start server")))
