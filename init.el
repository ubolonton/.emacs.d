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

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((elpy . "elpy")
          (org . "org")
          (cider . "melpa-stable")
          (helm-ag . "melpa-stable")
          (helm-projectile . "melpa-stable")
          (projectile . "melpa-stable")
          (helm . "melpa-stable"))))

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
         ace-jump-mode avy htmlize twittering-mode keyfreq
         expand-region
         eval-sexp-fu                   ; flash eval'ed code
         popwin                         ; make unimportant windows transient
         move-text                      ; move selected/current lines up/down
         linum-relative                 ; relative line number
         nyan-mode                      ; buffer position as nyan cat
         org org-bullets
         ox-reveal                      ; reveal.js slides from org-mode
         adaptive-wrap
         ido-ubiquitous                 ; List-narrowing UI
         flx-ido                        ; Improved flex matching for ido
         helm helm-swoop
         ;; number-font-lock-mode          ; Color numbers in code
         powerline                      ; Util helping configure mode-line
         ;; TODO: Use & combine with eproject
         projectile                     ; Project management
         emms                           ; Music
         paredit                        ; Structural editing with ()[]{}
         scpaste                        ; Publish highlighted code fragments
         exec-path-from-shell           ; Uhm, f*ck shell
         anzu                           ; Match count for search
         help+ help-fns+ info+
         pabbrev                        ; TODO: Find better alternative
         zeal-at-point                  ; Doc search
         moz                            ; interface with mozrepl
         ;; git
         magit magit-svn diff-hl
         ;; Vim emulation
         evil
         evil-surround
         evil-args
         evil-visualstar
         evil-numbers
         evil-nerd-commenter
         evil-matchit
         ;; Appearance
         rainbow-mode page-break-lines  ;; whitespace
         diminish                       ; Mode names => symbols
         highlight highlight-symbol highlight-parentheses hl-line+ idle-highlight-mode volatile-highlights
         ;; Sometimes (e.g. in terminals)
         solarized-theme
         zenburn-theme
         monokai-theme
         ;; Dired
         dired-details dired-details+
         dired+
         ;; Code folding
         fold-dwim fold-dwim-org hideshowvis
         ;; Languages
         flycheck
         flycheck-pyflakes
         edts                           ;erlang
         haskell-mode quack
         adoc-mode
         ess
         markdown-mode yaml-mode
         less-css-mode scss-mode
         clojure-mode clojurescript-mode cider
         scala-mode2 ensime
         helm-ag helm-projectile
         elisp-slime-nav
         js2-mode json-mode tern company-tern
         php-mode php-boris
         rvm
         elpy                           ;python
         web-mode
         emmet-mode                          ; html/css editing
         go-mode
         inf-mongo))
(dolist (p ublt/packages)
  (when (not (package-installed-p p))
    (package-install p)))



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

;;; Languages support ------------------------------------------------

(require 'ublt-programming)
(require 'ublt-factor)
(require 'ublt-erlang)
(require 'ublt-haskell)
(require 'ublt-sql)
(require 'ublt-web)
(require 'ublt-markups)
(require 'ublt-lisp)
(require 'ublt-python)
(require 'ublt-ruby)
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
