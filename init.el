;;; For browsing Emacs's C source. This must be set early.
(setq source-directory "~/Programming/Tools/emacs")

(setq load-prefer-newer t)

;;; Make package downloading a bit more secure.
(setq tls-checktrust 'ask)

(add-to-list 'load-path "~/.emacs.d/config")
(require 'ublt-util)

(defvar ublt/packages
  '(use-package
    dash
    auto-compile                        ;recompile Emacs Lisp on-save
    company company-box
    yasnippet
    yasnippet-snippets
    textmate undo-tree whole-line-or-region
    avy htmlize twittering-mode keyfreq
    ag
    expand-region
    eval-sexp-fu                        ; flash eval'ed code
    popwin                              ; make unimportant windows transient
    move-text                           ; move selected/current lines up/down
    nyan-mode                           ; buffer position as nyan cat
    org org-bullets
    ox-reveal                           ; reveal.js slides from org-mode
    adaptive-wrap
    helm
    swiper-helm
    helm-ag
    helm-projectile
    ;; number-font-lock-mode          ; Color numbers in code
    powerline                           ; Util helping configure mode-line
    ;; TODO: Use & combine with eproject
    projectile                          ; Project management
    emms                                ; Music
    paredit                             ; Structural editing with ()[]{}
    scpaste                             ; Publish highlighted code fragments
    exec-path-from-shell                ; Uhm, f*ck shell
    anzu                                ; Match count for search
    helpful
    info-colors
    pabbrev                             ; TODO: Find better alternative
    ;; git
    magit magit-svn diff-hl git-timemachine
    ;; Vim emulation
    evil
    evil-surround
    evil-args
    evil-visualstar
    evil-numbers
    evil-nerd-commenter
    evil-matchit
    ;; Appearance
    rainbow-mode
    diminish                      ; Mode names => symbols
    highlight-symbol highlight-parentheses idle-highlight-mode volatile-highlights
    ;; Sometimes (e.g. in terminals)
    solarized-theme
    zenburn-theme
    monokai-theme
    ;; Dired
    dired-collapse
    dired-rainbow
    diredfl
    all-the-icons-dired
    ;; Code folding
    hideshowvis
    ;; Languages
    flycheck
    edts                                ;erlang
    haskell-mode quack
    adoc-mode
    ess
    markdown-mode yaml-mode
    less-css-mode scss-mode
    clojure-mode cider helm-cider clj-refactor
    scala-mode
    elisp-slime-nav lisp-extra-font-lock
    cask-mode
    js2-mode json-mode
    typescript-mode
    php-mode php-boris
    elpy flycheck-pyflakes              ;python
    web-mode
    emmet-mode                          ; html/css editing
    go-mode
    lsp-mode lsp-ui
    rust-mode cargo company-lsp
    dockerfile-mode
    protobuf-mode
    systemd
    terraform-mode company-terraform
    inf-mongo))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-verbose t)

(dolist (p ublt/packages)
  (straight-use-package p))



;;; Path to stuffs that come from single files
(ublt/add-path "single-file-modes")

(ublt/set-up 'exec-path-from-shell
  (exec-path-from-shell-initialize))

;;; General usability
(require 'ublt-misc)
(require 'ublt-dvorak)
(require 'ublt-appearance)
(require 'ublt-navigation)
(require 'ublt-editing)

;;; Personal stuff
(ublt/set-up 'ublt-communication)
(ublt/set-up 'ublt-organization)


;;; More stuff

;;; Vim emulation
(require 'ublt-evil)
;;; File management
(require 'ublt-dired)
;;; Completion/narrowing interfaces
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
(require 'ublt-devops)
(require 'ublt-rust)

(ublt/set-up 'ublt-mix-modes)


(setq custom-file "~/.emacs.d/custom.el")
(condition-case err
    (load custom-file)
  (error (message "Error loading custom file")))


;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (unless (server-running-p)
      (server-start))
  (error (message "Could not start server")))

;;; Here because something above explicitly set it to nil.
(setq ring-bell-function 'ignore)
