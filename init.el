;;; For browsing Emacs's C source. This must be set early.
(setq source-directory "~/Programming/Tools/emacs")

(setq load-prefer-newer t)

;;; Make package downloading a bit more secure.
(setq tls-checktrust 'ask)

(add-to-list 'load-path "~/.emacs.d/config")
(require 'ublt-util)

;;; Package repositories.
(require 'package)
(dolist (source '(("org" . "https://orgmode.org/elpa/")
                  ("melpa-stable" . "https://stable.melpa.org/packages/")
                  ("melpa" . "https://melpa.org/packages/")
                  ("elpy" . "https://jorgenschaefer.github.io/packages/")
                  ))
  (add-to-list 'package-archives source t))
(setq
 package-archive-priorities '(("melpa-stable" . 1)
                              ("melpa" . 2)))
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((elpy . "elpy")
          (org . "org"))))

;;; Some packages mess up `package-archives'. This fixes that.
(defvar ublt/package-archives package-archives)
(add-hook 'after-init-hook (lambda () (setq package-archives ublt/package-archives)))
(package-initialize)

;;; Required packages
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar ublt/packages
  '(dash
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
    linum-relative                      ; relative line number
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
    rainbow-mode page-break-lines ;; whitespace
    diminish                      ; Mode names => symbols
    highlight highlight-symbol highlight-parentheses idle-highlight-mode volatile-highlights
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
    markdown-mode yaml-mode toml-mode
    less-css-mode scss-mode
    clojure-mode cider helm-cider clj-refactor
    scala-mode
    elisp-slime-nav
    cask-mode
    js2-mode json-mode
    typescript-mode
    php-mode php-boris
    elpy flycheck-pyflakes              ;python
    web-mode
    emmet-mode                          ; html/css editing
    go-mode
    lsp-mode lsp-ui
    rust-mode cargo racer lsp-rust company-lsp
    dockerfile-mode
    protobuf-mode
    systemd
    terraform-mode company-terraform
    inf-mongo))
(dolist (p ublt/packages)
  (ublt/package-install p))



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
