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
     textmate
     keyfreq
     org org-bullets
     ox-reveal                           ; reveal.js slides from org-mode
     adaptive-wrap
     ;; TODO: Use & combine with eproject
     projectile                          ; Project management
     emms                                ; Music
     exec-path-from-shell                ; Uhm, f*ck shell
     pabbrev                             ; TODO: Find better alternative
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
     highlight-symbol idle-highlight-mode
     ;; Sometimes (e.g. in terminals)
     solarized-theme
     zenburn-theme
     monokai-theme
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
     clojure-mode cider clj-refactor
     scala-mode
     elisp-slime-nav lisp-extra-font-lock
     cask-mode
     typescript-mode
     php-mode php-boris
     elpy flycheck-pyflakes              ;python
     web-mode
     emmet-mode                          ; html/css editing
     go-mode
     dockerfile-mode
     protobuf-mode
     systemd
     inf-mongo))

(pcase (getenv "EMACS_PACKAGE_MANAGER")
  ("package.el"
   (progn
     ;; Package repositories.
     (require 'package)
     (dolist (source '(("org" . "https://orgmode.org/elpa/")
                       ("melpa-stable" . "https://stable.melpa.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("elpy" . "https://jorgenschaefer.github.io/packages/")
                       ))
       (add-to-list 'package-archives source t))
     ;; Prefer stable packages.
     (setq package-archive-priorities '(("melpa-stable" . 1)
                                        ("melpa" . 2)))
     ;; Pin `elpy' and `org'.
     (when (boundp 'package-pinned-packages)
       (setq package-pinned-packages
             '((elpy . "elpy")
               (org . "org"))))

     ;; Some packages mess up `package-archives'. This fixes that.
     (defvar ublt/package-archives package-archives)
     (add-hook 'after-init-hook (lambda () (setq package-archives ublt/package-archives)))
     (package-initialize)

     (when (not package-archive-contents)
       (package-refresh-contents))

     (dolist (p ublt/packages)
       (ublt/package-install p))

     (require 'use-package)
     (require 'use-package-ensure)
     (setq
      ;; Since we have to use `:straight' `nil' for some packages when using `straight.el'.
      use-package-ignore-unknown-keywords t
      ;; We don't want to declare a package twice.
      use-package-always-ensure t)))
  (_
   (progn
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

     (straight-use-package 'use-package)
     ;; We don't want to declare a package twice.
     (setq straight-use-package-by-default t)

     (dolist (p ublt/packages)
       (straight-use-package p)))))

(setq use-package-verbose t
      ;; For `use-package-report' to work.
      use-package-compute-statistics t)



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
