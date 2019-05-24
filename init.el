;;; For browsing Emacs's C source. This must be set early.
(setq source-directory "~/Programming/Tools/emacs")

(setq load-prefer-newer t)

;;; Make package downloading a bit more secure.
(setq tls-checktrust 'ask)

(setq custom-file "~/.emacs.d/custom.el")

(add-to-list 'load-path "~/.emacs.d/lib")

(defvar ublt/packages
  '(use-package))

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
       (load bootstrap-file nil 'nomessage))

     ;; TODO: Remove this once https://github.com/raxod502/straight.el/issues/377 is fixed.
     (define-advice straight-use-package
         (:around (orig package &rest args) ignore-loaded)
       (if (and (symbolp package) (featurep package))
           t
         (apply orig package args)))

     (require 'ublt-straight-recipes)

     (straight-use-package 'use-package)
     ;; We don't want to declare a package twice.
     (setq straight-use-package-by-default t)

     (dolist (p ublt/packages)
       (straight-use-package p)))))

(setq use-package-verbose t
      ;; For `use-package-report' to work.
      use-package-compute-statistics t)



(use-package dash)

(add-to-list 'load-path "~/.emacs.d/config")
(require 'ublt-util)

;;; Path to stuffs that come from single files
(ublt/add-path "single-file-modes")

;;; Uhm, f*ck shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;; General usability
(require 'ublt-misc)
(require 'ublt-dvorak)
(require 'ublt-appearance)
(require 'ublt-navigation)
(require 'ublt-editing)

;;; Personal stuff
(require 'ublt-communication)
(require 'ublt-organization)


;;; More stuff

;;; Vim emulation
(require 'ublt-evil)
;;; File management
(require 'ublt-dired)
;;; Completion/narrowing interfaces
(ublt/with-demand
  (require 'ublt-helm))
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


;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (unless (server-running-p)
      (server-start))
  (error (message "Could not start server")))

;;; Here because something above explicitly set it to nil.
(setq ring-bell-function 'ignore)
