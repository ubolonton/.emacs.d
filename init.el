;;; For browsing Emacs's C source. This must be set early.
(setq source-directory "~/Programming/Tools/emacs")

;;; Prefer newly edited .el files over compiled .elc files.
(setq load-prefer-newer t)

;;; Make package downloading a bit more secure.
(setq tls-checktrust 'ask)

(defun ublt/init-rel-path (path)
  (expand-file-name path user-emacs-directory))

(setq custom-file (ublt/init-rel-path "custom.el"))
(add-hook 'after-init-hook (lambda ()
                             (with-demoted-errors "Error loading custom-file: %S"
                               (load custom-file))))

(add-to-list 'load-path (ublt/init-rel-path "lib"))
(add-to-list 'load-path (ublt/init-rel-path "config"))

(defvar ublt/packages
  '(use-package))

;;; XXX: Breaking change in Emacs 28. This doesn't work though.
(when (version<= "28" emacs-version)
  (eval-and-compile
    (defmacro define-obsolete-function-alias (obsolete-name current-name
                                                            &optional when docstring)
      "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"28.1\" \
\"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"28.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
      (declare (doc-string 4))
      `(progn
         (defalias ,obsolete-name ,current-name ,docstring)
         (make-obsolete ,obsolete-name ,current-name ,(or when "2022-04-17"))))))

;; XXX: New load suffix for dynamic module is `.dylib', but `autoload' has not been updated.
;;
;; error: tsc-dyn.dylib:0:0: error: scan-error: (Containing expression ends prematurely 57524 57525)
(when (version<= "28" emacs-version)
  (defun ublt/-remove-dylib-suffixes (suffixes)
    (cl-remove-if (lambda (s) (string-match-p "dylib" s))
                  suffixes))

  (define-advice make-directory-autoloads (:around (f &rest args) ublt/ignore-dylib)
    (advice-add 'get-load-suffixes :filter-return #'ublt/-remove-dylib-suffixes)
    (unwind-protect
        (apply f args)
      (advice-remove 'get-load-suffixes #'ublt/-remove-dylib-suffixes))))

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

     (defvar ublt/package-errors ())
     (defun ublt/package-install (pkg)
       (when (not (package-installed-p pkg))
         (condition-case err
             (package-install pkg nil)
           (error
            (setq ublt/package-errors (plist-put ublt/package-errors pkg err))
            (message (propertize "Failed to install %s: %s" 'face 'font-lock-keyword-face)
                     pkg err)))))

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
         (:around (orig recipe &rest args) ignore-loaded)
       (if (and (symbolp recipe)
                (featurep recipe))
           t
         (apply orig recipe args)))

     (load "ublt-straight-recipes")

     (straight-use-package 'use-package)
     (require 'use-package)

     ;; We don't want to declare a package twice.
     (setq straight-use-package-by-default t)

     (dolist (p ublt/packages)
       (straight-use-package p)))))

(setq use-package-verbose t
      ;; For `use-package-report' to work.
      use-package-compute-statistics t)



;;; Convenient list functions.
(use-package dash)

(require 'ublt-util)

;;; Uhm, f*ck shell
(use-package exec-path-from-shell
  :when (member system-type '(gnu/linux darwin))
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
