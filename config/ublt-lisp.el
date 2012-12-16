(require 'ublt-util)

;; SLIME customization
(eval-after-load "slime"
  '(progn
     ;; Extra features (contrib)
     (slime-setup
      '(slime-repl ;; slime-fuzzy
        ;; slime-highlight-edits
        slime-scratch
        slime-editing-commands
                   ))
     (setq slime-net-coding-system 'utf-8-unix
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           ;; common-lisp-hyperspec-root
           ;; "file:///Users/ubolonton/Programming/Tools/HyperSpec/"
           )
     ;; Use parentheses editting mode paredit
     (add-hook 'slime-mode-hook 'enable-paredit-mode t)
     ;; (add-hook 'slime-repl-mode-hook 'enable-paredit-mode t)
     ;; Steel Bank CL
     (add-to-list 'slime-lisp-implementations
                  '(sbcl ("sbcl")))
     ;; (ad-activate 'slime-read-interactive-args)
     ))


;;;; clojure settings ------------------
;;;; UPDATED: use ELPA and lein swank now
;;;; Clojure is added to top of slime-lisp-implementations by ELPA?
;;;; 2010-01-27: Disabled ELPA and relied on manual tweaking
;; - About the :init part: I think SLIME was written for Common Lisp,
;; so many of its features do not work with Clojure (same thing with
;; paredit)
;; UPDATED: No need. `slime-read-interactive-args' takes care of this
;; (add-to-list 'slime-lisp-implementations
;;   `(clojure ,(swank-clojure-cmd) :init swank-clojure-init))
;; - Force clojure-mode buffers to use SLIME
;; UPDATED: No need
;; (add-hook 'clojure-mode-hook 'slime-lisp-mode-hook)
;; - Redirect printing to REPL
;; (add-hook 'clojure-mode-hook
;;   (lambda () (slime-redirect-inferior-output t)))

;; clojure-mode customization
(eval-after-load "clojure-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
     (add-hook 'clojure-mode-hook 'enable-paredit-mode t)
     (define-clojure-indent
       (describe 'defun)
       (testing 'defun)
       (given 'defun)
       (using 'defun)
       (with 'defun)
       (it 'defun)
       (do-it 'defun))
     ;; (add-hook 'slime-repl-mode-hook
     ;;           'swank-clojure-slime-repl-modify-syntax t)
     ))

;; swank-clojure customization
(eval-after-load "swank-clojure"
  '(progn
     ;; Add a hook to modify repl making it more Clojure-friendly
     ;; (I haven't seen this used anywhere somehow!?!)

     ;; Don't use swank-clojure-project
     ;; (add-hook 'swank-clojure-project-hook
     ;;           (lambda ()
     ;;             (setq default-directory path)
     ;;             (add-to-list
     ;;              'swank-clojure-extra-vm-args "")))
     ))
(require 'swank-clojure-extra)

;;; XXX: Fix durendal instead
(ublt/set-up 'durendal
  ;; For REPL font-lock trick to work in Emacs 24
  (defun ublt/slime-repl-clojure-font-lock ()
    (font-lock-mode -1)
    (clojure-mode-font-lock-setup)
    (font-lock-mode +1))
  (defadvice durendal-enable-slime-repl-font-lock (after hack activate)
    (remove-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
    (add-hook 'slime-repl-mode-hook 'ublt/slime-repl-clojure-font-lock t))
  (defadvice durendal-disable-slime-repl-font-lock (before hack activate)
    (remove-hook 'slime-repl-mode-hook 'ublt/slime-repl-clojure-font-lock))
  (durendal-enable)

  (setq durendal-auto-compile? nil)
  )

(ublt/set-up "clojurescript-mode"
;;; XXX: Make this customizable
(when (> (display-color-cells) 8)
  (font-lock-add-keywords 'clojurescript-mode
                          '(("(\\|)" . 'esk-paren-face))))
  (add-hook 'clojurescript-mode-hook 'enable-paredit-mode))

;;;; ielm settings ---------------
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; ac-slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(require 'hippie-expand-slime)

;; (add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(provide 'ublt-lisp)
