(require 'ublt-util)
(require 'dash)

(defface ublt/lisp-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim (Lisp) parentheses."
  :group 'personal)

(dolist (mode '(scheme-mode
                emacs-lisp-mode
                lisp-mode
                clojure-mode
                clojurescript-mode))
  (font-lock-add-keywords mode '(("(\\|)" . 'ublt/lisp-paren-face))))

;; Scheme
(ublt/set-up 'quack
  (setq quack-fontify-style nil))

(ublt/set-up 'paredit
  (dolist (hook '(scheme-mode-hook
                  emacs-lisp-mode-hook
                  lisp-mode-hook
                  ielm-mode-hook
                  clojure-mode-hook
                  cider-repl-mode-hook))
    (add-hook hook (ublt/on-fn 'paredit-mode) t)))

(ublt/set-up 'lisp-mode
  (ublt/set-up 'eldoc
    (add-hook 'emacs-lisp-mode-hook (ublt/on-fn 'eldoc-mode)))

  (ublt/set-up 'auto-compile
    (auto-compile-on-save-mode +1)
    (setq auto-compile-display-buffer nil))

  (ublt/set-up 'elisp-slime-nav
    (add-hook 'emacs-lisp-mode-hook (ublt/on-fn 'elisp-slime-nav-mode)))

  (ublt/set-up 'lisp-extra-font-lock
    (lisp-extra-font-lock-global-mode +1)))

(ublt/set-up 'ielm
  (add-hook 'ielm-mode-hook
            (lambda () (setq comint-input-ring-file-name "~/.emacs.d/.ielm-input.hist"))))

(ublt/set-up 'clojure-mode
  (add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
  (define-clojure-indent
    (facts '(:defn (1)))
    (fact '(:defn (1)))))

(ublt/set-up 'clj-refactor
  (setq cljr-suppress-no-project-warning t))

(ublt/set-up 'cider
  (setq cider-prompt-for-symbol nil
        cider-font-lock-dynamically '(macro core var deprecated))

  (ublt/in '(darwin)
    (setq cider-jdk-src-paths (-> "find /Library/Java/JavaVirtualMachines -name src.zip | head -n 1"
                                  shell-command-to-string string-trim list)))

  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook (ublt/on-fn 'eldoc-mode))

  (ublt/set-up 'cider-repl
    (setq cider-repl-display-help-banner nil
          cider-repl-use-pretty-printing t
          cider-repl-popup-stacktraces t
          cider-repl-wrap-history t

          cider-repl-history-file "~/.emacs.d/.nrepl.hist"
          cider-repl-history-highlight-current-entry t
          cider-repl-history-highlight-inserted-item 'pulse
          cider-repl-history-current-entry-face 'secondary-selection
          cider-repl-history-inserted-item-face 'secondary-selection

          nrepl-log-messages t
          nrepl-hide-special-buffers t))

  (ublt/set-up 'helm-cider
    (helm-cider-mode +1)))

(provide 'ublt-lisp)
