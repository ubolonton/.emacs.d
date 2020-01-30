(require 'ublt-util)

(use-package dash)

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

(use-package paredit
  :hook ((scheme-mode
          emacs-lisp-mode lisp-mode ielm-mode
          clojure-mode cider-repl-mode
          cask-mode)
         . paredit-mode))

(use-package lisp-mode
  :ensure nil :straight nil
  :custom (emacs-lisp-docstring-fill-column 80))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package company-elisp
  :requires company
  :ensure nil :straight nil)

;;; Recompile Emacs Lisp on-save.
(use-package auto-compile
  :custom (auto-compile-display-buffer nil)
  :config (auto-compile-on-save-mode +1))

(use-package elisp-slime-nav
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package lisp-extra-font-lock
  :config (lisp-extra-font-lock-global-mode +1))

(use-package ielm
  :hook (ielm-mode . (lambda () (setq comint-input-ring-file-name "~/.emacs.d/.ielm-input.hist"))))

(use-package flycheck-package
  :config (flycheck-package-setup))

(ublt/with-defer
  (use-package cask-mode)

  ;; Scheme
  (use-package quack
    :custom (quack-fontify-style nil))

  (use-package clojure-mode
    :mode "\\.dtm$"
    :config (define-clojure-indent
              (facts '(:defn (1)))
              (fact '(:defn (1)))))

  (use-package clj-refactor
    :custom (cljr-suppress-no-project-warning t))

  (use-package cider
    :custom ((cider-prompt-for-symbol nil)
             (cider-font-lock-dynamically '(macro core var deprecated)))

    :config (ublt/in '(darwin)
              (setq cider-jdk-src-paths (-> "find /Library/Java/JavaVirtualMachines -name src.zip | head -n 1"
                                            shell-command-to-string string-trim list)))

    :hook ((cider-mode . cider-company-enable-fuzzy-completion)
           (cider-mode . eldoc-mode)))

  (use-package helm-cider
    :config (helm-cider-mode +1))

  (use-package cider-repl
    :ensure nil :straight nil
    :custom ((cider-repl-display-help-banner nil)
             (cider-repl-use-pretty-printing t)
             (cider-repl-popup-stacktraces t)
             (cider-repl-wrap-history t)

             (cider-repl-history-file "~/.emacs.d/.nrepl.hist")
             (cider-repl-history-highlight-current-entry t)
             (cider-repl-history-highlight-inserted-item 'pulse)
             (cider-repl-history-current-entry-face 'secondary-selection)
             (cider-repl-history-inserted-item-face 'secondary-selection)

             (nrepl-log-messages t)
             (nrepl-hide-special-buffers t))))

(provide 'ublt-lisp)
