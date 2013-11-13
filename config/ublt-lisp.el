(require 'ublt-util)

;; clojure-mode customization
(ublt/set-up 'clojure-mode
  (add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
  (add-hook 'clojure-mode-hook (ublt/on-fn 'paredit-mode) t)
  ;; (define-clojure-indent
  ;;   (describe 'defun)
  ;;   (testing 'defun)
  ;;   (given 'defun)
  ;;   (using 'defun)
  ;;   (with 'defun)
  ;;   (it 'defun)
  ;;   (do-it 'defun))
  )

;;; XXX: Fix durendal instead
(ublt/set-up 'durendal
  ;; For REPL font-lock trick to work in Emacs 24
  (defun ublt/repl-clojure-font-lock ()
    (font-lock-mode -1)
    (clojure-mode-font-lock-setup)
    (font-lock-mode +1))
  (durendal-enable)
  (setq durendal-auto-compile? nil)
;;; TODO: `ublt/set-up' should accept a list of features
  (add-hook 'cider-repl-mode-hook 'ublt/repl-clojure-font-lock))

(ublt/set-up "clojurescript-mode"
  ;; XXX: Make this customizable
  (when (> (display-color-cells) 8)
    (font-lock-add-keywords 'clojurescript-mode
                            '(("(\\|)" . 'esk-paren-face))))
  (add-hook 'clojurescript-mode-hook (ublt/on-fn 'paredit-mode)))

;;;; ielm settings ---------------
(add-hook 'ielm-mode-hook (ublt/on-fn 'paredit-mode))
(add-hook 'ielm-mode-hook
          (lambda () (setq comint-input-ring-file-name "~/.emacs.d/.ielm-input.hist")))

;;; @cider: making a keymap available without the mode being provided
;;; is just fucking insane
(ublt/set-up 'cider-repl
  (setq cider-repl-popup-stacktraces t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        cider-repl-history-file "~/.emacs.d/.nrepl.hist")
  (add-hook 'cider-repl-mode-hook (ublt/on-fn 'paredit-mode)))
(ublt/set-up 'cider-interaction
  (setq cider-popup-stacktraces nil))

(provide 'ublt-lisp)
