(require 'ublt-util)

(ublt/set-up 'js2-mode
  (add-hook 'js2-mode-hook 'esk-prog-mode-hook)
  (add-hook 'js2-mode-hook 'esk-paredit-nonlisp)
  (add-hook 'js2-mode-hook 'moz-minor-mode)
  (defalias 'javascript-mode 'js2-mode)

  (setcdr (assoc "\\.js\\'" auto-mode-alist)
          'js2-mode)

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2))

(ublt/set-up 'js

  ;; XXX: What is this for?
  (defvar javascript-mode-syntax-table js-mode-syntax-table)

  ;; MozRepl integration
  (add-hook 'js-mode-hook 'moz-minor-mode)
  (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

  (setq js-indent-level 2
        espresso-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.jsm$" . js-mode))

  ;; FIX
  (ublt/set-up 'starter-kit
    (add-hook 'js-mode-hook 'esk-paredit-nonlisp)))

;;; Syntax checking
;;; TODO: Buffer-local/dir-local config for jshint
(ublt/set-up 'flycheck
  ;; Install jshint with node
  (setq-default flycheck-jshintrc "~/.jshint.json")
  (add-hook 'js-mode-hook (ublt/on-fn 'flycheck-mode))
  (add-hook 'js2-mode-hook (ublt/on-fn 'flycheck-mode)))

(provide 'ublt-js)
