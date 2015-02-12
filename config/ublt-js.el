(require 'ublt-util)

(ublt/set-up 'js2-mode
  (ublt/set-up 'paredit
    (add-hook 'js2-mode-hook (ublt/on-fn 'paredit-mode)))
  (ublt/set-up 'moz
    (add-hook 'js2-mode-hook 'moz-minor-mode))

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset 2)

  (setcdr (assoc "\\.js\\'" auto-mode-alist) 'js2-mode))

(ublt/set-up 'js
  (ublt/set-up 'paredit
    (add-hook 'js-mode-hook (ublt/on-fn 'paredit-mode)))
  (ublt/set-up 'moz
    (add-hook 'js-mode-hook 'moz-minor-mode))

  (setq js-indent-level 2
        espresso-indent-level 2)

  (add-to-list 'auto-mode-alist '("\\.jsm$" . js-mode))  )

(ublt/set-up 'json-mode
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

;;; Syntax checking
;;; TODO: Buffer-local/dir-local config for jshint
(ublt/set-up 'flycheck
  ;; Install jshint with node
  (setq-default flycheck-jshintrc "~/.jshint.json")
  (add-hook 'js-mode-hook (ublt/on-fn 'flycheck-mode))
  (add-hook 'js2-mode-hook (ublt/on-fn 'flycheck-mode)))

;;; Code navigation & completion. Install "tern" globally with npm.
(ublt/set-up 'tern
  (add-hook 'js-mode-hook (ublt/on-fn 'tern-mode))
  (add-hook 'js2-mode-hook (ublt/on-fn 'tern-mode))

  (ublt/set-up 'company-tern
    (add-to-list 'company-backends 'company-tern)
    (setq company-tern-property-marker " ."
          company-tern-meta-as-single-line t)))

(provide 'ublt-js)
