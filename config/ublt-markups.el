(require 'ublt-util)

(defun ublt/tab-2-spaces ()
  (setq tab-width 2))

(ublt/set-up 'markdown-mode
  (setq markdown-fontify-code-blocks-natively t
        markdown-asymmetric-header t
        markdown-spaces-after-code-fence 0
        markdown-hide-urls t)
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
  (add-hook 'markdown-mode-hook #'ublt/run-prog-mode-hook))

(ublt/set-up 'yaml-mode
  (add-hook 'yaml-mode-hook 'ublt/tab-2-spaces)
  (add-hook 'yaml-mode-hook (ublt/off-fn 'variable-pitch-mode))
  (add-hook 'yaml-mode-hook #'ublt/run-prog-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(ublt/set-up 'adoc-mode
  (add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode)))

(ublt/set-up 'mustache-mode
  (add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode)))

(ublt/set-up 'conf-mode
  (add-to-list 'auto-mode-alist '("Pipfile$" . conf-toml-mode)))

(provide 'ublt-markups)
