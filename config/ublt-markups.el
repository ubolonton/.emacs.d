(require 'ublt-util)

(defun ublt/tab-2-spaces ()
  (setq tab-width 2))

(ublt/set-up 'markdown-mode
  (setq markdown-fontify-code-blocks-natively t)
  (add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode)))

(ublt/set-up 'yaml-mode
  (add-hook 'yaml-mode-hook 'ublt/tab-2-spaces)
  (add-hook 'yaml-mode-hook (ublt/off-fn 'variable-pitch-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(ublt/set-up 'adoc-mode
  (add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode)))

(ublt/set-up 'mustache-mode
  (add-to-list 'auto-mode-alist '("\\.mustache$" . mustache-mode)))

(provide 'ublt-markups)
