(require 'ublt-util)

(defun ublt/tab-2-spaces ()
  (setq tab-width 2))

(ublt/set-up 'markdown-mode
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

(ublt/set-up 'css-mode
  (ublt/set-up 'paredit
    (add-hook 'css-mode-hook (ublt/on-fn 'paredit-mode)))
  (ublt/set-up 'aggressive-indent
    (add-hook 'css-mode-hook (ublt/on-fn 'aggressive-indent-mode))))

(provide 'ublt-markups)
