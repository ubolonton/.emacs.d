(require 'ublt-util)

(defun ublt/tab-2-spaces ()
  (setq tab-width 2))

(use-package markdown-mode
  :custom ((markdown-fontify-code-blocks-natively t)
           (markdown-asymmetric-header t)
           (markdown-spaces-after-code-fence 0)
           (markdown-hide-urls t))
  :mode (("\\.md$" . gfm-mode)
         ("\\.markdown$" . gfm-mode))
  :hook (markdown-mode . ublt/run-prog-mode-hook))

(use-package yaml-mode
  :mode ("\\.yaml$"
         "\\Chart.lock\\'")
  :hook ((yaml-mode . (lambda () (setq tab-width 2)))
         (yaml-mode . (lambda () (variable-pitch-mode -1)))
         (yaml-mode . ublt/run-prog-mode-hook)))

(use-package adoc-mode
  :mode "\\.asciidoc$")

(use-package mustache-mode
  :mode "\\.mustache$")

(use-package conf-mode
  :ensure nil :straight nil
  :mode ("Pipfile$" . conf-toml-mode))

(provide 'ublt-markups)
