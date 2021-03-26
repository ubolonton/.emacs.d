(require 'ublt-util)

(use-package js2-mode
  :disabled t
  :mode ("\\.es6\\'" "\\.m?js\\'")
  :hook (js2-mode . auto-fill-mode)
  :custom
  (js2-highlight-level 0 "Use tree-sitter-hl")
  (js2-concat-multiline-strings nil)
  (js2-mode-show-parse-errors nil "Going to use tree-sitter")
  (js2-mode-show-strict-warnings nil "Going to use flycheck")
  (js2-strict-trailing-comma-warning nil "Going to use flycheck")
  (js2-strict-missing-semi-warning nil "Going to use flycheck")
  (js2-basic-offset 2))

(use-package js
  :mode ("\\.jsm$" . js-mode)
  :custom (js-indent-level 2))

(use-package typescript-mode
  :custom (typescript-indent-level 2))

(use-package json-mode
  :mode "\\.json$")

(use-package flycheck
  :hook
  (json-mode . flycheck-mode)
  (js2-mode . flycheck-mode)
  (js-mode . flycheck-mode)
  :custom (flycheck-jshintrc "~/.jshint.json"))

(provide 'ublt-js)
