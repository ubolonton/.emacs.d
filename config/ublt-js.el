(require 'ublt-util)

(use-package js2-mode
  :disabled t
  :mode ("\\.es6$" "\\.js$")
  :hook (js2-mode . auto-fill-mode)
  :custom
  (js2-highlight-level 3)
  (js2-concat-multiline-strings nil)
  ;; Use jshint instead
  (js2-mode-show-parse-errors t)
  (js2-mode-show-strict-warnings nil)
  (js2-strict-trailing-comma-warning nil)
  (js2-strict-missing-semi-warning nil)
  (js2-basic-offset 2))

(use-package paredit
  :hook
  (js2-mode . paredit-mode)
  (js-mode . paredit-mode))

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

(use-package typescript-mode)

(provide 'ublt-js)
