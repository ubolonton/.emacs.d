(require 'ublt-util)

(use-package erlang
  :mode ("\\.rel$" . erlang-mode))

(use-package flycheck
  :hook (erlang-mode . flycheck-mode))

(provide 'ublt-erlang)
