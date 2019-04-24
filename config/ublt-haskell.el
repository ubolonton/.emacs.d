(require 'ublt-util)

(use-package haskell-mode
  :mode "\\.hs$"
  :hook (haskell-mode . turn-on-haskell-indentation))

(provide 'ublt-haskell)
