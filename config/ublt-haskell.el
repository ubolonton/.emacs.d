(require 'ublt-util)

(ublt/set-up 'haskell-mode
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(provide 'ublt-haskell)
