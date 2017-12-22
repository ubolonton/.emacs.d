(require 'ublt-util)

(ublt/set-up 'racer
  (when (executable-find "racer")
    (add-hook 'rust-mode-hook (ublt/on-fn 'racer-mode)))
  (add-hook 'racer-mode-hook (ublt/on-fn 'eldoc-mode)))

(ublt/set-up 'company
  (add-hook 'racer-mode-hook (ublt/on-fn 'company-mode)))

(provide 'ublt-rust)
