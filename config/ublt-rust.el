(require 'ublt-util)

(ublt/set-up 'exec-path-from-shell
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

(ublt/set-up 'lsp-mode
  (add-hook 'rust-mode-hook #'lsp))

(provide 'ublt-rust)
