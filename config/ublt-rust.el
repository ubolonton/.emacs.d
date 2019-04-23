(require 'ublt-util)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-copy-envs '("RUST_SRC_PATH" "RUSTC_WRAPPER")))

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package cargo)

(use-package lsp-mode
  :hook (rust-mode . lsp))

(use-package company-lsp
  :after (company lsp-mode))

(provide 'ublt-rust)
