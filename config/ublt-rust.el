(require 'ublt-util)

(use-package exec-path-from-shell
  :when (memq system-type '(darwin gnu/linux))
  :config (exec-path-from-shell-copy-envs '("RUST_SRC_PATH" "RUSTC_WRAPPER")))

(use-package rust-mode
  :custom (rust-format-on-save nil))

(use-package cargo)

(use-package lsp-mode
  :custom ((lsp-ui-doc-enable nil)
           (lsp-ui-sideline-enable nil)
           (lsp-log-io t))
  :hook ((rust-mode . lsp)))

(provide 'ublt-rust)
