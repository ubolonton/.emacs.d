(require 'ublt-util)

(ublt/with-defer
  (use-package terraform-mode)
  (use-package dockerfile-mode)
  (use-package systemd)
  (use-package inf-mongo))

(use-package company-terraform
  :after (company terraform-mode)
  :config (add-to-list 'company-backends 'company-terraform))

(use-package flycheck
  :hook (sh-mode . flycheck-mode))

(provide 'ublt-devops)
