(require 'ublt-util)

(use-package terraform-mode)

(use-package company-terraform
  :after (company terraform-mode)
  :config (add-to-list 'company-backends 'company-terraform))

(provide 'ublt-devops)
