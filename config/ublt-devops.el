(require 'ublt-util)

(ublt/set-up 'company-terraform
  (add-to-list 'company-backends 'company-terraform))

(provide 'ublt-devops)
