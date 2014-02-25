(require 'ublt-util)

;; Factor
(condition-case err
    (progn
      (ublt/in '(darwin)
        (load-file "/Applications/factor/misc/fuel/fu.el"))
      (ublt/in '(gnu/linux)
        (load-file "~/Programming/factor/misc/fuel/fu.el")))
  (error (message "No Factor")))

(provide 'ublt-factor)
