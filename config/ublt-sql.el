(require 'ublt-util)

(use-package sql
  ;; FIX: Somehow sql-mode being a prog-mode does not help. So it
  ;; needs to be added manually
  :hook (sql-mode . ublt/add-watchwords)

  ;; Should ask for ports, what's with 8/9 using different ports
  ;; (5432/5433), and port forwarding to virtual/remote machines
  :config (add-to-list 'sql-postgres-login-params '(port :default 5432)))

;;; TODO: Implement more. This is currently just describe-table-at-point
(defun ublt/sql-describe-thing-at-point ()
  (interactive)
  (sql-send-string
   (format "\\d %s" (thing-at-point 'symbol))))

(provide 'ublt-sql)
