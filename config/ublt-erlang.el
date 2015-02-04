(require 'ublt-util)

(ublt/set-up 'erlang
  (add-to-list 'auto-mode-alist '("\\.rel$" . erlang-mode)))

(ublt/set-up 'edts
  (add-hook 'after-init-hook (lambda () (ublt/set-up 'edts-start))))

;;; XXX
(add-hook 'erlang-mode-hook (ublt/off-fn 'auto-complete-mode) t)

;; Syntax checking
(ublt/set-up 'flycheck
  ;; (add-hook 'erlang-mode-hook (ublt/on-fn 'flycheck-mode))

  ;; XXX
  (add-hook 'erlang-mode-hook (ublt/off-fn 'flymake-mode) t)

  (ublt/set-up 'edts
    (defun ublt/flycheck-erlang-options ()
      (let* ((root (eproject-root))
             (lib-dirs (eproject-attribute :lib-dirs))
             (app-include-dirs (eproject-attribute :app-include-dirs))
             (project-include-dirs (eproject-attribute :project-include-dirs))
             (make-include (lambda (dir) (list "-I" (concat root dir)))))
        (append
         (apply #'append (mapcar make-include app-include-dirs))
         (apply #'append (mapcar make-include project-include-dirs))))
      ;; (list "-I" "/home/ubolonton/Programming/cogini/bidandbuy/server/apps/auctions/include")
      )

    (flycheck-define-checker ublt-erlang
      "Ubolonton's flycheck checker for Erlang files"
      :command ("erlc" (eval (ublt/flycheck-erlang-options)) "-o" temporary-directory "-Wall" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
       (error line-start (file-name) ":" line ": " (message) line-end))
      :mode erlang-mode
      :predicate (lambda () (and (eql major-mode 'erlang-mode) (eproject-root))))

    (setq flycheck-checkers (delq 'erlang flycheck-checkers))
    (add-to-list 'flycheck-checkers 'ublt-erlang)))

(provide 'ublt-erlang)
