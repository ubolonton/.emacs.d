(require 'ublt-util)

(ublt/set-up 'erlang
  (add-to-list 'auto-mode-alist '("\\.rel$" . erlang-mode)))

;; (ublt/set-up 'edts
;;   (add-hook 'after-init-hook (lambda () (ublt/set-up 'edts-start))))

;;; XXX
(add-hook 'erlang-mode-hook (ublt/off-fn 'auto-complete-mode) t)

;; Syntax checking
(ublt/set-up 'flycheck
  ;; (add-hook 'erlang-mode-hook (ublt/on-fn 'flycheck-mode))

  ;; XXX
  (add-hook 'erlang-mode-hook (ublt/off-fn 'flymake-mode) t))

(provide 'ublt-erlang)
