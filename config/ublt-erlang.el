(require 'ublt-util)

(ublt/in '(gnu/linux)
  (ublt/set-up 'erlang-start
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/erlang")
    (setq erlang-root-dir "/usr/lib/erlang")
    (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

    (defun ublt/erlang-compile-and-display ()
      (interactive)
      (call-interactively 'erlang-compile)
      (call-interactively 'erlang-compile-display))
    (add-hook 'erlang-mode-hook 'esk-prog-mode-hook)))

;; Syntax checking
(ublt/set-up 'flycheck
  (add-hook 'erlang-mode-hook (ublt/on-fn 'flycheck-mode)))

(provide 'ublt-erlang)
