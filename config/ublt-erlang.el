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

;;; Syntax checking
(eval-after-load "flymake"
  '(ublt/set-up 'erlang-flymake

     ;;
     (defun ublt/erlang-flymake-get-include-dirs ()
       (cons (concat (eproject-root) "deps")
             (erlang-flymake-get-include-dirs)))

     (defun ublt/erlang-flymake-get-code-path-dirs ()
       (cons (concat (eproject-root) "deps")
             (erlang-flymake-get-code-path-dirs)))

     (setq erlang-flymake-get-include-dirs 'ublt/erlang-flymake-get-include-dirs
           erlang-flymake-get-code-path-dirs 'ublt/erlang-flymake-get-code-path-dirs)

     ))

(provide 'ublt-erlang)
