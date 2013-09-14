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
(eval-after-load "flymake"
  '(ublt/set-up 'erlang-flymake

     ;;
     (defun ublt/erlang-flymake-get-include-dirs ()
       (let ((dirs (erlang-flymake-get-include-dirs))
             (addition (condition-case nil
                           (concat (eproject-root) "deps")
                         (error nil))))
         (if addition
             (cons addition dirs)
           dirs)))

     (defun ublt/erlang-flymake-get-code-path-dirs ()
       (let ((dirs (erlang-flymake-get-code-path-dirs))
             (project-root (condition-case nil
                               (eproject-root)
                             (error nil))))
         (if project-root
             (append (list (concat project-root "deps/omlibrary/ebin")) ; XXX FIX
                     (list (concat project-root "deps"))
                     dirs)
           dirs)))

     (setq erlang-flymake-get-include-dirs-function 'ublt/erlang-flymake-get-include-dirs
           erlang-flymake-get-code-path-dirs-function 'ublt/erlang-flymake-get-code-path-dirs)

     )
  )

(provide 'ublt-erlang)
