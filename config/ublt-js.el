(require 'ublt-util)

;; Javascript (it seems js-mode in Emacs is newer than espresso)
;; v8: scons via apt-get (not pip or easy_install; among other things,
;; build tool & package manager is what clojure gets absolutely right,
;; whereas python sucks ass).
;; Actually screw that, use virtualenv and easy_install scons, as
;; described here https://github.com/koansys/jshint-v8. But remember
;; to do
;;
;; export SCONS_LIB_DIR=/path/to/virtual-scons-egg/scons-sth
;;
;; scons console=readline snapshot=on library=shared d8
;;
;; Well it's still complains about missing libv8.so. Just install
;; "node" then.
;;
;;
;; (defalias 'javascript-mode 'espresso-mode)
;; (setq js-mode-hook '())
;; (setq flymake-jslint-command "jslint")


(eval-after-load "js2-mode"
  '(progn
     (add-hook 'js2-mode-hook 'esk-prog-mode-hook)
     (add-hook 'js2-mode-hook 'esk-paredit-nonlisp)
     (add-hook 'js2-mode-hook 'moz-minor-mode)
     (defalias 'javascript-mode 'js2-mode)
     ;; XXX: Copied from starter-kit-js
     ;; fixes problem with pretty function font-lock
     (define-key js-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords
      'js2-mode `(("\\(function *\\)("
                   (0 (progn (compose-region (match-beginning 1)
                                             (match-end 1) "\u0192")
                             nil)))))
     (setcdr (assoc "\\.js\\'" auto-mode-alist)
             'js2-mode)))


(ublt/set-up 'js
  (when (ublt/legacy?)
    (add-hook 'js-mode-hook 'esk-prog-mode-hook))

  ;; XXX: What is this for?
  (defvar javascript-mode-syntax-table js-mode-syntax-table)

  ;; MozRepl integration
  (add-hook 'js-mode-hook 'moz-minor-mode)
  (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

  (setq js-indent-level 2
        espresso-indent-level 2))


;;; Syntax checking
(eval-after-load "flymake"
  '(progn
     (ublt/set-up 'flymake-jshint
       (setq jshint-configuration-path "~/.jshint.json")

       (defun ublt/flymake-js-maybe-enable ()
         (when (and buffer-file-name
                    (string-match "\\.js$" buffer-file-name))
           (flymake-mode +1)))
       (remove-hook 'js-mode-hook 'flymake-mode)
       (add-hook 'js-mode-hook 'ublt/flymake-js-maybe-enable))))


(provide 'ublt-js)
