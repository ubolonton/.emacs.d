(require 'ublt-util)

(use-package flycheck-pyflakes
  :after python)

(use-package elpy
  :disabled t
  :custom ((elpy-modules '(elpy-module-sane-defaults
                           elpy-module-company
                           elpy-module-eldoc
                           elpy-module-highlight-indentation
                           elpy-module-pyvenv))
           (elpy-rpc-backend "rope"))
  :config (elpy-enable))

(use-package python
  :mode (("\\.tac$" . python-mode)
         ("\\.bzl$" . python-mode))
  :hook (python-mode . (lambda () (setq tab-width 4)))
  :custom ((python-check-command "pyflakes")
           (python-shell-interpreter "ipython")
           (python-shell-interpreter-args "-i --colors=Linux")
           (python-shell-prompt-regexp "In \\[[0-9]?\\]: ")
           (python-shell-prompt-output-regexp "Out \\[[0-9]?\\]: ")
           (python-fill-docstring-style 'pep-257-nn)))

(use-package paredit
  :hook (python-mode . paredit-mode))

(use-package flycheck
  :hook (python-mode . ublt/python-maybe-flycheck)
  :config (defun ublt/python-maybe-flycheck ()
            (when (and buffer-file-name
                       (member (file-name-extension buffer-file-name) '("py")))
              (flycheck-mode +1))))


;;; TODO: completion, history search, and other ipythons stuff

;; ;;; TODO: Remove pymacs/ropemacs/ropemode?
;; ;; (use-package jedi
;; ;;   (add-hook 'python-mode-hook 'jedi:setup))

(provide 'ublt-python)
