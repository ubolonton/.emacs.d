(require 'ublt-util)

(require 'flycheck-pyflakes)

(ublt/set-up 'elpy
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-highlight-indentation
                       elpy-module-pyvenv))
  (setq elpy-rpc-backend "rope")
  (elpy-enable))

(ublt/set-up 'python
  (defun ublt/tab-4-spaces ()
    (setq tab-width 4))
  (add-hook 'python-mode-hook 'ublt/tab-4-spaces)

  (ublt/set-up 'flycheck
    (defun ublt/python-maybe-flycheck ()
      (when (member (file-name-extension buffer-file-name) '("py"))
        (flycheck-mode +1)))
    (add-hook 'python-mode-hook #'ublt/python-maybe-flycheck))

  (setq python-check-command "pyflakes"
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --colors=Linux"
        python-shell-prompt-regexp "In \\[[0-9]?\\]: "
        python-shell-prompt-output-regexp "Out \\[[0-9]?\\]: "
        python-fill-docstring-style 'pep-257-nn)

  (add-to-list 'auto-mode-alist '("\\.tac$" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.bzl$" . python-mode))

  (ublt/set-up 'paredit
    (add-hook 'python-mode-hook (ublt/on-fn 'paredit-mode))))


;;; TODO: completion, history search, and other ipythons stuff



;; ;;; TODO: Remove pymacs/ropemacs/ropemode?
;; ;; (ublt/set-up 'jedi
;; ;;   (add-hook 'python-mode-hook 'jedi:setup))

(provide 'ublt-python)
