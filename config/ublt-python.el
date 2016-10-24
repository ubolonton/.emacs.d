(require 'ublt-util)

(require 'flycheck-pyflakes)

(ublt/set-up 'elpy
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-highlight-indentation
                       elpy-module-pyvenv))
  (setq elpy-rpc-backend "rope")
  (elpy-enable)
  (elpy-use-ipython))

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


;; ;; ============================================================
;; ;; `http://taesoo.org/Opensource/Pylookup'
;; ;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
;; (setq pylookup-dir "~/.emacs.d/lib/pylookup")
;; (add-to-list 'load-path pylookup-dir)
;; ;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; ;; ;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; ;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
;;   "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; (autoload 'pylookup-update "pylookup"
;;   "Run pylookup-update and create the database at `pylookup-db-file'." t)

;; (defvar ac-source-rope
;;   '((candidates . (lambda () (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")
;; (defun set-up-rope-ac ()
;;   (interactive)
;;   (setq ac-sources (add-to-list 'ac-sources 'ac-source-yasnippet)))
;; (add-hook 'python-mode-hook 'set-up-rope-ac)


;; (add-to-list 'Info-directory-list "~/.emacs.d/lib/python")
;; (require 'info-look)
;; (info-lookup-add-help
;;  :mode 'python-mode
;;  :regexp "[[:alnum:]]+"
;;  :doc-spec
;;  '(("(python)Index" nil "")))

;; ;;; TODO: Remove pymacs/ropemacs/ropemode?
;; ;; (ublt/set-up 'jedi
;; ;;   (add-hook 'python-mode-hook 'jedi:setup))

(provide 'ublt-python)
