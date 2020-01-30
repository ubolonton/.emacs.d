;;; Generic programming stuff applicable to all languages

(require 'ublt-util)

(defun ublt/run-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(defun ublt/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|XXX\\|HACK\\|REFACTOR\\|NOCOMMIT\\|NTA\\)"
          1 font-lock-warning-face t))))

;;; Auto-fill, but only in comments.
(defun ublt/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'ublt/local-comment-auto-fill)
;;; We use variable-pitch font for comments and doc strings, so they are narrower.
(setq-default comment-fill-column 100)

;;; TODO: Enable this when there is a workaround for highlighted
;;; symbols always being displayed in fixed-width font
;; (add-hook 'prog-mode-hook #'idle-highlight-mode)
;; (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)

(add-hook 'prog-mode-hook 'ublt/add-watchwords)

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

(use-package lsp-mode
  :custom (lsp-prefer-flymake :none))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package cc-mode
  :mode ("\\.m$" . objc-mode))

(use-package nxml-mode
  :ensure nil :straight nil
  :mode "\\.xml.jin$")

(use-package applescript-mode
  :mode "\\.applescript$")

(use-package scala-mode
  :custom ((scala-indent:step 4)
           (scala-indent:align-parameters t)))

(use-package groovy-mode)

(use-package powershell-mode
  :mode "\\.ps1$"
  :custom (powershell-indent 4))

(provide 'ublt-programming)
