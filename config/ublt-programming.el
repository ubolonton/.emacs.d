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
  :custom ((lsp-prefer-flymake :none)
           (lsp-ui-doc-enable nil)
           (lsp-ui-sideline-enable nil)
           (lsp-log-io t)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package cc-mode
  :mode ("\\.m$" . objc-mode))

(use-package tsc
  :init (setq tsc-dyn-get-from '(:compilation :github)))

(use-package tree-sitter
  :defer t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config (global-tree-sitter-mode))

;; XXX: `tree-sitter-langs' attempts to install grammars from GitHub, even for an unsupported OS
;; like FreeBSD.
(ublt/in '(berkeley-unix)
 (setq tree-sitter-langs--testing t))

(use-package tree-sitter-langs
  :defer t
  :after tree-sitter
  :hook (sh-mode . (lambda ()
                     (setq-local tree-sitter-hl-use-font-lock-keywords t)))
  :config
  (progn
    (setq tree-sitter-langs--dir "~/Programming/projects/elisp-tree-sitter/langs/"
          tree-sitter-langs--repos-dir "~/Programming/projects/elisp-tree-sitter/langs/repos/")
    ;; Disable low-quality languages from`tree-sitter-langs' bundle.
    (dolist (mode '(yaml-mode
                    dockerfile-mode
                    makefile-mode
                    makefile-bsdmake-mode
                    makefile-gmake-mode
                    gitignore-mode
                    org-mode
                    markdown-mode
                    sql-mode))
      (setf (map-elt tree-sitter-major-mode-language-alist mode) nil))
    (add-function :before-until tree-sitter-hl-face-mapping-function
                  (lambda (capture-name)
                    (pcase capture-name
                      ("ublt.hidden" 'ublt/lisp-paren-face)
                      ("ublt.unsafe" 'rust-unsafe-face))))
    (tree-sitter-hl-add-patterns 'python
      [(pattern/subscript subscript: (string) @variable)
       ((string) @constant
        (.match? @constant "^[bru]*'"))
       ["{" "}"] @ublt.hidden])
    (tree-sitter-hl-add-patterns 'javascript
      [["{" "}"] @ublt.hidden])
    (tree-sitter-hl-add-patterns 'rust
      ["unsafe" @ublt.unsafe])
    (tree-sitter-hl-add-patterns 'bash
      [(test_command
        ["[[" "]]" "[" "]" "((" "))"] @ublt.hidden)])))

(use-package vterm)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :custom ((claude-code-ide-window-side 'left)
           (claude-code-ide-use-side-window nil))
  :config (claude-code-ide-emacs-tools-setup))

(use-package agent-shell
  :config (setq agent-shell-preferred-agent-config
	            (agent-shell-google-make-gemini-config)))

;; Some rarely-written, but sometimes-read languages

(use-package nxml-mode
  :ensure nil :straight nil
  :mode "\\.xml.jin$")

(use-package applescript-mode
  :mode "\\.applescript$")

(use-package scala-mode
  :custom ((scala-indent:step 2)
           (scala-indent:align-parameters t)))

(use-package powershell
  :custom (powershell-indent 4))

;; (use-package csharp-mode)

(use-package groovy-mode)

(use-package lua-mode)

(use-package go-mode)

(use-package ebnf-mode)

(use-package bazel)

(use-package just-mode)

(use-package swift-mode)

(provide 'ublt-programming)
