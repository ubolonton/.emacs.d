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
;; (add-hook 'prog-mode-hook (ublt/on-fn idle-highlight-mode))
;; (add-hook 'prog-mode-hook (ublt/on-fn auto-highlight-symbol-mode))

(add-hook 'prog-mode-hook 'ublt/add-watchwords)
(add-hook 'prog-mode-hook (ublt/on-fn 'hl-line-mode))

(ublt/set-up 'highlight-parentheses
  (add-hook 'prog-mode-hook (ublt/on-fn 'highlight-parentheses-mode)))

(ublt/set-up 'lsp-ui
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(ublt/set-up 'lsp-mode
  (setq lsp-prefer-flymake :none))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.xml.jin$" . nxml-mode))

(provide 'ublt-programming)
