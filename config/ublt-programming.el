;;; Generic programming stuff applicable to all languages

(defun ublt/run-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(defun ublt/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|XXX\\|HACK\\|REFACTOR\\|NOCOMMIT\\|NTA\\)"
          1 font-lock-warning-face t))))

(defun ublt/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

;;; TODO: Enable this when there is a workaround for highlighted
;;; symbols always being displayed in fixed-width font
;; (add-hook 'prog-mode-hook (ublt/on-fn idle-highlight-mode))
;; (add-hook 'prog-mode-hook (ublt/on-fn auto-highlight-symbol-mode))

(add-hook 'prog-mode-hook 'ublt/add-watchwords)
(add-hook 'prog-mode-hook 'ublt/local-comment-auto-fill)
(add-hook 'prog-mode-hook (ublt/on-fn 'hl-line-mode))
(add-hook 'prog-mode-hook (ublt/on-fn 'highlight-parentheses-mode))

(provide 'ublt-programming)
