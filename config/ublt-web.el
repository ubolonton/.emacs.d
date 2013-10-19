(require 'ublt-util)

(add-hook 'html-mode-hook 'turn-off-auto-fill-mode)

(ublt/set-up 'web-mode
  (setq web-mode-script-padding 0))

(ublt/set-up 'less-css-mode
  (add-hook 'less-css-mode-hook 'esk-prog-mode-hook)
  ;; (add-hook 'less-css-mode-hook 'enable-paredit-mode)
  )
(ublt/set-up 'css-mode
  (add-hook 'css-mode-hook 'esk-prog-mode-hook)
  ;; (add-hook 'css-mode-hook 'enable-paredit-mode)
  )

(add-to-list 'auto-mode-alist '("\\.mako?$" . html-mode))

;;; XXX
(ublt/set-up 'php-mode
  (setq php-mode-coding-style nil))

;; Emmet (Zen-coding)
(ublt/set-up 'emmet-mode
  (setq emmet-preview-default nil)
  ;; (make-variable-buffer-local 'emmet-indentation)
  (defun ublt/set-emmet-indentation ()
    (setq emmet-indentation tab-width))
  (add-hook 'sgml-mode-hook 'ublt/set-emmet-indentation))

(provide 'ublt-web)
