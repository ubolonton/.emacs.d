(require 'ublt-util)

(add-hook 'html-mode-hook (ublt/off-fn 'auto-fill-mode))

;;; FIX: This must be before web-mode is loaded, which is weird
(setq web-mode-extra-comment-keywords '("NTA" "FIX" "XXX"))
(ublt/set-up 'web-mode
  ;; `web-mode' doesn't highlight correctly if `font-lock-mode' is on.
  ;; Weird
  (add-hook 'web-mode-hook (ublt/off-fn 'font-lock-mode))
  (setq web-mode-script-padding 0
        web-mode-style-padding 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-tag-auto-close-style 2)
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mako?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.underscore$" . web-mode)))

(ublt/set-up 'less-css-mode
  (add-hook 'less-css-mode-hook 'esk-prog-mode-hook)
  ;; (add-hook 'less-css-mode-hook 'enable-paredit-mode)
  )
(ublt/set-up 'css-mode
  (add-hook 'css-mode-hook 'esk-prog-mode-hook)
  ;; (add-hook 'css-mode-hook 'enable-paredit-mode)
  )

;; (add-to-list 'auto-mode-alist '("\\.mako?$" . html-mode))

;;; XXX
(ublt/set-up 'php-mode
  (setq php-mode-coding-style nil))

;; Emmet (Zen-coding)
(ublt/set-up 'emmet-mode
  (setq emmet-preview-default nil)
  ;; (make-variable-buffer-local 'emmet-indentation)
  (defun ublt/set-up-emmet ()
    (emmet-mode +1)
    (set (make-local-variable 'emmet-indentation) tab-width))
  (dolist (m '(sgml-mode-hook
               html-mode-hook
               css-mode-hook
               less-css-mode-hook
               web-mode-hook))
    (add-hook m 'ublt/set-up-emmet)))

(provide 'ublt-web)
