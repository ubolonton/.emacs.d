(require 'ublt-util)


;;; Mix-language files

(use-package web-mode
  :mode ("\\.ftl$" "\\.jsx$" "\\.tsx$"
         "\\.html$" "\\.mako?$" "\\.mjml?$" "\\.handlebars$" "\\.underscore$")
  :hook ((web-mode . (lambda () (auto-fill-mode -1)))
         (web-mode . ublt/web-mode-jsx)
         (web-mode . ublt/web-mode--will-set-engine))
  :custom ((web-mode-extra-keywords '(("comment" . ("NTA" "FIX" "XXX"))))
           ;; Padding
           (web-mode-script-padding 0)
           (web-mode-style-padding 2)
           ;; Indentation
           (web-mode-markup-indent-offset 2)
           (web-mode-code-indent-offset 2)
           (web-mode-css-indent-offset 2)
           ;; Coloring a lot
           (web-mode-enable-current-element-highlight t)
           (web-mode-enable-block-face t)
           (web-mode-enable-part-face t)
           (web-mode-enable-comment-keywords t)
           (web-mode-enable-element-content-fontification t)
           ;; Auto-close when "</" is typed
           (web-mode-tag-auto-close-style 1))
  :config

  ;; FIX: This is a bad hack
  (define-advice web-mode-fontify-part (:around (f &rest args) ublt/tweak-jsx)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          (apply f args))
      (apply f args)))
  (defun ublt/web-mode-jsx ()
    (when (equal web-mode-content-type "jsx")
      (paredit-mode +1)))

  (defvar ublt/web-mode-engine nil
    "This is a hack to allow using dir-local variables to set `web-mode' engine.
FIX: `web-mode' should check local variables itself.
`http://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks'.
Example:

    ((web-mode
      (ublt/web-mode-engine . \"hugo\")))

See [[info:emacs#Directory Variables]].")
  (defun ublt/web-mode--will-set-engine ()
    (add-hook 'hack-local-variables-hook
              (lambda ()
                (when ublt/web-mode-engine
                  (web-mode-set-engine ublt/web-mode-engine)))
              nil :local)))


;; Emmet (Zen-coding)

(use-package emmet-mode
  :custom ((emmet-preview-default nil)
           (emmet-indentation 2))
  :config
  (defun ublt/set-up-emmet ()
    (emmet-mode +1)
    ;; Dynamic indentation after expansion based on tab width
    (set (make-local-variable 'emmet-indentation) tab-width))
  (dolist (m '(sgml-mode-hook
               html-mode-hook
               css-mode-hook
               less-css-mode-hook
               web-mode-hook))
    (add-hook m 'ublt/set-up-emmet)))


;;; Misc

(use-package less-css-mode
  :hook (less-css-mode . ublt/run-prog-mode-hook))

(use-package css-mode
  :hook (css-mode . ublt/run-prog-mode-hook)
  :custom (css-indent-offset 2))

(use-package aggressive-indent
  :hook (css-mode . aggressive-indent-mode))

(use-package paredit
  :hook (css-mode . paredit-mode))

(use-package sgml-mode
  :hook (html-mode . (lambda () (auto-fill-mode -1))))

;;; XXX
(use-package php-mode
  :defer t
  :custom (php-mode-coding-style nil)
  :hook (php-mode . (lambda () (setq c-basic-offset 4))))

(use-package flycheck
  :hook (php-mode . flycheck-mode))

(provide 'ublt-web)
