(require 'ublt-util)


;;; Mix-language files

;;; FIX: This must be before web-mode is loaded, which is weird

(use-package web-mode
  :init (setq web-mode-extra-comment-keywords '("NTA" "FIX" "XXX"))
  ;; FreeMarker templates
  :mode ("\\.ftl$" "\\.jsx$" "\\.tsx$"
         "\\.html$" "\\.mako?$" "\\.mjml?$" "\\.handlebars$" "\\.underscore$")
  :hook ((web-mode . (lambda () (auto-fill-mode -1)))
         (web-mode . ublt/web-mode-jsx)
         (web-mode . ublt/web-mode-set-engine))

  :custom (
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
  (define-advice web-mode-highlight-part (:around (f &rest args) ublt/tweak-jsx)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          (apply f args))
      (apply f args)))
  (defun ublt/web-mode-jsx ()
    (when (equal web-mode-content-type "jsx")
      (paredit-mode +1)))

  (add-to-list 'web-mode-engine-file-regexps '("mako" . "\\.mako?\\'"))

  ;; XXX: Quick-n-dirty hack to highlight `o-blog' templates
  (with-eval-after-load 'org-src
    (when (functionp 'org-src-font-lock-fontify-block)
      (defun ublt/web-mode-font-lock-lisp-tags (limit)
        (while (search-forward "<lisp>" limit t)
          (let ((open-end (match-end 0)))
            (if (search-forward "</lisp>" limit t)
                (let ((close-beg (match-beginning 0)))
                  ;; TODO: Figure out how to add a face without
                  ;; nullifying the effect of `font-lock-add-keywords'
                  ;; (font-lock-append-text-property open-end close-beg 'font-lock-face 'web-mode-block-face)
                  ;; (font-lock-append-text-property open-end close-beg 'face 'web-mode-block-face)
                  (org-src-font-lock-fontify-block "emacs-lisp" open-end close-beg))))))
      (add-to-list 'web-mode-font-lock-keywords 'ublt/web-mode-font-lock-lisp-tags t)
      (font-lock-add-keywords
       'web-mode
       '(("(\\(ob:\\)\\(\\(\\w\\|-\\)+\\)"
          (1 font-lock-variable-name-face)
          (2 font-lock-function-name-face)))
       'append)))

  ;; This is a hack to allow using dir-local variables to set
  ;; `web-mode' engine. FIX: `web-mode' should check local variables
  ;; itself.
  ;; `http://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks'
  (defvar ublt/web-mode-engine nil)
  (defun ublt/web-mode-set-engine ()
    (add-hook 'hack-local-variables-hook
              (lambda ()
                (when ublt/web-mode-engine
                  (web-mode-set-engine ublt/web-mode-engine)))
              nil t)))


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
