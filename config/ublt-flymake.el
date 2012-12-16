(require 'ublt-util)

(require 'flymake)
;; (defun flymake-php-init ()
;;   (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;;          (local (file-relative-name temp (file-name-directory buffer-file-name))))
;;     (list "php" (list "-f" local "-l"))))
;; (add-to-list 'flymake-err-line-patterns
;;   '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))
;; (add-hook 'php-mode-hook (ublt/on-fn 'flymake-mode))

(defun ublt/flymake-err-at (pos)
  (let ((overlays (overlays-at pos)))
    (remove nil
            (mapcar (lambda (overlay)
                      (and (overlay-get overlay 'flymake-overlay)
                           (overlay-get overlay 'help-echo)))
                    overlays))))

(defface ublt/flymake-message-face
  `((t (:inherit font-lock-keyword-face)))
  "Face for flymake message echoed in the minibuffer.")
(defun ublt/flymake-err-echo ()
  "Echo flymake error message in the minibuffer (not saving to *Messages*)."
  (ublt/status-message "%s"
             (propertize (mapconcat 'identity
                                    (ublt/flymake-err-at (point)) "\n")
                         'face 'ublt/flymake-message-face)))

(defadvice flymake-goto-next-error (after display-message activate)
  (ublt/flymake-err-echo))
(defadvice flymake-goto-prev-error (after display-message activate)
  (ublt/flymake-err-echo))

(eval-after-load "js"
  '(ublt/set-up 'flymake-jshint
     (setq jshint-configuration-path "~/.jshint.json")
     (defun ublt/flymake-js-enable ()
       (when (and buffer-file-name
                  (string-match "\\.js$" buffer-file-name))
         (flymake-mode +1)))
     (remove-hook 'js-mode-hook 'flymake-mode)
     (add-hook 'js-mode-hook 'ublt/flymake-js-enable)))

(eval-after-load "php-mode"
  '(ublt/set-up 'flymake-php
     (add-hook 'php-mode-hook 'flymake-php-load)))



(eval-after-load "ublt-python"
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  ;; From `starter-kit-ruby.el'
  (defun ublt/flymake-python-enable ()
    (when (and buffer-file-name
               ;; flymake and mumamo are at odds, so look at buffer
               ;; name instead of `major-mode' when deciding whether
               ;; to turn this on
               (string-match "\\.py$" buffer-file-name)
               (file-writable-p
                (file-name-directory buffer-file-name))
               (file-writable-p buffer-file-name)
               (if (fboundp 'tramp-list-remote-buffers)
                   (not (subsetp
                         (list (current-buffer))
                         (tramp-list-remote-buffers)))
                 t))
      (flymake-mode t)))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))

  (add-hook 'python-mode-hook 'ublt/flymake-python-enable)
  )

(dolist (hook '(emacs-lisp-mode-hook))
  (add-hook hook (ublt/on-fn 'flymake-mode)))

(provide 'ublt-flymake)
