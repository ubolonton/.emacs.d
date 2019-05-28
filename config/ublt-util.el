;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'dash)

(defun ublt/advice-remove-all (symbol)
  (advice-mapc (lambda (f _)
                 (advice-remove symbol f))
               symbol))

(defun ublt/advice-remove (symbol advice-symbol)
  (advice-mapc (lambda (f _)
                 (when (or (eq f advice-symbol)
                           (eq f (intern (concat symbol "@" advice-symbol))))
                   (advice-remove symbol f)))
               symbol))

(defun ublt/advice-list (symbol)
  (let ((advices nil))
    (advice-mapc (lambda (f _)
                   (setq advices (append advices `(,f))))
                 symbol)
    advices))



(defun ublt/enable (funcs)
  (dolist (f funcs)
    (put f 'disabled nil)))

;;; This is for stuff like
;;;
;;; (add-hook 'hexl-mode (ublt/off paredit-mode))
;;;
(defmacro ublt/off (minor-mode)
  `(lambda () (,minor-mode -1)))

;; To help separating OS-specific stuffs
(defmacro ublt/in (systems &rest body)
  "Run BODY if `system-type' is in the list of SYSTEMS."
  (declare (indent 1))
  `(when (member system-type ,systems)
     ,@body))

(defmacro ublt/with-demand (&rest body)
  (declare (indent 0))
  `(eval-when-compile
     (let ((use-package-always-demand t))
       ,@body)))

(defmacro ublt/with-defer (&rest body)
  (declare (indent 0))
  `(eval-when-compile
     (let ((use-package-always-defer t))
       ,@body)))

(defvar ublt/ok-features ())
(defvar ublt/error-features ())
;;; XXX: Hmm
(defun ublt/require (feature &optional filename noerror)
  (if noerror
      (condition-case err
          (progn
            (let ((name (require feature filename)))
              (add-to-list 'ublt/ok-features feature t)
              (message "Feature `%s' ok" feature)
              name))
        (error
         (setq ublt/error-features (plist-put ublt/error-features feature err))
         (message "Feature `%s' failed" feature)
         nil))
    (require feature filename)))

(defvar ublt/package-errors ())
(defun ublt/package-install (pkg)
  (when (not (package-installed-p pkg))
    (condition-case err
        (package-install pkg nil)
      (error
       (setq ublt/package-errors (plist-put ublt/package-errors pkg err))
       (message (propertize "Failed to install %s: %s" 'face 'font-lock-keyword-face)
                pkg err)))))

;;; TODO: Use `use-package'.
(defmacro ublt/set-up (feature &rest body)
  "Try loading the feature, running BODY afterward, notifying
user if not found. This is mostly for my customizations, since I
don't want a feature failing to load to affect other features in
the same file. Splitting everything out would result in too many
files."
  (declare (indent 1))
  `(let ((f (if (stringp ,feature) (intern ,feature) ,feature)))
     (when (ublt/require f nil t)
       ,@body)))

;; TODO: Isn't this about appearance?
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<ublt/in\\>" . font-lock-keyword-face)
   ("\\<ublt/set-up\\>" . font-lock-keyword-face)
   ("\\<with-eval-after-load +'\\(.*\\)\\>" 1 font-lock-constant-face)
   ("\\<use-package +\\(.*\\)\\>" 1 font-lock-constant-face)
   ("\\<ublt/set-up +'\\(.*\\)\\>" 1 font-lock-constant-face)) 'append)

(defun ublt/status-message (&rest args)
  "Show a message in the minibuffer without logging. Useful for
transient messages like error messages when hovering over syntax
errors."
  (let ((message-log-max nil))
    (apply #'message args)))

(defun ublt/isearch-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))

(defun ublt/eval-sexp-at-point ()
  (interactive)
  (prin1 (eval (read (thing-at-point 'sexp)))))

(defun ublt/isearch-exit-other-end (_rbeg _rend)
  "Exit isearch, but at the other end of the search string.
  This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

;;; `http://www.emacswiki.org/emacs/SearchAtPoint'
(defun ublt/isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

;;; FIX: Use `dash' library
(defun ublt/assoc! (list-var key val)
  (let* ((list (symbol-value list-var))
         (entry (assoc key list)))
    (if (null entry)
        (add-to-list list-var (cons key val))
      (setcdr entry val))))

(defmacro ublt/save-column (&rest body)
  (declare (indent 0))
  `(let ((c (or goal-column (current-column))))
     ,@body
     (move-to-column c)))

(defun ublt/get-string-from-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(declare-function helm-default-info-index-list "helm-info")
(declare-function helm-get-info-files "helm-info")
(defun ublt/helm-info-reload ()
  "Reload `helm-info' index. Use after loading a package with info doc."
  (require 'helm-info)
  (custom-set-variables
   '(helm-default-info-index-list (helm-get-info-files))))



(defun ublt/show-as (how &optional pred)
  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (ok (or (not pred) (funcall pred beg end))))
    (when ok
      (compose-region beg end how 'decompose-region))
    nil))


;;; Tools to tweak theme.

(defun ublt/theme-fontify-theme-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dolist (face (face-list))
     (save-excursion
       (when (search-forward (concat "`(" (symbol-name face)) nil t)
         (put-text-property (match-beginning 0) (match-end 0)
                            'font-lock-face face))))))

(defun ublt/theme-debug ()
  "Enable theme-debugging in this buffer."
  (interactive)
  (use-package rainbow-mode
    :functions (rainbow-mode))
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'ublt/theme-fontify-theme-buffer)
  (rainbow-mode +1))

(defun ublt/color-at-point-lighten (percent)
  (interactive "p")
  (let ((c (thing-at-point 'sexp))
        (p (point)))
    (when (stringp c)
      (let ((new-color (color-lighten-name c percent))
            (bound (bounds-of-thing-at-point 'sexp)))
        (delete-region (car bound) (cdr bound))
        (insert new-color)
        (goto-char p)))))

(defun ublt/color-at-point-darken (percent)
  (interactive "p")
  (ublt/color-at-point-lighten (- percent)))

(when nil
  (global-set-key (kbd "s-<up>") 'ublt/color-at-point-lighten)
  (global-set-key (kbd "s-<down>") 'ublt/color-at-point-darken))


(provide 'ublt-util)
