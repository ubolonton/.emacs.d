(defun ublt/enable (funcs)
  (dolist (f funcs)
    (put f 'disabled nil)))

(defun ublt/add-path (path)
  "Add to load-path a path relative to ~/.emacs.d/lib/"
  (add-to-list 'load-path (concat "~/.emacs.d/lib/" path)))

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

;;; Source -
;;; `http://sites.google.com/site/steveyegge2/my-dot-emacs-file'
;;; TODO: Use
(defun ublt/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

(defun ublt/move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))

;;; `http://www.emacswiki.org/emacs/ZapToISearch'
(defun ublt/zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode. The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)))

(defun ublt/isearch-exit-other-end (rbeg rend)
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

(defun ublt/maybe-unquote (s)
  "XXX: This allow using both quoted and unquoted form (since a
macro already has its parameters quoted). Because Lisp syntax is
not regular enough. Uh huh."
  (if (and (eq (type-of s) 'cons)
           (eq (car s) 'quote))
      (cadr s)
    s))

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

;;; Turns out `font-lock-mode' already has similar functions
;; ;;; XXX: This currently works only for `font-lock-face' and `face',
;; ;;; because other props may have different structures
;; (defun ublt/add-text-property (start end prop value)
;;   (when (< start end)
;;     (let ((pos start))
;;       (while (< pos end)
;;         (let* ((next (next-single-property-change pos prop nil end))
;;                (cur (get-text-property pos prop)))
;;           ;; TODO: How about the prop-list case?
;;           (put-text-property pos next prop
;;                              (cond
;;                               ((listp cur) (cons value cur))
;;                               (t (list value cur))))
;;           (setq pos next))))))

;; (defun ublt/add-face (start end face)
;;   "Put `face' at the beginning of `face' property of text from
;; `start' to `end'."
;;   (ublt/add-text-property start end 'face face)
;;   ;; (ublt/add-text-property start end 'font-lock-face face)
;;   )

(defun ublt/find-chars (str)
  (mapc
   (lambda (x)
     (let ((nn (get-char-code-property x 'name)))
       (when
           (and (not (null nn))
                (string-match str nn))
         (insert-char x)
         (insert " " nn "\n"))))
   (number-sequence 0 (expt 2 16))))

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
  (ublt/define-keys global-map
    "s-<up>" 'ublt/color-at-point-lighten
    "s-<down>" 'ublt/color-at-point-darken))


(provide 'ublt-util)
