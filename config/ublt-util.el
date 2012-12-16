(defun ublt/enable (funcs)
  (dolist (f funcs)
          (put f 'disabled nil)))


;;; nxhtml is having troubles with emacs 24, so I have to use both 24
;;; & 23 now
(defun ublt/legacy? ()
  (< emacs-major-version 24))


(defun ublt/add-path (path)
  "Add to load-path a path relative to ~/.emacs.d/lib/"
  (add-to-list 'load-path (concat "~/.emacs.d/lib/" path)))


(defvar ublt/on-fns (make-hash-table))
(defun ublt/on-fn (minor-mode-fn)
  (let ((fn (gethash minor-mode-fn ublt/on-fns)))
    (if fn fn
      (puthash minor-mode-fn
               `(lambda () (,minor-mode-fn +1))
               ublt/on-fns))))


;; To help separating OS-specific stuffs
(defmacro ublt/in (systems &rest body)
  "Run BODY if `system-type' is in the list of SYSTEMS."
  (declare (indent 1))
  `(when (member system-type ,systems)
     ,@body))


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
(defmacro ublt/set-up (feature &rest body)
  "Try to load the feature, running BODY afterward, notifying
user if not found. This is mostly for my customizations, since I
don't want a feature failing to load to affect other features in
the same file. Splitting everything out would result in too many
files."
  (declare (indent 1))
  `(let ((f (if (stringp ,feature) (intern ,feature) ,feature)))
     (when (ublt/require f nil t)
       ,@body)))




(defun ublt/status-message (&rest args)
  "Show a message in the minibuffer without logging. Useful for
transient messages like error messages when hovering over syntax
errors."
  (let ((message-log-max nil))
    (apply #'message args)))


;;; TODO: Use this
(defun ublt/isearch-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))


;;; TODO: Mode-specific rules?
(defun ublt/set-indent-level (chars)
  (setq c-basic-offset chars)
  (setq tab-width chars))


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


(provide 'ublt-util)
