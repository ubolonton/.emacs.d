;; To help separating OS-specific stuffs
(defmacro ublt/in (systems &rest body)
  "Run BODY if `system-type' is in the list of SYSTEMS."
  (declare (indent 1))
  `(when (member system-type ,systems)
     ,@body))

(defun ublt/status-message (&rest args)
  "Show a message in the minibuffer without logging. Useful for
transient messages like error messages when hovering over syntax
errors."
  (let ((message-log-max nil))
    (apply #'message args)))

(defvar ublt/set-up-features ())
(defvar ublt/failed-features ())
(defmacro ublt/set-up (feature &rest body)
  "Try to load the feature, running BODY afterward, notifying
user if not found. This is mostly for my customizations, since I
don't want a feature failing to load to affect other features in
the same file. Splitting everything out would result in too many
files."
  (declare (indent 1))
  `(let ((f (if (stringp ,feature) (intern ,feature) ,feature)))
    (if (not (require f nil t))
        (progn (message "ublt/customize: `%s' not found" f)
               (add-to-list 'ublt/failed-features f t))
      (add-to-list 'ublt/set-up-features f t)
      ,@body)))

;;; TODO: Use this
(defun ublt/isearch-other-window ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward-regexp)))

(provide 'ublt-util)
