(require 'ublt-util)

(require 'dired+)

;;; Apparently this works much better than dired-do-async-shell-command and
;; nohup trickeries
(defun ublt/dired-open-native ()
  "Open marked files (or the file the cursor is on) from dired."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (n (length files)))
    (when (or (<= n 3)
              (y-or-n-p (format "Open %d files?" n)))
      (dolist (file files)
        (call-process (case system-type
                        ('darwin "open")
                        ('gnu/linux "gnome-open"))
                      nil 0 nil file)))))

;;; TODO: Improve & use
(defun ublt/dired-rsync (dest)
  (interactive
   (list (expand-file-name (read-file-name "Rsync to:"))))

  (let ((files (dired-get-marked-files nil current-prefix-arg)))
    (dolist (file files)
      (message "%s => %s" file dest)
      (start-process "rsync" "*rsync*" "rsync"
                     "-rvz" file dest))))

;; Highlight current line
(add-hook 'dired-mode-hook (ublt/on-fn 'hl-line-mode))

;; Hide details
(ublt/set-up 'dired-details+
  (setq dired-details-hide-link-targets nil)
  ;; Hide unimportant files
  (setq-default dired-omit-mode t
                dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\."))

;; Directories first by default. "s d" to change locally
(ublt/set-up 'dired-sort-map
  (setq dired-listing-switches "--group-directories-first -alh"))

(setq
 ;; Offer the other window's path as default when copying
 dired-dwim-target t

 ;; Make find-name-dired ignore case
 find-name-arg "-iname")

(provide 'ublt-dired)