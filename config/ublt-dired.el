(require 'ublt-util)

(ublt/in '(darwin)
  (setq insert-directory-program "/opt/local/libexec/gnubin/ls"))

(use-package hl-line
  :hook (dired-mode . hl-line-mode))

(use-package dired
  :straight nil
  :custom (
           ;; Offer the other window's path as default when copying
           (dired-dwim-target t)

           ;; Make find-name-dired ignore case
           (find-name-arg "-iname")

           (dired-hide-details-hide-symlink-targets nil))
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :straight nil
  :custom ((dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
           (dired-omit-mode t)
           (dired-guess-shell-alist-user
            (list
             (list "\\.t\\(ar\\.bz2\\|bz\\)\\'"
                   "tar xvjf"
                   "bunzip2 -c * | tar xvf -"
                   ;; Extract files into a separate subdirectory
                   '(concat "mkdir " (file-name-sans-extension file)
                            "; bunzip2 -c * | tar -C "
                            (file-name-sans-extension file) " -xvf -")
                   ;; Optional decompression.
                   "bunzip2")))))

(use-package dired-aux
  :straight nil
  :custom (dired-isearch-filenames 'dwim))

;; Directories first by default. "s d" to change locally
(ublt/set-up 'dired-sort-map
  (setq dired-listing-switches "--group-directories-first -alhG1v"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

;;; Make dired more colorful.
(use-package diredfl
  :config (diredfl-global-mode +1))

(use-package dired-rainbow
  :config (dired-rainbow-define-chmod executable "#D98D54" "-.*x.*"))

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
        (call-process (pcase system-type
                        ('darwin "open")
                        ('gnu/linux "xdg-open"))
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

(provide 'ublt-dired)
