(require 'ublt-util)

(ublt/set-up 'magit
  (setq
   ;; ;; Show original windows when quitting magit.
   ;; magit-restore-window-configuration t

   ;; magit status buffer should not be a pop-up (in the sense of
   ;; not being volatile or temporary like helm buffer). This is
   ;; important for small screens.
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1

   ;; `all' would be nice, but it's too slow for large diffs
   magit-diff-refine-hunk t

   magit-diff-paint-whitespace 'status

   ;; Git directories
   magit-repository-directories '(("~/Programming/projects" . 1)
                                  ("~/Programming/lib" . 1)
                                  ("~/Programming/adatao" . 1))
   magit-repository-directories-depth 1

   ;; Other
   magit-log-show-refname-after-summary t
   magit-blame-mode-lighter " Bl")

  (eval-after-load 'diff-hl
    '(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  ;; ;; XXX
  ;; (defadvice magit-process (around dont-switch activate)
  ;;   (save-selected-window
  ;;     ad-do-it))
  )

(ublt/set-up 'magit-popup
  (dolist (switch '((?m "Exclude merges" "--no-merges")
                    (?M "Show only merges" "--merges")
                    (?t "Sort topologically" "--topo-order")
                    (?f "Exclude foreign" "--first-parent")))
    (apply #'magit-define-popup-switch 'magit-log-popup switch)))

(ublt/set-up 'git-commit
  (setq git-commit-summary-max-length 70))

(ublt/set-up 'magit-svn)

(provide 'ublt-git)
