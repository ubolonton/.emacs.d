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
                                  ("~/Programming/Tools" . 1)
                                  ("~/Programming/adatao" . 1)
                                  ("~/Programming/arimo" . 1))
   magit-repository-directories-depth 1

   magit-refresh-status-buffer nil

   magit-log-margin '(t age-abbreviated magit-log-margin-width t 10)


   ;; Other
   magit-log-show-refname-after-summary t
   magit-blame-mode-lighter "🔥")

  ;; XXX: The initialization of this is icky. We use `global-auto-revert-mode' anyway, so disable it here.
  (magit-auto-revert-mode -1)

  (eval-after-load 'diff-hl
    '(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  ;; XXX: `magit-patch-id' calls out to shell, which suffers shell initialization delay. This is
  ;; a temporary workaround. The proper fix is probably writing a C extension or something that
  ;; calls git directly. The mid-term fix is probably calling git directly, not going through shell.
  ;; Either that, or going through shell without initialization.
  (defadvice magit-patch-id (around speed-up activate)
    (let ((shell-file-name "sh"))
      ad-do-it))

  (add-to-list 'load-path "~/Programming/projects/magit-libgit2/elisp")
  (ublt/set-up 'magit-libgit2-x)

  ;; ;; XXX
  ;; (defadvice magit-process (around dont-switch activate)
  ;;   (save-selected-window
  ;;     ad-do-it))
  )

(ublt/set-up 'magit-diff
  ;; gravatars
  (setq magit-revision-show-gravatars '("^Author:     " .  "^Commit:     "))
  (defun ublt/disable-line-spacing ()
    (setq line-spacing 0))
  (add-hook 'magit-revision-mode-hook #'ublt/disable-line-spacing))

(ublt/set-up 'magit-refs
  (setq magit-refs-show-commit-count 'branch))

(ublt/set-up 'magit-popup
  (dolist (switch '((?m "Exclude merges" "--no-merges")
                    (?M "Show only merges" "--merges")
                    (?t "Sort topologically" "--topo-order")
                    (?f "Exclude foreign" "--first-parent")))
    (apply #'magit-define-popup-switch 'magit-log-popup switch)))

(ublt/set-up 'git-commit
  (add-hook 'git-commit-mode-hook
            (lambda () (setq fill-column 80)))
  (setq git-commit-summary-max-length 70))

(ublt/set-up 'magit-svn)

(ublt/set-up 'magit-blame
  (setq magit-blame-time-format "%F"))

(provide 'ublt-git)
