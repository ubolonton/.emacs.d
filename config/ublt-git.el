(require 'ublt-util)

(ublt/set-up 'magit
  (setq magit-remote-ref-format 'remote-slash-branch
        ;; Don't show content of untracked directory
        magit-status-verbose-untracked nil
        magit-restore-window-configuration t
        ;; magit status buffer should not be a pop-up (in the sense of
        ;; not volatile or temporary like helm buffer). This is
        ;; important for small screen such as mine.
        magit-status-buffer-switch-function 'switch-to-buffer
        ;; origin/xyz => xyz
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        ;; TODO: Face
        magit-set-upstream-on-push t
        ;; `all' would be nice, but it's too slow for large diffs
        magit-diff-refine-hunk t
        ;; Looks like turning this off doesn't help with refined
        ;; diffs, but still
        magit-diff-use-overlays nil
        )
  ;; XXX: Make magit support customizing switches instead
  ;; (dolist (switch '(("-M" "No merge commits" "--no-merges")
  ;;                   ("-t" "Topo Order" "--topo-order")))
  ;;   (let* ((logging (assq 'logging magit-key-mode-groups))
  ;;          (switches (assq 'switches logging)))
  ;;     (setcdr (last switches)
  ;;             (cons switch nil))))

  ;; XXX
  (defadvice magit-display-process (around dont-switch activate)
    (save-selected-window
      ad-do-it)))

;;; magit 2.1 up
(ublt/set-up 'magit
  (setq
   ;; Show original windows when quitting magit.
   magit-restore-window-configuration t

   ;; magit status buffer should not be a pop-up (in the sense of
   ;; not being volatile or temporary like helm buffer). This is
   ;; important for small screens.
   magit-status-buffer-switch-function 'switch-to-buffer

   ;; `all' would be nice, but it's too slow for large diffs
   magit-diff-refine-hunk t

   ;; Use repo base names in buffer names
   magit-status-buffer-name-format   "*magit: %b*"
   magit-refs-buffer-name-format     "*magit-refs: %b*"
   magit-log-buffer-name-format      "*magit-log: %b*"
   magit-cherry-buffer-name-format   "*magit-cherry: %b*"
   magit-reflog-buffer-name-format   "*magit-reflog: %b*"
   magit-process-buffer-name-format  "*magit-process: %b*"
   magit-stashes-buffer-name-format  "*magit-stashes: %b*"
   magit-stash-buffer-name-format    "*magit-stash: %b*"
   magit-diff-buffer-name-format     "*magit-diff: %b*"
   magit-revision-buffer-name-format "*magit-rev: %b*"

   ;; Git directories
   magit-repository-directories '("~/Programming/projects"
                                  "~/Programming/lib")
   magit-repository-directories-depth 1

   ;; Other
   magit-log-show-refname-after-summary t
   magit-blame-mode-lighter " Bl"
   ;; TODO: Check these. They don't seem to work.
   ;; magit-log-format-graph-function 'magit-log-format-graph-function
   ;; magit-completing-read-function 'helm-completing-read-with-cands-in-buffer
   ))

(ublt/set-up 'git-commit-mode
  (setq git-commit-summary-max-length 70))

(ublt/set-up 'magit-svn)

;; ;;; Messages all the sync git invocations
;; (defadvice magit-git-insert (around show-sync-cmd activate)
;;   (let ((args (ad-get-arg 0)))
;;     (message (concat "git "
;;                      (mapconcat 'identity (append magit-git-standard-options args)
;;                                 " ")))
;;     ad-do-it))

;;; Growl async git invocations (in Ubuntu libnotify is used, which
;;; sucks)
(eval-after-load "todochiku"
  '(defadvice magit-run* (around show-async-cmd activate)
     (let ((cmd-args (ad-get-arg 0))
           (todochiku-timeout 1))
       (todochiku-message
        "Magit"
        (mapconcat 'identity cmd-args " ")
        (todochiku-icon 'social))
       ad-do-it)))

(provide 'ublt-git)
