;; TODO: Submit this to emacs-starter-kit. Older versions of magit
;; seem to ignore this option. Version 1 uses it as a list. I don't
;; understand why emacs-starter-kit set it as a string.
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
        ;; TODO: Face
        ;; FIX: 'all is currently buggy, throwing "Can't find the
        ;; beginning of the file". When it's fixed use 'all instead.
        magit-diff-refine-hunk t)
  ;; XXX: Make magit support customizing switches instead
  (dolist (switch '(("-M" "No merge commits" "--no-merges")
                    ("-t" "Topo Order" "--topo-order")))
    (let* ((logging (assq 'logging magit-key-mode-groups))
           (switches (assq 'switches logging)))
      (setcdr (last switches)
              (cons switch nil)))))

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
