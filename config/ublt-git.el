;; TODO: Submit this to emacs-starter-kit. Older versions of magit
;; seem to ignore this option. Version 1 uses it as a list. I don't
;; understand why emacs-starter-kit set it as a string.
(require 'ublt-util)

(require 'magit)
(require 'magit-svn)
(setq magit-diff-options '("-w"))
;; magit status buffer should not be a pop-up (in the sense of not
;; volatile or temporary like helm buffer). This is important for
;; small screen such as mine.
(setq magit-status-buffer-switch-function 'switch-to-buffer)

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

;; git-emacs (some complementary features)
;; (require 'git-emacs)

(provide 'ublt-git)
