(require 'ublt-util)

;;; TODO: Clean up

(require 'helm-config)
(require 'helm-match-plugin)
(require 'helm-regexp)
(require 'helm-buffers)
(require 'helm-files)
(setq helm-mp-highlight-delay 0.7
      helm-mp-highlight-threshold 4)
(setq ublt/helm-sources
      '(helm-c-source-ffap-line
        helm-c-source-ffap-guesser
        helm-c-source-buffers-list
        ;; helm-c-source-files-in-current-dir+
        helm-c-source-bookmarks
        helm-c-source-recentf
        helm-c-source-file-cache
        helm-c-source-locate)
      ;; Additions
      ;; helm-c-source-semantic
      ;; helm-c-source-git-project-files
      ;; helm-c-source-emacs-process
      )
(ublt/in '(gnu/linux)
  (setq helm-c-locate-command "locate -i -r '%s'"))

(defun ublt/helm ()
  (interactive)
  (helm-other-buffer ublt/helm-sources "*ublt/helm*"))

;; Find occurences of current symbol
;; TODO: turn on follow-mode by default for this
(require 'thingatpt)
(defun ublt/helm-occur-at-point ()
  (interactive)
  (let ((helm-follow-mode t))
    (helm :sources helm-c-source-occur
          :input (thing-at-point 'symbol))))

;;; TODO: Maybe customize faces is better (per-source selection of
;;; fixed-pitch/variable-pitched font)?
;;; XXX: `helm-M-x' does not define a source
(defun ublt/helm-should-use-variable-pitch? (sources)
  "Determine whether all of SOURCES should use variable-pitch
font (fixed-pitch is still preferable)."
  (every (lambda (x)
             (member x '(;; helm-c-source-ffap-line
                         ;; helm-c-source-ffap-guesser
                         ;; helm-c-source-buffers-list
                         helm-c-source-bookmarks
                         ;; helm-c-source-recentf
                         ;; helm-c-source-file-cache
                         ;; helm-c-source-filelist
                         ;; helm-c-source-files-in-current-dir+
                         ;; helm-c-source-files-in-all-dired
                         ;; helm-c-source-locate
                         helm-c-source-emacs-process
                         helm-c-source-org-headline
                         helm-c-source-emms-streams
                         helm-c-source-emms-files
                         helm-c-source-emms-dired
                         helm-c-source-google-suggest
                         helm-c-source-apt
                         ;; helm-c-source-helm-commands
                         )))
         sources))
(defun ublt/helm-tweak-appearance ()
  "Use variable-pitched font for helm if it's suitable for
all of the sources."
  (with-current-buffer helm-buffer
    (when (ublt/helm-should-use-variable-pitch? helm-sources)
      (variable-pitch-mode +1))
    (setq line-spacing 0.6)
    (text-scale-increase 1)))
(add-hook 'helm-after-initialize-hook 'ublt/helm-tweak-appearance)
;;; XXX: Big hack!
;;; TODO: Move to ublt-appearance?
(defadvice helm-initialize-overlays (after tweak-appearance activate)
  (condition-case nil
      (with-current-buffer helm-action-buffer
        (variable-pitch-mode +1)
        (setq line-spacing 0.6)
        (text-scale-increase 1))
    (error nil)))

(provide 'ublt-helm)
