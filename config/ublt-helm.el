(eval-when-compile
  (require 'cl))
(require 'ublt-util)

;;; TODO: Clean up

(require 'helm-config)
(require 'helm-match-plugin)
(require 'helm-regexp)
(require 'helm-buffers)
(require 'helm-files)
(setq helm-mp-highlight-delay 0.7
      helm-mp-highlight-threshold 4
      helm-maybe-use-default-as-input t
      ;; Better-looking separator for multi-line sources
      helm-candidate-separator "────────────────────────────────────────────────────────────────────────────────"
      ;; So C-w put the current symbol in helm's prompt
      helm-yank-symbol-first t)

(defun ublt/helm-sources ()
  (let ((base '( ;; helm-c-source-ffap-line
                ;; helm-c-source-ffap-guesser
                helm-source-buffers-list
                helm-source-ido-virtual-buffers
                ;; helm-source-files-in-current-dir ; use ido
                helm-source-pp-bookmarks
                helm-source-recentf
                helm-source-file-cache
                helm-source-locate
                ;; Additions
                ;; helm-c-source-semantic
                ;; helm-c-source-git-project-files
                ;; helm-c-source-emacs-process
                )))
    (if (featurep 'helm-cmd-t)
        (cons (helm-cmd-t-get-create-source (helm-cmd-t-root-data)) base)
      base)))

(dolist (pattern '("\\.pyc$" "\\.elc$"))
  (add-to-list 'helm-boring-file-regexp-list pattern))

;;; Quote the the search string
(ublt/in '(gnu/linux)
  (setq helm-locate-command "locate %s -r %s"))

(defun ublt/helm ()
  (interactive)
  (helm-other-buffer (ublt/helm-sources) "*ublt/helm*"))

;; TODO: Turn on follow-mode by default for helm-occur

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
                         helm-source-bookmarks
                         helm-source-pp-bookmarks
                         ;; helm-source-recentf
                         ;; helm-source-file-cache
                         ;; helm-source-filelist
                         ;; helm-source-files-in-current-dir+
                         ;; helm-source-files-in-all-dired
                         ;; helm-source-locate
                         helm-source-emacs-process
                         helm-source-org-headline
                         helm-source-emms-streams
                         helm-source-emms-files
                         helm-source-emms-dired
                         ;; helm-source-google-suggest
                         helm-source-apt
                         ;; helm-source-helm-commands
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

;;; FIX: Does not work
;; (defun ublt/helm-exit-minibuffer-other-window ()
;;   (interactive)
;;   ;; (helm-quit-and-execute-action 'other-window)
;;   (with-helm-window
;;     (other-window 1)
;;     (call-interactively 'helm-exit-minibuffer))
;;   )

(ublt/set-up 'helm-cmd-t
  (add-to-list 'ublt/helm-sources 'helm-c-source-cmd-t-caches))

(provide 'ublt-helm)
