(eval-when-compile
  (require 'cl))
(require 'ublt-util)

;;; TODO: Clean up

;;; FIX: Check why helm flickers when input changes

(require 'helm-config)

(defun ublt/helm-enable-fuzzy (sources-and-classes)
  (dolist (setting sources-and-classes)
    (destructuring-bind (s class) setting
      (let ((source (symbol-value s)))
        (set s (helm-make-source (helm-attr 'name source) class
                 :fuzzy-match t))))))

(ublt/set-up 'helm-match-plugin
  (setq helm-mp-highlight-delay 0.7
        helm-mp-highlight-threshold 4))

(ublt/set-up 'helm-files
  (setq helm-ff-file-name-history-use-recentf t
        ;; helm-ff-auto-update-initial-value t
        helm-ff-transformer-show-only-basename nil
        helm-file-cache-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-ff-search-library-in-sexp t)
  (dolist (pattern '("\\.pyc$" "\\.elc$"))
    (add-to-list 'helm-boring-file-regexp-list pattern)))

(ublt/set-up 'helm-buffers
  (setq helm-buffers-fuzzy-matching t)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers))))

(ublt/set-up 'helm-locate
  (ublt/in '(gnu/linux)
    (setq helm-locate-command "locate %s -e -A --regex %s"
          helm-locate-fuzzy-match nil)))

(ublt/set-up 'helm-bookmark
  (ublt/helm-enable-fuzzy
   '((helm-source-pp-bookmarks helm-source-basic-bookmarks)
     (helm-source-bookmarks helm-source-basic-bookmarks))))

(ublt/set-up 'helm-imenu
  (setq helm-imenu-delimiter " "
        helm-imenu-fuzzy-match t
        ;; This is a misfeature when combined with "use default as
        ;; input", so disable it
        helm-imenu-execute-action-at-once-if-one nil))

(ublt/set-up 'helm-net
  (setq helm-google-suggest-use-curl-p (when (executable-find "curl") t)
        helm-home-url "https://www.google.com"))

(ublt/set-up 'helm-command
  (setq helm-M-x-fuzzy-match t))

(ublt/set-up 'helm-org)
(ublt/set-up 'helm-regexp)
(ublt/set-up 'helm-cmd-t)
(ublt/set-up 'helm-man)

(ublt/set-up 'helm-swoop
  (setq helm-swoop-speed-or-color t
        helm-swoop-use-line-number-face t))

(ublt/set-up 'helm-elisp
  (setq helm-apropos-fuzzy-match t))


(setq helm-maybe-use-default-as-input t
      helm-quick-update t
      helm-split-window-in-side-p 'below
      helm-move-to-line-cycle-in-source t
      ;; Better-looking separator for multi-line sources
      helm-candidate-separator "────────────────────────────────────────────────────────────────────────────────"
      ;; So C-w put the current symbol in helm's prompt
      helm-yank-symbol-first t)

(dolist (source '(helm-source-man-pages))
  (add-to-list 'helm-sources-using-default-as-input source))


(defun ublt/helm-sources ()
  (let ((base '( ;; helm-c-source-ffap-line
                ;; helm-c-source-ffap-guesser
                helm-source-buffers-list
                helm-source-recentf
                helm-source-ido-virtual-buffers
                helm-source-buffer-not-found
                helm-source-files-in-current-dir
                ;; helm-source-bookmarks
                helm-source-pp-bookmarks
                helm-source-file-cache
                helm-source-locate)))
    (if (featurep 'helm-cmd-t)
        ;; FIX
        (condition-case nil
            (append base (list (helm-cmd-t-get-create-source (helm-cmd-t-root-data))))
          (error base))
      base)))

(defun ublt/helm ()
  (interactive)
  (helm-other-buffer (ublt/helm-sources) "*ublt/helm*"))


;;; FIX: Check why there is flickering with helm-occur and
;;; helm-source-buffers-list but not helm-swoop while moving through
;;; the result list. If it can be fixed, enable follow-mode for
;;; helm-source-buffers-list

;;; TODO: Add file/buffer navigation once it's possible to do
;;; dedicated transparent frame for helm
(defun ublt/helm-enable-follow-mode ()
  (dolist (source (list helm-source-occur
                        helm-source-moccur
                        ;; helm-source-org-headline
                        ;; helm-source-pp-bookmarks
                        helm-source-imenu
                        ;; helm-source-buffers-list
                        ))
    (condition-case nil
        (helm-attrset 'follow 1 source)
      (error nil))))
(add-hook 'helm-before-initialize-hook #'ublt/helm-enable-follow-mode)


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
  "Use `variable-pitch' font for helm if it's suitable for
all of the sources."
  (with-current-buffer helm-buffer
    (when (ublt/helm-should-use-variable-pitch? helm-sources)
      (variable-pitch-mode +1))
    (setq line-spacing 0.6)
    ;; (text-scale-increase 1)
    ))
(add-hook 'helm-after-initialize-hook 'ublt/helm-tweak-appearance)
;;; XXX: Big hack!
;;; TODO: Move to ublt-appearance?
(defadvice helm-initialize-overlays (after tweak-appearance activate)
  "Use big `variable-pitch' font to show actions."
  (condition-case nil
      (with-current-buffer helm-action-buffer
        (variable-pitch-mode +1)
        (setq line-spacing 0.6)
        (text-scale-increase 1))
    (error nil)))


(defvar ublt/helm-exit-other-window-p nil)

(defun ublt/helm-maybe-exit-minibuffer-other-window ()
  (interactive)
  ;; We cannot use `let' because the action is executed after exiting
  ;; the minibuffer, not during
  (setq ublt/helm-exit-other-window-p t)
  (call-interactively 'helm-maybe-exit-minibuffer))

;;; For some reason combining them into a single around advice didn't work
(defadvice helm-execute-selection-action-1
    (before maybe-other-window activate)
  (when ublt/helm-exit-other-window-p
    (when (= (count-windows) 1)
      (split-window-horizontally))
    (other-window 1)))

(defadvice helm-execute-selection-action-1
    (around maybe-other-window-cleanup activate)
  (unwind-protect ad-do-it
    (setq ublt/helm-exit-other-window-p nil)))


(helm-mode +1)

(provide 'ublt-helm)
