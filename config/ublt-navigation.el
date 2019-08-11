(require 'ublt-util)
(require 'ublt-fix-jumpy-scroll)

(use-package highlight-symbol)


;;; Extra navigation functions

;;; Better C-a
(defun ublt/back-to-indentation-or-line-beginning ()
  "Go back to indentation, or if already there, to the beginning
of line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))


;;; Better C-x C-x
;;; TODO: Shortcut for setting mark without activating region
;;; TODO: Better shortcut than C-x C-x
;; `http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/'
;;; FIX: The desire behavior should be:
;;; - Don't do anything special if region is active.
;;; - Don't activate region if it's not active.
;;; Basically keep region's activation state
;;; TODO: Maybe it should be an advice?
(defun ublt/exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (let ((active (region-active-p)))
    (exchange-point-and-mark)
    (when (not active)
      (deactivate-mark nil))))


;;; Swap windows
;;; `http://sites.google.com/site/steveyegge2/my-dot-emacs-file'
(defun ublt/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))


(use-package recentf
  :ensure nil :straight nil
  :custom (recentf-max-saved-items 300))


;;; Switch to last buffer
;; Often we want to switch back-n-forth between 2 buffers
(defun ublt/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))



(defun ublt/browse-url-at-point ()
  (interactive)
  (pcase major-mode
    ('markdown-mode (call-interactively #'markdown-follow-thing-at-point))
    ('gfm-mode (call-interactively #'markdown-follow-thing-at-point))
    ('org-mode (org-open-at-point t))
    (_ (call-interactively #'org-open-at-point-global))))


;;; Scrolling settings
(setq redisplay-dont-pause t

      ;; Scroll early when scrolling off-screen
      scroll-margin 5
      ;; Don't center when scrolling off-screen
      scroll-conservatively 10000
      ;; 1-line-at-a-time when scrolling off-screen
      scroll-step 1

      ;; Preserve current line position relative to window
      scroll-preserve-screen-position 'always
      ;; Number of overlapped lines to keep when scrolling by
      ;; screenfull
      next-screen-context-lines 15)


(define-advice move-to-window-line-top-bottom (:around (f &rest args) ublt/keep-column)
  "Try to keep the current column, or `goal-column'."
  (ublt/save-column (apply f args)))


(use-package grep
  :ensure nil :straight nil
  :config
  ;; XXX
  (when (equal grep-find-command '("find . -type f -exec grep -nH -e {} +" . 34))
    (grep-apply-setting 'grep-find-command
                        '("find . -type f -exec grep -nH -e '' {} +" . 35)))
  (when (equal grep-command "grep -nH -e ")
    (grep-apply-setting 'grep-command
                        "grep -nHr --exclude-dir={.bzr,.cvs,.git,.hg,.svn} . -e ")))


;;; Jump around by finding textual anchors.
(use-package avy
  :custom ((avy-background t)
           (avy-all-windows 'all-frames)
           (avy-keys (list             ;Dvorak, stronger finger first, left hand first. TODO: Fewer?
                      ?u ?h            ;index
                      ?i ?d            ;index ←→
                      ?e ?t            ;middle
                      ?o ?n            ;ring
                      ?p ?g            ;index ↑
                      ?k ?m            ;index ↓
                      ?. ?c            ;middle ↑
                      ?, ?r            ;ring ↑
                      ?j ?w            ;middle ↓
                      ?q ?v            ;ring ↓
                      ?a ?s            ;pinkies
                      ))
           (avy-dispatch-alist '((?x . avy-action-kill-move)
                                 (?X . avy-action-kill-stay)
                                 (?T . avy-action-teleport)
                                 (?M . avy-action-mark)
                                 (?C . avy-action-copy)
                                 (?y . avy-action-yank)
                                 (?I . avy-action-ispell)))
           (avy-style 'at-full)))


;;; Make unimportant windows transient
(use-package popwin
  :config (popwin-mode +1))



(use-package ag
  :config (add-to-list 'ag-arguments "--follow"))

(use-package rg
  :custom ((rg-group-result t)
           (rg-hide-command nil)
           (rg-align-position-numbers t)
           (rg-command-line-flags '("--follow"))))

(use-package wgrep)



(defun ublt/narrow-or-widen ()
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (call-interactively #'narrow-to-region)))

(defun ublt/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'ublt-navigation)
