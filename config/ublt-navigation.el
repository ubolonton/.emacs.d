(require 'ublt-util)

;;; Extra navigation functions

(eval-when-compile
  (require 'cl))


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
(define-key global-map [remap exchange-point-and-mark]
  'ublt/exchange-point-and-mark-no-activate)


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


(ublt/set-up 'recentf
  (setq recentf-max-saved-items 300))

;;; Switch to last buffer
;; Often we want to switch back-n-forth between 2 buffers
(defun ublt/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))


(defadvice forward-page (after advice-recenter-top activate)
  "Eliminate possible jumpiness (both vertical & horizontal)."
  ;; Make ^L stay at the same place while scrolling by page
  (ublt/recenter-near-top)
  ;; To keep auto-hscroll from kicking in
  (beginning-of-line))



(if (functionp 'org-open-at-point-global)
    (defalias 'ublt/browse-url-at-point 'org-open-at-point-global)
  (defun ublt/browse-url-at-point ()
    (interactive)
    (case major-mode
      ('org-mode (call-interactively 'org-open-at-point))
      (t (call-interactively 'browse-url-at-point)))))


;;; Scrolling settings
(setq redisplay-dont-pause t

      ;; Scroll early when scrolling off-screen
      scroll-margin 4
      ;; Don't center when scrolling off-screen
      scroll-conservatively 10000
      ;; 1-line-at-a-time when scrolling off-screen
      scroll-step 1

      ;; Preserve current line position relative to window
      scroll-preserve-screen-position 1
      ;; Number of overlapped lines to keep when scrolling by
      ;; screenfull
      next-screen-context-lines 5)

;;; TODO: This should be `goto-char's job to check
;;; `scroll-margin' and act appropriately.
(defun ublt/avoid-top-scroll-margin ()
  "Scroll text together with cursor (i.e. preserving `point') out
of window's top part restricted by `scroll-margin' if needed."
  ;; Make sure we are back at the initial point afterward
  (save-excursion
    (let ((initial (point))
          (this-scroll-margin
           (min (max 0 scroll-margin)
                (truncate (/ (window-body-height) 4.0)))))
      ;; Fix window-text, move cursor to border
      (move-to-window-line this-scroll-margin)
      (let ((now (point)))
        (when (> now initial)
          ;; Fix window-cursor, move text down to meet the initial line
          (scroll-down (count-screen-lines initial now)))))))

;;; Note that we don't count the screen lines between the current
;;; point and the window's bottom edge. That calculation would
;;; probably be too involved.
(defun ublt/avoid-bottom-scroll-margin ()
  "Scroll text together with cursor (i.e. preserving `point') out
of window's bottom part restricted by `scroll-margin' if needed."
  ;; Make sure we are back at the initial point afterward
  (save-excursion
    ;; FIX: This is for functions that do funky things with
    ;; buffer/window. Maybe it's better to use `around' advices
    ;; instead?
    (with-current-buffer (window-buffer (selected-window))
      (let ((initial (point))
            (this-scroll-margin
             (min (max 0 (1+ scroll-margin))
                  (truncate (/ (window-body-height) 4.0)))))
        ;; Fix window-text, move cursor to border
        (move-to-window-line (- this-scroll-margin))
        (let ((now (point)))
          (when (< now initial)
            ;; Fix window-cursor, move text up to meet the initial line
            (scroll-up-line (count-screen-lines now initial))))))))

;;; FIX: Maybe advicing `scroll-up' or`goto-char' is better because
;;; `scroll-margin' affects other scroll functions, and other things
;;; as well. But they are low level, C functions!
(defadvice scroll-up-command (after play-nice-with-scroll-margin activate)
  "Fix window jumping when the buffer is scrolled while the
cursor is above the `scroll-margin' (e.g. when a buffer is first
created), caused by `scroll-preserve-screen-position' not taking
`scroll-margin' into account."
  ;; FIX: Should find a way to calculate the current (line, column)
  ;; pair in window coordinate system and use that.
  (ublt/avoid-top-scroll-margin))

;; (defmacro ublt/advice-scroller (f)
;;   `(defadvice ,f (after play-nice-with-scroll-margin activate)
;;      "Fix window jumping. See the same advice for `scroll-up-command'."
;;      (ublt/avoid-top-scroll-margin)))

;;; XXX: Generic for no reason. Duh!
(defmacro ublt/advice-scroller (scroll-fn where)
  (let ((fn (ublt/maybe-unquote scroll-fn)))
    (case (ublt/maybe-unquote where)
      ('top
       `(defadvice ,fn (after avoid-top-scroll-margin activate)
          "Fix window jumping. See the same advice for `scroll-up-command'."
          (ublt/avoid-top-scroll-margin)))
      ('bottom
       `(defadvice ,fn (after avoid-bottom-scroll-margin activate)
          "Fix window jumping. See the same advice for `scroll-up-command'."
          (ublt/avoid-bottom-scroll-margin)))
      ('both
       `(progn
          (ublt/advice-scroller ,fn 'top)
          (ublt/advice-scroller ,fn 'bottom))))))

(dolist
    (wisdom '((top Info-scroll-up)
              (top evil-scroll-page-down)
              (top twittering-scroll-up)
              (top scroll-down-command)
              (bottom end-of-buffer)
              (bottom Info-scroll-down)
              (bottom cider-repl-return)
              (bottom cider-repl-emit-result)
              (bottom cider-repl-emit-output)
              (bottom cider-repl-emit-prompt)
              (both magit-visit-file-item)
              (both highlight-symbol-prev)
              (both highlight-symbol-next)
              (both mwheel-scroll)
              (both ublt/exchange-point-and-mark-no-activate)
              (both exchange-point-and-mark)))
  (destructuring-bind (where fn) wisdom
    (eval `(ublt/advice-scroller ,fn ,where))))

;; ;;; XXX: Does not work. Seems to be ignored most of the time
;; (defvar ublt/is-avoiding-margin nil)
;; (defadvice goto-char (after play-nice-with-scroll-margin activate)
;;   (unless ublt/is-avoiding-margin
;;     (let ((ublt/is-avoiding-margin t))
;;       (ublt/avoid-top-scroll-margin))))


(defadvice move-to-window-line-top-bottom (around keep-column activate)
  "Try to keep the current column, or `goal-column'."
  (ublt/save-column ad-do-it))


(ublt/set-up 'grep
  ;; XXX
  (when (equal grep-find-command '("find . -type f -exec grep -nH -e {} +" . 34))
    (grep-apply-setting 'grep-find-command
                        '("find . -type f -exec grep -nH -e '' {} +" . 35)))
  (when (equal grep-command "grep -nH -e ")
    (grep-apply-setting 'grep-command
                        "grep -nHr --exclude-dir={.bzr,.cvs,.git,.hg,.svn} . -e "))
  )


(ublt/set-up 'ace-jump-mode
  (setq ace-jump-mode-case-sensitive-search nil
        ace-jump-mode-submode-list '(ace-jump-word-mode
                                     ace-jump-line-mode
                                     ace-jump-char-mode)
        ace-jump-word-mode-use-query-char t
        ace-jump-mode-move-keys (nconc (loop for i from ?a to ?z collect i)
                                       (loop for i from ?0 to ?9 collect i)
                                       ;; (loop for i from ?A to ?Z collect i)
                                       )))

(ublt/set-up 'avy
  (setq
   avy-background t
   ;; Dvorak, stronger finger first, left hand first?
   avy-keys '(?u ?h ?i ?d ?e ?t ?o ?n  ?p ?g ?k ?m  ?. ?c  ?, ?r  ?j ?w  ?q ?v  ?a ?s)
   avy-style 'at-full))


(ublt/set-up 'popwin
  (popwin-mode +1))


;;; Whitespace-only diffs are not interesting most of the time
(setq-default ediff-ignore-similar-regions t)


;;; Doc search
(ublt/set-up 'zeal-at-point
  (dolist (pair '((python-mode . "python")
                  (ruby-mode . "chef")
                  (latex-mode . "latex")
                  (sql-mode . "postgresql")
                  (clojurescript-mode . "clojure")))
    (add-to-list 'zeal-at-point-mode-alist pair)))


;;; What's the point of jumping to a section's start but putting it at
;;; the bottom of the window? This somewhat fixes it

(defun ublt/recenter-near-top ()
  (recenter (max 5
                 (/ (window-body-height) 5)
                 scroll-margin)))

(defadvice ido-imenu (after bring-into-view activate)
  (ublt/recenter-near-top))

;;; This can be achieved by setting `find-function-recenter-line'. But
;;; we want it to be a little more dynamic, so use an advice instead.
(defadvice find-function-do-it (after bring-into-view activate)
  (ublt/recenter-near-top))

;;; TODO: This is getting tiresome. There must be a more systematic way.
(defadvice occur-mode-goto-occurrence (after bring-into-view activate)
  (ublt/recenter-near-top))

;;; This is needed because the help buttons use `find-function'
;;; library in a very weird way.
(defadvice help-button-action (around bring-into-view activate)
  ;; Somehow button-get returns nil after the call, so "after" advice
  ;; does not work.
  (let* ((button (ad-get-arg 0))
         (type (button-get button 'type)))
    ad-do-it
    (when (memq type '(help-function-def
                       help-variable-def
                       help-face-deff))
      (ublt/recenter-near-top))))

;;; TODO: Test this extensively
(defmacro ublt/save-window-view (&rest body)
  `(let* ((window (get-buffer-window))
          (pos (window-start window)))
     ,@body
     ;; TODO: Maybe just set this if we are still in the same buffer?
     (set-window-start window pos)))

;;; TODO: Are we sure magit-refresh is the one?
;;; FIX: This doesn't fix window view jumping when changing hunk size
;;; (I'm not sure if it's better or worse though). It does fix the
;;; jumpiness when showing hunk for the first time after a refresh
;;; though.
(defadvice magit-refresh (around bring-into-view activate)
  (ublt/save-window-view ad-do-it))

;;; Try keeping window's view of the buffer the same
(defadvice magit-status (around bring-into-view activate)
  (let* ((buffer (current-buffer))
         (window (get-buffer-window))
         (pos (window-start window)))
    ad-do-it
    (when (eq buffer (current-buffer))
      (set-window-start window pos))))

;;; Seems like the above issues were fixed in magit?
(ad-deactivate 'magit-refresh)
(ad-deactivate 'magit-status)

(defun ublt/narrow-or-widen ()
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (call-interactively #'narrow-to-region)))

(defun ublt/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'ublt-navigation)
