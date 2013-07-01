(require 'ublt-util)

;;; Extra navigation functions
(eval-when-compile (require 'cl))

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
;; `http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/'
(defun ublt/exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
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

;;; Switch to last buffer
;; Often we want to switch back-n-forth between 2 buffers
(defun ublt/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;;; Make ^L stay at the same place while scrolling by page
(defadvice forward-page (after advice-recenter-top activate)
  (recenter 1))

(defun ublt/browse-url-at-point ()
  (interactive)
  (case major-mode
    ('org-mode (call-interactively 'org-open-at-point))
    (t (call-interactively 'browse-url-at-point))))

;;; Scrolling settings
(setq redisplay-dont-pause t
      ;; Scroll early (FIX: this cause flickering if the buffer is
      ;; scrolled when the cursor is on the first line (when a buffer
      ;; is first created))
      ;; scroll-margin 0
      ;; Don't center when scrolling off-screen
      scroll-conservatively 10000
      scroll-step 1
      ;; Preserve current line position relative to window
      scroll-preserve-screen-position 1
      ;; Number of overlapped lines to keep when scrolling by page
      next-screen-context-lines 5)

(ublt/set-up 'grep
  ;; XXX
  (when (equal grep-find-command '("find . -type f -exec grep -nH -e {} +" . 34))
    (grep-apply-setting 'grep-find-command
                        '("find . -type f -exec grep -nH -e '' {} +" . 35))))

(ublt/set-up 'ace-jump-mode
  (setq ace-jump-mode-case-sensitive-search nil
        ace-jump-mode-submode-list '(ace-jump-word-mode
                                     ace-jump-line-mode
                                     ace-jump-char-mode)
        ace-jump-word-mode-use-query-char t
        ace-jump-mode-move-keys (nconc (loop for i from ?a to ?z collect i)
                                       (loop for i from ?A to ?Z collect i))))

;;; Whitespace-only diffs are not interesting most of the time
(setq-default ediff-ignore-similar-regions t)


;;; What's the point of jumping to a section's start but putting it at
;;; the bottom of the window? This somewhat fixes it

;;; TODO: Depend on window height in lines
;;; (defun recenter-sensibly ())

(defadvice ido-imenu (after bring-into-view activate)
  (recenter 10))

;;; Somehow button-get returns nil after the call, so "after" advice
;;; does not work
(defadvice help-button-action (around bring-into-view activate)
  (let* ((button (ad-get-arg 0))
         (type (button-get button 'type)))
    ad-do-it
    (when (eq type 'help-function-def)
      (recenter 10))))

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

;; (ad-deactivate 'magit-refresh)

;; (defadvice magit-refresh (around bring-into-view activate)
;;   (let* ((window (get-buffer-window))
;;          (pos (window-start window)))
;;     ad-do-it
;;     (set-window-start window pos)))

;; (ad-unadvise 'magit-refresh)
;; (ad-deactivate 'help-button-action)

(provide 'ublt-navigation)
