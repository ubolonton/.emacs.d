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

(provide 'ublt-navigation)
