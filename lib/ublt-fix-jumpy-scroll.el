(require 'nadvice)

(defun ublt/maybe-unquote (s)
  "XXX: This allow using both quoted and unquoted form (since a
macro already has its parameters quoted). Because Lisp syntax is
not regular enough. Uh huh."
  (if (and (eq (type-of s) 'cons)
           (eq (car s) 'quote))
      (cadr s)
    s))

;;; TODO: This should be `goto-char's job to check
;;; `scroll-margin' and act appropriately.
(defun ublt/avoid-top-scroll-margin (&rest _)
  "Scroll text together with cursor (i.e. preserving `point') out
of window's top part restricted by `scroll-margin' if needed."
  ;; Make sure we are back at the initial point afterward
  (save-excursion
    (let ((initial (point))
          (this-scroll-margin
           (min (max 0 (1+ scroll-margin))
                (truncate (/ (window-body-height) 4.0)))))
      ;; Fix window-text, move cursor to border
      (move-to-window-line this-scroll-margin)
      (let ((now (point)))
        (when (> now initial)
          ;; Fix window-cursor, move text down to meet the initial line
          (scroll-down (count-screen-lines initial now)))))))

;;; FIX: This is slow.
(defun ublt/avoid-bottom-scroll-margin (&rest _)
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
             ;; +2 to account for last line being partial, and for variable-pitch face.
             (min (max 0 (+ 2 scroll-margin))
                  (truncate (/ (window-body-height) 4.0)))))
        ;; Fix window-text, move cursor to border
        (move-to-window-line (- this-scroll-margin))
        (let ((now (point)))
          (when (< now initial)
            ;; Fix window-cursor, move text up to meet the initial line
            (scroll-up-line (count-screen-lines now initial))))))))

;;; What's the point of jumping to a section's start but putting it at
;;; the bottom of the window? This somewhat fixes it
(defun ublt/recenter-near-top (&rest _)
  "Scroll the current line to the top part of the current window."
  (recenter (max 5
                 (round (* (window-body-height) 0.25))
                 scroll-margin)))

;; ;;; FIX: Maybe advicing `scroll-up' or`goto-char' is better because
;; ;;; `scroll-margin' affects other scroll functions, and other things
;; ;;; as well. But they are low level, C functions!
;; (define-advice scroll-up-command (:after (&rest _) ublt/play-nice-with-scroll-margin)
;;   "Fix window jumping when the buffer is scrolled while the
;; cursor is above the `scroll-margin' (e.g. when a buffer is first
;; created), caused by `scroll-preserve-screen-position' not taking
;; `scroll-margin' into account."
;;   ;; FIX: Should find a way to calculate the current (line, column)
;;   ;; pair in window coordinate system and use that.
;;   (ublt/avoid-top-scroll-margin))

(defmacro ublt/fix-jumpy-scroll (scroll-fn &rest wheres)
  (declare (indent 1))
  (let ((scroll-fn (ublt/maybe-unquote scroll-fn)))
    `(progn
       (advice-remove ',scroll-fn #'ublt/avoid-top-scroll-margin)
       (advice-remove ',scroll-fn #'ublt/avoid-bottom-scroll-margin)
       (advice-remove ',scroll-fn #'ublt/recenter-near-top)
       ,@(mapcar (lambda (where)
                   (pcase (ublt/maybe-unquote where)
                     (:top `(advice-add ',scroll-fn :after #'ublt/avoid-top-scroll-margin))
                     (:bottom `(advice-add ',scroll-fn :after #'ublt/avoid-bottom-scroll-margin))
                     (:recenter `(advice-add ',scroll-fn :after #'ublt/recenter-near-top))
                     (where (error "Invalid where: %S" where))))
                 wheres))))



(ublt/fix-jumpy-scroll paredit-move-forward :bottom)
(ublt/fix-jumpy-scroll paredit-move-backward :top)

(ublt/fix-jumpy-scroll magit-diff-visit-file :bottom)

(ublt/fix-jumpy-scroll highlight-symbol-prev :top :bottom)
(ublt/fix-jumpy-scroll highlight-symbol-next :top :bottom)

(ublt/fix-jumpy-scroll evilmi-jump-items :top :bottom)



(ublt/fix-jumpy-scroll find-function-do-it :recenter)
(ublt/fix-jumpy-scroll racer--find-file :recenter)
(ublt/fix-jumpy-scroll occur-mode-goto-occurrence :recenter)
(ublt/fix-jumpy-scroll forward-page :recenter)
(ublt/fix-jumpy-scroll magit-todos--goto-item :recenter)

(define-advice forward-page (:after (&rest _) ublt/no-hscroll)
  (beginning-of-line))

;;; This is needed because the help buttons use `find-function'
;;; library in a very weird way.
(define-advice help-button-action (:around (f button &rest args) ublt/bring-into-view)
  ;; Somehow button-get returns nil after the call, so "after" advice
  ;; does not work.
  (let* ((type (button-get button 'type)))
    (apply f button args)
    (when (memq type '(help-function-def
                       help-variable-def
                       help-face-deff))
      (ublt/recenter-near-top))))

(setq-default next-error-recenter '(4)) ; middle

(provide 'ublt-fix-jumpy-scroll)
