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

;; ;;; FIX: Maybe advicing `scroll-up' or`goto-char' is better because
;; ;;; `scroll-margin' affects other scroll functions, and other things
;; ;;; as well. But they are low level, C functions!
;; (defadvice scroll-up-command (after play-nice-with-scroll-margin activate)
;;   "Fix window jumping when the buffer is scrolled while the
;; cursor is above the `scroll-margin' (e.g. when a buffer is first
;; created), caused by `scroll-preserve-screen-position' not taking
;; `scroll-margin' into account."
;;   ;; FIX: Should find a way to calculate the current (line, column)
;;   ;; pair in window coordinate system and use that.
;;   (ublt/avoid-top-scroll-margin))

(defmacro ublt/fix-jumpy-scroll (scroll-fn where)
  (let* ((scroll-fn (ublt/maybe-unquote scroll-fn))
         (avoid-top 'ublt/avoid-top-scroll-margin)
         (avoid-bottom 'ublt/avoid-bottom-scroll-margin)
         (add-top-advice `(advice-add ',scroll-fn :filter-return #',avoid-top))
         (add-bottom-advice `(advice-add ',scroll-fn :filter-return #',avoid-bottom))
         (remove-top-advice `(advice-remove ',scroll-fn #',avoid-top))
         (remove-bottom-advice `(advice-remove ',scroll-fn #',avoid-bottom)))
    (pcase (ublt/maybe-unquote where)
      (:top `(progn ,add-top-advice
                    ,remove-bottom-advice))
      (:bottom `(progn ,add-bottom-advice
                       ,remove-top-advice))
      (:none `(progn ,remove-top-advice
                     ,remove-bottom-advice))
      (:both `(progn ,add-top-advice
                     ,add-bottom-advice))
      (where (error "Invalid where: %S" where)))))

(ublt/fix-jumpy-scroll paredit-move-forward :bottom)
(ublt/fix-jumpy-scroll paredit-move-backward :top)
(ublt/fix-jumpy-scroll magit-diff-visit-file :bottom)
(ublt/fix-jumpy-scroll highlight-symbol-prev :both)
(ublt/fix-jumpy-scroll highlight-symbol-next :both)
(ublt/fix-jumpy-scroll evilmi-jump-items :both)

(setq-default next-error-recenter '(4)) ; middle

(provide 'ublt-fix-jumpy-scroll)
