
;;; Move line/region up/down

;;; `http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs'
(defun ublt/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))
(defun ublt/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
arg lines down."
  (interactive "*p")
  (ublt/move-text-internal arg))
(defun ublt/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
arg lines up."
  (interactive "*p")
  (ublt/move-text-internal (- arg)))

;;; Cycling and extending selection
;; TODO: Make `defun' selection work with non-Lisp

(ublt/set-up 'thing-cmds
  ;; Use `mark-enclosing-sexp' for Lisp languages
  (defun ublt/cycle-prose-region ()
    "Cycle selection for prose (non-programming text)."
    (interactive)
    (let ((thing-types '("word" "sentence" "paragraph" "page")))
      (call-interactively 'cycle-thing-region)))
  (defun ublt/cycle-code-region ()
    "Cycle selection for code of line-based languages."
    (interactive)
    (let ((thing-types '("symbol" "string" "line" "defun")))
      (call-interactively 'cycle-thing-region)))

  (defun ublt/restore-thing-cmds-point ()
    "Restore caret position after upon quiting cycling
selection. Works on `mark-enclosing-sexp'."
    (cond
     ((and (member last-command '(ublt/cycle-code-region ublt/cycle-prose-region))
           cycle-thing-region-point)
      (goto-char cycle-thing-region-point))
     ((member last-command '(mark-enclosing-sexp))
      (goto-char (car mark-ring)))))
  (add-hook 'deactivate-mark-hook 'ublt/restore-thing-cmds-point))

;;; Toggle CUA mode, starting CUA rect if turning on
(defun ublt/toggle-cua-rect (&optional reopen)
  (interactive "P")
  (if (not cua-mode)
      (progn
        (cua-mode +1)
        ;; HACK: calling it directly does not seem to work even though
        ;; the help is shown
        (run-at-time "0 sec" nil 'cua-set-rectangle-mark))
    (progn
      (cua-clear-rectangle-mark)
      (cua-mode -1))))

;;; Copy/cut whole line if no region is selected
;; `http://www.emacswiki.org/emacs/WholeLineOrRegion'
(dolist (command (list 'kill-ring-save 'kill-region
                       'clipboard-kill-ring-save
                       'clipboard-kill-region))
  (put command 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
           (list (line-beginning-position) (line-beginning-position 2))))))
;;; Because they set mark if the region is not active
(defadvice kill-ring-save (after pop-spurious-mark activate)
  (unless (use-region-p)
    (pop-mark)))
(defadvice kill-region (after pop-spurious-mark activate)
  (unless (use-region-p)
    (pop-mark)))

;; Prefer UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;;; TODO: Test this in Windows. If it works, use `ublt/in'
;;(set-clipboard-coding-system 'utf-16le-dos)

;;; Misc

;;; Obfuscate URL at point
(ublt/set-up 'obfusurl)

;; Kill region by DEL/delete (Emacs 24)
(setq delete-active-region 'kill)

;; Typing/deleting deletes selected text if any
(delete-selection-mode 1)

;; Don't understand why it's 8 by default
(setq-default tab-width 4)

;; Teh awesome
(ublt/set-up 'undo-tree
  (global-undo-tree-mode))

;; No autosave/backup
(setq auto-save-default nil
      make-backup-files nil)

;; Automatically update files whose contents were changed
(global-auto-revert-mode 1)
;; (setq auto-revert-check-vc-info t)

(setq skeleton-pair t)

(provide 'ublt-editing)
