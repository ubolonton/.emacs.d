(require 'ublt-util)

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

(defun ublt/toggle-letter-case ()
  "Toggle the letter case of current symbol or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”.

See `http://ergoemacs.org/emacs/modernization_upcase-word.html'
"
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'symbol)))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))


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

;;; Maybe this is enough, and the above is not needed anymore?
(ublt/set-up 'whole-line-or-region
  (defun ublt/duplicate-line (prefix)
    (interactive "p")
    (whole-line-or-region-kill-ring-save)
    (call-interactively 'whole-line-or-region-yank)))

;; Prefer UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-coding-system 'utf-8
      locale-coding-system 'utf-8
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(ublt/set-up 'htmlize
  (setq htmlize-html-charset "utf-8"))
;;; TODO: Test this in Windows. If it works, use `ublt/in'
;;(set-clipboard-coding-system 'utf-16le-dos)

;;; Paredit ----------------------------------------------------------
(ublt/set-up 'paredit
  ;; (defun ublt/enable-paredit-mode ()
  ;;   "Enable paredit-mode without checking paren balance."
  ;;   (let ((current-prefix-arg t))
  ;;     (paredit-mode +1)))
  ;; XXX: Seems unclean
  (defadvice paredit-mode (around force activate)
    (if (eq major-mode 'python-mode)
        (let ((current-prefix-arg t))
          ad-do-it)
      ad-do-it))

  ;; Making paredit work with delete-selection-mode
  ;; `http://whattheemacsd.com//setup-paredit.el-03.html'
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-open-round 'delete-selection t)
  (put 'paredit-open-square 'delete-selection t)
  (put 'paredit-open-curly 'delete-selection t)
  (put 'paredit-doublequote 'delete-selection t)
  (put 'paredit-newline 'delete-selection t)

  (defun ublt/paredit-space-for-open? (endp delimiter)
    "Don't insert space for ( [ \" in these modes."
    (not (and (member major-mode '(comint-mode python-mode javascript-mode js-mode js2-mode))
              (member delimiter '(?\( ?\[ ?\")))))
  (add-to-list 'paredit-space-for-delimiter-predicates
               'ublt/paredit-space-for-open?)

  ;; Since I use paredit in many modes, it's better to use its
  ;; comment-dwim only in lisp modes
  (defadvice comment-dwim (around lisp-specific activate)
    (if (member major-mode '(lisp-mode emacs-lisp-mode clojure-mode scheme-mode))
        (call-interactively 'paredit-comment-dwim)
      (message "normal")
      ad-do-it)))

;; auto-complete
(ublt/set-up 'auto-complete-config
  (ac-config-default)
  (ac-flyspell-workaround)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/data/auto-complete/dict")
  (global-auto-complete-mode +1)
  ;; (add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
  (setq-default ac-auto-start nil
                ac-sources '(ac-source-yasnippet
                             ac-source-dictionary
                             ac-source-words-in-buffer
                             ac-source-words-in-same-mode-buffers
                             ac-source-words-in-all-buffer
                             ac-source-abbrev))
  (setq ac-delay 0.5
        ac-auto-show-menu 1
        ac-quick-help-delay 0.8)

  (dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                      sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                      html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                      lisp-mode textile-mode markdown-mode tuareg-mode
                                      nxhtml-mode))
    (add-to-list 'ac-modes mode)))


;;; Yasnippet --------------------------------------------------------

(ublt/set-up 'yasnippet
  (setq yas/root-directory  "~/.emacs.d/data/yasnippet/snippets"
        yas/prompt-functions '(yas/dropdown-prompt ;; yas/ido-prompt yas/no-prompt
                                                   )
        yas/trigger-key nil)
  (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)
  (yas/load-directory yas/root-directory)
  (yas/global-mode +1))

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

(setq skeleton-pair t)

;; Automatically update files whose contents were changed
(global-auto-revert-mode 1)
;; (setq auto-revert-check-vc-info t)


(provide 'ublt-editing)
