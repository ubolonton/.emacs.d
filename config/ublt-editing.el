(require 'ublt-util)


;;; Move line/region up/down
(use-package move-text)



(defun ublt/unfill-paragraph ()
  "Does the inverse of `fill-paragraph', by calling it with
`fill-column' set to a large number."
  (interactive)
  (let ((fill-column (point-max)))
    (call-interactively 'fill-paragraph)))


;;; Cycling and extending selection
(use-package expand-region)


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
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "title case"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2)
      (put this-command 'state "title case")
      (ublt/status-message "Title Case"))
     ((string= "title case" (get this-command 'state))
      (upcase-region p1 p2)
      (put this-command 'state "all caps")
      (ublt/status-message "ALL CAPS"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2)
      (put this-command 'state "all lower")
      (ublt/status-message "all lower")))))


;;; Copy/cut/duplicate whole line if no region is selected
(use-package whole-line-or-region
  :config (defun ublt/duplicate-line (prefix)
            (interactive "p")
            ;; FIX: This looks dirty
            (call-interactively 'whole-line-or-region-kill-ring-save)
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
(define-coding-system-alias 'UTF-8 'utf-8)
(use-package htmlize
  :custom (htmlize-html-charset "utf-8"))
;;; TODO: Test this in Windows. If it works, use `ublt/in'
;;(set-clipboard-coding-system 'utf-16le-dos)


;;; Paredit ----------------------------------------------------------
;;; Structural editing with ()[]{}.
(use-package paredit
  :config
  ;; (defun ublt/enable-paredit-mode ()
  ;;   "Enable paredit-mode without checking paren balance."
  ;;   (let ((current-prefix-arg t))
  ;;     (paredit-mode +1)))
  ;; XXX: Seems unclean
  (defadvice paredit-mode (around force activate)
    "Force turning on for `python-mode'."
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
    (not (and (member major-mode '(comint-mode python-mode javascript-mode js-mode js2-mode web-mode))
              (member delimiter '(?\( ?\[ ?\")))))
  (add-to-list 'paredit-space-for-delimiter-predicates
               'ublt/paredit-space-for-open?)

  ;; Since I use paredit in many modes, it's better to use its
  ;; comment-dwim only in lisp modes
  (defadvice comment-dwim (around lisp-specific activate)
    "Use `paredit-comment-dwim', but only in lisp code."
    (if (member major-mode '(lisp-mode emacs-lisp-mode clojure-mode scheme-mode))
        (call-interactively 'paredit-comment-dwim)
      ad-do-it))

  ;; FIX: This is too ad-hoc
  (defadvice paredit-backward (before fix-evil-off-by-1 activate)
    (when (member evil-state '(motion normal))
      (forward-char)))
  (defadvice paredit-forward (after fix-evil-off-by-1 activate)
    (when (member evil-state '(motion normal))
      (backward-char))))


;;; Automatic completion

(use-package company
  :custom ((company-idle-delay 0.3)
           (company-minimum-prefix-length 2)
           (company-require-match nil)
           (company-tooltip-flip-when-above t)
           (company-frontends '(company-pseudo-tooltip-frontend
                                company-preview-frontend))
           (company-tooltip-align-annotations t)
           (company-selection-wrap-around t)
           (company-transformers '(company-sort-by-occurrence)))
  :config (global-company-mode +1))

(use-package company-box
  :hook (company-mode . company-box-mode))


;;; Yasnippet --------------------------------------------------------

(use-package yasnippet
  :custom (yas-choose-keys-first t)
  ;; Don't, use a dedicate key binding for yas
  ;; (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/data/yasnippet/snippets")
  (yas-global-mode +1))

(use-package yasnippet-snippets)


;;; Misc

;;; Recompile Emacs Lisp on-save.
(use-package auto-compile)

;;; TODO: Use this
(defun ublt/remove-hard-wrap ()
;;; TODO: Unfill paragraphs one-by-one, skipping those that should not
;;; be unfilled
  (interactive)
  (read-only-mode -1)
  (mark-whole-buffer)
  (ublt/unfill-paragraph)
  (read-only-mode +1)
  (visual-line-mode +1))
;; (add-hook 'help-mode-hook #'ublt/remove-hard-wrap)
;;; Doesn't work well with summary node :(
;; (defadvice Info-goto-node (after remove-hard-wrap activate)
;;   (ublt/remove-hard-wrap))

(defun ublt/cleanup-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(setq
 ;; Kill region by DEL/delete (Emacs 24)
 delete-active-region 'kill

 ;; No autosave/backup
 auto-save-default nil
 make-backup-files nil

 ;; Save clipboard's content into kill-ring before replacing it
 save-interprogram-paste-before-kill t

 ;; yank-pop-change-selection t
 kill-do-not-save-duplicates t)

;; Typing/deleting deletes selected text if any
(delete-selection-mode +1)

;; Don't understand why it's 8 by default
(setq-default tab-width 4)

;; Teh awesome
(use-package undo-tree
  :config (global-undo-tree-mode +1))

;; Automatically update files whose contents were changed
(global-auto-revert-mode +1)
;; (setq auto-revert-check-vc-info t)

;; emacs-mac-app https://bitbucket.org/mituharu/emacs-mac
(ublt/in '(darwin)
  (setq
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-pass-command-to-system nil))

(provide 'ublt-editing)
