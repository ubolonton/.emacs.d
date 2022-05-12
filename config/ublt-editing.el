;;; -*- lexical-binding: t; coding: utf-8 -*-

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
  :config
  (defun ublt/duplicate-line (_)
    (interactive "p")
    ;; FIX: This looks dirty
    (call-interactively 'whole-line-or-region-kill-ring-save)
    (call-interactively 'yank))
  (whole-line-or-region-global-mode))


;; Prefer UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
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
  ;; Making paredit work with delete-selection-mode
  ;; `http://whattheemacsd.com//setup-paredit.el-03.html'
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-open-round 'delete-selection t)
  (put 'paredit-open-square 'delete-selection t)
  (put 'paredit-open-curly 'delete-selection t)
  (put 'paredit-doublequote 'delete-selection t)
  (put 'paredit-newline 'delete-selection t)

  (defun ublt/paredit-space-for-open? (_endp delimiter)
    "Don't insert space for ( [ \" in these modes."
    (not (and (member major-mode '(comint-mode python-mode javascript-mode js-mode js2-mode web-mode))
              (member delimiter '(?\( ?\[ ?\")))))
  (add-to-list 'paredit-space-for-delimiter-predicates
               'ublt/paredit-space-for-open?)

  ;; Since I use paredit in many modes, it's better to use its
  ;; comment-dwim only in lisp modes
  (define-advice comment-dwim (:around (f &rest args) ublt/use-paredit-maybe)
    "Use `paredit-comment-dwim', but only in lisp code."
    (if (member major-mode '(lisp-mode emacs-lisp-mode clojure-mode scheme-mode))
        (call-interactively 'paredit-comment-dwim)
      (apply f args))))


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

(use-package company-posframe
  :after company
  :custom ((company-posframe-font "Fantasque Sans Mono")
           (company-posframe-quickhelp-delay nil))
  :config (company-posframe-mode +1))


;;; Yasnippet --------------------------------------------------------

(ublt/with-defer
  (use-package yasnippet
    :custom (yas-choose-keys-first t)
    ;; Don't, use a dedicate key binding for yas
    ;; (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    :config
    (add-to-list 'yas-snippet-dirs (ublt/init-rel-path "data/yasnippet/snippets"))
    (yas-global-mode +1))

  (use-package yasnippet-snippets))


;;; ------------------------------------------------------------------
;;; pullover
(use-package pullover
  :when (memq window-system '(mac ns))
  :ensure nil :straight nil
  :demand t
  :load-path "~/Programming/projects/pullover/"
  :hook (pullover-mode . (lambda () (call-interactively 'evil-insert)))
  :bind (:map pullover-mode-map
              ([remap ublt/kill-this-buffer] . pullover-cancel)))

(use-package markdown-mode
  :custom (pullover-major-mode 'gfm-mode))


;;; ------------------------------------------------------------------
(use-package edit-indirect)


;;; ------------------------------------------------------------------
;;; Misc

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

(use-package undo-tree
  :custom ((undo-tree-visualizer-timestamps t)
           (undo-tree-visualizer-diff t)
           (undo-tree-auto-save-history nil "Eliminate ~undo-tree~ files."))
  :config (global-undo-tree-mode +1))

;; Automatically update files whose contents were changed
(use-package autorevert
  :custom ((auto-revert-avoid-polling t "Save power"))
  :config (global-auto-revert-mode +1))
;; (setq auto-revert-check-vc-info t)

(provide 'ublt-editing)
