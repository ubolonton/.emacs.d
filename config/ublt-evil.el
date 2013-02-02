(require 'ublt-util)

(defface ublt/evil-insert-tag
  `((t (:inherit font-lock-comment-delimiter-face :slant normal :weight bold)))
  "Evil insert mode indicator face")
(defface ublt/evil-normal-tag
  `((t (:inherit diredp-mode-line-flagged :weight bold)))
  "Evil normal mode indicator face")
(defface ublt/evil-emacs-tag
  `((t (:inherit diredp-mode-line-marked :weight bold)))
  "Evil emacs mode indicator face")
(defface ublt/evil-visual-tag
  `((t (:inherit font-lock-preprocessor-face)))
  "Evil visual mode indicator face")
(setq evil-mode-line-format 'before
      evil-normal-state-tag (propertize "« ☢ »" 'face 'ublt/evil-normal-tag)
      evil-insert-state-tag (propertize "( I )" 'face 'ublt/evil-insert-tag)
      evil-emacs-state-tag  (propertize "( E )" 'face 'ublt/evil-emacs-tag)
      evil-visual-state-tag (propertize "( ∞ )" 'face 'ublt/evil-visual-tag)
      evil-motion-state-cursor '(box "YellowGreen")
      evil-normal-state-cursor '(box "YellowGreen")
      evil-insert-state-cursor '(bar "yellow")
      evil-emacs-state-cursor  '(bar "yellow")
      evil-visual-state-cursor '(box "#F86155")
      )

(require 'evil)

;;; Modes that should be insert state by default
(dolist (mode '(sql-interactive-mode
                magit-log-edit-mode erlang-shell-mode
                dired-mode inferior-moz-mode inferior-octave-mode
                inferior-ess-mode
                grep-mode pylookup-mode))
  (add-to-list 'evil-insert-state-modes mode))

;;; REPL modes: go to prompt on switching to insert mode
(defun ublt/repl-goto-prompt ()
  (when (member major-mode
                '(sql-interactive-mode eshell-mode
                  erlang-shell-mode slime-repl-mode
                  inferior-moz-mode inferior-octave-mode
                  inferior-emacs-lisp-mode))
    (goto-char (point-max))))
(add-hook 'evil-insert-state-entry-hook 'ublt/repl-goto-prompt)

;; (setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
(evil-mode +1)

(ublt/set-up 'surround
  (setq-default surround-pairs-alist
                '((?\( . ("(" . ")"))
                  (?\[ . ("[" . "]"))
                  (?\{ . ("{" . "}"))

                  (?\) . ("( " . " )"))
                  (?\] . ("[ " . " ]"))
                  (?\} . ("{ " . " }"))

                  (?# . ("#{" . "}"))
                  (?b . ("(" . ")"))
                  (?B . ("{" . "}"))
                  (?> . ("<" . ">"))

                  (?\/ . ("/* " . " */"))

                  ;; Single-quoted strings
                  (?\' . ("'" . "'"))

                  ;; Emacs-style quotes
                  (?\` . ("`" . "'"))

                  ;; Python multi-line strings
                  (?d . ("\"\"\"" . "\"\"\""))
                  (?D . ("'''" . "'''"))

                  (?t . surround-read-tag)
                  (?< . surround-read-tag)))
  (global-surround-mode +1))

;;; TODO: inner?
(ublt/set-up 'thingatpt
  (defun ublt/backward-defun (&optional arg)
    (forward-thing 'defun (- arg)))
  (defun ublt/forward-defun (&optional arg)
    (forward-thing 'defun arg))
  (evil-define-text-object evil-a-defun (count &optional beg end type)
    "Select a defun."
    (evil-an-object-range
     count beg end type #'ublt/forward-defun #'ublt/backward-defun))
  (evil-define-text-object evil-inner-defun (count &optional beg end type)
    "Select inner defun."
    (evil-inner-object-range
     count beg end type #'ublt/forward-defun #'ublt/backward-defun))
  (evil-define-text-object evil-a-symbol (count &optional beg end type)
    "Select a symbol."
    (evil-an-object-range
     count beg end type #'forward-symbol))
  (evil-define-text-object evil-inner-symbol (count &optional beg end type)
    "Select a symbol."
    (evil-inner-object-range
     count beg end type #'forward-symbol)))

(provide 'ublt-evil)
