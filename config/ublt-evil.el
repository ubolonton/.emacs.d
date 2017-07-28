(require 'ublt-util)

(require 'sgml-mode)

;;; TODO: tag, comment, sexp

;; FIX evil: evil-inner-tag include the character "<" of the closing
;; tag if content is empty

;; evil & surround's tag support does not work well with idiosyncratic html tags like
;; <meta ...> <br />

(defgroup ubolonton nil ""
  :group 'personal)

(defface ublt/evil-insert-tag
  `((t (:inherit font-lock-comment-delimiter-face :slant normal :weight bold)))
  "Evil insert mode indicator face")
(defface ublt/evil-motion-tag
  `((t (:inherit diredp-mode-line-flagged :weight bold)))
  "Evil motion mode indicator face")
(defface ublt/evil-normal-tag
  `((t (:inherit diredp-mode-line-flagged :weight bold)))
  "Evil normal mode indicator face")
(defface ublt/evil-emacs-tag
  `((t (:inherit diredp-mode-line-marked :weight bold)))
  "Evil emacs mode indicator face")
(defface ublt/evil-visual-tag
  `((t (:inherit font-lock-preprocessor-face)))
  "Evil visual mode indicator face")

(ublt/set-up 'evil
  ;; Visual indicators
  (setq evil-mode-line-format 'before
        evil-normal-state-tag (propertize "( N )" 'face 'ublt/evil-normal-tag)
        evil-motion-state-tag (propertize "( M )" 'face 'ublt/evil-motion-tag)
        evil-insert-state-tag (propertize "( I )" 'face 'ublt/evil-insert-tag)
        evil-emacs-state-tag  (propertize "( E )" 'face 'ublt/evil-emacs-tag)
        evil-visual-state-tag (propertize "( V )" 'face 'ublt/evil-visual-tag)
        evil-motion-state-cursor '(box "Yellow")
        evil-normal-state-cursor '(box "Yellow")
        evil-insert-state-cursor '(bar "YellowGreen")
        evil-emacs-state-cursor  '(bar "YellowGreen")
        evil-visual-state-cursor '(box "#F86155")

        evil-move-cursor-back nil
        evil-want-visual-char-semi-exclusive t)

  ;; Modes that should be insert state by default
  (dolist (mode '(sql-interactive-mode
                  twittering-edit-mode
                  magit-log-edit-mode
                  git-commit-mode
                  erlang-shell-mode
                  dired-mode
                  inferior-octave-mode
                  inferior-ess-mode
                  inferior-lisp-mode
                  inferior-haskell-mode
                  haskell-interactive-mode
                  inf-mongo-mode
                  grep-mode
                  pylookup-mode
                  cider-repl-mode
                  php-boris-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (dolist (mode '(Info-mode
                  cider-browse-ns-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (dolist (mode '(org-mode))
    (add-to-list 'evil-normal-state-modes mode))

  ;; Hmm
  (dolist (mode '(Info-mode))
    (setq evil-motion-state-modes
          (delq mode evil-motion-state-modes)))

  (dolist (mode '(occur-mode))
    (add-to-list 'evil-motion-state-modes mode))

  (eval-after-load 'git-commit
    '(add-hook 'git-commit-setup-hook 'evil-insert-state))

  ;; REPL modes: go to prompt on switching to insert mode
  (defun ublt/repl-goto-prompt ()
    (when (member major-mode
                  '(eshell-mode
                    sql-interactive-mode
                    erlang-shell-mode slime-repl-mode
                    inferior-octave-mode
                    inferior-emacs-lisp-mode
                    inferior-lisp-mode
                    inferior-haskell-mode
                    haskell-interactive-mode
                    cider-repl-mode
                    php-boris-mode))
      (goto-char (point-max))))
  (add-hook 'evil-insert-state-entry-hook 'ublt/repl-goto-prompt)

  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

  (evil-mode +1))


(defun ublt/sgml-get-context (count)
  (save-excursion
    (let ((result))
      (dotimes (i count result)
        (setq result (sgml-get-context))))))


(defun ublt/sgml-get-tag (count)
  (car (last (ublt/sgml-get-context count))))


;; TODO: Extend to line start/end if it's only whitespaces left
;; TODO: Handle all tag types
;; TODO: Using `sgml-skip-tag-forward' and `sgml-skip-tag-backward'
;; might not be a good idea because they are marked as FIXME
;; TODO: Put this in evil proper
;; TODO: Handle edge cases properly
(defun ublt/evil-an-sgml-range (count)
  (let* ((tag (ublt/sgml-get-tag count))
         (start (sgml-tag-start tag))
         (end (sgml-tag-end tag)))
    (case (sgml-tag-type tag)
      ((empty decl cdata) (list start end))
      ('open
       (save-excursion
         (goto-char start)
         (sgml-skip-tag-forward 1)
         (list start (point))))
      ('close
       (save-excursion
         (goto-char end)
         (sgml-skip-tag-backward 1)
         (list (point) end)))
      ('comment (TODO))
      ('pi (TODO))
      ('jsp (TODO))                     ; WAT?
      (t nil))))


(evil-define-text-object evil-a-defun (count &optional beg end type)
  "Select a defun."
  :extend-selection t
  (evil-select-an-object 'evil-defun beg end type count))

;; TODO: This should be body-only
(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select a defun."
  :extend-selection nil
  (evil-select-inner-object 'evil-defun beg end type count))

(evil-define-text-object evil-a-sexp (count &optional beg end type)
  "Select a sexp."
  :extend-selection t
  ;; FIX: Hmm
  (evil-select-inner-object 'sexp beg end type count))

;; TODO: This should not include the parentheses
(evil-define-text-object evil-inner-sexp (count &optional beg end type)
  "Select a sexp."
  :extend-selection nil
  (evil-select-inner-object 'sexp beg end type count))

(evil-define-text-object evil-a-url (count &optional beg end type)
  "Select a url."
  :extend-selection t
  ;; FIX: Hmm
  (evil-select-inner-object 'url beg end type count))

;; TODO: This should be domain only, or excluding the protocol
(evil-define-text-object evil-inner-url (count &optional beg end type)
  "Select a url."
  :extend-selection nil
  (evil-select-inner-object 'url beg end type count))


(ublt/set-up 'evil-surround
  (setq-default evil-surround-pairs-alist
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

                  (?t . evil-surround-read-tag)
                  (?< . evil-surround-read-tag)

                  (?f . evil-surround-function)))
  (global-evil-surround-mode +1))

(ublt/set-up 'evil-visualstar)
(ublt/set-up 'evil-args)
(ublt/set-up 'evil-numbers)
(ublt/set-up 'evil-nerd-commenter)
(ublt/set-up 'evil-matchit
  (global-evil-matchit-mode +1))

(provide 'ublt-evil)
