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

(use-package evil
  ;; Visual indicators
  :custom ((evil-mode-line-format 'before)
           (evil-normal-state-tag (propertize "( N )" 'face 'ublt/evil-normal-tag))
           (evil-motion-state-tag (propertize "( M )" 'face 'ublt/evil-motion-tag))
           (evil-insert-state-tag (propertize "( I )" 'face 'ublt/evil-insert-tag))
           (evil-emacs-state-tag  (propertize "( E )" 'face 'ublt/evil-emacs-tag))
           (evil-visual-state-tag (propertize "( V )" 'face 'ublt/evil-visual-tag))
           (evil-move-cursor-back nil)
           (evil-want-visual-char-semi-exclusive t))

  :config

  ;; XXX: Seems like these must be set after `evil' is loaded to take effect. FIX:
  ;; Make `evil-define-state' respect the existing values instead.
  (setq-default evil-motion-state-cursor '(box "Yellow")
                evil-normal-state-cursor '(box "Yellow")
                evil-insert-state-cursor '(bar "YellowGreen")
                evil-emacs-state-cursor  '(bar "YellowGreen")
                evil-visual-state-cursor '(box "#F86155"))

  ;; Modes that should be insert state by default
  (dolist (mode '(;; REPL
                  cider-repl-mode
                  erlang-shell-mode
                  haskell-interactive-mode
                  inf-mongo-mode
                  inferior-ess-mode
                  inferior-haskell-mode
                  inferior-lisp-mode
                  inferior-octave-mode
                  php-boris-mode
                  sql-interactive-mode
                  ;; Prose composing UI
                  git-commit-mode
                  magit-log-edit-mode
                  twittering-edit-mode
                  ;; Other
                  helm-major-mode
                  dired-mode))
    (add-to-list 'evil-insert-state-modes mode))
  (add-hook 'org-capture-mode-hook #'evil-insert-state)

  (dolist (mode '(Info-mode
                  helpful-mode
                  grep-mode
                  rg-mode
                  cider-browse-ns-mode
                  cider-repl-history-mode
                  cider-classpath-mode
                  cider-stacktrace-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (dolist (mode '(org-mode))
    (add-to-list 'evil-normal-state-modes mode))

  ;; Hmm
  (dolist (mode '(Info-mode))
    (setq evil-motion-state-modes
          (delq mode evil-motion-state-modes)))

  (dolist (mode '(occur-mode))
    (add-to-list 'evil-motion-state-modes mode))

  (use-package git-commit
    :hook (git-commit-setup . evil-insert-state))

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

  (with-eval-after-load 'paredit
    ;; FIX: This is too ad-hoc
    (define-advice paredit-backward (:before (&rest _) ublt/fix-evil-off-by-1)
      (when (member evil-state '(motion normal))
        (forward-char)))
    (define-advice paredit-forward (:after (&rest _) ublt/fix-evil-off-by-1)
      (when (member evil-state '(motion normal))
        (backward-char))))

  (evil-mode +1))

(use-package all-the-icons
  :config
  ;; We put this here instead of using `:after' since we want `all-the-icons' to be extra stuff, not
  ;; a hard dependency for using `evil'.
  (use-package evil
    :custom ((evil-insert-state-tag (all-the-icons-octicon "pencil"
                                                           :v-adjust 0.01
                                                           :height 1.09
                                                           :face 'all-the-icons-dred))
             (evil-normal-state-tag (all-the-icons-octicon "rocket"
                                                           :v-adjust 0.05
                                                           :face 'all-the-icons-dred))
             (evil-motion-state-tag (all-the-icons-octicon "rocket"
                                                           :v-adjust 0.05
                                                           :face 'all-the-icons-lyellow))
             (evil-visual-state-tag (all-the-icons-octicon "eye"
                                                           :v-adjust 0.05
                                                           :face 'all-the-icons-dred))
             (evil-emacs-state-tag (all-the-icons-fileicon "elisp"
                                                           :v-adjust -0.15
                                                           :face 'all-the-icons-dred)))))

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
    (pcase (sgml-tag-type tag)
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

(use-package evil-surround
  :custom (evil-surround-pairs-alist
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
  :config (global-evil-surround-mode +1))

(use-package evil-visualstar)
(use-package evil-args)
(use-package evil-numbers)
(use-package evil-nerd-commenter)
(use-package evil-matchit
  :config (global-evil-matchit-mode +1))

(provide 'ublt-evil)
