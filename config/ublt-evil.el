(require 'ublt-util)

;; FIX evil: evil-inner-tag include the character "<" of the closing
;; tag if content is empty

;; evil & surround's tag support does not work well with idiosyncratic html tags like
;; <meta ...> <br />

(defgroup ubolonton nil ""
  :group 'personal)

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

(ublt/set-up 'evil
  ;; Visual indicators
  (setq evil-mode-line-format 'before
        evil-normal-state-tag (propertize "« ☢ »" 'face 'ublt/evil-normal-tag)
        evil-insert-state-tag (propertize "( I )" 'face 'ublt/evil-insert-tag)
        evil-emacs-state-tag  (propertize "( E )" 'face 'ublt/evil-emacs-tag)
        evil-visual-state-tag (propertize "( ∞ )" 'face 'ublt/evil-visual-tag)
        evil-motion-state-cursor `(box ,(face-attribute 'org-done :foreground))
        evil-normal-state-cursor `(box ,(face-attribute 'org-done :foreground))
        evil-insert-state-cursor '(bar "YellowGreen")
        evil-emacs-state-cursor  '(bar "YellowGreen")
        evil-visual-state-cursor `(box ,(face-attribute 'font-lock-keyword-face :foreground)))

  ;; Modes that should be insert state by default
  (dolist (mode '(sql-interactive-mode
                  twittering-edit-mode
                  magit-log-edit-mode
                  git-commit-mode
                  erlang-shell-mode
                  dired-mode
                  inferior-moz-mode
                  inferior-octave-mode
                  inferior-ess-mode
                  inferior-lisp-mode
                  inferior-haskell-mode
                  haskell-interactive-mode
                  grep-mode
                  pylookup-mode
                  cider-repl-mode
                  php-boris-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (dolist (mode '(Info-mode
                  org-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; REPL modes: go to prompt on switching to insert mode
  (defun ublt/repl-goto-prompt ()
    (when (member major-mode
                  '(eshell-mode
                    sql-interactive-mode
                    erlang-shell-mode slime-repl-mode
                    inferior-moz-mode
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


;; (defun ublt/evil-inner-sgml-range (count)
;;   (let* ((tag (ublt/sgml-get-tag count))
;;          (start (sgml-tag-start tag))
;;          (end (sgml-tag-end tag)))
;;     (case (sgml-tag-type tag)
;;       ((empty decl cdata) nil)
;;       ('open
;;        (save-excursion
;;          (goto-char end)
;;          (let ((inner-start (point)))
;;            (goto-char start)
;;            (sgml-skip-tag-forward 1)
;;            (let ((close (ublt/sgml-get-tag 1)))
;;              (list inner-start (sgml-tag-start close))))))
;;       ('close
;;        (save-excursion
;;          (goto-char start)
;;          (let ((inner-end (point)))
;;            (goto-char end)
;;            (sgml-skip-tag-backward 1)
;;            (let ((open (ublt/sgml-get-tag 1)))
;;              (list (sgml-tag-end open) inner-end)))))
;;       ('comment (TODO))
;;       ('pi (TODO))
;;       ('jsp (TODO))                     ; WAT?
;;       (t nil))))


;; TODO: Support BEG END TYPE
(ublt/set-up 'evil
  (evil-define-text-object evil-an-sgml-tag (count &optional beg end type)
    "Select a sgml tag block."
    :extend-selection nil
    (ublt/evil-an-sgml-range count)))


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

  ;; (evil-define-text-object evil-inner-defun (count &optional beg end type)
  ;;   "Select inner defun."
  ;;   (evil-inner-object-range
  ;;    count beg end type #'ublt/forward-defun
  ;;   #'ublt/backward-defun))

  (evil-define-text-object evil-a-symbol (count &optional beg end type)
    "Select a symbol."
    (evil-an-object-range
     count beg end type #'forward-symbol))

  ;; (evil-define-text-object evil-inner-symbol (count &optional beg end type)
  ;;   "Select a symbol."
  ;;   (evil-inner-object-range
  ;;    count beg end type #'forward-symbol))
  )

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

(provide 'ublt-evil)
