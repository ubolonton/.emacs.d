;;; -*- lexical-binding: t; coding: utf-8 -*-

;;; This could have been a great idea, if when two themes both set a face's `:inherit', they get
;;; merged. Instead, it's 'last writer wins', so this only works with themes that don't set
;;; `:inherit' for these faces.


(defgroup ublt-pitch nil
  "Pitch assignments for faces."
  :group 'faces)

;; Generally this should include faces at the top of a hierarchy, i.e. not inheriting from any other face.
(defcustom ublt/fixed-pitch-faces
  '(button
    mode-line
    font-lock-builtin-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-preprocessor-face
    font-lock-variable-name-face
    font-lock-warning-face
    font-lock-constant-face
    ;; highlight-numbers-number
    ;; lisp-extra-font-lock-quoted
    ;; lisp-extra-font-lock-quoted-function
    ;; highlight-function-calls-face
    ;; web-mode-html-tag-bracket-face
    ;; nxml-element-prefix
    ;; nxml-tag-delimiter
    ;; nxml-entity-ref-name
    ;; org-special-keyword
    org-table
    org-formula
    ;; org-meta-line ; :inherit font-lock-comment-face
    org-date
    org-block ; :inherit shadow
    org-checkbox ; :inherit bold
    org-time-grid
    ;; org-agenda-current-time ; :inherit org-time-grid
    org-scheduled-previously
    org-agenda-done
    org-agenda-structure
    org-tag
    org-column
    shadow ; XXX: This affects so many faces. Consider it carefully.
    ;; org-code ; :inherit shadow
    ;; org-verbatim ; :inherit shadow
    ;; markdown-pre-face ; :inherit markdown-code-face
    ;; markdown-html-tag-delimiter-face ; :inherit markdown-markup-face
    markdown-markup-face ; :inherit shadow
    magit-hash
    git-commit-comment-action
    ;; Info-quoted ; :inherit fixed-pitch-serif
    help-argument-name ; :inherit italic
    describe-variable-value
    company-tooltip
    ublt/evil-emacs-tag
    ublt/evil-motion-tag
    ublt/evil-normal-tag
    ublt/evil-insert-tag
    ublt/evil-visual-tag
    ;; Not sure what this is and how it should look.
    ;; helm-ff-directory
    )
  "Faces that should always use fixed-width font."
  :type '(repeat face)
  :group 'ublt-pitch)

(defcustom ublt/variable-pitch-faces
  '(font-lock-comment-face
    mode-line-buffer-id
    magit-section-heading
    magit-section-secondary-heading
    magit-log-author
    magit-log-date
    ;; magit-blame-highlight
    magit-blame-heading ; :inherit magit-blame-highlight
    git-commit-comment-file
    ;; org-level-1
    ;; org-level-2
    org-scheduled-today
    org-scheduled
    ;; mode-line
    header-line ; :inherit mode-line
    ;; helm-header ; :inherit header-line
    helm-source-header
    helm-action
    helm-buffer-process
    diredfl-dir-heading
    diredfl-file-name
    ublt/flycheck-message-face)
  "Faces that should always use variable-width font."
  :type '(repeat face)
  :group 'ublt-pitch)

;;;###theme-autoload
(deftheme ublt-pitch "Fixed-width/variable-width pitch assignments for faces.")

(apply #'custom-theme-set-faces 'ublt-pitch
       (append
        (mapcar (lambda (face) `(,face ((((type graphic)) (:inherit (fixed-pitch))))))
                ublt/fixed-pitch-faces)
        (mapcar (lambda (face) `(,face ((((type graphic)) (:inherit (variable-pitch))))))
                ublt/variable-pitch-faces)))

;; ;;;###autoload
;; (when (and (boundp 'custom-theme-load-path) load-file-name)
;;   (add-to-list 'custom-theme-load-path
;;                (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ublt-pitch)
