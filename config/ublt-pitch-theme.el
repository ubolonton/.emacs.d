;;; -*- lexical-binding: t -*-
;;; This could have been a great idea, if when two themes both set a face's `:inherit', they get
;;; merged. Instead, it's 'last writer wins', so this only works with themes that don't set
;;; `:inherit' for these faces. TODO: Change this into a mode that hooks into
;;; `enable-theme-functions' to ensure the each tracked face has the base face in its inheritance
;;; chain. (Find the set of non-inheriting faces that covers all the tracked faces.)
;;;

(require 'seq)

(defun ublt-pitch-face-remapping-faces (face)
  "Return the face names that FACE gets remapped to."
  (seq-filter (lambda (spec) (and (symbolp spec) (not (equal spec face))))
              (alist-get face face-remapping-alist)))

(defun ublt-pitch--patch-face-remapping-alist (_symbol newval _operation buffer)
  (when (and buffer (not (equal newval face-remapping-alist)))
    (message "Changing in %s" buffer)))

(defvar-local ublt/text-scale-mode-remapping nil
  "Current remapping cookie for base pitch faces.")

(defvar ublt/text-scale-mode-additional-base-faces '(fixed-pitch variable-pitch)
  "Additional faces that should be remapped by `text-scale-mode'.")

(defun ublt-pitch--reconcile-face-remappings (&rest _)
  "Tweak `face-remapping-alist' to reconcile potentially conflicting changes.

There are 2 main sources of face remappings:
1. `buffer-face-mode' remaps `default', typically to `variable-pitch'.
2. `text-scale-mode' remaps `default' to add `:height'.

Our package is mainly about keeping certain faces consistently
fixed-width/variable-width, by making them inherit `varible-pitch' and
`fixed-pitch'. These are not scaled by `text-scale-mode', so this function
scales them. Additionally, when `variable-pitch-mode' is turned on, `default' is
remapped to `variable-pitch', so texts are incorrectly scaled twice. This
function takes that into account, scaling only either."
  (dolist (remapping ublt/text-scale-mode-remapping)
    (face-remap-remove-relative remapping))
  (setq ublt/text-scale-mode-remapping nil)
  (when text-scale-mode-remapping
    ;; We used to get the ratio from the `text-scale-mode-remapping' cookie, but its internal
    ;; structure changed, breaking us. Now we compute ourselves instead.
    (let ((ratio (expt text-scale-mode-step
                       text-scale-mode-amount))
          ;; TODO: Use `buffer-face-mode-remapping'?
          (remapped-default (ublt-pitch-face-remapping-faces 'default)))
      (dolist (face ublt/text-scale-mode-additional-base-faces)
        ;; If `default' is remapped to `variable-pitch' (or another face), then don't remap
        ;; `variable-pitch'. That mostly works. However, faces that inherit from `variable-pitch'
        ;; won't be scaled. TODO FIX XXX: The proper fix would have been for `text-rescale-mode' to
        ;; take `buffer-face-mode' into account, i.e. scaling the underlying face, not `default'.
        ;;
        ;; That said, this mainly affects always-variable-pitch texts in default-variable-pitch
        ;; buffers. It doesn't affect code buffers, so maybe we can live with it, for now.
        (unless (memq face remapped-default)
          (push (face-remap-add-relative face :height ratio)
                ublt/text-scale-mode-remapping)))))
  (force-window-update (current-buffer)))

;;;###autoload
(progn
  (advice-add 'text-scale-mode :after #'ublt-pitch--reconcile-face-remappings)
  (advice-add 'buffer-face-mode :after #'ublt-pitch--reconcile-face-remappings))

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

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ublt-pitch)
