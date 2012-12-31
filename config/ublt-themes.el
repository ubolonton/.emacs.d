;; Ubolonton's personal dark color theme for Emacs.
;;
;; A mix of Blackboard and Succulent themes.
;;
;; MIT License Copyright (c) 2012 Nguyễn Tuấn Anh <ubolonton at gmail dot com>
;; Credits:
;; - TextMate theme Succulent
;; - TextMate theme Blackboard
;; - Emacs port of Blackboard theme by JD Huntington
;; All patches welcome

(require 'color-theme)


;;; TODO: Use Emacs 24's deftheme
;;; TODO: Give better names e.g. +1 +2 -1 -2 or ++ --
;;; TODO: Font-lock-type-face is too bright
;;; TODO: Background is a bit dull
;;; TODO: Info color distribution is not appropriate
;;; TODO: More grayscale colors for stuffs like (setq show-paren-style
;;; 'expression)
;;; TODO: +/- names sometimes implies saturation, sometimes implies
;;; blackness
;;; TODO: More saturation level

;;; XXX: color-theme's old versions' bug
;; (defun color-theme-alist (plist)
;;   (cond
;;    ((equal plist nil) nil)
;;    ((consp (car plist)) plist)
;;    ((not (symbolp (car plist)))
;;     (error "Wrong type argument: plist, %S" plist))
;;    (t
;;     (plist-to-alist plist))))

;;; This idea comes from solarized theme
(defvar ublt/colors
  '(; Name       RGB       256-color
    (bg          "#0C1320" "#121212")
    (bg+1        "#131A27" "#1c1c1c")
    (bg+2        "#1F2633" "#262626")
    (bg+3        "#2C3340" "#303030")

    (fg-3        "#505764" "#4e4e4e")
    (fg-2        "#626976" "#626262")
    (fg-1        "#747B88" "#767676")
    (fg          "#858C99" "#8a8a8a")
    (fg+1        "#B8C1C9" "#a8a8a8")

    ;; TODO: "Semantic" names

    ;; Warning, error, failure
    (red         "#FF0000" "#FF0000")                 ; red
    ;; Subtractive
    (red-1       "#CD1111" "#AF0000")
    ;; mode-line special
    (red-2       "#8B0000" "#870000")   ; Dried blood
    ;; Error background
    (red-3       "#8B1A1A" "#870000")
    ;; Power, todo, commit, prompt
    (red-4       "#F86155" "#FF5F5F")   ; Salmon
    ;; Raw, write, exec
    (red-5       "#D98D54" "#D7875F")   ; Skin

    ;; Grouping
    (orange+1    "#FF4500" "#FF5F00")           ; OrangeRed
    ;; Parameter
    (orange      "#FF8C00" "#FF8700")          ; DarkOrange

    ;; Header, variable
    (gold+1      "#B8860B" "#AF8700")       ; DarkGoldenrod
    ;; Header, unique
    (gold-1      "#EEDD82" "#D7D787")      ; LightGoldenrod

    ;; Cursor, debug
    (yellow      "#FFFF00" "#FFFF00")              ; yellow
    ;; Status, mild attention
    (yellow-1    "#9ACD32" "#87D75F")         ; YellowGreen

    ;; Function, dir, essence, into-your-eyes
    (green       "#00DD00" "#87FF87")              ; green2
    ;; String, name, output, additive
    (green-1     "#228B22" "#00875F")         ; ForestGreen
    ;; Doc string, weird thing
    (green-2     "#2E8B57" "#00875F")            ; SeaGreen
    ;; Done, table, input
    (green-3     "#90EE90" "#87FF87")          ; LightGreen

    ;; Type, header
    (cyan+1      "#00FA9A" "#00FF87")   ; MediumSpringGreen
    ;; Prompt, link, result, suffix
    (cyan        "#00CDCD" "#00D7D7")               ; cyan3
    ;; Whitespace
    (cyan-1      "#66CDAA" "#5FD7AF")         ; aquamarine3
    ;; Mode line, link
    (cyan-2      "#89A1F3" "#87AFFF")

    ;; Bult-in, constant
    (blue        "#0084CF" "#0087AF")
    ;; Reference, file, buffer
    (blue-1      "#6A5ACD" "#5F5FD7")
    ;; Secondary selection
    (blue-2      "#223360" "#005F87")

    ;; Comment, ignored, visited
    (purple      "#805DBB" "#875F87")
    ))

;;;###autoload
(defun color-theme-ubolonton-dark ()
  "Mix of Blackboard & Succulent color themes.

  Created 2010-05-29 by Nguyễn Tuấn Anh. Works in GUI and
  256-color terminals, but the latter is not recommended."
  (interactive)
  (flet ((find-color (name)
                     (nth (if window-system 1 2)
                          (assoc name ublt/colors))))
    (let* ((variable-pitch-font (face-attribute 'variable-pitch :font))
           (bg          (find-color 'bg))
           (bg+1        (find-color 'bg+1))
           (bg+2        (find-color 'bg+2))
           (bg+3        (find-color 'bg+3))
           (fg          (find-color 'fg))
           (fg+1        (find-color 'fg+1))
           (fg-1        (find-color 'fg-1))
           (fg-2        (find-color 'fg-2))
           (fg-3        (find-color 'fg-3))
           (red         (find-color 'red))
           (red-1       (find-color 'red-1))
           (red-2       (find-color 'red-2))
           (red-3       (find-color 'red-3))
           (red-4       (find-color 'red-4))
           (red-5       (find-color 'red-5))
           (orange      (find-color 'orange))
           (orange+1    (find-color 'orange+1))
           (gold-1      (find-color 'gold-1))
           (gold+1      (find-color 'gold+1))
           (yellow      (find-color 'yellow))
           (yellow-1    (find-color 'yellow-1))
           (green       (find-color 'green))
           (green-1     (find-color 'green-1))
           (green-2     (find-color 'green-2))
           (green-3     (find-color 'green-3))
           (cyan        (find-color 'cyan))
           (cyan+1      (find-color 'cyan+1))
           (cyan-1      (find-color 'cyan-1))
           (cyan-2      (find-color 'cyan-2))
           (blue        (find-color 'blue))
           (blue-1      (find-color 'blue-1))
           (blue-2      (find-color 'blue-2))
           (purple      (find-color 'purple))

           (warning      `(:foreground ,red))
           (error-hl     `(:background ,red-3))
           (power        `(:foreground ,red-4))
           (raw          `(:foreground ,red-5))

           (minus        `(:foreground ,red-1))
           (plus         `(:foreground ,green-1))
           (context      `(:foreground ,fg-3))
           (dimmed       `(:foreground ,bg+3))

           (param        `(:foreground ,orange))
           (mutable      `(:foreground ,gold+1))
           (exception-hl `(:background ,yellow-1))

           (essence      `(:foreground ,green))
           (more         `(:foreground ,green))
           (structured   `(:foreground ,green))

           (name         `(:foreground ,green-1))
           (doc          `(:foreground ,green-2))

           (type         `(:foreground ,cyan+1))
           (portal       `(:foreground ,cyan-2))
           (teleport     `(:foreground ,cyan))
           (prompt       `(:foreground ,cyan))

           (built-in     `(:foreground ,blue))

           (reference    `(:foreground ,blue-1))

           (dimmed-hl    `(:background ,bg+1))
           (normal-hl    `(:background ,bg+2 :weight normal))
           (special-hl   `(:background ,blue-2))

           (note         `(:foreground ,purple)) ; meta?

           (status       `(:background ,cyan-2))
           )
      (color-theme-install
       `(color-theme-ubolonton-dark

         ;; Frame parameters
         ((background-color . ,bg)
          (foreground-color . ,fg)
          (background-mode . dark)
          (border-color . ,bg)
          (cursor-color . ,yellow))

         ;; Variables
         ((ibus-cursor-color . (,green ,yellow ,yellow)))

         (default ((t (:background ,bg :foreground ,fg))))
         (variable-pitch ((t (:background ,bg :foreground ,fg))))
         (border-glyph ((t (nil))))     ; What's this?
         (buffers-tab ((t (:background ,bg :foreground ,fg)))) ; What's this?
         (shadow ((t (:foreground ,bg+3))))

         ;; Code highlighting
         (font-lock-builtin-face
          ((t (:foreground ,blue))))
         (font-lock-comment-face
          ((t (:foreground ,purple :italic t))))
         (font-lock-comment-delimiter-face
          ((t (:inherit font-lock-comment-face :foreground ,bg+2))))
         (font-lock-doc-string-face
          ((t ,doc)))
         (font-lock-function-name-face
          ((t ,essence)))
         (font-lock-keyword-face
          ((t ,power)))
         (font-lock-reference-face
          ((t (:foreground ,blue-1))))  ; What's this?
         (font-lock-regexp-grouping-backslash
          ((t (:foreground ,bg+1))))
         (font-lock-regexp-grouping-construct
          ((t (:foreground ,orange+1))))
         (font-lock-string-face
          ((t (:foreground ,green-1))))
         (font-lock-doc-face
          ((t (:inherit font-lock-string-face))))
         (font-lock-type-face
          ((t (:foreground ,cyan+1))))
         (font-lock-preprocessor-face
          ((t ,raw)))
         (font-lock-variable-name-face
          ((t ,mutable)))
         (font-lock-warning-face
          ((t ,warning)))
         ;; TODO: Different shade
         (font-lock-constant-face
          ((t (:foreground ,blue))))

         (js2-function-param-face
          ((t ,param)))
         (js2-jsdoc-type-face
          ((t (:inherit font-lock-type-face))))
         (js2-jsdoc-tag-face
          ((t (:inherit font-lock-builtin-face))))

         ;; Misc
         (isearch-fail
          ((t (,@warning ,@exception-hl))))
         (gui-element
          ((t (:background ,fg+1 :foreground ,bg)))) ; What's this?
         (text-cursor
          ((t (:background ,yellow :foreground ,bg)))) ; What's this?
         (minibuffer-prompt
          ((t (,@prompt :bold t ))))
         (left-margin ((t (nil))))
         (toolbar ((t (nil))))
         (fringe ((t (,@context ,@dimmed-hl))))
         (link ((t ,portal)))
         (match ((t (:background ,bg+3))))
         (escape-glyph
          ((t (:foreground ,cyan :bold t))))

         ;; Highlighting
         (region
          ((t ,normal-hl)))             ; selection
         (secondary-selection
          ((t ,special-hl)))
         (hl-line
          ((t ,dimmed-hl)))                ; line highlighting
         (hl-sexp-face
          ((t (:inherit hl-line))))
         (highlight
          ((t ,normal-hl)))             ; highlighting
         (highline-face
          ((t (:background ,green-2)))) ; What's this?
         (zmacs-region
          ((t (:inherit region))))      ; What's this?
         (highlight-changes
          ((t (:background ,bg+3))))
         (highlight-symbol-face
          ((t (:background ,bg+3))))
         (pp^L-highlight
          ((t (:foreground ,purple :bold t))))
         (lazy-highlight
          ((t ,normal-hl)))

         (eval-sexp-fu-flash
          ((t ,normal-hl)))

         ;; Mode line
         (mode-line
          ((t (,@status :foreground ,bg
                        :box (:line-width 1 :color ,cyan-2)
                        :font ,variable-pitch-font
                        :height 0.9
                        ;; :height 0.8
                        ))))
         (mode-line-inactive
          ((t (:inherit mode-line :background ,bg+3 :foreground ,fg+1
                        :box (:color ,bg+3)))))
         (mode-line-buffer-id
          ((t (:bold t :height 1.2))))
         (mode-line-highlight
          ((t (:inherit mode-line))))
         (which-func
          ((t (:foreground ,red-2 :height 1.2 :bold t))))

         ;; dired, dired+
         (diredp-file-name
          ((t (:inherit default))))
         (diredp-dir-heading
          ((t (:foreground ,gold-1 :bold t))))
         (dired-symlink
          ((t ,teleport)))
         (diredp-symlink
          ((t ,teleport)))
         (diredp-no-priv
          ((t (:foreground ,bg+2 ,@dimmed-hl))))
         (diredp-read-priv
          ((t (,@more ,@dimmed-hl))))
         (diredp-write-priv
          ((t (,@raw ,@dimmed-hl))))
         (diredp-exec-priv
          ((t (,@power ,@dimmed-hl))))
         (diredp-dir-priv
          ((t ,more)))
         (diredp-number
          ((t (:foreground ,green-2))))
         (diredp-flag-mark-line
          ((t ,special-hl)))
         (diredp-deletion
          ((t (:foreground ,bg :background ,red))))
         (diredp-deletion-file-name
          ((t (:foreground ,red))))
         (diredp-compressed-file-suffix
          ((t (:foreground ,blue))))
         (diredp-file-suffix
          ((t ,context)))
         (diredp-ignored-file-name
          ((t (:foreground ,purple :italic t))))

         ;; SLIME debug buffer
         (sldb-topline-face
          ((t (:foreground ,red :bold t))))
         (sldb-condition-face
          ((t (:foreground ,cyan :bold t))))
         (sldb-section-face
          ((t (,@more :bold t))))

         ;; SLIME REPL
         (slime-repl-input-face
          ((nil ,context)))
         (slime-repl-output-face
          ((t (:foreground ,green-1))))
         (slime-repl-result-face
          ((t (:foreground ,cyan))))
         (slime-highlight-edits-face
          ((t ,normal-hl)))
         (slime-repl-prompt-face
          ((t (:foreground ,red-4))))

         ;; Auto-complete
         (ac-completion-face
          ((t (:inherit hl-line :foreground ,fg-2))))
         (ac-candidate-face
          ((t (:foreground ,fg ,@normal-hl :slant normal :weight normal))))
         (ac-selection-face
          ((t (:foreground ,fg+1 :background ,bg+3 :slant normal :bold t))))
         (ac-slime-menu-face
          ((t (:inherit ac-candidate-face))))
         (ac-slime-selection-face
          ((t (:inherit ac-selection-face))))

         ;; ido mini-buffer
         (ido-first-match
          ((t (:foreground ,gold-1))))
         (ido-only-match
          ((t (:inherit ido-first-match :bold t))))
         (ido-subdir
          ((t (:inherit diredp-dir-priv))))

         ;; org-mode
         (org-level-1
          ((t (:foreground ,green))))
         (org-level-2
          ((t (:foreground ,blue))))
         (org-level-3
          ((t (:foreground ,green-1))))
         (org-level-4
          ((t (:foreground ,purple))))
         (org-level-5
          ((t (:foreground ,red-4))))
         (org-level-6
          ((t (:foreground ,orange))))
         (org-level-7
          ((t (:foreground ,gold+1))))
         (org-level-8
          ((t (:foreground ,yellow-1))))
         (org-table
          ((t (:foreground ,green-3))))
         (org-hide
          ((t (:foreground ,bg))))
         (org-code
          ((t (:foreground ,fg-2))))
         (org-meta-line
          ((t ,dimmed)))
         (org-mode-line-clock
          ((t (:foreground ,red-2 :bold t))))
         (org-link
          ((t (:inherit link))))
         (org-date
          ((t (:foreground ,cyan :underline t))))
         (org-todo
          ((t (:foreground ,red-4))))
         (org-done
          ((t (:foreground ,green-3))))

         ;; Whitespaces
         (whitespace-space
          ((t (:background ,bg :foreground ,cyan-1))))
         (whitespace-tab
          ((t (:inherit whitespace-space))))
         (whitespace-newline
          ((t (:inherit whitespace-space))))

         ;; Parentheses highlighting
         (show-paren-mismatch
          ((t (:inherit font-lock-warning))))
         (show-paren-match
          ((t (:foreground ,fg+1 :bold t))))
         (paren-face-mismatch
          ((t (:inherit show-paren-mismatch))))
         (paren-face-match
          ((t (:inherit show-paren-match))))
         (paren-face-no-match
          ((t (:background ,yellow))))
         ;; Parentheses dimming in Lisp modes
         (esk-paren-face
          ((t (:foreground ,bg+3))))

         ;; flyspell
         (flyspell-incorrect
          ((t (:underline ,red))))
         (flyspell-duplicate
          ((t (:underline ,yellow))))

         ;; diff
         (diff-added
          ((t ,plus)))
         (diff-removed
          ((t ,minus)))
         (diff-context
          ((t ,context)))
         (diff-indicator-added
          ((t (:inherit diff-added))))
         (diff-indicator-removed
          ((t (:inherit diff-removed))))
         (diff-header
          ((t (:inherit header-line))))
         (diff-file-header
          ((t (:foreground ,cyan+1))))
         (diff-hunk-header
          ((t (:foreground ,blue :italic t))))

         ;; ediff
         (ediff-current-diff-A
          ((t (:background "#2C1320")))) ; #2C1320
         (ediff-current-diff-B
          ((t (:background "#0C3320")))) ; #002200
         (ediff-current-diff-C
          ((t (:background "#2C3320"))))
         (ediff-current-diff-Ancestor
          ((t (:background "#0C1350"))))
         (ediff-fine-diff-A
          ((t (:background "#5C3340" :foreground ,fg+1))))
         (ediff-fine-diff-B
          ((t (:background "#1C6340" :foreground ,fg+1))))
         (ediff-fine-diff-C
          ((t (:background "#5C6340" :foreground ,fg+1))))
         (ediff-fine-diff-Ancestor
          ((t (:background "#1C2360" :foreground ,fg+1))))
         (ediff-even-diff-A
          ((t ,normal-hl)))
         (ediff-even-diff-B
          ((t ,normal-hl)))
         (ediff-even-diff-C
          ((t (:background ,bg+3))))
         (ediff-even-diff-Ancestor
          ((t (:background ,bg+3))))
         (ediff-odd-diff-A
          ((t (:background ,bg+3))))
         (ediff-odd-diff-B
          ((t (:background ,bg+3))))
         (ediff-odd-diff-C
          ((t ,normal-hl)))
         (ediff-odd-diff-Ancestor
          ((t ,normal-hl)))

         ;; magit
         (magit-item-highlight
          ((t ,dimmed-hl)))
         (magit-section-title
          ((t (:foreground ,gold+1 :bold t))))
         (magit-branch
          ((t ,more)))
         (magit-diff-file-header
          ((t (:inherit diff-file-header))))
         (magit-diff-hunk-header
          ((t (:inherit diff-hunk-header))))
         (magit-diff-add
          ((t (:inherit diff-added))))
         (magit-diff-del
          ((t (:inherit diff-removed))))
         (magit-diff-none
          ((t (:inherit diff-context))))
         (magit-log-head-label-default
          ((t (:inherit magit-log-head-label-remote :foreground ,yellow-1))))
         (magit-log-sha1
          ((t (:foreground ,red-4))))
         (magit-menu-selected-option
          ((t (:foreground ,green))))
         (magit-item-mark
          ((t (:inherit secondary-selection))))

         ;; info
         (info-xref
          ((t (,@portal :bold t))))
         (info-xref-visited
          ((t (:inherit font-lock-comment-face :bold t :slant normal))))
         (info-quoted-name
          ((t (:inherit font-lock-constant-face))))
         (info-single-quote
          ((t (:inherit font-lock-builtin-face))))
         (info-string
          ((t (:inherit font-lock-doc-string-face))))
         (info-reference-item
          ((t (:inherit font-lock-constant-face))))
         (info-function-ref-item
          ((t (:inherit font-lock-function-name-face))))
         (info-user-option-ref-item
          ((t ,param)))
         (info-variable-ref-item
          ((t ,mutable)))
         (info-macro-ref-item
          ((t (:inherit font-lock-keyword-face))))
         (info-special-form-ref-item
          ((t ,raw)))
         (info-command-ref-item
          ((t (:inherit font-lock-type-face))))

         ;; Null out most attributes, because it seems to inherit
         ;; the face of each line's first character.
         (linum
          ((t (:inherit fringe :foreground ,bg+3 :slant normal :bold t
                        :underline nil :strike-through nil :overline nil
                        :box nil))))

         ;; erc
         (erc-notice-face
          ((t (:foreground ,bg+3))))
         (erc-nick-default-face
          ((t (:foreground ,green-1))))
         (erc-timestamp-face
          ((t (:foreground ,purple))))

         (eshell-prompt
          ((t ,prompt)))

         (comint-highlight-input
          ((t (:foreground ,green-3))))

         ;; helm
         (helm-header
          ((t (:foreground ,gold+1 :bold t))))
         (helm-source-header
          ((t (:foreground ,gold+1 :bold t))))
         ;; XXX: Maybe I just don't believe in themes with narrow
         ;; selection of colors
         (helm-selection
          ((t (:inherit secondary-selection))))
         (helm-selection-line
          ((t (:inherit secondary-selection))))
         (helm-match
          ((t (:foreground ,fg+1 ,@normal-hl :bold t))))
         (helm-overlay-line-face
          ((t ,normal-hl)))
         (helm-file-name
          ((t (:foreground ,cyan-2))))
         (helm-ff-file
          ((t (:inherit helm-file-name))))
         (helm-ff-directory             ; helm-dir-priv
          ((t (:inherit diredp-dir-priv ,dimmed-hl))))
         (helm-ff-symlink
          ((t (:inherit diredp-symlink))))
         (helm-ff-executable
          ((t (:inherit diredp-exec-priv))))
         (helm-candidate-number
          ((t (:background ,yellow-1 :foreground ,bg :bold t))))
         (helm-separator
          ((t (:foreground ,bg+2))))
         (helm-grep-file
          ((t ,special-hl)))
         (helm-moccur-buffer
          ((t (:foreground ,blue-1))))
         (helm-grep-finish
          ((t (:foreground ,green-2))))

         (ecb-default-highlight-face
          ((t (:bold t ,@normal-hl))))

         ;; yasnippet
         (yas--field-debug-face
          ((t (:underline ,yellow))))
         (yas-field-highlight-face
          ((t (:background ,bg+3))))
         ;; For compatibility
         (yas/field-debug-face
          ((t (:underline ,yellow))))
         (yas/field-highlight-face
          ((t (:background ,bg+3))))

         ;; mumamo-mode
         (mumamo-background-chunk-major
          ((t (:background ,bg))))
         (mumamo-background-chunk-submode1
          ((t ,dimmed-hl)))
         (mumamo-background-chunk-submode2
          ((t ,dimmed-hl)))
         (mumamo-border-face-in
          ((t (:foreground ,bg+3 :bold t))))
         (mumamo-border-face-out
          ((t (:foreground ,bg+3 :bold t))))

         (help-argument-name
          ((t (:foreground ,blue))))

         ;; Manual pages
         (woman-bold
          ((t (:foreground ,blue :bold t))))
         (woman-italic
          ((t (:foreground ,cyan+1))))
         (woman-addition
          ((t (:foreground ,gold+1))))
         (woman-unknown
          ((t (:foreground ,red-4))))

         ;; undo-tree
         (undo-tree-visualizer-default-face
          ((t (:foreground ,bg+3))))
         (undo-tree-visualizer-current-face
          ((t ,essence)))
         (undo-tree-visualizer-active-branch-face
          ((t (:foreground ,green-1))))

         ;; ???
         (hexl-address-region
          ((t (:foreground ,blue))))
         (hexl-ascii-region
          ((t (:foreground ,blue))))

         ;; Twitter
         (twittering-uri-face
          ((t (:inherit link))))
         (twittering-username-face
          ((t (:foreground ,green-1))))

         ;; Python
         (py-exception-name-face
          ((t (:inherit font-lock-type-face))))
         (py-pseudo-keyword-face
          ((t (:inherit font-lock-constant-face))))
         (py-XXX-tag-face
          ((t (:inherit font-lock-warning-face))))
         (py-builtins-face
          ((t (:inherit font-lock-builtin-face))))
         (py-class-name-face
          ((t (:inherit font-lock-type-face))))
         (py-decorators-face
          ((t (:inherit font-lock-builtin-face))))

         ;; Code folding
         (hideshowvis-hidable-face
          ((t (:foreground ,bg+2))))
         (hs-fringe-face
          ((t (,@more :box (:line-width 2 :color ,fg+1 :style released-button)))))
         (hs-face
          ((t (:background ,bg+3))))

         (sml-modeline-vis-face
          ((t (:inherit mode-line))))
         (sml-modeline-end-face
          ((t (:background ,bg+3 :foreground ,fg))))

         (compilation-info
          ((t ,essence)))
         (compilation-error
          ((t (:foreground ,red))))
         (compilation-line-number
          ((t (:foreground ,orange))))

         (flymake-errline
          ((t ,error-hl)))
         (flymake-warnline
          ((t (:underline ,yellow-1))))

         (nxml-tag-delimiter
          ((t (:inherit esk-paren-face))))
         (nxml-element-local-name
          ((t (:foreground ,cyan-2))))

         ;; My own custom faces
         (ublt-twitter-meta-face
          ((t (:height 0.9 :foreground ,bg+3))))
         (ublt/flymake-message-face
          ((t (:foreground ,red-4 :bold t))))
         (eproject-ido-imenu-file-path
          ((t (:foreground ,bg+2))))
         (ublt/emms-mode-line-face
          ((t (:height 1.0))))
         (ublt/mode-line-major-mode
          ((t (:bold t))))
         ;; (ublt/evil-emacs-tag
         ;;  ((t (:foreground ,blue :height 1.2))))
         ;; (ublt/evil-normal-tag
         ;;  ((t (:foreground ,red-2 :bold t :height 1.2))))
         ;; (ublt/evil-insert-tag
         ;;  ((t (:foreground ,bg :bold t :height 1.2))))
         ;; (ublt/evil-visual-tag
         ;;  ((t (:foreground ,red :bold t :height 1.2))))

         ;; Skype
         (skype--face-my-message
          ((t (:background ,bg))))
         (skype--face-other-message
          ((t (:background ,bg))))
         (skype--face-my-time-field
          ((t (:foreground ,bg+2 :height 0.8))))
         (skype--face-other-time-field
          ((t (:foreground ,bg+2 :height 0.8))))
         (skype--face-optional-field
          ((t (:foreground ,bg+1))))
         (skype--face-user-field
          ((t (:foreground ,blue :bold t))))

         ))

      ;; Color theme seems to mix this up, restore it
      (set-face-font 'variable-pitch variable-pitch-font)

      (setq
       hl-paren-colors `("Orange" ,yellow "Greenyellow"
                         ,green "Springgreen" "Cyan"
                         ,blue-1 "Magenta" "Purple"
                         "Orange" ,yellow "Greenyellow"
                         ,green "Springgreen" "Cyan"
                         ,blue-1 "Magenta" "Purple"))
      )))

(provide 'ublt-themes)
