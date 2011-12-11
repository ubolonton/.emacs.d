;; Ubolonton's personal dark color theme for Emacs.
;;
;; A mix of Blackboard and Succulent themes.
;;
;; MIT License Copyright (c) 2011 Nguyễn Tuấn Anh <ubolonton at gmail dot com>
;; Credits:
;; - TextMate theme Succulent
;; - TextMate theme Blackboard
;; - Emacs port of Blackboard theme by JD Huntington
;; All patches welcome

(require 'color-theme)
;; (require 'color-util)                  ; color conversion

;;; TODO: Use Emacs 24's deftheme
;;; TODO: Give better names e.g. +1 +2 -1 -2 or ++ --
;;; TODO: Font-lock-type-face is too bright
;;; TODO: Background is a bit dull
;;; TODO: Info color distribution is not appropriate
;;; TODO: More grayscale colors for stuffs like (setq show-paren-style
;;; 'expression)
;;; TODO: +/- names sometimes implies saturation, sometimes implies blackness

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

    (fg-3        "#454E56" "#808080")
    (fg-2        "#5E676F" "#8a8a8a")
    (fg-1        "#788189" "#949494")
    (fg          "#919AA2" "#9e9e9e")
    (fg+1        "#B8C1C9" "#a8a8a8")

    (red         "#FF0000" "#FF0000")                 ; Red
    (red-1       "#F86155" "#FF5F5F")
    (red-3       "#D98D54" "#D7875F")
    (red-4       "#8B0000" "#870000")

    (orange      "#FF8C00" "#FF8700")          ; DarkOrange
    (orange-1    "#FF4500" "#FF5F00")           ; OrangeRed

    (gold        "#EEDD82" "#D7D787")      ; LightGoldenRod
    (gold-1      "#B8860B" "#AF8700")       ; DarkGoldenRod

    (yellow      "#FFFF00" "#FFFF00")              ; Yellow
    (yellow-1    "#9ACD32" "#87D75F")         ; YellowGreen

    (green       "#00DD00" "#87FF87")              ; Green2
    (green-1     "#228B22" "#00875F")         ; ForestGreen
    (green-2     "#2E8B57" "#00875F")            ; SeaGreen
    (green-3     "#90EE90" "#87FF87")          ; LightGreen

    (cyan        "#00CDCD" "#00D7D7")               ; Cyan3
    (cyan+1      "#00FA9A" "#00FF87")   ; MediumSpringGreen
    (cyan+2      "#66CDAA" "#5FD7AF")         ; Aquamarine3
    (cyan+3      "#89A1F3" "#87AFFF")

    (blue        "#0084CF" "#0087AF")
    (blue-1      "#6A5ACD" "#5F5FD7")

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
    (let ((variable-pitch-font (face-attribute 'variable-pitch :font))
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
          (red-3       (find-color 'red-3))
          (red-4       (find-color 'red-4))
          (orange      (find-color 'orange))
          (orange-1    (find-color 'orange-1))
          (gold        (find-color 'gold))
          (gold-1      (find-color 'gold-1))
          (yellow      (find-color 'yellow))
          (yellow-1    (find-color 'yellow-1))
          (green       (find-color 'green))
          (green-1     (find-color 'green-1))
          (green-2     (find-color 'green-2))
          (green-3     (find-color 'green-3))
          (cyan        (find-color 'cyan))
          (cyan+1      (find-color 'cyan+1))
          (cyan+2      (find-color 'cyan+2))
          (cyan+3      (find-color 'cyan+3))
          (blue        (find-color 'blue))
          (blue-1      (find-color 'blue-1))
          (purple      (find-color 'purple))
          )
      (color-theme-install
       `(color-theme-ubolonton-dark

         ;; Frame parameters
         ((background-color . ,bg)
          (foreground-color . ,fg)
          (background-mode . dark)
          (border-color . ,bg)
          (cursor-color . ,yellow)
          ;; (mouse-color . "Sienna1")
          )

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
          ((t (:foreground ,green-3))))
         (font-lock-function-name-face
          ((t (:foreground ,green))))
         (font-lock-keyword-face
          ((t (:foreground ,red-1))))
         (font-lock-preprocessor-face
          ((t (:foreground ,red-3))))
         (font-lock-reference-face
          ((t (:foreground ,blue-1))))  ; What's this?
         (font-lock-regexp-grouping-backslash
          ((t (:foreground ,bg+1))))
         (font-lock-regexp-grouping-construct
          ((t (:foreground ,orange-1))))
         (font-lock-string-face
          ((t (:foreground ,green-1))))
         (font-lock-doc-face
          ((t (:inherit font-lock-string-face))))
         (font-lock-type-face
          ((t (:foreground ,cyan+1))))
         (font-lock-variable-name-face
          ((t (:foreground ,gold-1))))
         (font-lock-warning-face
          ((t (:foreground ,red))))
         (font-lock-constant-face
          ((t (:inherit font-lock-builtin-face))))

         ;; Misc
         (isearch-fail
          ((t (:foreground ,red :background ,yellow-1))))
         (gui-element
          ((t (:background ,fg+1 :foreground ,bg)))) ; What's this?
         (region
          ((t (:background ,bg+2))))    ; selection
         (hl-line
          ((t (:background ,bg+1))))    ; line highlighting
         (hl-sexp-face
          ((t (:inherit hl-line))))
         (highlight
          ((t (:background ,bg+2))))    ; highlighting (& line in anything)
         (highline-face
          ((t (:background ,green-2)))) ; What's this?
         (zmacs-region
          ((t (:inherit region))))      ; What's this?
         (text-cursor
          ((t (:background ,yellow :foreground ,bg)))) ; What's this?
         (minibuffer-prompt
          ((t (:foreground ,cyan :bold t ))))
         (left-margin ((t (nil))))
         (toolbar ((t (nil))))
         (fringe ((t (:background ,bg+1 :foreground ,fg-3))))
         (link ((t (:foreground ,cyan+3 :underline t))))
         (match ((t (:background ,bg+3))))

         ;; Mode line
         (mode-line
          ((t (:background ,cyan+3 :foreground ,bg :height 0.8
                           :box (:line-width 1 :color ,cyan+3)))))
         (mode-line-inactive
          ((t (:inherit mode-line :background ,bg+3 :foreground ,fg+1))))
         (mode-line-buffer-id
          ((t (:bold t :height 1.2))))
         (mode-line-highlight
          ((t (:inherit mode-line))))
         (mode-line-inactive
          ((t (:inherit mode-line :background ,bg+2))))
         (which-func
          ((t (:foreground ,red-4 :height 1.2 :bold t))))
         ;; (which-func
         ;;  ((t (:foreground "#124800" :height 1.2 :bold t))))

         ;; SLIME debug buffer
         (sldb-topline-face
          ((t (:foreground ,red :bold t))))
         (sldb-condition-face
          ((t (:foreground ,cyan :bold t))))
         (sldb-section-face
          ((t (:foreground ,green :bold t))))

         ;; SLIME REPL
         (slime-repl-input-face
          ((nil (:foreground ,fg-3))))
         (slime-repl-output-face
          ((t (:foreground ,green-1))))
         (slime-repl-result-face
          ((t (:foreground ,cyan))))
         (slime-highlight-edits-face
          ((t (:background ,bg+2))))
         (slime-repl-prompt-face
          ((t (:inherit font-lock-keyword-face))))

         ;; Auto-complete
         (ac-completion-face
          ((t (:inherit hl-line :foreground ,fg-2))))
         (ac-candidate-face
          ((t (:foreground ,fg :background ,bg+2 :slant normal :weight normal))))
         (ac-selection-face
          ((t (:foreground ,fg+1 :background ,bg+3 :slant normal :bold t))))
         (ac-slime-menu-face
          ((t (:inherit ac-candidate-face))))
         (ac-slime-selection-face
          ((t (:inherit ac-selection-face))))

         ;; ido mini-buffer
         (ido-first-match
          ((t (:foreground ,gold))))
         (ido-only-match
          ((t (:inherit ido-first-match :bold t))))
         (ido-subdir
          ((t (:foreground ,green))))

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
          ((t (:foreground ,red-1))))
         (org-level-6
          ((t (:foreground ,orange))))
         (org-level-7
          ((t (:foreground ,gold-1))))
         (org-level-8
          ((t (:foreground ,yellow-1))))
         (org-table
          ((t (:foreground ,green-3))))
         (org-hide
          ((t (:foreground ,bg))))
         (org-code
          ((t (:foreground ,fg-2))))
         (org-meta-line
          ((t (:foreground ,bg+2))))
         (org-mode-line-clock
          ((t (:foreground ,red-4 :bold t))))
         (org-link
          ((t (:inherit link))))
         (org-date
          ((t (:foreground ,cyan :underline t))))
         (org-todo
          ((t (:foreground ,red-1))))
         (org-done
          ((t (:foreground ,green-3))))

         ;; Whitespaces
         (whitespace-space
          ((t (:background ,bg :foreground ,cyan+2))))
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

         (eval-sexp-fu-flash
          ((t (:background ,bg+2))))

         ;; flyspell
         (flyspell-incorrect
          ((t (:underline ,red))))
         (flyspell-duplicate
          ((t (:underline ,yellow))))

         ;; magit
         (magit-item-highlight
          ((t (:background ,bg+1))))
         (magit-section-title
          ((t (:foreground ,gold-1 :bold t))))
         (magit-branch
          ((t (:inherit font-lock-function-name-face))))
         (magit-diff-file-header
          ((t (:foreground ,cyan+1))))
         (magit-diff-hunk-header
          ((t (:inherit font-lock-builtin-face :italic t))))
         (magit-log-head-label-default
          ((t (:inherit magit-log-head-label-remote :foreground ,yellow-1))))
         (magit-log-sha1
          ((t (:inherit font-lock-keyword-face))))
         (magit-diff-add
          ((t (:foreground ,green))))
         (magit-diff-del
          ((t (:foreground ,red))))
         (magit-diff-none
          ((t (:foreground ,fg-3))))
         (magit-menu-selected-option
          ((t (:inherit font-lock-function-name-face))))

         ;; info
         (info-xref
          ((t (:foreground ,cyan :bold t))))
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
          ((t (:inherit font-lock-variable-name-face))))
         (info-variable-ref-item
          ((t (:inherit font-lock-variable-name-face))))
         (info-macro-ref-item
          ((t (:inherit font-lock-keyword-face))))
         (info-special-form-ref-item
          ((t (:inherit font-lock-preprocessor-face))))
         (info-command-ref-item
          ((t (:inherit font-lock-type-face))))

         (linum
          ((t (:inherit fringe :foreground ,bg+3 :slant normal :bold t))))

         (eshell-prompt
          ((t (:foreground ,cyan))))

         ;; erc
         (erc-notice-face
          ((t (:foreground ,bg+3))))
         (erc-nick-default-face
          ((t (:inherit font-lock-string-face))))
         (erc-timestamp-face
          ((t (:inherit font-lock-comment-face :slant normal))))

         (comint-highlight-input
          ((t (:foreground ,green-3))))

         ;; dired+
         (diredp-file-name
          ((t (:inherit default))))
         (diredp-dir-heading
          ((t (:foreground ,gold :bold t))))
         (diredp-symlink
          ((t (:foreground ,red-1))))
         (diredp-no-priv
          ((t (:foreground ,bg+2 :background ,bg+1))))
         (diredp-read-priv
          ((t (:background ,green-2))))
         (diredp-write-priv
          ((t (:background ,red-3))))
         (diredp-exec-priv
          ((t (:background ,red-3))))
         (diredp-dir-priv
          ((t (:foreground ,green))))
         (diredp-number
          ((t (:foreground ,green-2))))
         (diredp-flag-mark-line
          ((t (:foreground ,cyan))))
         (diredp-deletion
          ((t (:foreground ,bg :background ,red))))
         (diredp-deletion-file-name
          ((t (:foreground ,red))))
         (diredp-compressed-file-suffix
          ((t (:foreground ,blue))))
         (diredp-file-suffix
          ((t (:foreground ,cyan))))
         (diredp-ignored-file-name
          ((t (:inherit font-lock-comment-face))))

         ;; anything
         (anything-header
          ((t (:foreground ,gold-1 :bold t))))
         (anything-file-name
          ((t (:inherit font-lock-type-face))))
         (anything-match
          ((t (:foreground ,fg+1 :background ,bg+2 :bold t))))
         (anything-overlay-line-face
          ((t (:background ,bg+2))))
         (anything-ff-directory         ; anything-dir-priv
          ((t (:inherit diredp-dir-priv :background ,bg+1))))
         (anything-ff-symlink
          ((t (:inherit diredp-symlink))))
         (anything-ff-file
          ((t (:inherit font-lock-type-face))))
         (anything-ff-executable
          ((t (:inherit diredp-exec-priv))))
         (anything-candidate-number
          ((t (:inherit isearch-fail, :foreground ,bg :bold t))))

         (escape-glyph
          ((t (:foreground ,cyan :bold t))))

         (ecb-default-highlight-face
          ((t (:bold t :background ,bg+2))))

         ;; yasnippet
         (yas/field-debug-face
          ((t (:underline ,yellow))))
         (yas/field-highlight-face
          ((t (:background ,bg+3))))

         (lazy-highlight
          ((t (:background ,bg+2))))

         ;; mumamo-mode
         (mumamo-background-chunk-major
          ((t (:background ,bg))))
         (mumamo-background-chunk-submode1
          ((t (:background ,bg))))
         (mumamo-background-chunk-submode2
          ((t (:background ,bg+1))))
         (mumamo-border-face-in
          ((t (:foreground ,bg+3 :bold t))))
         (mumamo-border-face-out
          ((t (:foreground ,bg+3 :bold t))))

         (pp^L-highlight
          ((t (:foreground ,purple :bold t))))

         (help-argument-name
          ((t (:inherit font-lock-builtin-face))))

         (highlight-symbol-face
          ((t (:background ,bg+3))))

         ;; Manual pages
         (woman-bold
          ((t (:inherit font-lock-builtin-face :bold t))))
         (woman-italic
          ((t (:inherit font-lock-type-face))))
         (woman-addition
          ((t (:inherit font-lock-variable-name-face))))
         (woman-unknown
          ((t (:inherit font-lock-keyword-face))))

         ;; undo-tree
         (undo-tree-visualizer-default-face
          ((t (:foreground ,bg+3))))
         (undo-tree-visualizer-current-face
          ((t (:foreground ,green))))
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
          ((t (:foreground ,green :box (:line-width 2 :color ,fg+1 :style released-button)))))
         (hs-face
          ((t (:background ,bg+3))))

         (highlight-changes
          ((t (:background ,bg+3))))

         (sml-modeline-vis-face
          ((t (:inherit mode-line))))
         (sml-modeline-end-face
          ((t (:background ,bg+3 :foreground ,fg))))

         (compilation-info
          ((t (:inherit font-lock-function-name-face))))
         (compilation-error
          ((t (:foreground ,red))))

         ;; XXX: FIX:
         (flymake-errline
          ((t (:background "Firebrick4"))))

         (nxml-tag-delimiter
          ((t (:inherit esk-paren-face))))
         ;; (nxml-element-local-name
         ;;  ((t (:foreground "#009900"))))

         ;; My own custom faces
         (ublt-twitter-meta-face
          ((t (:height 0.9 :foreground ,bg+3))))
         (ublt/flymake-message-face
          ((t (:foreground ,red-1 :bold t))))
         (eproject-ido-imenu-file-path
          ((t (:inherit 'org-meta-line))))
         (ublt/emms-mode-line-face
          ((t (:font ,variable-pitch-font :height 1.2))))
         (ublt/mode-line-major-mode
          ((t (:bold t))))
         (ublt/evil-emacs-tag
          ((t (:inherit font-lock-builtin-face :height 1.2))))
         (ublt/evil-normal-tag
          ((t (:foreground ,red-4 :bold t :height 1.2))))
         (ublt/evil-insert-tag
          ((t (:foreground ,bg :bold t :height 1.2))))
         (ublt/evil-visual-tag
          ((t (:foreground ,red :bold t :height 1.2))))

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

      ;; FIX: Change anything-config to make these `defcustom' instead,
      ;; and why the hell does a `defcustom' not work?
      (setq
       anything-c-buffers-face2 'font-lock-builtin-face
       anything-c-buffers-face3 'italic
       hl-paren-colors `("Orange" ,yellow "Greenyellow"
                         ,green "Springgreen" "Cyan"
                         ,blue-1 "Magenta" "Purple"
                         "Orange" ,yellow "Greenyellow"
                         ,green "Springgreen" "Cyan"
                         ,blue-1 "Magenta" "Purple"))
      )))

(provide 'ublt-themes)
