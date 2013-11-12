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


;;; TODO: Light theme, font integration, version for all-bold faces
;;; FIX: Don't base variable-width font size on fixed-width font size
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
    (cyan-2      "#7991E3" "#87AFFF")

    (blue        "#0000cc" "#0000ff")
    ;; Bult-in, constant
    (blue-1      "#0084CF" "#0087AF")
    ;; Reference, file, buffer
    (blue-2      "#6A5ACD" "#5F5FD7")
    ;; Secondary selection
    (blue-3      "#223360" "#005F87")

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
    (let* (;; (variable-pitch-family (face-attribute 'variable-pitch :family))
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
           (blue-3      (find-color 'blue-3))
           (purple      (find-color 'purple))

           (warning      `(:foreground ,red))
           (error-hl     `(:background ,red-3))
           (power        `(:foreground ,red-4))
           (commitment   `(:foreground ,red-4))
           (raw          `(:foreground ,red-5))

           (minus        `(:foreground ,red-1))
           (plus         `(:foreground ,green-1))
           (context      `(:foreground ,fg-3))
           (dimmed       `(:foreground ,bg+3))
           (shadowed     `(:foreground ,bg+2))

           (param        `(:foreground ,orange))
           (mutable      `(:foreground ,gold+1))
           (exception-hl `(:background ,yellow-1))

           (essence      `(:foreground ,green))
           (more         `(:foreground ,green))
           (structured   `(:foreground ,green))

           (string       `(:foreground ,green-1))
           (doc          `(:foreground ,green-2))

           (type         `(:foreground ,cyan+1))
           (portal       `(:foreground ,cyan-2))
           (teleport     `(:foreground ,cyan))
           (prompt       `(:foreground ,cyan))

           (constant     `(:foreground ,blue-1))

           (reference    `(:foreground ,blue-2))

           (dimmed-hl    `(:background ,bg+1))
           (normal-hl    `(:background ,bg+2 :weight light))
           (strong-hl    `(:background ,bg+3))
           (special-hl   `(:background ,blue-3))

           (strong       `(:foreground ,fg+1))

           (note         `(:foreground ,purple)) ; meta?

           (status       `(:background ,cyan-2))

           (reset        `(:weight normal :slant normal :underline nil :box nil
                                   :strike-through nil :inverse-video nil :overline nil))

           ;; Fixed-width font
           (fw           `(
                           :font ,(face-attribute 'default :font)
                           :fontset ,(face-attribute 'default :fontset)
                           :weight ,(face-attribute 'default :weight)
                           :height ,(face-attribute 'default :height)))
           ;; Variable-width font
           (vw           `(
                           :fontset ,(face-attribute 'variable-pitch :fontset)
                           :font ,(face-attribute 'variable-pitch :font)
                           :weight ,(face-attribute 'variable-pitch :weight)
                           :height ,(face-attribute 'variable-pitch :height)))

           (fheight      (face-attribute 'default :height))
           (ffontset     (face-attribute 'default :fontset))
           )
      ;; (message "Before %s" (face-attribute 'default :height))

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

         (default ((t (,@fw :background ,bg :foreground ,fg))))
         ;; FIX: Height should be font-dependent in general
         (variable-pitch ((t (,@vw :background ,bg :foreground ,fg))))

         (border-glyph ((t (nil))))     ; What's this?
         (buffers-tab ((t (:background ,bg :foreground ,fg)))) ; What's this?
         (shadow ((t ,dimmed)))

         ;; Code highlighting
         (font-lock-builtin-face
          ((t (,@fw ,@constant))))
         (font-lock-comment-face
          ((t (,@fw ,@note :slant italic))))
         (font-lock-comment-delimiter-face
          ((t (:inherit font-lock-comment-face ,@shadowed))))
         (font-lock-doc-string-face
          ((t (,@fw ,@doc))))
         (font-lock-function-name-face
          ((t (,@fw ,@essence))))
         (font-lock-keyword-face
          ((t (,@fw ,@power))))
         (font-lock-reference-face
          ((t (,@fw ,@reference))))           ; TODO What's this?
         (font-lock-regexp-grouping-backslash
          ((t (,@fw :foreground ,bg+1))))
         (font-lock-regexp-grouping-construct
          ((t (,@fw :foreground ,orange+1))))
         (font-lock-string-face
          ((t (,@fw ,@string))))
         (font-lock-doc-face
          ((t (,@fw ,@string))))
         (font-lock-type-face
          ((t (,@fw ,@type))))
         (font-lock-preprocessor-face
          ((t (,@fw ,@raw))))
         (font-lock-variable-name-face
          ((t (,@fw ,@mutable))))
         (font-lock-warning-face
          ((t (,@fw ,@warning))))
         (font-lock-constant-face
          ((t (,@fw ,@constant))))            ; TODO: Different shade
         (number-font-lock-face
          ((t (,@fw ,@constant))))

         (js2-function-param-face
          ((t ,param)))
         (js2-jsdoc-type-face
          ((t (:inherit font-lock-type-face))))
         (js2-jsdoc-tag-face
          ((t (:inherit font-lock-builtin-face))))

         (js2-function-param
          ((t ,param)))
         (js2-jsdoc-type
          ((t (:inherit font-lock-type-face))))
         (js2-jsdoc-tag
          ((t (:inherit font-lock-builtin-face))))

         (js3-function-param-face
          ((t ,param)))
         (js3-jsdoc-type-face
          ((t (:inherit font-lock-type-face))))
         (js3-jsdoc-tag-face
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
         (link ((t (,@portal :underline ,bg+3))))
         (match ((t ,strong-hl)))
         (escape-glyph                  ; Special characters
          ((t (,@prompt :bold t))))     ; TODO

         ;; Highlighting
         (region
          ((t ,normal-hl)))             ; selection
         (secondary-selection
          ((t ,special-hl)))
         (hl-line
          ((t ,dimmed-hl)))                ; line highlighting
         (hl-sexp-face
          ((t ,dimmed-hl)))
         (highlight
          ((t ,normal-hl)))             ; highlighting
         (highline-face
          ((t (:background ,green-2)))) ; TODO What's this?
         (zmacs-region
          ((t (:inherit region))))      ; TODO What's this?
         (highlight-changes
          ((t ,strong-hl)))
         (highlight-symbol-face
          ((t ,strong-hl)))
         (pp^L-highlight
          ((t (,@note :bold t))))
         (lazy-highlight
          ((t ,normal-hl)))

         (eval-sexp-fu-flash
          ((t ,normal-hl)))

         ;; Mode line
         (mode-line
          ((t (,@status ,@vw :foreground ,bg
                        :box (:line-width 1 :color ,cyan-2)
                        ))))
         (mode-line-inactive
          ((t (:inherit mode-line ,@strong-hl ,@strong
                        :box (:color ,bg+3)))))
         (mode-line-buffer-id
          ((t (:bold t :height 1.2))))
         (mode-line-highlight
          ((t (:inherit mode-line))))
         (which-func
          ((t (:foreground ,red-2 :height 1.1 :bold t))))
         (anzu-mode-line
          ((t (:foreground ,red-2 :weight bold))))

         ;; dired, dired+
         (diredp-file-name
          ((t (:inherit default))))     ; TODO
         (diredp-dir-heading
          ((t (:foreground ,gold-1 :bold t))))  ; TODO
         (dired-symlink
          ((t ,teleport)))
         (diredp-symlink
          ((t ,teleport)))
         (diredp-no-priv
          ((t (,@dimmed-hl ,@shadowed))))
         (diredp-read-priv
          ((t (,@dimmed-hl ,@more))))
         (diredp-write-priv
          ((t (,@dimmed-hl ,@raw))))
         (diredp-exec-priv
          ((t (,@dimmed-hl ,@power))))
         (diredp-dir-priv
          ((t ,more)))
         (diredp-number
          ((t ,doc)))                   ; TODO
         (diredp-flag-mark-line
          ((t ,special-hl)))            ; Selection mark
         (diredp-deletion
          ((t ,error-hl)))              ; Deletion mark
         (diredp-deletion-file-name
          ((t ,warning)))
         (diredp-compressed-file-suffix
          ((t ,constant)))              ; TODO
         (diredp-file-suffix
          ((t ,context)))
         (diredp-ignored-file-name
          ((t (,@note :italic t))))
         (diredp-mode-line-marked
          ((t (:bold t :foreground ,blue))))
         (diredp-mode-line-flagged
          ((t (:bold t :foreground ,red-2))))

         ;; SLIME debug buffer
         (sldb-topline-face
          ((t (:foreground ,red :bold t)))) ; TODO
         (sldb-condition-face
          ((t (:foreground ,cyan :bold t)))) ; TODO
         (sldb-section-face
          ((t (,@more :bold t))))       ; TODO

         ;; SLIME REPL
         (slime-repl-input-face
          ((nil ,context)))
         (slime-repl-output-face
          ((t ,string)))
         (slime-repl-result-face
          ((t (:foreground ,cyan))))    ; TODO
         (slime-highlight-edits-face
          ((t ,normal-hl)))
         (slime-repl-prompt-face
          ((t ,commitment)))

         (cider-repl-input-face
          ((t (:background ,bg+2))))

         ;; Auto-complete & popup
         (ac-completion-face
          ((t (,@dimmed-hl :foreground ,fg-2 :slant normal :weight normal))))
         (ac-candidate-face
          ((t (,@normal-hl :foreground ,fg :slant normal :weight normal))))
         (ac-selection-face
          ((t (,@strong-hl ,@strong :slant normal :bold t))))
         (ac-slime-menu-face
          ((t (:inherit ac-candidate-face))))
         (ac-slime-selection-face
          ((t (:inherit ac-selection-face))))
         (popup-face
          ((t (:inherit ac-candidate-face))))
         (popup-tip-face
          ((t (,@dimmed-hl ,@string))))
         (popup-summary-face
          ((t (:inherit popup-face ,@note))))

         (dropdown-list-face
          ((t (:inherit ac-completion-face))))
         (dropdown-list-selection-face
          ((t (:inherit ac-selection-face))))

         ;; ido mini-buffer
         (ido-first-match
          ((t (:foreground ,gold-1))))  ; TODO
         (ido-only-match
          ((t (:inherit ido-first-match :bold t))))
         (ido-subdir
          ((t ,more)))

         ;; org-mode
         ;; TODO: levels
         (org-document-title
          ((t (,@vw ,@string :height 2.0 :bold t))))
         (org-special-keyword
          ((t (,@vw ,@constant))))
         (org-level-1
          ((t (,@vw ,@essence :height 1.6))))
         (org-level-2
          ((t (,@vw ,@mutable :height 1.4))))
         (org-level-3
          ((t (,@vw ,@commitment :height 1.2))))
         (org-level-4
          ((t (,@vw ,@note))))
         (org-level-5
          ((t (,@vw ,@param))))
         (org-level-6
          ((t (,@vw ,@param))))
         (org-level-7
          ((t (,@vw :foreground ,yellow-1))))
         (org-table
          ((t (,@fw :foreground ,green-3)))) ; TODO
         (org-hide
          ((t (:foreground ,bg))))
         (org-code
          ((t (:inherit font-lock-builtin-face))))    ; TODO
         (org-meta-line
          ((t (,@fw ,@context))))
         (org-mode-line-clock
          ((t (:foreground ,red-2 :bold t)))) ; TODO
         (org-link
          ((t (:inherit link))))
         (org-date
          ((t (:foreground ,cyan :underline t))))  ; TODO
         (org-todo
          ((t ,commitment)))
         (org-done
          ((t (:foreground ,green-3))))  ; TODO
         (org-block-background
          ((t (,@fw))))
         (org-table
          ((t (,@fw))))
         (org-block-begin-line
          ((t (,@fw ,@shadowed))))
         (org-block-end-line
          ((t (,@fw ,@shadowed))))

         (markdown-link-face
          ((t (,@vw ,@power))))
         (markdown-url-face
          ((t (,@vw :inherit link))))
         (markdown-url-face
          ((t (,@vw :inherit link))))

         ;; Whitespaces
         (whitespace-space
          ((t (:background ,bg :foreground ,cyan-1)))) ; TODO
         (whitespace-tab
          ((t (:inherit whitespace-space))))
         (whitespace-newline
          ((t (:inherit whitespace-space))))

         ;; Parentheses highlighting
         (show-paren-mismatch
          ((t (:inherit font-lock-warning))))
         (show-paren-match
          ((t (,@strong))))
         (paren-face-mismatch
          ((t (:inherit show-paren-mismatch))))
         (paren-face-match
          ((t (:inherit show-paren-match))))
         (paren-face-no-match
          ((t (:background ,yellow))))  ; TODO
         ;; Parentheses dimming in Lisp modes
         (esk-paren-face
          ((t ,dimmed)))
         (sp-show-pair-match-face
          ((t (:inherit show-paren-match))))

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
          ((t (:foreground ,cyan+1))))  ; TODO
         (diff-hunk-header
          ((t (,@constant :italic t)))) ; TODO

         ;; ediff
         ;; I think I lost my vision of "small semantic color set"
         ;; here. Semantic maybe but small, probably not. So how do we
         ;; actually organize them?
         (ediff-current-diff-A
          ((t (:background "#2C1320")))) ; #2C1320
         (ediff-current-diff-B
          ((t (:background "#0C3320")))) ; #002200
         (ediff-current-diff-C
          ((t (:background "#2C3320"))))
         (ediff-current-diff-Ancestor
          ((t (:background "#0C1350"))))
         (ediff-fine-diff-A
          ((t (:background "#5C3340" :weight bold))))
         (ediff-fine-diff-B
          ((t (:background "#1C6340" :weight bold))))
         (ediff-fine-diff-C
          ((t (:background "#5C6340" :weight bold))))
         (ediff-fine-diff-Ancestor
          ((t (:background "#1C2360" :weight bold))))
         (ediff-even-diff-A
          ((t ,normal-hl)))
         (ediff-even-diff-B
          ((t ,normal-hl)))
         (ediff-even-diff-C
          ((t ,strong-hl)))
         (ediff-even-diff-Ancestor
          ((t ,strong-hl)))
         (ediff-odd-diff-A
          ((t ,strong-hl)))
         (ediff-odd-diff-B
          ((t ,strong-hl)))
         (ediff-odd-diff-C
          ((t ,normal-hl)))
         (ediff-odd-diff-Ancestor
          ((t ,normal-hl)))

         ;; magit
         (magit-item-highlight
          ((t ,dimmed-hl)))
         (magit-section-title
          ((t (,@mutable :weight bold)))) ; TODO
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
          ((t (:inherit magit-log-head-label-remote :foreground ,yellow-1)))) ; TODO WHat is this?
         (magit-log-sha1
          ((t ,commitment)))
         (magit-menu-selected-option
          ((t (:foreground ,green))))   ; TODO What is this?
         (magit-item-mark
          ((t (:inherit secondary-selection))))
         (magit-tag
          ((t (:foreground ,green-2 :box ,bg+3)))) ; TODO
         (magit-log-head-label-tags
          ((t (:inherit magit-tag))))
         (magit-log-author
          ((t ,context)))
         (magit-log-date
          ((t ,dimmed)))
         (magit-log-author-date-cutoff
          ((t (:inherit magit-log-author :weight bold))))

         ;; info
         (info-xref
          ((t (,@portal :bold t))))
         (info-xref-visited
          ((t (,@note :bold t))))
         (info-quoted-name
          ((t ,constant)))
         (info-single-quote
          ((t ,constant)))
         (info-string
          ((t ,doc)))
         (info-reference-item
          ((t ,constant)))
         (info-function-ref-item
          ((t ,essence)))
         (info-user-option-ref-item
          ((t ,param)))
         (info-variable-ref-item
          ((t ,mutable)))
         (info-macro-ref-item
          ((t ,power)))
         (info-special-form-ref-item
          ((t ,raw)))
         (info-command-ref-item
          ((t ,type)))

         ;; Null out most attributes, because it seems to inherit
         ;; the face of each line's first character.
         (linum
          ((t (:inherit fringe ,@dimmed :slant normal :bold t
                        :underline nil :strike-through nil :overline nil
                        :box nil))))

         ;; erc
         (erc-notice-face
          ((t ,dimmed)))
         (erc-nick-default-face
          ((t ,string)))              ; TODO
         (erc-current-nick-face
          ((t ,constant)))
         (erc-my-nick-face
          ((t ,constant)))
         (erc-timestamp-face
          ((t ,note)))
         (erc-prompt-face
          ((t (,@prompt :bold t))))
         (erc-command-indicator-face
          ((t (:slant italic :weight normal))))
         (erc-button
          ((t (:slant normal))))

         (eshell-prompt
          ((t ,prompt)))

         (comint-highlight-input
          ((t (:foreground ,green-3)))) ; TODO

         ;; helm
         (helm-header
          ((t (,@mutable :bold t))))    ; TODO
         (helm-source-header
          ((t (,@mutable :bold t))))    ; TODO
         ;; XXX: Maybe I just don't believe in themes with narrow
         ;; selection of colors
         (helm-selection
          ((t (:inherit secondary-selection))))
         (helm-selection-line
          ((t (:inherit secondary-selection))))
         (helm-match
          ((t (,@strong ,@normal-hl :bold t))))
         (helm-overlay-line-face
          ((t ,normal-hl)))
         (helm-file-name
          ((t ,portal)))                ; TODO
         (helm-ff-file
          ((t (:inherit helm-file-name))))
         (helm-ff-directory             ; helm-dir-priv
          ((t (:inherit diredp-dir-priv ,dimmed-hl))))
         (helm-ff-symlink
          ((t (:inherit diredp-symlink))))
         (helm-ff-executable
          ((t (:inherit diredp-exec-priv))))
         (helm-candidate-number
          ((t (:background ,yellow-1 :foreground ,bg :bold t)))) ; TODO
         (helm-separator
          ((t ,shadowed)))
         (helm-grep-file
          ((t ,special-hl)))
         (helm-moccur-buffer
          ((t ,reference)))  ; TODO
         (helm-grep-finish
          ((t ,doc)))                   ; TODO
         (helm-swoop-target-line-face
          ((t (:inherit secondary-selection))))
         (helm-swoop-target-line-block-face
          ((t (:inherit secondary-selection))))
         ;; TODO
         (helm-swoop-target-word-face
          ((t ,normal-hl)))

         (ecb-default-highlight-face
          ((t (:bold t ,@normal-hl))))

         ;; yasnippet
         (yas--field-debug-face
          ((t (:underline ,yellow))))
         (yas-field-highlight-face
          ((t ,strong-hl)))
         ;; For compatibility
         (yas/field-debug-face
          ((t (:underline ,yellow))))
         (yas/field-highlight-face
          ((t ,strong-hl)))

         ;; mumamo-mode
         (mumamo-background-chunk-major
          ((t (:background ,bg))))
         (mumamo-background-chunk-submode1
          ((t ,dimmed-hl)))
         (mumamo-background-chunk-submode2
          ((t ,dimmed-hl)))
         (mumamo-border-face-in
          ((t (,@dimmed :bold t))))
         (mumamo-border-face-out
          ((t (,@dimmed :bold t))))

         (web-mode-preprocessor-face
          ((t (:foreground ,fg-3))))
         (web-mode-html-attr-name-face
          ((t (:inherit font-lock-variable-name-face))))
         (web-mode-html-tag-face
          ((t (:inherit font-lock-builtin-face))))
         (web-mode-part-face
          ((t (:background ,bg+1))))
         (web-mode-block-face
          ((t (:background ,bg+2))))
         (web-mode-css-selector-face
          ((t (:inherit font-lock-function-name-face))))
         (web-mode-current-element-highlight-face
          ((t (:inherit highlight))))
         (web-mode-comment-keyword-face
          ((t (:inherit font-lock-warning-face))))

         (help-argument-name
          ((t (:foreground ,blue-1))))    ; TODO

         ;; Manual pages
         (woman-bold
          ((t (:foreground ,blue-1 :bold t)))) ; TODO
         (woman-italic
          ((t (:foreground ,cyan+1))))
         (woman-addition
          ((t ,mutable)))               ; TODO
         (woman-unknown
          ((t (:foreground ,red-4))))   ; TODO

         ;; undo-tree
         (undo-tree-visualizer-default-face
          ((t ,dimmed)))
         (undo-tree-visualizer-current-face
          ((t ,essence)))
         (undo-tree-visualizer-active-branch-face
          ((t ,string)))                ; TODO

         ;; ???
         (hexl-address-region
          ((t (:foreground ,blue-1))))    ; TODO
         (hexl-ascii-region
          ((t (:foreground ,blue-1))))    ; TODO

         ;; Twitter
         (twittering-uri-face
          ((t (:inherit link))))
         (twittering-username-face
          ((t ,string)))                ; TODO

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
          ((t ,shadowed)))
         (hs-fringe-face
          ((t (,@more :box (:line-width 2 :color ,fg+1 :style released-button)))))
         (hs-face
          ((t ,strong-hl)))

         (sml-modeline-vis-face
          ((t (:inherit mode-line))))
         (sml-modeline-end-face
          ((t (,@strong-hl :foreground ,fg))))

         (compilation-info
          ((t ,essence)))
         (compilation-error
          ((t ,warning)))
         (compilation-line-number
          ((t ,param)))                 ; TODO

         (ace-jump-face-foreground
          ((t (,@reset ,@warning))))

         (flymake-errline
          ((t ,error-hl)))
         (flymake-warnline
          ((t (:underline ,yellow-1))))

         (nxml-tag-delimiter
          ((t (:inherit esk-paren-face))))
         (nxml-element-local-name
          ((t ,portal)))                ; TODO

         ;; My own custom faces
         (ublt-twitter-meta-face
          ((t (:height 0.9 ,@shadowed))))
         (ublt/flymake-message-face
          ((t (,@commitment :bold t))))  ; TODO
         (eproject-ido-imenu-file-path
          ((t ,shadowed)))
         (ublt/emms-mode-line-face
          ((t (:height 1.0))))
         (ublt/mode-line-major-mode
          ((t (:bold t))))
         ;; (ublt/evil-emacs-tag
         ;;  ((t (:foreground ,blue-1 :height 1.2))))
         ;; (ublt/evil-normal-tag
         ;;  ((t (:foreground ,red-2 :bold t :height 1.2))))
         ;; (ublt/evil-insert-tag
         ;;  ((t (:foreground ,bg :bold t :height 1.2))))
         ;; (ublt/evil-visual-tag
         ;;  ((t (:foreground ,red :bold t :height 1.2))))

         ;; Skype TODO
         (skype--face-my-message
          ((t (:background ,bg))))
         (skype--face-other-message
          ((t (:background ,bg))))
         (skype--face-my-time-field
          ((t (,@shadowed :height 0.8))))
         (skype--face-other-time-field
          ((t (,@shadowed :height 0.8))))
         (skype--face-optional-field
          ((t (:foreground ,bg+1))))
         (skype--face-user-field
          ((t (:foreground ,blue-1 :bold t))))

         (haskell-interactive-face-prompt
          ((t (,@commitment))))

         ))

      ;; Color theme seems to mess this up, restore it
      ;; (message "After %s" (face-attribute 'default :height))
      ;; (message "%s" fw)
      (set-face-attribute 'default nil :height fheight)
      ;; This does not work actually. Emacs is bad at handling default vs. normal
      ;; face/font/fontset.
      (set-face-attribute 'default nil :fontset ffontset)
      ;; This does not seem to be messed up anymore
      ;; (set-face-font 'variable-pitch variable-pitch-family)
      ;; (message "Done %s" (face-attribute 'default :height))

      (setq
       hl-paren-colors `("Orange" ,yellow "Greenyellow"
                         ,green "Springgreen" "Cyan"
                         ,blue-2 "Magenta" "Purple"
                         "Orange" ,yellow "Greenyellow"
                         ,green "Springgreen" "Cyan"
                         ,blue-2 "Magenta" "Purple"))
      )))

(provide 'ublt-themes)


;;; NTA TODO: Consider these (from http://designmodo.github.com/Flat-UI/)
'("#1abc9c" "Turquoise"
  "#16a085" "Green sea"
  "#2ecc71" "Emerland"
  "#27ae60" "Nephritis"
  "#3498db" "Peter river"
  "#2980b9" "Belize hole"
  "#9b59b6" "Amethyst"
  "#8e44ad" "Wisteria"
  "#34495e" "Wet asphalt"
  "#2c3e50" "Midnight blu"
  "#f1c40f" "Sun flower"
  "#f39c12" "Orange"
  "#e67e22" "Carrot"
  "#d35400" "Pumpkin"
  "#e74c3c" "Alizarin"
  "#c0392b" "Pomegranate"
  "#ecf0f1" "Clouds"
  "#bdc3c7" "Silver"
  "#95a5a6" "Concrete"
  "#7f8c8d" "Asbestos")
