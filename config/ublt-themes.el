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
;;; TODO: This is tightly integrated with the fonts used (Fira Sans,
;;; Fantasque...). Change that maybe?

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

;;; TODO: Check if the built-in face `fixed-pitch' can be used instead
(defface ublt/default-fixed-width
  '((t (:inherit default)))
   "")

(defface ublt/default-variable-width
  '((t (:inherit variable-pitch)))
  "")

(defvar ublt/text-scale-fw-remapping nil)
(make-variable-buffer-local 'ublt/text-scale-fw-remapping)

(defvar ublt/text-scale-vw-remapping nil)
(make-variable-buffer-local 'ublt/text-scale-vw-remapping)

(defadvice text-scale-mode (after scale-base-faces activate)
  "Additionally scale other base faces so that all faces are
scaled. This \"base face\" trick is used by `ublt-themes'."
  (let ((ratio (car (last text-scale-mode-remapping))))
    (when ublt/text-scale-fw-remapping
      (face-remap-remove-relative ublt/text-scale-fw-remapping))
    (when ublt/text-scale-vw-remapping
      (face-remap-remove-relative ublt/text-scale-vw-remapping))
    (when ratio
      (setq ublt/text-scale-fw-remapping
            (face-remap-add-relative 'ublt/default-fixed-width
                                     :height ratio))
      (setq ublt/text-scale-vw-remapping
            (face-remap-add-relative 'ublt/default-variable-width
                                     :height ratio)))
    (force-window-update (current-buffer))))


;;;###autoload
(defun color-theme-ubolonton-dark ()
  "Mix of Blackboard & Succulent color themes.

  Created 2010-05-29 by Nguyễn Tuấn Anh. Works in GUI and
  256-color terminals, but the latter is not recommended."
  (interactive)
  (flet ((find-color (name)
                     (nth (if window-system 1 2)
                          (assoc name ublt/colors))))
    (let* ( ;; (variable-pitch-family (face-attribute 'variable-pitch :family))
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
           (normal-hl    `(:background ,bg+2))
           (strong-hl    `(:background ,bg+3))
           (special-hl   `(:background ,blue-3))

           (strong       `(:foreground ,fg+1))

           (note         `(:foreground ,purple)) ; meta?

           (status       `(:background ,cyan-2))

           (reset        `(:weight normal :slant normal :underline nil :box nil
                                   :strike-through nil :inverse-video nil :overline nil))

           ;; FIX: Using this make faces unscalable
           ;; Fixed-width font
           (fw0           `(
                            :font ,(face-attribute 'default :font)
                            :fontset ,(face-attribute 'default :fontset)
                            ;; :weight ,(face-attribute 'default :weight)
                            ;; :height ,(face-attribute 'default :height)
                            ))
           ;; Variable-width font
           (vw0           `(
                            :fontset ,(face-attribute 'variable-pitch :fontset)
                            :font ,(face-attribute 'variable-pitch :font)
                            ;; :weight ,(face-attribute 'variable-pitch :weight)
                            ;; :height ,(face-attribute 'variable-pitch :height)
                            ))

           ;; Mixins
           (fw             '(:inherit ublt/default-fixed-width))
           (vw             '(:inherit ublt/default-variable-width))
           (vw-italic      `(,@vw :weight light :slant italic))

           ;; (fheight      (face-attribute 'default :height))
           ;; (ffontset     (face-attribute 'default :fontset))
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

         (default ((t (,@fw0))))
         ;; FIX: Height should be font-dependent in general
         (variable-pitch ((t (,@vw0 :foreground ,fg-1))))

         ;; Most faces that wish to always use
         ;; fixed-width/variable-width font should inherit these, not
         ;; `default', which gets font remapped. Because we do want
         ;; these faces to participate in scale remapping, we use
         ;; dynamic inheritance instead of static mixin, so that it's
         ;; suffice to scale remap 2 faces. You may wonder where
         ;; is is desirable to mix fixed-width/variable-width fonts;
         ;; Org-mode, markdown, info... and to a lesser degree, html
         ;; code. Basically places that mix prose and code.
         (ublt/default-fixed-width
          ((t (,@fw0))))
         (ublt/default-variable-width
          ((t (,@vw0))))

         (border-glyph ((t (nil))))     ; What's this?
         (buffers-tab ((t (:background ,bg :foreground ,fg)))) ; What's this?
         (shadow ((t ,dimmed)))

         ;; Code highlighting
         (font-lock-builtin-face
          ((t (,@fw ,@constant))))
         (font-lock-comment-face
          ((t (,@vw-italic ,@note))))
         (font-lock-comment-delimiter-face
          ((t (:inherit font-lock-comment-face ,@shadowed))))
         (font-lock-doc-string-face
          ((t (,@fw ,@doc))))
         (font-lock-function-name-face
          ((t (,@fw ,@essence))))
         (font-lock-keyword-face
          ((t (,@fw ,@power))))
         (font-lock-reference-face
          ((t (,@fw ,@reference))))     ; TODO What's this?
         (font-lock-regexp-grouping-backslash
          ((t (,@fw :foreground ,bg+1))))
         (font-lock-regexp-grouping-construct
          ((t (,@fw :foreground ,orange+1))))
         (font-lock-string-face
          ((t (,@fw ,@string))))
         (font-lock-doc-face
          ((t (,@vw-italic ,@string))))
         (font-lock-type-face
          ((t (,@fw ,@type))))
         (font-lock-preprocessor-face
          ((t (,@fw ,@raw))))
         (font-lock-variable-name-face
          ((t (,@fw ,@mutable))))
         (font-lock-warning-face
          ((t (,@fw ,@warning))))
         (font-lock-constant-face
          ((t (,@fw ,@constant))))      ; TODO: Different shade
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
          ((t (,@prompt :bold t))))
         (left-margin ((t (nil))))
         (toolbar ((t (nil))))
         (fringe ((t (:foreground ,fg-2 ,@fw))))
         (vertical-border ((t (:foreground ,bg+2))))
         (link ((t (,@portal :underline ,bg+3))))
         (match ((t ,strong-hl)))
         (escape-glyph                  ; Special characters
          ((t (,@prompt :bold t))))     ; TODO
         (button
          ((t (:inherit link))))

         ;; Highlighting
         (region
          ((t (,@normal-hl :inherit t))))           ; selection
         (secondary-selection
          ((t ,special-hl)))
         (hl-line
          ((t ,dimmed-hl)))             ; line highlighting
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
         ;; FIX: `idle-highlight' uses `highlight-regexp' which sets
         ;; instead of merging face. Fix that. Merging is better
         (idle-highlight
          ((t ,normal-hl)))
         (highlight-indentation-face
          ((t (:strike-through ,bg+1))))

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
          ((t (:bold t :height 1.0))))
         (mode-line-highlight
          ((t (:inherit mode-line))))
         (which-func
          ((t (:foreground ,red-2 :height 1.0 :bold t))))
         (anzu-mode-line
          ((t (:foreground ,red-2 :weight bold))))

         ;; dired, dired+
         (diredp-file-name
          ((t (:inherit default))))     ; TODO
         (diredp-dir-heading
          ((t (:foreground ,gold-1 :bold t)))) ; TODO
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

         (comint-highlight-prompt
          ((t (,@type))))
         (comint-highlight-input
          ((t (:background ,bg+1 :foreground ,fg-1))))
         (cider-repl-prompt-face
          ((t (:inherit font-lock-constant-face))))
         (cider-repl-input-face
          ((t (:inherit comint-highlight-input))))
         (cider-repl-result-face
          ((t (,@constant))))           ; TODO

         ;; Auto-complete & popup
         (ac-completion-face
          ((t (,@fw ,@dimmed-hl :foreground ,fg-2 :slant normal :weight normal))))
         (ac-candidate-face
          ((t (,@fw ,@normal-hl :foreground ,fg :slant normal :weight normal))))
         (ac-selection-face
          ((t (,@fw ,@strong-hl ,@strong :slant normal :bold t))))
         (ac-slime-menu-face
          ((t (:inherit ac-candidate-face))))
         (ac-slime-selection-face
          ((t (:inherit ac-selection-face))))
         (popup-face
          ((t (:inherit ac-candidate-face))))
         (popup-tip-face
          ((t (,@fw ,@dimmed-hl ,@string))))
         (popup-summary-face
          ((t (:inherit popup-face ,@note))))

         (company-tooltip
          ((t (,@fw ,@strong-hl :foreground ,cyan-1))))
         (company-tooltip-selection
          ((t (:inherit company-tooltip :foreground ,cyan :background ,fg-3))))
         (company-tooltip-common
          ((t (:inherit company-tooltip ,@mutable))))
         (company-tooltip-common-selection
          ((t (:inherit company-tooltip-selection ,@param))))
         (company-scrollbar-bg
          ((t (:background ,fg-3))))
         (company-scrollbar-fg
          ((t (:background ,green-2))))
         (company-preview
          ((t (:foreground ,fg-3))))
         (company-preview-common
          ((t (:inherit company-preview))))
         (company-tooltip-annotation
          ((t (:inherit company-tooltip ,@power))))

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
         (flx-highlight-face
          ((t (:foreground ,blue-1 :weight bold))))

         ;; org-mode
         ;; TODO: levels
         (org-document-title
          ((t (,@string :height 2.0 :bold t))))
         (org-special-keyword
          ((t (,@constant))))
         (org-indent
          ((t ())))
         ;; We use `normal' not `bold' for these because we use Fira Sans
         (org-level-1
          ((t (,@constant :weight normal :height 1.4))))
         (org-level-2
          ((t (,@mutable :weight normal :height 1.2))))
         (org-level-3
          ((t (,@string :weight normal :height 1.1))))
         (org-level-4
          ((t (:foreground ,cyan+1 :weight normal :height 1.0))))
         (org-level-5
          ((t (,@param))))
         (org-level-6
          ((t (:foreground ,yellow-1))))
         (org-level-7
          ((t (:foreground ,green-3))))
         (org-level-8
          ((t (,@note))))
         (org-table
          ((t (,@fw :foreground ,green-3)))) ; TODO
         (org-formula
          ((t (,@fw ,@param))))
         (org-hide
          ((t (:foreground ,bg))))
         (org-code
          ((t (:inherit font-lock-builtin-face)))) ; TODO
         (org-verbatim
          ((t (:inherit font-lock-keyword-face))))
         (org-meta-line
          ((t (,@fw ,@context))))
         (org-document-info-keyword
          ((t (:inherit org-meta-line))))
         (org-mode-line-clock
          ((t (:foreground ,red-2 :bold t)))) ; TODO
         (org-link
          ((t (:inherit link))))
         (org-date
          ((t (,@fw :foreground ,cyan :underline t)))) ; TODO
         (org-todo
          ((t ,commitment)))
         (org-done
          ((t (:foreground ,green-3)))) ; TODO
         ;; FIX: This makes comment in code block fixed-width :(
         (org-block
          ((t (,@fw))))
         (org-block-background
          ((t (,@fw))))
         (org-block-begin-line
          ((t (,@fw ,@shadowed))))
         (org-block-end-line
          ((t (:inherit org-block-begin-line))))
         (org-checkbox
          ((t (,@fw :weight bold :box (:line-width 1 :style released-button :color ,bg)))))
         (org-time-grid
          ((t (,@fw :foreground ,gold-1))))
         (org-agenda-structure
          ((t (,@fw :foreground "LightSkyBlue"))))
         (org-agenda-date-today
          ((t (:inherit org-agenda-date :underline t))))
         (org-agenda-date-weekend
          ((t (:inherit org-agenda-date :slant italic))))
         (org-agenda-current-time
          ((t (,@fw :inherit org-time-grid :background ,bg+2))))
         (org-scheduled
          ((t (,@vw-italic :foreground ,green-2))))
         (org-scheduled-previously
          ((t (,@fw :foreground "Chocolate1" :slant italic))))
         (org-scheduled-today
          ((t (,@vw :foreground ,green-3))))
         (org-column
          ((t (,@fw :slant normal))))

         ;; TODO: Make org/markdown share most faces
         (markdown-link-face
          ((t (,@teleport))))
         (markdown-url-face
          ((t (:inherit org-link))))
         (markdown-url-face
          ((t (:inherit org-link))))
         (markdown-header-delimiter-face
          ((t (,@dimmed))))
         (markdown-header-face-1
          ((t (:inherit org-level-1))))
         (markdown-header-face-2
          ((t (:inherit org-level-2))))
         (markdown-header-face-3
          ((t (:inherit org-level-3))))
         (markdown-header-face-4
          ((t (:inherit org-level-4))))
         (markdown-header-face-5
          ((t (:inherit org-level-5))))
         (markdown-header-face-6
          ((t (:inherit org-level-6))))
         (markdown-list-face
          ((t (,@mutable :weight bold))))
         (markdown-pre-face
          ((t (,@fw ,@dimmed-hl ,@string)))) ;TODO
         (markdown-bold-face
          ((t (,@mutable :weight bold))))
         (markdown-italic-face
          ((t (,@mutable :slant italic))))

         (rst-level-1
          ((t (:inherit org-level-1))))
         (rst-level-2
          ((t (:inherit org-level-2))))
         (rst-level-3
          ((t (:inherit org-level-3))))
         (rst-adornment
          ((t (,@context))))
         (rst-block
          ((t (:inherit markdown-list-face))))
         (rst-literal
          ((t (:inherit org-code :weight bold)))) ;TODO
         (rst-reference
          ((t (:inherit org-code))))    ;TODO
         (rst-definition
          ((t (:inherit org-link))))
         (rst-emphasis1
          ((t (:inherit markdown-italic-face))))
         (rst-emphasis2
          ((t (:inherit markdown-bold-face))))

         (markup-title-0-face
          ((t (:inherit org-document-title))))
         (markup-title-1-face
          ((t (:inherit org-level-1))))
         (markup-title-2-face
          ((t (:inherit org-level-2))))
         (markup-title-3-face
          ((t (:inherit org-level-3))))
         (markup-title-4-face
          ((t (:inherit org-level-4))))
         (markup-title-5-face
          ((t (:inherit org-level-5))))
         (markup-internal-reference-face
          ((t (:inherit org-link))))
         (markup-typewriter-face
          ((t (:inherit org-code))))
         (markup-code-face
          ((t (:inherit markdown-pre-face))))
         (markup-list-face
          ((t (:inherit markdown-list-face))))

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
         (ublt/lisp-paren-face
          ((t ,dimmed)))
         ;; TODO: Remove
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
         (diff-refine-added
          ((t (:inherit diff-added ,@normal-hl))))
         (diff-refine-removed
          ((t (:inherit diff-removed ,@normal-hl))))
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

         ;; XXX FIX ediff: `ediff-util' adds another overlay with face
         ;; `default' on top of the refinement overlay to cancel out
         ;; the refinement (by overriding) when the cursor is out.
         ;; This also overrides other faces. It should have removed
         ;; the overlay, or unset the overlay's face, or whatever.

         ;; ediff
         ;; I think I lost my vision of "small semantic color set"
         ;; here. Semantic maybe but small, probably not. So how do we
         ;; actually organize them?
         ;; (ediff-current-diff-A
         ;;  ((t (:background "#2C1320")))) ; #2C1320
         ;; (ediff-current-diff-B
         ;;  ((t (:background "#0C3320")))) ; #002200
         ;; (ediff-current-diff-C
         ;;  ((t (:background "#2C3320"))))
         ;; (ediff-current-diff-Ancestor
         ;;  ((t (:background "#0C1350"))))
         ;; ;; (ediff-fine-diff-A
         ;; ;;  ((t (:background "#5C3340" :weight bold))))
         ;; ;; (ediff-fine-diff-B
         ;; ;;  ((t (:background "#1C6340" :weight bold))))
         ;; ;; (ediff-fine-diff-C
         ;; ;;  ((t (:background "#5C6340" :weight bold))))
         ;; ;; (ediff-fine-diff-Ancestor
         ;; ;;  ((t (:background "#1C2360" :weight bold))))
         ;; (ediff-fine-diff-A
         ;;  ((t (:background "#3C1E2F"))))
         ;; (ediff-fine-diff-B
         ;;  ((t (:background "#15442F"))))
         ;; (ediff-fine-diff-C
         ;;  ((t (:background "#3C442F"))))
         ;; (ediff-fine-diff-Ancestor
         ;;  ((t (:background "#151E63"))))
         (ublt/ediff-1
          ((t (,@normal-hl))))
         (ublt/ediff-2
          ((t (,@strong-hl))))
         (ediff-even-diff-A
          ((t (:inherit ublt/ediff-1))))
         (ediff-even-diff-B
          ((t (:inherit ublt/ediff-1))))
         (ediff-even-diff-C
          ((t (:inherit ublt/ediff-2))))
         (ediff-even-diff-Ancestor
          ((t (:inherit ublt/ediff-2))))
         (ediff-odd-diff-A
          ((t (:inherit ublt/ediff-2))))
         (ediff-odd-diff-B
          ((t (:inherit ublt/ediff-2))))
         (ediff-odd-diff-C
          ((t (:inherit ublt/ediff-1))))
         (ediff-odd-diff-Ancestor
          ((t (:inherit ublt/ediff-1))))

         ;; HSV (V)
         ;; orig: 17 20 20 31
         ;; low: 13 15 15 25
         ;; hi: 21 22 25 36
         (ediff-current-diff-A
          ((t (:background "#210E18"))))
         (ediff-current-diff-B
          ((t (:background "#092618"))))
         (ediff-current-diff-C
          ((t (:background "#212618"))))
         (ediff-current-diff-Ancestor
          ((t (:background "#090F3D"))))
         (ediff-fine-diff-A
          ((t (:background "#361727"))))
         (ediff-fine-diff-B
          ((t (:background "#0D3823"))))
         (ediff-fine-diff-C
          ((t (:background "#374028"))))
         (ediff-fine-diff-Ancestor
          ((t (:background "#0E165C"))))

         ;; magit
         (magit-item-highlight
          ((t (,@dimmed-hl))))
         (magit-section-title
          ((t (,@vw ,@mutable :weight bold)))) ; TODO
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
          ((t (,@fw ,@commitment))))
         (magit-menu-selected-option
          ((t (:foreground ,green))))   ; TODO What is this?
         (magit-item-mark
          ((t (:inherit secondary-selection))))
         (magit-tag
          ((t (:foreground ,green-2 :box ,bg+3)))) ; TODO
         (magit-log-head-label-tags
          ((t (:inherit magit-tag))))
         (magit-log-author
          ((t (,@vw ,@context))))
         (magit-log-date
          ((t (,@vw ,@dimmed))))
         (magit-log-author-date-cutoff
          ((t (:inherit magit-log-author :weight bold))))
         (magit-log-message
          ((t (,@vw-italic))))
         (magit-key-mode-switch-face
          ((t (:inherit font-lock-type-face))))
         ;; TODO
         (magit-blame-header
          ((t (:inherit magit-header :background ,bg+2 :foreground ,fg))))
         ;; (magit-blame-sha1
         ;;  ((t ())))
         ;; (magit-blame-culprit
         ;;  ((t ())))
         ;; (magit-blame-time
         ;;  ((t ())))
         ;; (magit-blame-subject
         ;;  ((t ())))

         ;; (git-commit-summary-face
         ;;  ((t (:height 1.05 :weight bold))))
         (git-commit-summary-face
          ((t (:inherit magit-log-message))))
         (git-commit-overlong-summary-face
          ((t (:inherit git-commit-summary-face :foreground ,red))))
         (git-commit-nonempty-second-line-face
          ((t (:inherit (git-commit-summary-face ublt/flycheck-message-face)))))

         ;; info
         (info-title-1
          ((t (:inherit org-level-1))))
         (info-title-2
          ((t (:inherit org-level-2))))
         (info-title-3
          ((t (:inherit org-level-3))))
         (info-title-4
          ((t (:inherit org-level-4))))
         (info-menu-header
          ((t (:weight bold))))
         (info-xref
          ((t (,@portal :weight bold))))
         (info-xref-visited
          ((t (:inherit info-xref ,@note))))
         (info-quoted-name
          ((t (,@fw ,@constant))))
         (info-single-quote
          ((t ,constant)))
         (info-string
          ((t (,@doc :weight bold :slant italic))))
         (info-reference-item
          ((t (,@fw ,@mutable :weight bold :height 1.1))))
         (ublt/info-ref-item
          ((t (:weight bold :slant italic :height 0.7))))
         (info-function-ref-item
          ((t (,@essence :inherit ublt/info-ref-item))))
         (info-constant-ref-item
          ((t (,@constant :inherit ublt/info-ref-item))))
         (info-user-option-ref-item
          ((t (,@param :inherit ublt/info-ref-item :slant normal))))
         (info-variable-ref-item
          ((t (,@mutable :inherit ublt/info-ref-item))))
         (info-macro-ref-item
          ((t (,@power :inherit ublt/info-ref-item))))
         (info-special-form-ref-item
          ((t (,@power :inherit ublt/info-ref-item :slant normal))))
         (info-command-ref-item
          ((t (,@type :inherit ublt/info-ref-item))))

         (apropos-symbol
          ((t (:inherit info-reference-item))))
         (apropos-function-button
          ((t (:inherit info-function-ref-item))))
         (apropos-variable-button
          ((t (:inherit info-variable-ref-item))))
         (apropos-misc-button
          ((t (:inherit info-constant-ref-item))))

         ;; Null out most attributes, because it seems to inherit
         ;; the face of each line's first character.
         (linum
          ((t (:inherit fringe ,@dimmed :slant normal :weight normal
                        :underline nil :strike-through nil :overline nil
                        :background ,bg :box nil))))
         (linum-relative-current-face
          ((t (:inherit linum ,@dimmed-hl :foreground ,fg-3 :weight bold))))

         ;; erc
         (erc-notice-face
          ((t ,dimmed)))
         (erc-nick-default-face
          ((t ,string)))                ; TODO
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

         ;; helm
         (helm-header
          ((t (,@mutable :bold t))))    ; TODO
         (helm-source-header
          ((t (,@vw ,@mutable :weight bold :height 1.5)))) ; TODO
         ;; XXX: Maybe I just don't believe in themes with narrow
         ;; selection of colors
         (helm-selection
          ((t (:inherit secondary-selection))))
         (helm-selection-line
          ((t (:inherit secondary-selection))))
         (helm-match
          ((t (:foreground ,yellow-1 :weight bold))))
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
          ((t ,reference)))             ; TODO
         (helm-grep-finish
          ((t ,doc)))                   ; TODO
         (helm-swoop-target-line-face
          ((t (:inherit secondary-selection))))
         (helm-swoop-target-line-block-face
          ((t (:inherit secondary-selection))))
         ;; TODO
         (helm-swoop-target-word-face
          ((t ,normal-hl)))
         (helm-action
          ((t (,@vw :height 1.1))))
         (helm-buffer-directory
          ((t (:inherit helm-ff-directory))))

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
         (web-mode-param-name-face
          ((t (:inherit font-lock-constant-face))))
         (web-mode-html-attr-equal-face
          ((t ,context)))
         (web-mode-html-tag-face
          ((t (:inherit font-lock-builtin-face))))
         (web-mode-part-face
          ((t (:background ,bg+1))))
         (web-mode-block-face
          ((t (:background ,bg+1))))
         (web-mode-block-delimiter-face
          ((t (:inherit web-mode-block-face :foreground ,bg+3))))
         (web-mode-css-selector-face
          ((t (:inherit font-lock-function-name-face))))
         (web-mode-current-element-highlight-face
          ((t (:inherit highlight))))
         (web-mode-comment-keyword-face
          ((t (:inherit font-lock-warning-face))))
         (web-mode-html-tag-bracket-face
          ((t (,@fw ,@context))))

         (nxml-element-local-name
          ((t (,@fw ,@constant))))
         (nxml-element-prefix
          ((t (,@fw ,@context))))
         (nxml-tag-delimiter
          ((t (,@fw ,@dimmed))))
         ;; (nxml-element-local-name
         ;;  ((t ,portal)))                ; TODO


         (help-argument-name
          ((t (:foreground ,blue-1))))  ; TODO

         ;; Manual pages
         (woman-bold
          ((t (,@constant :weight bold)))) ; TODO
         (woman-italic
          ((t (:foreground ,cyan+1))))
         (woman-addition
          ((t ,mutable)))               ; TODO
         (woman-unknown
          ((t (:foreground ,red-4))))   ; TODO
         (Man-overstrike
          ((t (:inherit woman-bold))))
         (Man-underline
          ((t (:inherit woman-italic))))

         ;; undo-tree
         (undo-tree-visualizer-default-face
          ((t ,dimmed)))
         (undo-tree-visualizer-current-face
          ((t ,essence)))
         (undo-tree-visualizer-active-branch-face
          ((t ,string)))                ; TODO

         ;; ???
         (hexl-address-region
          ((t (:foreground ,blue-1))))  ; TODO
         (hexl-ascii-region
          ((t (:foreground ,blue-1))))  ; TODO

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
          ((t (:underline nil :box nil
                          :strike-through nil :inverse-video nil :overline nil
                          :background ,bg :foreground "Yellow"
                          :weight normal))))
         (ace-jump-face-background
          ((t (:foreground ,bg+3))))

         (flymake-errline
          ((t ,error-hl)))
         (flymake-warnline
          ((t (:underline ,yellow-1))))
         (flycheck-error
          ((t ,error-hl)))

         ;; My own custom faces
         (ublt-twitter-meta-face
          ((t (:height 0.9 ,@shadowed))))
         (ublt/flycheck-message-face
          ((t (,@vw-italic ,@commitment)))) ; TODO
         (eproject-ido-imenu-file-path
          ((t ,shadowed)))
         (ublt/emms-mode-line-face
          ((t (:height 1.0))))
         (ublt/mode-line-major-mode
          ((t (:bold t))))
         (ublt/evil-emacs-tag
          ((t (:foreground ,blue-1))))
         (ublt/evil-motion-tag
          ((t (:foreground ,red-2 :bold t))))
         (ublt/evil-normal-tag
          ((t (:foreground ,red-2 :bold t))))
         (ublt/evil-insert-tag
          ((t (:foreground ,bg :bold t))))
         (ublt/evil-visual-tag
          ((t (:foreground ,red-2 :bold t))))

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

      ;; ;; Color theme seems to mess this up, restore it
      ;; ;; (message "After %s" (face-attribute 'default :height))
      ;; ;; (message "%s" fw)
      ;; (set-face-attribute 'default nil :height fheight)
      ;; ;; This does not work actually. Emacs is bad at handling default vs. normal
      ;; ;; face/font/fontset.
      ;; (set-face-attribute 'default nil :fontset ffontset)
      ;; ;; This does not seem to be messed up anymore
      ;; ;; (set-face-font 'variable-pitch variable-pitch-family)
      ;; ;; (message "Done %s" (face-attribute 'default :height))

      ;; (setq
      ;;  hl-paren-colors `("Orange" ,yellow "Greenyellow"
      ;;                    ,green "Springgreen" "Cyan"
      ;;                    ,blue-2 "Magenta" "Purple"
      ;;                    "Orange" ,yellow "Greenyellow"
      ;;                    ,green "Springgreen" "Cyan"
      ;;                    ,blue-2 "Magenta" "Purple"))

      (setq
       hl-paren-colors
       `("#00FF00"
         "#00DD00"
         "#00BB00"
         "#009900"
         "#007700"
         "#005500"
         ))
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
