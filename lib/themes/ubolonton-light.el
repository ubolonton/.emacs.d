;;; XXX: Don't look here, obsolete & not maintained
;; Ubolonton's personal light color theme for Emacs.
;;
;; A mix of Blackboard and Succulent themes.
;;
;; MIT License Copyright (c) 2011 Nguyen Tuan Anh <ubolonton at gmail dot com>
;; Credits:
;; - The TextMate theme Blackboard
;; - The Emacs port of Blackboard theme by JD Huntington
;; - The Textmate theme Succulent
;; All patches welcome

(require 'color-theme)
(require 'color-util)                  ; color conversion

(defvar ublt/light-background (clut-yuv->str '(0.667 -0.004 -0.027))) ; was "#A0AFA8"
(defvar ublt/light-background-1 (clut-str-with-y ublt/light-background 0.60))
(defvar ublt/light-background-2 (clut-str-with-y ublt/light-background 0.55))
(defvar ublt/light-background-3 (clut-str-with-y ublt/light-background 0.50))
(defvar ublt/light-foreground (clut-yuv->str '(0.06 0.02 -0.02)))
(defvar ublt/light-foreground-1 "#293831")
(defvar ublt/light-blue "#008FDF")
(defvar ublt/light-red "Red")
(defvar ublt/light-red-1 "#F86155")
(defvar ublt/light-cyan "Cyan")
(defvar ublt/light-cyan-1 "#89A1F3")
(defvar ublt/light-green "Green")
(defvar ublt/light-green-1 "ForestGreen")
(defvar ublt/light-green-2 "SeaGreen")
(defvar ublt/light-green-3 "LightGreen")
(defvar ublt/light-blue-1 "#131B35")
(defvar ublt/light-purple "#805DBB")

;;;###autoload
(defun color-theme-ubolonton-light ()
  "Mix of Blackboard & Succulent color themes.

  Created 2010-05-29 by Nguyen Tuan Anh. No attempt has been made
  to adapt this to work in terminal."
  (interactive)
  (setq anything-c-buffers-face2 'font-lock-builtin-face) ; dirty hack
  (color-theme-install
   `(color-theme-ubolonton-light
     ((background-color . ,ublt/light-background)
      (foreground-color . ,ublt/light-foreground)
      (background-mode . light)
      (border-color . "White")
      (cursor-color . "#000000")
      (mouse-color . "Sienna1")) ;=
     (default ((t (:background ,ublt/light-background :foreground ,ublt/light-foreground))))
     (variable-pitch ((t (:background ,ublt/light-background :foreground ,ublt/light-foreground :bold t))))
     (border-glyph ((t (nil))))         ; not yet
     (buffers-tab ((t (:background ,ublt/light-background :foreground ,ublt/light-foreground)))) ; not yet

     ;; Code highlighting
     (font-lock-builtin-face
      ((t (:foreground "#0088AA"))))
     (font-lock-comment-face
      ((t (:foreground "#720FB1" :italic t))))
     (font-lock-comment-delimiter-face
      ((t (:inherit font-lock-comment-face))))
     (font-lock-doc-string-face
      ((t (:foreground "DarkOrange")))) ; not yet
     (font-lock-function-name-face
      ((t (:foreground "DarkGreen" :bold t))))
     (font-lock-keyword-face
      ((t (:foreground ,ublt/light-red-1))))
     (font-lock-preprocessor-face
      ((t (:foreground "Brown"))))
     (font-lock-reference-face
      ((t (:foreground "SlateBlue")))) ; not yet
     (font-lock-regexp-grouping-backslash
      ((t (:foreground "#E9C062")))) ; not yet
     (font-lock-regexp-grouping-construct
      ((t (:foreground ,ublt/light-red))))
     (font-lock-string-face
      ((t (:foreground ,ublt/light-green-1))))
     (font-lock-doc-face
      ((t (:inherit font-lock-string-face))))
     (font-lock-type-face
      ((t (:foreground "#00F8B7" :bold t))))
     (font-lock-variable-name-face
      ((t (:foreground "LightGoldenRod"))))
     (font-lock-warning-face
      ((t (:foreground ,ublt/light-red))))
     (font-lock-constant-face
      ((t (:inherit font-lock-builtin-face))))

     ;; Misc
     (isearch-fail
      ((t (:foreground "Red" :background "YellowGreen")))) ;=
     (gui-element
      ((t (:background "#D4D0C8" :foreground "Black")))) ; not yet
     (region
      ((t (:background ,ublt/light-background-2)))) ; selection
     (highlight
      ((t (:background ,ublt/light-background-1)))) ; line highlighting
     (highline-face
      ((t (:background ,ublt/light-green-2)))) ; not yet
     (zmacs-region
      ((t (:background "Snow" :foreground "Blue")))) ; not yet
     (text-cursor
      ((t (:background "Yellow" :foreground "Black")))) ; not yet
     (minibuffer-prompt
      ((t (:foreground ,ublt/light-cyan :bold t ))))
     (left-margin ((t (nil))))
     (toolbar ((t (nil))))
     (fringe ((t (:background ,ublt/light-background-3 :foreground "black"))))

     (mode-line
      ((t (:background "Grey55" :foreground "Black")))) ; not yet
     (modeline-inactive
      ((t (:background "#192123"))))
     (mode-line-buffer-id
      ((t (:bold t))))
     (mode-line-highlight
      ((t nil)))
     (mode-line-inactive
      ((t (:background ,ublt/light-background-2))))

     ;; SLIME debug buffer
     (sldb-topline-face
      ((t (:foreground ,ublt/light-red :bold t))))
     (sldb-condition-face
      ((t (:foreground ,ublt/light-cyan :bold t))))
     (sldb-section-face
      ((t (:foreground "Green" :bold t))))

     ;; SLIME REPL
     (slime-repl-input-face
      ((t (:foreground ,ublt/light-green-3 :bold t))))
     (slime-repl-result-face
      ((t (:foreground ,ublt/light-cyan :bold t))))
     (slime-highlight-edits-face
      ((t (:background "Gray20"))))

     ;; ido mini-buffer
     (ido-first-match
      ((t (:foreground "Green" :bold t))))
     (ido-only-match
      ((t (:inherit ido-first-match))))
     (ido-subdir
      ((t (:foreground "DarkOrange"))))

     ;; org-mode
     (org-level-1
      ((t (:foreground "Green"))))
     (org-level-2
      ((t (:foreground "LightGoldenRod"))))
     (org-level-6
      ((t (:foreground ,ublt/light-green-1 :bold t))))
     (org-table
      ((t (:foreground ,ublt/light-green-3))))
     (org-hide
      ((t (:foreground ,ublt/light-foreground))))
     (org-code ;=
      ((t (:foreground "Gray30"))))
     (org-meta-line ;=
      ((t (:foreground ,ublt/light-background-2))))
     (org-mode-line-clock ;=
      ((t (:foreground "Red4" :bold t))))

     ;; Whitespaces
     (whitespace-tab
      ((t (:background ,ublt/light-background :foreground "Aquamarine3"))))
     (whitespace-space
      ((t (:background ,ublt/light-background :foreground "Aquamarine3"))))
     (whitespace-newline
      ((t (:background ,ublt/light-background :foreground "Aquamarine3"))))

     ;; Parentheses highlighting
     (show-paren-mismatch ((t (:inherit font-lock-warning))))
     (show-paren-match ((t (:foreground "Yellow"))))
     ;; Parentheses in Lisp modes
     (esk-paren-face
      ((t (:foreground ,ublt/light-background-3 :bold t))))

     ;; flyspell ;=
     (flyspell-incorrect
      ((t (:underline ,ublt/light-red))))
     (flyspell-duplicate
      ((t (:underline "Yellow"))))

     ;; Magit
     (magit-item-highlight
      ((t (:background "#151923"))))
     (magit-section-title ;=
      ((t (:foreground "DarkGoldenRod" :bold t))))
     (magit-branch ;=
      ((t (:inherit font-lock-function-name-face))))
     (magit-diff-file-header ;=
      ((t (:foreground "Red"))))
     (magit-diff-hunk-header ;=
      ((t (:inherit font-lock-builtin-face :italic t))))
     ;; info
     (info-xref ;=
      ((t (:foreground ,ublt/light-cyan :bold t))))


     (linum
      ((t (:foreground "Grey60"))))

     (eshell-prompt ;=
      ((t (:foreground ,ublt/light-cyan))))

     (erc-notice-face ;=
      ((t (:foreground ,ublt/light-background-1))))

     (comint-highlight-input ;=
      ((t (:foreground ,ublt/light-green-3))))

     ;; dired plus ;=
     (diredp-file-name
      ((t (:inherit default))))
     (diredp-dir-heading
      ((t (:foreground "LightGoldenRod" :bold t))))
     (diredp-symlink
      ((t (:foreground ,ublt/light-red-1))))
     (diredp-no-priv
      ((t (:foreground ,ublt/light-background-2 :background ,ublt/light-background-1))))
     (diredp-read-priv
      ((t (:background ,ublt/light-green-2))))
     (diredp-write-priv
      ((t (:background "RosyBrown4"))))
     (diredp-exec-priv
      ((t (:background "Brown"))))
     (diredp-dir-priv
      ((t (:foreground ,ublt/light-green))))
     (diredp-number
      ((t (:foreground ,ublt/light-green-2))))
     (diredp-flag-mark-line
      ((t (:foreground "Cyan"))))
     (diredp-deletion
      ((t (:foreground ,ublt/light-blue-1 :background ,ublt/light-red))))
     (diredp-deletion-file-name
      ((t (:foreground ,ublt/light-red))))
     (diredp-compressed-file-suffix
      ((t (:foreground ,ublt/light-blue))))
     (diredp-file-suffix
      ((t (:foreground ,ublt/light-cyan))))
     (diredp-ignored-file-name
      ((t (:inherit font-lock-comment-face))))

     (anything-header
      ((t (:foreground "Orange" :bold t))))
     ;; rest ;=
     (anything-overlay-line-face
      ((t (:background ,ublt/light-background-2))))
     (anything-dir-priv
      ((t (:inherit diredp-dir-priv :background ,ublt/light-background-2))))
     (anything-file-name
      ((t (:inherit font-lock-type-face))))

     (escape-glyph
      ((t (:foreground "Cyan" :bold t))))

     (ecb-default-highlight-face
      ((t (:bold t :background ,ublt/light-background-2))))

     (yas/field-debug-face
      ((t (:underline "Yellow"))))
     (yas/field-highlight-face
      ((t (:background ,ublt/light-background-3))))

     (lazy-highlight
      ((t (:background ,ublt/light-background-2))))

     (mumamo-background-chunk-major
      ((t (:background ,ublt/light-background))))
     (mumamo-background-chunk-submode1
      ((t (:background ,ublt/light-background))))
     (mumamo-border-face-in
      ((t (:foreground ,ublt/light-background-3 :bold t))))
     (mumamo-border-face-out
      ((t (:foreground ,ublt/light-background-3 :bold t))))
     )))

(provide 'ubolonton-light)
