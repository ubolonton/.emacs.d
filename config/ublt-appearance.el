;;; -*- lexical-binding: t; coding: utf-8 -*-
(require 'ublt-util)

;;; Font, colors, text appearance



;;; Default size, cursor
(pcase system-type
  ('darwin (modify-all-frames-parameters
            '((left-fringe . 8)
              (right-fringe . 4)
              ;; TODO: Make this work with Mac port.
              (ns-transparent-titlebar . t)
              (ns-appearance . dark)
              (inhibit-double-buffering . nil))))
  ('windows-nt (modify-all-frames-parameters
                '((cursor-type . bar))))
  ;; In Linux the colors may be off. Use xrandr to tweak gamma system-wide.
  ('gnu/linux (modify-all-frames-parameters
               '((top . 0) (left . 0) (width . 119)
                 (fullscreen . fullheight) (cursor-type . bar)))))

;;; This is needed when redshift is running, since it overwrites
;;; xrandr's gamma settings
(ublt/in '(gnu/linux darwin)
  (defun ublt/set-gamma (g)
    (modify-all-frames-parameters
     `((screen-gamma . ,g))))

  (defun ublt/toggle-gamma ()
    (interactive)
    (ublt/set-gamma
     (if (equal 2.7 (frame-parameter nil 'screen-gamma))
         nil 2.7))))


;;; TODO: Make frame title more useful: project + buffer names
;;; XXX: https://github.com/syl20bnr/spacemacs/issues/10938
;; https://www.reddit.com/r/emacs/comments/8gaoqu/help_on_debugging_performreplace_args_out_of_range/
(defun ublt/frame-title ()
  (or buffer-file-name (buffer-name)))

(setq frame-title-format '(:eval (ublt/frame-title)))


;; Fonts
(when (display-graphic-p)
  ;; Font-mixing obsession
  (ublt/set-up
      (pcase system-type
        ('gnu/linux 'ublt-font)
        ('berkeley-unix 'ublt-font)
        ('darwin 'ublt-font-osx))
    ;; Non-code text reads better in proportional font
    (defvar ublt/disable-variable-pitch-mode nil)
    (make-local-variable 'ublt/disable-variable-pitch-mode)
    ;; This isn't pretty, but the alternative is using a variable-pitch
    ;; font as the default, and creating a `fixed-pitch-mode'.
    (defun ublt/variable-pitch-mode-maybe ()
      ;; `http://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks'
      (when (not ublt/disable-variable-pitch-mode)
        (variable-pitch-mode +1))
      (add-hook 'hack-local-variables-hook
                (lambda ()
                  (when ublt/disable-variable-pitch-mode
                    (variable-pitch-mode -1)))
                nil t))
    (defun ublt/variable-pitch-mode-fundamental ()
      (when (eq major-mode 'fundamental-mode)
        (ublt/variable-pitch-mode-maybe)))
    (dolist (hook '(erc-mode-hook
                    text-mode-hook
                    Info-mode-hook
                    helpful-mode-hook
                    help-mode-hook
                    apropos-mode-hook
                    ess-help-mode-hook
                    Man-mode-hook woman-mode-hook
                    twittering-mode-hook
                    skype--chat-mode-hook
                    org-mode-hook
                    markdown-mode-hook
                    html-mode-hook
                    git-commit-mode-hook
                    twittering-edit-mode-hook
                    package-menu-mode-hook))
      (add-hook hook 'ublt/variable-pitch-mode-maybe))
    (add-hook 'find-file-hook 'ublt/variable-pitch-mode-fundamental)
    (dolist (hook '(hexl-mode
                    dns-mode-hook
                    cider-browse-spec-view-mode-hook))
      (add-hook hook (ublt/off variable-pitch-mode)))
    ))


;;;Color theme
(ublt/with-defer
  (use-package zenburn-theme)
  (use-package solarized-theme))
(use-package monokai-theme)
(when (y-or-n-p "Load theme?")
  (if (window-system)
      (ublt/set-up 'ublt-dark-theme
        (load-theme 'ublt-dark t))
    (load-theme 'monokai t)))


;;; Whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; FIX: `whitespace-mode' faces inherit the default face, thus would
;; not work with mixed fixed-width/variable-width fonts (which means
;; most of my buffer now!). It's the same situation with
;; `idle-highlight'. For these modes to be useful some workarounds are
;; needed, probably using overlays.

;; `http://xahlee.org/emacs/whitespace-mode.html'
(setq whitespace-style
      '(face tabs newline space-mark tab-mark newline-mark))
(setq whitespace-display-mappings
      '(
        (space-mark 32 [?\ ] [46])           ; whitespace
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
                                        ;   (newline-mark ?\n [8629 ?\n] [182 ?\n]) ; new-line
        (newline-mark ?\n [?¶ ?\n] [182 ?\n]) ; new-line
                                        ;   (tab-mark ?\t [9654 ?\t] [92 ?\t])         ; tab
        (tab-mark ?\t [?» ?\t] [92 ?\t])         ; tab
      ))


;;; Code folding

;;; FIX: Figure out why `web-mode' does not highlight code if this
;; section is not present, even though it complains about not
;; supporting `hideshow'.
;;; XXX: This makes terminal Emacs hang, so only use if there's a
;; window system
(use-package hideshow
  :when window-system
  :custom ((hs-hide-comments-when-hiding-all nil)
           (hs-isearch-open t))
  :config (define-advice hs-toggle-hiding (:around (f &rest args) ublt/keep-point)
            "Try to keep point after toggling."
            (save-excursion
              (apply f args))))

;; (ublt/set-up 'hideshowvis
;;   (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
;;   (defcustom hs-fringe-face 'hs-fringe-face
;;     "*Specify face used to highlight the fringe on hidden regions."
;;     :type 'face
;;     :group 'hideshow)
;;   (defface hs-fringe-face
;;     '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
;;     "Face used to highlight the fringe on folded regions"
;;     :group 'hideshow)
;;   (defcustom hs-face 'hs-face
;;     "*Specify the face to to use for the hidden region indicator"
;;     :type 'face
;;     :group 'hideshow)
;;   (defface hs-face
;;     '((t (:background "#ff8" :box t)))
;;     "Face to hightlight the ... area of hidden regions"
;;     :group 'hideshow)
;;   (defun display-code-line-counts (ov)
;;     (when (eq 'code (overlay-get ov 'hs))
;;       (let* ((marker-string "*fringe-dummy*")
;;              (marker-length (length marker-string))
;;              (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
;;              )
;;         (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
;;         (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
;;         (overlay-put ov 'before-string marker-string)
;;         (put-text-property 0 (length display-string) 'face 'hs-face display-string)
;;         (overlay-put ov 'display display-string)
;;         )))
;;   (setq hs-set-up-overlay 'display-code-line-counts
;;         hideshowvis-ignore-same-line nil))


;;; Empty lines at the end of a buffer. TODO: Use this once it doesn't
;; mess with comint modes anymore
;; (ublt/set-up 'vim-empty-lines-mode
;;   (global-vim-empty-lines-mode +1))


(defun ublt/toggle-fullscreen ()
  "Tested only in X."
  (interactive)
  (let* ((types '(fullboth fullheight ;; fullwidth
                           ))
         (i (get this-command 'pos))
         (N (length types)))
    (setq i (if i (% i N) 0))
    (modify-frame-parameters nil (list (cons 'fullscreen (nth i types))))
    (message "Fullscreen: %s" (nth i types))
    (put this-command 'pos (% (1+ i) N))))


;;; Sometimes we need to see through
(defun ublt/toggle-alpha ()
  (interactive)
  (let ((a (frame-parameter nil 'alpha)))
    (if (or (not (numberp a)) (= a  100))
        (set-frame-parameter nil 'alpha 5)
      (set-frame-parameter nil 'alpha 100))))


;;; Text wrapping.
;;; Looks like only tweaking `visual-line-mode' is needed (I was
;;; probably confused by global/local variables)

(setq-default
 ;; XXX: `helm-popup-frame' is shown during a blocking operation. This allows "hiding" it.
 frame-alpha-lower-limit 0
 ;; This affects how lines are wrapped (we want wrapping at word
 ;; boundary not in the middle of a word)
 word-wrap t
 ;; Is this really necessary? (maybe, because visual-line-mode is off
 ;; by default)
 truncate-lines t)

;;; This seems to be a superset of word-wrap?
(global-visual-line-mode -1)

;;; Don't wrap where alignment is important
(dolist (hook '(archive-zip-mode-hook
                dired-mode-hook
                sql-mode-hook
                org-agenda-mode-hook))  ; XXX: not working
  (add-hook hook (ublt/off visual-line-mode)))

;;; Always wrap where text should flow.
(use-package simple
  :ensure nil :straight nil
  :hook ((twittering-mode markdown-mode org-mode html-mode) . visual-line-mode))


;;; Sometimes buffers have the same names
;; from `http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html'
(use-package uniquify
  ;; FIX: `straight.el' doesn't seem to recognize some built-in packages.
  :ensure nil :straight nil
  :custom ((uniquify-buffer-name-style 'reverse)
           (uniquify-separator "  ")
           ;; Rename after killing uniquified
           (uniquify-after-kill-buffer-p t)
           ;; Don't muck with special buffers
           (uniquify-ignore-buffers-re "^\\*")))


;;; mode-line appearance
(defgroup ubolonton nil ""
  :group 'personal)

(defface ublt/mode-line-major-mode
  '((t :bold t))
  "Face for mode-line major mode string")
(defface ublt/mode-line-clock
  '((t :bold t))
  "Face for mode-line clock")

(use-package powerline
  :custom (mode-line-format '("%e" (:eval (ublt/powerline))))
  ;; FIX: This code duplicates powerline, which duplicates Emacs'
  :config
  (defpowerline ublt/powerline-narrow-indicator
    (let (real-point-min real-point-max)
      (save-excursion
        (save-restriction
          (widen)
          (setq real-point-min (point-min)
                real-point-max (point-max))))
      (when (or (/= real-point-min (point-min))
                (/= real-point-max (point-max)))
        (propertize "η"
                    'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: Remove narrowing from the current buffer"
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 'mode-line-widen)))))

  (defpowerline ublt/powerline-clock
    (concat "["
            (propertize (format-time-string "%H:%M %d/%m")
                        'face 'ublt/mode-line-clock
                        'help-echo (concat (format-time-string "%c") "; "
                                           (emacs-uptime "Uptime: %hh")))
            "]"))

  ;; Show buffer position with Nyan Cat.
  (use-package nyan-mode
    :demand t
    :when window-system
    :custom (nyan-bar-length 24))

  (unless (functionp 'nyan-create)
    (defun nyan-create () ""))

  ;; XXX: A modified `all-the-icons-icon-for-mode'
  (defun ublt/buffer-icon (&rest arg-override)
    (if (and (buffer-file-name)
             (all-the-icons-auto-mode-match?))
        (apply 'all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) arg-override)
      (apply 'all-the-icons-icon-for-mode major-mode arg-override)))

  (defun ublt/powerline ()
    (let* ((active (powerline-selected-window-active))
           (mode-line (if active 'mode-line 'mode-line-inactive)))
      (list (powerline-raw mode-line-mule-info)
            (powerline-raw mode-line-remote)
            (powerline-raw mode-line-modified)

            (powerline-raw " ")
            (powerline-buffer-size)

            (powerline-raw " ")
            (powerline-raw mode-line-buffer-identification)

            (powerline-raw " ")
            (ublt/powerline-narrow-indicator)

            (powerline-raw " ")
            (powerline-raw evil-mode-line-tag)

            (powerline-raw " ")
            (powerline-raw "%4l" 'fixed-pitch) ; line
            (powerline-raw ":" 'fixed-pitch)
            (powerline-raw "%2c" 'fixed-pitch) ; column
            (powerline-raw " " 'fixed-pitch)
            (powerline-raw (nyan-create))

            (powerline-raw " ")
            (powerline-vc)

            (powerline-raw " ")
            (powerline-raw "(")

            ;; Major mode
            (or (if active
                    (ublt/buffer-icon :v-adjust -0.1
                                      :face 'all-the-icons-dred)
                  (ublt/buffer-icon :v-adjust -0.1))
                (powerline-major-mode 'ublt/mode-line-major-mode))

            (powerline-raw " ")
            (powerline-minor-modes mode-line)
            (powerline-raw ")")

            (powerline-raw " ")
            (ublt/powerline-clock)

            (powerline-raw mode-line-misc-info)))))


;;; Make mode-line uncluttered by changing how minor modes are shown

;;; TODO: Use images (propertize "mode" 'display (find-images ...))
;;; XXX: This looks so weird
(use-package diminish
  :config
  (defun ublt/diminish (mode display-text &optional feature)
    (condition-case err
        (if feature
            (eval-after-load feature
              `(condition-case err
                   (diminish ',mode ,display-text)
                 (error (message (format "Error diminishing \"%s\": %s" ,mode err)))))
          (diminish mode display-text))
      (error (message (format "Error diminishing \"%s\": %s" mode err)))))
  '(
    ъ 1 2 3 4 5   6 7 8 9 0 х
    э б ю з н   а п с к д \
    ф щ у г ш   в р е т ы -
    ж й о л ч   и ь ц м я
    ё Б               . =
    )
  ;; mode name - displayed text - feature name (file name)
  (dolist (m '((paredit-mode               "()" paredit)
               (projectile-mode            "Пр" projectile)
               (undo-tree-mode             "" undo-tree)
               (yas-minor-mode             "яс" yasnippet)
               (flycheck-mode              " !" flycheck)
               (flymake-mode               " !" flymake)
               (flyspell-mode              " !" flyspell)
               (hs-minor-mode              " ⊕" hideshow)
               (company-mode               "" company)
               (rainbow-mode               "🌈" rainbow-mode)
               (helm-mode                  "" helm)
               (anzu-mode                  " Σ" anzu)
               (isearch                    " Σ")
               (auto-fill-function         " ⏎")
               (visual-line-mode           " ⤾")
               ;; (slime-mode              "SLIME")
               (haskell-indentation-mode   "･" haskell-indentation)
               (org-indent-mode            "" org-indent)
               (whole-line-or-region-mode  "" whole-line-or-region)
               (buffer-face-mode           "" face-remap)
               (volatile-highlights-mode   "" volatile-highlights)
               (elisp-slime-nav-mode       "" elisp-slime-nav)
               (highlight-parentheses-mode "" highlight-parentheses)
               (hi-lock-mode               "" hi-lock)
               (eldoc-mode                 "")
               (page-break-lines-mode      "")
               ))
    (pcase-let ((`(,mode ,display ,feature) m))
      (ublt/diminish mode display feature))))


;;; TODO: Use a prettification mode

;;; - `pretty-symbols': simplistic
;;; - `pretty-symbol-mode': built-in, simplistic (1 symbol ->
;;; 1 char, no conditional).
;;; - `pretty-mode': lots of mapping, not customizable, no conditional.
'(∧∧∧∧∧
  ∨∨∨∨∨
  $this->foo
  $this➞foo
  $this➝foo
  $this➜foo
  $this➔foo
  $this➛foo
  $this→foo
  ⟶⟶⟶
  $this⇾foo
  $this⤞foo)

(defun ublt/-no-ligature-support (_beg _end)
  (not (and
        (eq window-system 'mac)
        mac-auto-operator-composition-mode)))

(font-lock-add-keywords
 'web-mode `(("\\(function\\)"
              (0 (ublt/show-as ?ƒ)))))
(font-lock-add-keywords
 'ess-mode `(("\\(function\\)"
              (0 (ublt/show-as ?ƒ)))))
(font-lock-add-keywords
 'php-mode `(("\\(->\\)"
              (0 (ublt/show-as ?➛ #'ublt/-no-ligature-support)))
             ("\\(=>\\)"
              (0 (ublt/show-as ?⇒ #'ublt/-no-ligature-support)))
             ("\\(array\\)("
              (0 (ublt/show-as ?▸)))
             ("\\(function\\)"
              (0 (ublt/show-as ?ƒ)))
             ("{\\|}\\|;\\|\\$" . 'ublt/lisp-paren-face)
             ("\\(ret\\)urn"
              (0 (ublt/show-as ?▸)))
             ("ret\\(urn\\)"
              (0 (ublt/show-as ?▸)))))
(dolist (mode '(js-mode js2-mode web-mode))
  (font-lock-add-keywords
   mode `(("{\\|}\\|;" . 'ublt/lisp-paren-face)
          ("\\(function\\)"
           (0 (ublt/show-as ?ƒ)))
          ;; yield => γζ
          ("\\(yi\\)eld"
           (0 (ublt/show-as ?γ)))
          ("yi\\(eld\\)"
           (0 (ublt/show-as ?ζ)))
          ;; return => ▸▸
          ("\\(ret\\)urn"
           (0 (ublt/show-as ?▸)))
          ("ret\\(urn\\)"
           (0 (ublt/show-as ?▸))))))
(dolist (mode '(clojure-mode))
  (font-lock-add-keywords
   mode `(("{\\|}\\|;" . 'ublt/lisp-paren-face)
          ;; fn => ƒ
          ("(\\(\\<fn\\>\\)"
           (0 (ublt/show-as ?ƒ))))))
(dolist (mode '(emacs-lisp-mode))
  (font-lock-add-keywords
   mode `(("\\(lambda\\)"
           (0 (ublt/show-as ?λ))))))
(dolist (mode '(python-mode))
  (font-lock-add-keywords
   mode `(
          ;; yield => γζ
          ("\\(yi\\)eld"
           (0 (ublt/show-as ?γ)))
          ("yi\\(eld\\)"
           (0 (ublt/show-as ?ζ)))
          ("\\(lambda\\)"
           (0 (ublt/show-as ?λ))))))
(unless (eq window-system 'mac)
  (font-lock-add-keywords
   'scala-mode `(("\\(=>\\)"
                  (0 (ublt/show-as ?⇒))))))


;;; Bigger minibuffer text
(defun ublt/minibuffer-setup ()
  (setq-local face-remapping-alist '((default :height 1.0)))
  (setq line-spacing 0.3))
(add-hook 'minibuffer-setup-hook #'ublt/minibuffer-setup)


;;; Looks like `adaptive-wrap' degrades performance on large buffers (e.g. check
;;; out draft.org). Disable it for now. TODO: Find out why
;; (ublt/set-up 'adaptive-wrap
;;   ;; FIX: Don't overwrite, change the source
;;   (defun adaptive-wrap-prefix-function (beg end)
;;     "Indent the region between BEG and END with adaptive filling."
;;     (goto-char beg)
;;     (while (< (point) end)
;;       (let ((lbp (line-beginning-position)))
;;         (put-text-property (point)
;;                            (progn (search-forward "\n" end 'move) (point))
;;                            'wrap-prefix
;;                            (adaptive-wrap-fill-context-prefix lbp (point))
;;                            ;; (propertize (adaptive-wrap-fill-context-prefix lbp (point))
;;                            ;;             ;; For it to work with `variable-pitch-mode'
;;                            ;;             'face 'default)
;;                            ))))
;;   (define-advice visual-line-mode (:after (&rest _) ublt/adaptive-wrap)
;;     (if (and visual-line-mode adaptive-fill-mode)
;;         (progn
;;           (adaptive-wrap-prefix-mode +1)
;;           (auto-fill-mode -1))
;;       (adaptive-wrap-prefix-mode -1))))

;;; TODO: Move to editing
(define-advice visual-line-mode (:after (&rest _) ublt/no-hard-wrapping)
  "Turn off `auto-fill-mode' (automatic hard wrapping)."
  (when visual-line-mode
    (auto-fill-mode -1)))

;;; Change highlighting
(use-package diff-hl
  :disabled t
  :custom (diff-hl-draw-borders nil)
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (global-diff-hl-mode)
  (if (display-graphic-p)
      (setq diff-hl-side 'right)
    (setq diff-hl-side 'left)
    (diff-hl-margin-mode)))


;;; Misc

(setq
 ;; To avoid tiny gaps between Emacs's and other apps' windows.
 frame-resize-pixelwise t

 ;; 2x70 instead of the default 2x80 so that side-by-side is preferable
 split-width-threshold 300
 split-height-threshold 200

 ;; Tile ediff windows horizontally
 ediff-split-window-function 'split-window-horizontally

 ;; Don't create new frame for ediff's control window
 ediff-window-setup-function 'ediff-setup-windows-plain

 ;; Bells suck, both visible and audible
 visible-bell (pcase system-type
                ('gnu/linux nil)
                ('darwin nil))

 show-paren-delay 0

 ;; Limit minibuffer to 20% frame height
 max-mini-window-height 0.2
 ;; resize-mini-windows 'grow-only

 ;; Echo keystrokes faster
 echo-keystrokes 0.2

 ;; woman settings
 woman-fill-column 80
 woman-fill-frame t
 woman-default-indent 7
 )

;;; Maybe move other window's point before and after minibuffer invocation
;; ;;; XXX: This is bad
;; (defun ublt/redisplay ()
;;   (message "%s" (selected-window)))
;; (add-hook 'minibuffer-setup-hook 'ublt/redisplay)

(setq-default
 line-spacing 0.15

 ;; Default width that triggers hard-wrap.
 fill-column 100
 )

;; Buffed-up help system
(use-package helpful)

;;; Colorize info pages.
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;; XXX: Find out why `python-mode' is upset by `which-func-mode'
;; ;;; Show current function name in mode-line
;; (which-func-mode +1)

(use-package highlight-numbers
  :config (defun ublt/-maybe-highlight-numbers ()
            ;; FIX: Make them compatible
            (unless (member major-mode '(web-mode))
              (highlight-numbers-mode +1)))
  :hook (prog-mode . ublt/-maybe-highlight-numbers))

;;; Flash eval'ed Lisp code.
(use-package eval-sexp-fu
  :custom ((eval-sexp-fu-flash-duration 0.4)
           (eval-sexp-fu-flash-error-duration 0.6)))

;;; Highlight last changes.
(use-package volatile-highlights
  :config (volatile-highlights-mode +1))

;;; Show match count for searches.
(use-package anzu
  :custom ((anzu-minimum-input-length 2)
           (anzu-search-threshold 100))
  :config (global-anzu-mode +1))

(use-package htmlize
  :custom ((htmlize-ignore-face-size nil)
           (htmlize-css-name-prefix "htmlize-")
           (htmlize-html-major-mode 'html-mode)))

;;; TODO: Tweak htmlize instead. `htmlfontify' does not work with org
;;; blocks.
(use-package htmlfontify
  :ensure nil :straight nil
  :custom (hfy-page-header 'ublt/hfy-page-header)
  :config (defun ublt/hfy-page-header (file style)
            ;; FIX: Escape file name???
            (format "<!DOCTYPE html>
<html>
  <head>
    <title>%s</title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
%s
" file style)))

(use-package rainbow-mode
  :defer t)

(provide 'ublt-appearance)
