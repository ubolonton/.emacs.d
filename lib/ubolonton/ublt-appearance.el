;;; Font, colors, text appearance
(require 'cl)

;;; Fonts
(defun ublt/toggle-fonts ()
  (interactive)
  (let* ((fonts (case system-type
                  ('darwin '("DejaVu Sans Mono-14" "Menlo-14" "Monaco-14"
                             "Consolas-15"))
                  ('gnu/linux '("DejaVu Sans Mono-10" "Inconsolata-12"
                                "DejaVu Sans Condensed-11"))
                  (t '("Courier New-12" "Arial-12"))))
         (cur-pos (get this-command 'pos))
         (N (length fonts))
         font)
    (setq cur-pos (if cur-pos (% cur-pos N) 0))
    (setq font (nth cur-pos fonts))
    (modify-all-frames-parameters (list (cons 'font font) (cons 'height 100)))
    (message "Font: %s" font)
    (put this-command 'pos (% (1+ cur-pos) N))))

;; Font and size default
(case system-type
  ('darwin (modify-all-frames-parameters
            '((top . 0) (left . 0) ;(width . 1280) (height . 800)
              (cursor-type . bar)
              (font . "DejaVu Sans Mono-14"))))
  ('windows-nt (modify-all-frames-parameters
                '((cursor-type . bar)
                  (font . "Courier New-12"))))
  ('gnu/linux (modify-all-frames-parameters
               '((top . 0) (left . 0) (width . 119)
                 (fullscreen . fullheight) (cursor-type . bar)
                 ;; FIX: This may be specific to my laptop
                 ;; (screen-gamma . 2.205)
                 ;; And this, my new monitor, even after calibration?!?
                 (screen-gamma . 2.7)
                 (font . "DejaVu Sans Mono-10")))))

;; TODO: Enable this except for twitter buffers
;; (blink-cursor-mode 1)

;; Non-code text reads better in proportional font
(when (member window-system '(x ns w32))
  (set-face-font 'variable-pitch (case system-type
                                   ('gnu/linux "DejaVu Sans Condensed-11")
                                   ('darwin "Helvetica-16")
                                   (t "Arial"))))
(defun turn-on-variable-pitch-mode ()
  (interactive)
  (variable-pitch-mode +1))
(dolist (hook '(erc-mode-hook
                Info-mode-hook
                help-mode-hook
                lyric-mode-hook
                Man-mode-hook woman-mode-hook
                twittering-mode-hook
                emms-playlist-mode-hook))
  (add-hook hook 'turn-on-variable-pitch-mode))

;;; Ubolonton's theme
(ublt/set-up 'ublt-themes
;;; TODO: Fix highlight-parentheses-mode so that switching theme
;;; switches parentheses' colors correctly.
  (condition-case nil
      (let ((hour (string-to-number (format-time-string "%H"))))
        (if (and (<= 8 hour) (<= hour 17)
                 (y-or-n-p "Use solarized light theme"))
            (color-theme-sanityinc-solarized-light)
          (color-theme-ubolonton-dark)))
    (error (color-theme-ubolonton-dark)))
  (setq
   hl-paren-colors `("Orange" "Yellow" "Greenyellow"
                     "Green" "Springgreen" "Cyan"
                     "#6A5ACD" "Magenta" "Purple"
                     "Orange" "Yellow" "Greenyellow"
                     "Green" "Springgreen" "Cyan"
                     "#6A5ACD" "Magenta" "Purple")))

;;; Whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (FIX: currently not showing in Emacs 24) whitespace-mode
;; `http://xahlee.org/emacs/whitespace-mode.html'
(setq whitespace-style
      '(face spaces tabs newline space-mark tab-mark newline-mark))
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

;;; Rainbow parentheses for coding modes
(defun turn-on-hl-paren ()
  (interactive)
  (highlight-parentheses-mode +1))
;; (add-hook 'prog-mode-hook 'turn-on-hl-paren t)
;; Work-around for a bug in highlight-parentheses-mode which messes up
;; the overlays, making the colors off if the mode is turned on twice
;; (e.g. by prog-mode-hook and by desktop-mode, which keeps track of
;; active minor modes from last session)
(defadvice highlight-parentheses-mode (around work-around-hl-bug activate)
  (unless (and highlight-parentheses-mode
               (= 1 (ad-get-arg 0)))
    ad-do-it))

;;; Code folding

;; Like org-mode TAB and S-TAB
;; XXX: I uncomment the region at the end of `hideshowvis' instead
;; of copying it here. Should fix that.
(require 'fold-dwim-org)
(setq fold-dwim-org/trigger-keys-block '((kbd "TAB")))
(defun ublt/code-folding-setup ()
  (hs-minor-mode 1)
  (fold-dwim-org/minor-mode 1)
  (hideshowvis-enable))
(add-hook 'prog-mode-hook 'ublt/code-folding-setup)

(ublt/set-up 'hideshowvis
  (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
  (defcustom hs-fringe-face 'hs-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)
  (defface hs-fringe-face
    '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  (defcustom hs-face 'hs-face
    "*Specify the face to to use for the hidden region indicator"
    :type 'face
    :group 'hideshow)
  (defface hs-face
    '((t (:background "#ff8" :box t)))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)
  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((marker-string "*fringe-dummy*")
             (marker-length (length marker-string))
             (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
             )
        (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
        (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
        (overlay-put ov 'before-string marker-string)
        (put-text-property 0 (length display-string) 'face 'hs-face display-string)
        (overlay-put ov 'display display-string)
        )))
  (setq hs-set-up-overlay 'display-code-line-counts
        hideshowvis-ignore-same-line nil))


;;; ^L visualization
;; (require 'pp-c-l)
(ublt/set-up 'pp-c-l
  (pretty-control-l-mode +1)
  (setq pp^L-^L-string "❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄ Section ❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄❄"
        pp^L-^L-string-pre "\n"
        pp^L-^L-string-post ""))

;;; Prevent modes that list stuffs from wrapping lines
(defun ublt/listing-settings ()
  (setq truncate-lines t
        word-wrap nil
        visual-line-mode nil))
(dolist (hook '(emms-browser-show-display-hook
                archive-zip-mode-hook
                dired-mode-hook
                org-agenda-mode-hook))  ; XXX: not working
  (add-hook hook 'ublt/listing-settings t))

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
        (set-frame-parameter nil 'alpha 88)
      (set-frame-parameter nil 'alpha 100))))

;;; TODO: Still don't understand this line-wrapping business
(put 'ublt/toggle-line-wrap 'state 'wrap)
(toggle-truncate-lines +1)
(setq-default truncate-lines t)
(setq-default word-wrap nil)
(global-visual-line-mode -1)

(defun ublt/toggle-line-wrap ()
  (interactive)
  (case (get 'ublt/toggle-line-wrap 'state)
    ('long (progn
             (put 'ublt/toggle-line-wrap 'state 'wrap)
             (toggle-truncate-lines +1)
             (setq-default truncate-lines t)
             (setq-default word-wrap nil)
             (global-visual-line-mode -1)
             (visual-line-mode -1)))
    ('wrap (progn
             (put 'ublt/toggle-line-wrap 'state 'long)
             (toggle-truncate-lines -1)
             (setq-default truncate-lines nil)
             (setq-default word-wrap t)
             (global-visual-line-mode +1)
             (visual-line-mode +1)))))

;;; Sometimes buffers have the same names
;; from `http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html'
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator "  "
      ;; Rename after killing uniquified
      uniquify-after-kill-buffer-p t
      ;; Don't muck with special buffers
      uniquify-ignore-buffers-re "^\\*")

;; Uncluttered shell prompt
(setq eshell-prompt-function (lambda ()
                               (concat
                                (format-time-string "%R")
                                " [" (car (last (split-string
                                                 (abbreviate-file-name
                                                  (eshell/pwd)) "/")))
                                "] " (if (= (user-uid) 0)
                                        "# " "$ "))))


;;; mode-line appearance
(defface ublt/mode-line-major-mode
  '((t :bold t))
  "Face for mode-line major mode string")
(setq mode-line-modes
      '(
        #("%[" 0 2
          (help-echo "Recursive edit, type C-M-c to get out"))
        #("(" 0 1
          (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
        (:propertize
         ("" mode-name)
         face ublt/mode-line-major-mode
         mouse-face mode-line-highlight
         help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes"
         local-map (keymap (mode-line keymap
                                      (mouse-2 . describe-mode)
                                      (down-mouse-1 menu-item "Menu Bar" ignore :filter
                                                    (lambda
                                                      (_)
                                                      (mouse-menu-major-mode-map))))))
        ("" mode-line-process)
        #("%n" 0 2
          (local-map (keymap
                      (mode-line keymap
                                 (mouse-2 . mode-line-widen)))
                     mouse-face mode-line-highlight help-echo "mouse-2: Remove narrowing from the current buffer"))
        (:propertize
         ("" minor-mode-alist)
         mouse-face mode-line-highlight
         help-echo "Minor mode\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode\nmouse-3: Toggle minor modes"
         local-map (keymap (mode-line keymap
                                      (mouse-2 . mode-line-minor-mode-help)
                                      (down-mouse-1 . mouse-minor-mode-menu))))

        #(")" 0 1
          (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
        #("%]" 0 2
          (help-echo "Recursive edit, type C-M-c to get out"))
        #(" " 0 1
          (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
        ;; XXX: Not really about modes
        (:eval (propertize (format-time-string "[%H:%M %d/%m]")
                           'face 'bold
                           'help-echo (concat (format-time-string "%c; ") (emacs-uptime "Uptime: %hh"))))

        ))

;; (list
;;        "("
;;        '(:eval (propertize "%m" 'face 'ublt/mode-line-major-mode))
;;        minor-mode-alist
;;        ")")

;;; Misc

;; 2x70 instead of the default 2x80 so that side-by-side is preferable
(setq split-width-threshold 140)

;; TODO: move to corresponding mode sections
;; Tile ediff windows horizontally
(setq ediff-split-window-function 'split-window-horizontally)


;; Sparse lines
(setq-default line-spacing 0.2)

;; Visible bell on GTK sucks
(setq visible-bell (case system-type
                     ('gnu/linux nil)
                     (t t)))

;; No show-paren delay
(setq show-paren-delay 0)

;; Limit minibuffer to 20% frame height
(setq max-mini-window-height 0.2)

;; Technicolor
(require 'info+)

;; Buffed-up help system
(require 'help-macro+)
(require 'help+)
(require 'help-fns+)

;; woman settings
(setq woman-fill-column 80
      woman-fill-frame t
      woman-default-indent 7)

;; Fringe
(set-fringe-mode '(8 . 0))

;; 70-char column width
(setq-default fill-column 70)

(provide 'ublt-appearance)
