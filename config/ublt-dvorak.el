;;; -*- lexical-binding: t; coding: utf-8 -*-
(require 'ublt-util)

;;; My Dvorak key map for Dvorak. Keys are extensively remapped and
;;; translated.
;;   ' , . p y    f g c r l   / = \
;;   a o e u i    d h t n s   -
;;   ; q j k x    b m w v z
;;;
;;; TODO: remap

;;; Special navigations

;; C-s-t  M-<next>
;; C-s-c  M-<prior>

;;; C-M => M-
;; C-M-h  M-<left>
;; C-M-n  M-<right>
;; C-M-c  M-<up>
;; C-M-t  M-<down>

;; M-M    C-M-u
;; M-V    C-M-d

;; M-m    M-p
;; M-v    M-n

;;; ?
;; M-s-g
;; M-s-r
;; M-s-m
;; M-s-v

(pcase system-type
  ('windows-nt (setq w32-pass-lwindow-to-system nil
                     w32-pass-rwindow-to-system nil
                     w32-lwindow-modifier 'super
                     w32-rwindow-modifier 'super))
  ;; emacs-mac-app https://bitbucket.org/mituharu/emacs-mac
  ('darwin (setq mac-command-modifier 'super
                 mac-option-modifier 'meta
                 mac-pass-command-to-system nil)))


;; Helper to define keys
(defun ublt/define-keys (key-map &rest ps)
  "Define key binding pairs for KEY-MAP."
  (declare (indent 1))
  (let ((i 0))
    (while (< i (length ps))
      (let ((src (elt ps i))
            (dst (elt ps (1+ i))))
        (define-key key-map
          (if (symbolp src)
              (vector 'remap src)
            (read-kbd-macro src))
          (if (stringp dst)
              (read-kbd-macro dst)
            dst)))
      (setq i (+ i 2)))))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(ublt/define-keys\\) \\(.*\\)\\>"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face))
   ("\\<\\(ublt/undefine-keys\\) \\(.*\\)\\>"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face))
   ("\\<\\(ublt/keys\\) *\\(.*\\) *\\_<\\(.*\\)\\>"
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face)
    (3 font-lock-variable-name-face)))
 'append)

(defun ublt/undefine-keys (key-map keys)
  (declare (indent 1))
  (dolist (key keys)
    (define-key key-map (read-kbd-macro key) nil)))

(defmacro ublt/keys (package map &rest mappings)
  (declare (indent 2))
  `(with-eval-after-load ,package
     (ublt/define-keys ,map
       ,@mappings)
     (message "Updated keymap `%s'" ',map)))


;; Mouse.
(if (functionp 'xref-find-definitions-at-mouse)
    (defalias 'ublt/mouse-xref-find-definitions #'xref-find-definitions-at-mouse)
  (defun ublt/mouse-xref-find-definitions (event)
    "Move point to mouse EVENT's location, then call the command bound to `M-.'."
    (interactive "e")
    (mouse-set-point event)
    (when-let ((command (key-binding (kbd "M-."))))
      (call-interactively command))))

(defun ublt/mouse-pop-tag-mark (event)
  "Call the command bound to `M-\,'."
  (interactive "e")
  (when-let ((command (key-binding (kbd "M-,"))))
    (call-interactively command)))


;;; Custom global bindings -------------------------------------------
;; TODO:
;; M-y
;; M-o
;; M-a
;; M-j
;; M-k
;; M-z
;; M-1 ... M-0
;; M-` M-'
;; M-= M-\

;;; Keys shadowed by translation:
;; "M-h"     'mark-paragraph
;; "M-c"     'capitalize-word
;; "M-t"     'transpose-words
;; "M-i"     'tab-to-tab-stop
;; "M-d"     'kill-word
;; "M-e"     'forward-sentence
;; "M-u"     'upcase-word
;; "M-v"     'scroll-up
;; "C-M-n"   'forward-list
;; "C-M-c"   'exit-recursive-edit
;; "C-M-t"   'transpose-sexps

;;; Keys shadowed by rebinding:
;; "C-M-r"    something with isearch backward repeat regexp
;; "s-h"     'ns-do-hide-emacs
;; "M-g g"   'goto-line
;; "M-g n"   'next-error
;; "M-g p"   'previous-error
;; "M-r"     'move-to-window-line-top-bottom 'paredit-raise-sexp
;; "M-l"     'downcase-word

;;; These are translated so that they can be pervasive, as most modes
;;; rebind them (syntactically override), instead of remapping the
;;; original command (semantically override). NTA: Many of these are
;;; now handled system-wide by `autokey', but are kept here just in case
(ublt/define-keys key-translation-map
  ;; OSX conventions
  "s-c"    "M-w"                        ;   copy
  "s-x"    "C-w"                        ; ✂ cut
  "s-v"    "C-y"                        ;   paste
  "s-V"    "M-y"                        ;   paste cycle
  "s-s"    "C-x C-s"                    ;   save
  "s-S"    "C-x s"                      ;   save some buffers
  "s-z"    "C-_"                        ; ↺ undo
  "s-Z"    "M-_"                        ; ↻ redo
  "s-a"    "C-x h"                      ;   mark all
  "s-o"    "C-x C-f"                    ;   open file

  ;; Other super-based translations
  "s-t"    "M-."                        ; push reference
  "s-T"    "M-,"                        ; pop reference

  ;; XXX: ⬅➡ WTF Unicode. There's no RIGHTWARDS BLACK ARROW
  ;; Movement keys (right hand)
  "M-c"    "<up>"                       ; ⬆
  "M-t"    "<down>"                     ; ⬇
  "M-h"    "<left>"                     ; ⬅
  "M-n"    "<right>"                    ; ➡
  "M-C"    "<prior>"                    ; ▲ scroll up
  "M-T"    "<next>"                     ; ▼ scroll down
  "M-H"    "C-<up>"                     ; ⬆ paragraph up
  "M-N"    "C-<down>"                   ; ⬇ paragraph down
  "M-g"    "C-<left>"                   ; ⬅ word left
  "M-r"    "C-<right>"                  ; ➡ word right
  "M-G"    "M-<"                        ; ⇱ buffer home
  "M-R"    "M->"                        ; ⇲ buffer end
  "C-s-t"  "M-<next>"                   ; ▲ other window scroll up
  "C-s-c"  "M-<prior>"                  ; ▼ other window scroll down

  ;; Deletion (left hand)
  "M-e"    "DEL"                        ; ⌫
  "M-u"    "<kp-delete>"                ; ⌦
  "M-."    "M-DEL"                      ; ⌫ delete word
  "M-p"    "M-<kp-delete>"              ; ⌦ delete word

  ;; "Help" `autokey'
  "C-<backspace>" "M-DEL"
  "C-<delete>"    "M-<kp-delete>"
  "<backspace>"   "DEL"
  "<delete>"      "<kp-delete>"
  "<home>"        "C-a"
  "<end>"         "C-e"
  "C-<home>"      "M-<"
  "C-<end>"       "M->"

  ;; TODO: Make this work (need to handle ESC prefix as well).
  ;; ;; "Help" `Karabiner-Elements' (e.g. we want C-g -> ⎋ system-wide, except in Emacs).
  ;; "<escape>" "C-g"

  "M-i"    "C-k"
  "M-d"    "C-a"
  "M-D"    "C-e"

  ;; Nut!!! But seriously much more effective
  "M-SPC"  "C-SPC"
  "C-SPC"  "M-SPC"

  "C-M-h"  "M-<left>"                ; ⬅ list (except for org-mode)
  "C-M-n"  "M-<right>"               ; ➡ list (except for org-mode)
  "C-M-c"  "M-<up>"                  ; ⤂ paredit splice-kill, org up
  "C-M-t"  "M-<down>"                ; ⤃ paredit splice-kill, org down

  "M-M"    "C-M-u"                      ; ⬉( up list
  "M-V"    "C-M-d"                      ; ⬊( down list

  "M-m"    "M-p"                        ; ⬁ special (history, errors)
  "M-v"    "M-n"                        ; ⬂ special (history, errors)

  "s-4"    "C-x 4"                      ; do something other window
  ;; "s-r"    "C-x r"
  ;; "s-R"    "C-x r j"

  ;; "s-P"    "C-c p"                      ; projectile

  "M-F"    "s-<escape>"                 ; evil's motion state
  "M-f"    "<escape>"                   ; evil's normal state
  )

;;; See the configuration files for zsh (`.zshrc'), readline
;;; (`.inputrc'), Konsole (`default.keytab')
;; (define-key local-function-key-map "[25~" (kbd "<backspace>"))
;; (define-key local-function-key-map "[26~" (kbd "C-<backspace>"))
;; (define-key local-function-key-map "[27~" (kbd "C-<delete>"))

(ublt/define-keys global-map
  "C-h"           'ublt/hydra-help/body
  "C-M-k"         'hydra-pause-resume

  "s-`"           'other-frame

  ;; Windows manipulation
  "s-1"           'delete-other-windows
  "s-2"           'split-window-vertically
  "s-3"           'split-window-horizontally
  "s-0"           'delete-window
  "s-)"           'bury-buffer
  "s-w"           'other-window
  "s-W"           'ublt/swap-windows

  ;; Utilities, super-
  "s-i"           'helm-semantic-or-imenu
  "s-d"           'helm-command-prefix
  ;; "s-y"           '
  "s-h"           'ublt/helm
  ;; "C-s-h"         'helm-follow-mode
  "s-f"           'helm-swoop
  "s-F"           'helm-projectile
  ;; "s-F"           'projectile-find-file
  "s-g"           'magit-status
  "s-C-g"         'magit-file-dispatch
  "s-G"           'rg
  "s-H"           'helm-ag
  "s-m"           'avy-goto-word-1
  "s-M"           'ublt/hydra-avy/body
  "s-r"           'org-capture
  "s-R"           'org-agenda
  "s-n"           'ublt/switch-to-last-buffer
  "s-b"           'ublt/browse-url-at-point
  "s-B"           'dash-at-point
  "s-p"           'pop-global-mark
  "s-P"           'avy-pop-mark
  "s-<backspace>" 'ublt/toggle-alpha
  "s-<return>"    'ublt/toggle-fullscreen
  "s-/"           'variable-pitch-mode
  "s-\\"          'align-regexp
  "s-u"           'revert-buffer        ; ⟲
  "s-k"           'ublt/kill-this-buffer
  ;; "s-l"           'goto-line
  "s-l"           'helm-swoop
  "s-C"           'ublt/duplicate-line
  "s-+"           'text-scale-increase
  "s-="           'text-scale-increase
  "s--"           'text-scale-decrease

  "C-s-n"         'forward-page
  "C-s-h"         'backward-page

  ;; TODO: These should be translated
  "s-["           'backward-page
  "s-]"           'forward-page

  ;; Can't use remapping here, since it would make `undo-tree' not turning on.
  "C-_"           'undo-tree-undo
  "M-_"           'undo-tree-redo

  ;; Line/region movement
  "M-s-h"         'textmate-shift-left
  "M-s-n"         'textmate-shift-right
  "M-s-c"         'move-text-up
  "M-s-t"         'move-text-down
  "M-s-˙"         'textmate-shift-left  ; OS X
  "M-s-˜"         'textmate-shift-right ; OS X
  "M-s-ç"         'move-text-up    ; OS X
  "M-s-†"         'move-text-down  ; OS X
  "s-<up>"        'move-text-up
  "s-<down>"      'move-text-down
  "s-<left>"      'textmate-shift-left
  "s-<right>"     'textmate-shift-right

  "M-s-=" 'evil-numbers/inc-at-pt
  "M-s--" 'evil-numbers/dec-at-pt

  "C-M-r"         'highlight-symbol-next
  "C-M-g"         'highlight-symbol-prev

  ;; Deletion
  "<kp-delete>"   'delete-char
  "M-<kp-delete>" 'kill-word
  "M-I"           'whole-line-or-region-kill-region

  "C-="           'text-scale-increase

  "M-Q"           'ublt/unfill-paragraph

  ;; Toggling
  "<f9> l"        'global-display-line-numbers-mode
  "<f9> <f9>"     'ublt/toggle-fonts
  "<f9> <f12>"    'ublt/toggle-line-wrap

  ;; Ubuntu
  "M-<f4>"        'kmacro-start-macro-or-insert-counter ; F3 is taken by xbindkeys

  ;; Right, use C-u if you want digit args
  "M-5"           'query-replace
  "M-%"           'query-replace-regexp
  "M-6"           'ublt/toggle-letter-case
  ;; "M-7"           'ublt/toggle-cua-rect
  ;; "M-8"           'ublt/cycle-prose-region
  ;; "M-9"           'mark-enclosing-sexp
  "M-7"           'er/expand-region
  "M-4"           'er/expand-region
  "s-O"           'er/expand-region

  ;; Misc
  "C-c C-x C-o"   'org-clock-out
  "M-l"           'move-to-window-line-top-bottom
  "M-b"           'hippie-expand        ; more convenient here
  "M-B"           nil                   ; use yas-minor-mode-map
  "M-s"           nil                   ; use yas-minor-mode-map
  "C-z"           nil                   ; who needs suspend-frame?
  "M-u"           nil                   ; TODO: Assign M-U to sth else.
  "S-s-SPC"       'whitespace-mode
  "M-x"           'helm-M-x          ; C-x C-m for the original
  "M-X"           'smex-major-mode-commands
  "C-x C-b"       'helm-mini       ; Because it's to easy to mis-press
  "C-x b"         'helm-mini
  "C-x <return>"  'term
  "C-x B"         'ibuffer
  "C-S-s"         'ublt/isearch-other-window

  "M-<left>"      'backward-list
  "M-<right>"     'forward-list

  ;; FIX: Still have to override mode-specific bindings. There must
  ;; be something better
  "M-TAB"         'company-complete
  "S-TAB"         'company-select-previous
  "<backtab>"     'company-select-previous

  ;; TODO: Something (were upcase/downcase region)
  "C-x C-u"       'nil
  "C-x C-l"       'nil

  ;; This is used instead of remapping, otherwise `visual-line-mode' interferes with `org-mode'.
  "C-a"           'ublt/back-to-indentation-or-line-beginning

  ;; TODO: Rearrange the s- combos
  "C-c p f"       'projectile-find-file

  "M-o"           'isearch-occur

  "C-<f10>"       'menu-bar-mode
  "C-s"           'isearch-forward-regexp
  "C-r"           'isearch-backward-regexp
  ;; FIX
  "C-s-s"         'isearch-forward

  ;; FIX
  "C-c r"         'revert-buffer
  "C-c y"         'bury-buffer

  ;; FIX
  "C-x m"         'eshell
  "C-x C-m"       'shell

  "C-x C-f"       'helm-find-files

  ;; FIX
  "C-c e"         'esk-eval-and-replace
  "M-j"           'join-line

  "C-c g"         'magit-status
  "C-C n"         'ublt/cleanup-buffer

  "s-<mouse-4>"   'text-scale-increase
  "s-<mouse-5>"   'text-scale-decrease
  "S-<mouse-4>"   'backward-paragraph
  "S-<mouse-5>"   'forward-paragraph
  "M-<mouse-4>"   'scroll-down-line
  "M-<mouse-5>"   'scroll-up-line

  ;; Apparently it's system-dependent whether they are mouse-4/mouse-5 or wheel-down/wheel-up.
  "s-<wheel-down>" 'text-scale-increase
  "s-<wheel-up>"   'text-scale-decrease
  "M-<wheel-down>" 'scroll-down-line
  "M-<wheel-up>"   'scroll-up-line

  "s-<mouse-1>"    'ublt/mouse-xref-find-definitions
  "S-s-<mouse-1>"  'ublt/mouse-pop-tag-mark)

;;; Remapping some commands to their improved versions (keeping the key bindings).
(ublt/define-keys global-map
  'move-beginning-of-line 'ublt/back-to-indentation-or-line-beginning
  'exchange-point-and-mark 'ublt/exchange-point-and-mark-no-activate)

(when window-system
  (ublt/define-keys global-map
    ;; TODO: Use hydra.
    "M-O" 'helm-multi-swoop))




;;; TODO: Put this in `ublt-appearance'.
(use-package hydra
  :config
  (setq-default
   ;; We don't use :custom, since this is dynamically set based on whether we are in a GUI.
   hydra-hint-display-type (if (memq window-system '(mac ns x))
                               'posframe 'lv)
   hydra-posframe-show-params
   (list :internal-border-width 1
         :poshandler #'posframe-poshandler-point-bottom-left-corner))
  ;; The advice is needed to use the current theme's color.
  (define-advice hydra-posframe-show (:around (f &rest args) ublt/tweak-appearance)
    (let ((hydra-posframe-show-params
           (append `(:internal-border-color ,(face-attribute 'mode-line :background))
                   `(:background-color ,(face-attribute 'hl-line :background))
                   hydra-posframe-show-params)))
      (apply f args)))

  (define-advice ublt/hydra-help/body (:around (f &rest args) ublt/center)
    (let ((hydra-posframe-show-params (append '(:poshandler posframe-poshandler-frame-center)
                                              hydra-posframe-show-params)))
      (apply f args)))

  (defhydra ublt/hydra-info (:hint nil :color teal)
    "
  Search ^^        Open manual ^^
^^^^^^───────────────────────────────────────────────────
  _e_ elisp        _C-e_ elisp        _C-v_ evil
  _o_ org-mode     _C-o_ org-mode     _C-t_ transient
  _g_ magit          _G_ magit        _C-u_ use-package
  _r_ emacs        _C-r_ emacs
  _l_ cl           _C-l_ cl
  _s_ symbol

 [_h_] helm doc    [_C-i_] all info
"
    ("e" helm-info-elisp)
    ("o" helm-info-org)
    ("g" helm-info-magit)
    ("r" helm-info-emacs)
    ("s" info-lookup-symbol)
    ("l" helm-info-cl)

    ("C-e" (info "elisp"))
    ("C-o" (info "org"))
    ("G"   (info "magit"))
    ("C-r" info-emacs-manual)
    ("C-l" (info "cl"))
    ("C-u" (info "use-package"))

    ("C-v" (info "evil"))
    ("C-t" (info "transient"))

    ("C-i" info)
    ("h" helm-documentation))

  (defhydra ublt/hydra-help (:hint nil :color teal)
    "
  Describe ^^      Keys ^^            Go to ^^           Docs ^^
^^^^^^^^────────────────────────────────────────────────────────────────
  _f_ function     _k_ brief key      _C-f_ function     _C-i_ info
  _c_ command      _K_ describe key   _C-v_ variable     _C-w_ woman
  _v_ variable     _w_ where is cmd   _C-l_ library
  _s_ symbol       _b_ all bindings   _C-k_ key
  _m_ mode         ^^                   _F_ face
  _o_ at point     ^^                 _C-o_ at point
  ^^               ^^                 _C-u_ config
 [_C-h_] use built-in help    [_l_] view lossage
"
    ("f" helpful-callable)
    ("c" helpful-command)
    ("v" helpful-variable)
    ("s" helpful-symbol)
    ("m" describe-mode)
    ("o" helpful-at-point)
    ("p" describe-package)

    ("k" describe-key-briefly)
    ("K" helpful-key)
    ("w" where-is)
    ("b" describe-bindings)

    ("C-f" find-function)
    ("C-v" find-variable)
    ("C-k" find-function-on-key)
    ("C-l" find-library)
    ("C-o" find-function-at-point)
    ("C-u" use-package-jump-to-package-form)
    ("F"   find-face-definition)

    ("C-i" ublt/hydra-info/body)
    ("C-w" helm-man-woman)

    ("C-h" help-for-help)
    ("l" view-lossage)))

(use-package which-key
  :disabled
  :custom (which-key-popup-type 'minibuffer)
  :config (which-key-mode))


;;; Remapping.
(ublt/keys 'undo-tree undo-tree-visualizer-mode-map
  'evil-previous-visual-line 'undo-tree-visualize-undo
  'evil-next-visual-line 'undo-tree-visualize-redo)


;;; Help navigation
(ublt/keys "help-mode" help-mode-map
  "M-s-h" 'help-go-back
  "M-s-n" 'help-go-forward
  "C-f"   'help-follow-symbol
  "M-."   'elisp-slime-nav-find-elisp-thing-at-point)
(ublt/keys 'apropos apropos-mode-map
  "M-."   'elisp-slime-nav-find-elisp-thing-at-point)

(ublt/keys "info" Info-mode-map
  "M-s-h" 'Info-history-back
  "M-s-n" 'Info-history-forward
  "C-M-x" 'ublt/eval-sexp-at-point
  "M-."   'elisp-slime-nav-find-elisp-thing-at-point)

(ublt/keys 'help-mode help-map
  "a" 'apropos
  "A" 'apropos-command
  "V" 'apropos-value
  "C-k" 'helm-descbinds)


;;; Evil -------------------------------------------------------------
;;; TODO: Swap WORD & word
(ublt/keys 'evil evil-normal-state-map
  ;; Preparation for motion map
  "h" nil "H" nil
  "n" nil "N" nil
  "c" nil "C" nil
  "t" nil "T" nil
  "g" nil "G" nil
  "r" nil "R" nil
  "l" nil "L" nil

  ;; (c)hange => (j)ab
  "j"        'evil-change
  "J"        'evil-change-line

  "U"        'undo-tree-redo

  "C-r"      nil
  "M-."      nil                   ; evil-repeat-pop-next
  "s-<escape>" 'evil-motion-state
  "<escape>" 'evil-force-normal-state

  ;; (r)eplace => (b)
  "b"        'evil-replace
  "B"        'evil-replace-state

  ;; g => e
  "e"        nil
  "e&"       'evil-ex-repeat-global-substitute
  "e8"       'what-cursor-position
  "ea"       'what-cursor-position
  "ei"       'evil-insert-resume
  "eJ"       'evil-join-whitespace
  "eq"       'evil-fill-and-move
  "ew"       'evil-fill
  "eu"       'evil-downcase
  "eU"       'evil-upcase
  "ef"       'find-file-at-point
  "eF"       'evil-find-file-at-point-with-line
  "e?"       'evil-rot13
  "e~"       'evil-invert-case
  "e;"       'goto-last-change
  "e,"       'goto-last-change-reverse
  "en"       'ublt/narrow-or-widen

  "ee"       'evilnc-comment-operator
  "eci"      'evilnc-comment-or-uncomment-lines
  "ecc"      'evilnc-copy-and-comment-lines
  "ecp"      'evilnc-comment-or-uncomment-paragraphs
  "ecr"      'comment-or-uncomment-region
  "ecv"      'evilnc-toggle-invert-comment-line-by-line
  )
(ublt/keys 'evil evil-motion-state-map
  "s-<escape>" 'evil-motion-state
  "<escape>" 'evil-force-normal-state

  ;; Dvorak, positional
  "h"     'evil-backward-char      ; ⬅
  "n"     'evil-forward-char       ; ➡
  "H"     'evil-backward-arg
  "N"     'evil-forward-arg
  ;; Dvorak, positional (line)
  "c"     'evil-previous-visual-line      ; ⬆
  "t"     'evil-next-visual-line          ; ⬇
  "C"     'evil-scroll-line-up
  "T"     'evil-scroll-line-down
  ;; Dvorak, positional (word)
  "g"     'evil-backward-word-begin ; -⬅
  "G"     'evil-backward-WORD-begin ; |⬅
  "r"     'evil-forward-word-end    ; ➡-
  "R"     'evil-forward-WORD-end    ; ➡|

  "SPC"   'evil-scroll-page-down
  "S-SPC" 'evil-scroll-page-up

  ;; (n)ext => (l)ook
  "l"     'evil-search-next
  "L"     'evil-search-previous
  ;; (c)hange => (j)ab
  "j"     'evil-change
  "J"     'evil-change-line
  ;; (t)o => к (Russian)
  "k"     'evil-find-char-to
  "K"     'evil-find-char-to-backward
  ;; (r)eplace => (b)
  "b"     'evil-replace
  "B"     'evil-replace-state

  ;; g => (e)vil do
  "e"     nil
  "ed"    'evil-goto-definition
  "ee"    nil
  "eE"    'evil-backward-WORD-end
  "eg"    'evil-goto-first-line
  "ej"    'evil-next-visual-line
  "ek"    'evil-previous-visual-line
  "e0"    'evil-beginning-of-visual-line
  "e_"    'evil-last-non-blank
  "e^"    'evil-first-non-blank-of-visual-line
  "e$"    'evil-end-of-visual-line
  "e\C-]" 'find-tag
  "ev"    'evil-visual-restore
  "en"       'ublt/narrow-or-widen

  "*"     'highlight-symbol-next
  "#"     'highlight-symbol-prev

  "C-b"    nil
  "C-d"    nil
  "C-e"    nil
  "C-f"    nil
  "C-o"    nil
  "C-y"    nil
  )
(ublt/keys 'evil evil-insert-state-map
  "s-<escape>" 'evil-motion-state
  "<escape>" 'evil-normal-state

  "C-n" nil                        ; evil-complete-next
  "C-p" nil                        ; evil-complete-previous
  "C-r" nil                        ; evil-paste-from-register
  "C-w" nil                        ; evil-delete-backward-word
  "C-x C-n" nil                    ; evil-complete-next-line
  "C-x C-p" nil                    ; evil-complete-previous-line
  "C-t" nil                        ; evil-shift-right-line
  "C-a" nil                        ; evil-paste-last-insertion
  "C-e" nil                        ; evil-copy-from-below
  "C-y" nil                        ; evil-copy-from-above
  "C-k" nil                        ; evil-insert-digraph
  )
(ublt/keys 'evil evil-visual-state-map
  "s-<escape>" 'evil-motion-state
  "<escape>" 'evil-exit-visual-state
  "R" nil
  "C-n"       'ublt/narrow-or-widen
  )
(ublt/keys 'evil evil-replace-state-map
  "s-<escape>" 'evil-motion-state
  "<escape>" 'evil-normal-state)
(ublt/keys 'evil evil-emacs-state-map
  "s-<escape>" 'evil-motion-state
  "<escape>" 'evil-normal-state)
(ublt/keys 'evil evil-insert-state-map
  "C-k" nil
  "C-o" nil
  "C-e" nil
  "C-y" nil)
(ublt/keys 'evil evil-outer-text-objects-map
  "a" 'evil-outer-arg
  "d" 'evil-a-defun
  "t" 'evil-a-tag
  ;; "T" 'evil-a-tag
  "s" 'evil-a-sexp
  "S" 'evil-a-sentence
  ;; "*" 'evil-a-symbol
  ;; "l" 'evil-a-sexp
  "u" 'evil-a-url
  )
(ublt/keys 'evil evil-inner-text-objects-map
  "a" 'evil-inner-arg
  ;; "T" 'evil-inner-tag
  "d" 'evil-inner-defun
  "t" 'evil-inner-tag
  "S" 'evil-inner-sentence
  "s" 'evil-inner-sexp
  "u" 'evil-inner-url
  ;; "l" 'evil-inner-sexp
  )
(ublt/keys 'evil-integration evil-motion-state-map
  "k"     'evil-avy-goto-char-in-line
  )
(ublt/keys 'evil-integration global-map
  "s-m" 'evil-avy-goto-word-1
  "s-M" 'evil-avy-goto-char-2
  )


;; (ublt/keys 'evil evil-inner-text-objects-map
;;   "d" 'evil-inner-defun
;;   "S" 'evil-inner-symbol
;;   "*" 'evil-inner-symbol)


;;; Diff navigation
(defun ublt/setup-ediff-mode-map ()
  (ublt/define-keys 'ediff-mode-map
    ;; TODO: Previously we used c/t, but 3-way diff view needs "c".
    "<up>"   'ediff-previous-difference
    "<down>" 'ediff-next-difference
    ))

(with-eval-after-load 'ediff
  (add-hook 'ediff-keymap-setup-hook 'ublt/setup-ediff-mode-map))


;;; Helm
(ublt/keys "helm-config" helm-command-map
  "g"   'helm-google-suggest
  "l"   'helm-locate
  "p"   'helm-list-emacs-process
  "M-o" 'helm-occur
  "M-O" 'helm-multi-occur
  "o"   'helm-occur
  "O"   'helm-multi-occur
  "SPC" 'helm-all-mark-rings
  "i"   'helm-imenu)
(ublt/keys 'helm helm-map
  "s-h"         'minibuffer-keyboard-quit
  "s-<return> " 'minibuffer-keyboard-quit
  "C-x h"       'helm-mark-all
  "M-a"         'helm-toggle-all-marks
  "C-f"         'helm-follow-mode
  "M-<right>"   'helm-next-source
  "M-<left>"    'helm-previous-source
  "M-<up>"      'helm-follow-action-backward
  "M-<down>"    'helm-follow-action-forward
  'ublt/switch-to-last-buffer 'helm-select-action
  'other-window               'ublt/helm-maybe-exit-minibuffer-other-window)
(ublt/keys 'helm-buffers helm-buffer-map
  "s-k" 'helm-buffer-run-kill-buffers)
(ublt/keys 'helm-files helm-find-files-map
  "<left>" 'helm-find-files-up-one-level
  "<right>" 'helm-execute-persistent-action)
(ublt/keys 'helm-files helm-read-file-map
  "<left>" 'helm-find-files-up-one-level
  "<right>" 'helm-execute-persistent-action)
(ublt/keys 'minibuffer minibuffer-local-map
  "C-c C-l"     'helm-minibuffer-history)


;;; HTML/CSS

;;; TODO: Consistent bindings for tree-editing (HTML & Lisp)
(dolist (fms '(("nxhtml-mumamo" nxhtml-mumamo-mode-map)
               ("sgml-mode" sgml-mode-map)
               ("html-mode" html-mode-map)
               ("nxml-mode" nxml-mode-map)
               ("web-mode"  web-mode-map)))
  (pcase-let ((`(,file ,map) fms))
    (eval-after-load file
      `(ublt/define-keys ,map
         "s-<right>" 'sgml-skip-tag-forward
         "s-<left>"  'sgml-skip-tag-backward
         "M-<right>" 'sgml-skip-tag-forward
         "M-<left>"  'sgml-skip-tag-backward))))

(ublt/keys 'emmet-mode emmet-mode-keymap
  "M-RET" 'emmet-expand-yas)

(ublt/keys 'web-mode web-mode-map
  "M-<up>"    'web-mode-element-previous
  "M-<down>"  'web-mode-element-next
  "M-a"       'web-mode-fold-or-unfold
  "C-M-u"     'web-mode-element-parent
  "C-M-d"     'web-mode-element-child
  "M-("       'web-mode-element-wrap)


;;; auto-complete and yasnippet
(ublt/keys 'company company-active-map
  "C-s" 'company-filter-candidates
  "C-d" 'company-show-location
  "<right>" 'company-complete-selection)

(ublt/keys "yasnippet" yas-minor-mode-map
  "TAB" nil
  "<tab>" nil
  "M-B" 'yas-expand
  "M-s" 'yas-expand
  )
(ublt/keys "slime" slime-mode-map
  "M-TAB" 'company-complete)

(ublt/keys "python-mode" py-mode-map
  "M-TAB" 'company-complete)
(ublt/keys "python-mode" py-shell-map
  "M-TAB" 'company-complete)


;;; Error navigation
(ublt/keys 'flycheck flycheck-mode-map
  "M-p" 'flycheck-previous-error
  "M-n" 'flycheck-next-error)
(dolist (fms '(("js" js-mode-map)
               ("js2-mode" js2-mode-map)
               ("python-mode" py-mode-map)
               ("python" python-mode-map)
               ("php-mode" php-mode-map)
               ("lisp-mode" emacs-lisp-mode-map)
               ("erlang" erlang-mode-map)
               ("ruby-mode" ruby-mode-map)))
  (pcase-let ((`(,file ,map) fms))
    (eval-after-load file
      `(ublt/define-keys ,map
         "M-p" nil
         "M-n" nil))))

(ublt/keys 'rg rg-mode-map
  "M-p" 'previous-error-no-select
  "M-n" 'next-error-no-select)


;;; Paredit
(ublt/keys 'paredit paredit-mode-map
  "{"             'paredit-open-curly
  "}"             'paredit-close-curly
  "M-("           'paredit-wrap-round
  "M-)"           'paredit-forward-slurp-sexp
  ;; XXX: Can't use this, since Terminal escape sequences start with it.
  "M-["           nil
  "M-{"           'paredit-wrap-curly
  "M-r"           nil              ; was paredit-raise-sexp
  "M-<backspace>" 'paredit-backward-kill-word
  "M-<kp-delete>" 'paredit-forward-kill-word
  "<backspace>"   'paredit-backward-delete
  "<kp-delete>"   'paredit-forward-delete
  "C-<left>"      nil
  "C-<right>"     nil
  "M-<left>"      'paredit-backward
  "M-<right>"     'paredit-forward
  "C-M-s"         'paredit-splice-sexp
  ;; TODO: advice comment-dwim instead
  "M-;"           nil)

(ublt/keys "python-mode" py-mode-map
  "{"       'paredit-open-curly
  "}"       'paredit-close-curly)
(ublt/keys "js2-mode" js2-mode-map
  "{"   'paredit-open-curly
  "}"   'paredit-close-curly-and-newline)


;;; Languages with interactive REPL
;;; TODO: sql, ruby, factor, haskell, octave
;;; For mode with a REPL: lisps, python, js

;; In source buffer (c r l are adjacent on Dvorak!!!):
;; C-c C-c  ; eval top-level expression at point
;; C-c C-r  ; eval region
;; C-c C-l  ; eval buffer/file
;; C-c C-k  ; eval buffer/file (current)
;; C-c v    ; eval buffer/file (current)
;; C-c C-s  ; go to REPL
;;; TODO:
;; C-M-x    ; eval top-level expression at point

;; In REPL buffer
;; M-n      ; previous input matching current
;; M-p      ; next input matching current


;;; Talking about bad defaults!!! (These are like 100 times better)
(ublt/keys "comint" comint-mode-map
  "M-p" 'comint-previous-matching-input-from-input
  "M-n" 'comint-next-matching-input-from-input)
(ublt/keys 'haskell-mode haskell-mode-map
  "C-x C-d" nil
  "C-c C-z" nil
  "C-c C-s" 'haskell-interactive-switch
  "C-c C-l" 'haskell-process-load-file
  "C-c C-b" nil
  "C-c C-t" 'haskell-process-do-type
  "C-c C-i" 'haskell-process-do-info
  "C-c M-." nil
  "C-c C-d" nil)
(ublt/keys "lisp-mode" lisp-mode-shared-map
  "RET" 'reindent-then-newline-and-indent)
(ublt/keys 'elisp-mode emacs-lisp-mode-map
  "M-TAB"   nil
  "C-c C-c" 'eval-defun
  "C-M-x"   'ublt/eval-defun
  "C-c C-l" 'eval-buffer
  "C-c C-s" 'ielm)
(ublt/keys "lisp-mode" lisp-mode-map
  "C-c C-s" 'switch-to-lisp)
(ublt/keys 'elisp-mode lisp-interaction-mode-map
  "M-TAB"   nil
  "C-c C-c" 'eval-defun
  "C-M-x"   'ublt/eval-defun
  "C-c C-r" 'eval-region
  "C-c C-l" 'eval-buffer
  "C-c C-s" 'ielm)
(ublt/keys 'ielm ielm-map
  "M-TAB"   nil
  "M-."     'elisp-slime-nav-find-elisp-thing-at-point)
(ublt/keys "clojure-mode" clojure-mode-map
  "C-c C-s" 'run-lisp)
(ublt/keys "slime" slime-mode-map
  "M-TAB"   nil
  "C-c C-l" 'slime-compile-and-load-file
  "C-c C-k" 'slime-load-file
  "C-c v"   'slime-load-file
  "C-c C-s" 'slime-switch-to-output-buffer)
(ublt/keys 'cider-mode cider-mode-map
  "M-TAB"   nil
  "C-c v"   'cider-load-buffer
  "C-c C-s" 'cider-switch-to-repl-buffer)
(ublt/keys 'cider-repl cider-repl-mode-map
  "M-TAB"   nil
  "C-c C-s" 'cider-switch-to-last-clojure-buffer)
(ublt/keys 'scheme scheme-mode-map
  "C-c C-s" 'switch-to-scheme
  "C-c v"   'scheme-compile-file)
(ublt/keys 'ess-mode ess-mode-map
  "C-c v"   'ess-eval-buffer
  ;; "C-c C-s" 'ess-switch-to-inferior-or-script-buffer
  )

(ublt/keys "factor-mode" factor-mode-map
  "C-c C-c" 'fuel-eval-definition
  "C-c C-s" 'run-factor)
(ublt/keys "factor-mode" fuel-mode-map
  "C-c C-c" 'fuel-eval-definition
  "C-c C-s" 'run-factor)

(ublt/keys "octave-mode" octave-mode-map
  "C-c C-c" 'octave-send-defun
  "C-c C-r" 'octave-send-region
  "C-c C-s" 'octave-show-process-buffer)

;; (ublt/keys "python-mode" py-mode-map
;;   "C-c C-c" 'py-execute-def-or-class ; was py-execute-buffer
;;   "C-c C-r" 'py-execute-region       ; was py-shift-region-right
;;   "C-c C-l" 'py-execute-buffer       ; was py-shift-region-left
;;   "C-c C-s" 'py-shell                ; was py-execute-string
;;   "C-c v"   'py-execute-buffer)

(ublt/keys 'elpy elpy-mode-map
  "C-c C-c" 'python-shell-send-defun    ;was elpy-shell-send-region-or-buffer
  "C-c C-r" 'python-shell-send-region   ;was elpy-refactor
  "C-c C-v" 'python-shell-send-buffer   ;was elpy-check
  "C-c C-l" 'python-shell-send-file
  "C-c C-s" 'elpy-shell-switch-to-shell ;was elpy-rgrep-symbol
  "M-p"     nil                         ;was elpy-nav-backward-definition
  "M-n"     nil                         ;was elpy-nav-forward-definition
  "C-<up>"  nil                         ;was elpy-nav-backward-definition
  "C-<down>"  nil                       ;was elpy-nav-forward-definition
  "M-a"     nil                         ;was elpy-nav-backward-statement
  "M-e"     nil                         ;was elpy-nav-forward-statement
  "C-<left>" nil                        ;was elpy-nav-backward-iblock
  "C-<right>" nil
  "M-<left>"  nil
  "M-<right>" nil
  "M-,"       'pop-tag-mark
  ;; "M-<left>" 'elpy-nav-backward-definition ;was backward-list
  ;; "M-<right>" 'elpy-nav-forward-definition ;was forward-list
  "C-M-i" nil
  )

(ublt/keys "sql" sql-mode-map
  "C-c C-s" 'sql-product-interactive ; was sql-send-string
  "C-M-x"   'sql-send-paragraph
  "C-c C-d" 'ublt/sql-describe-thing-at-point)

;; (with-eval-after-load "erlang"
;;   (add-hook 'erlang-mode-hook
;;             (lambda ()
;;               (ublt/define-keys erlang-mode-map
;;                 "C-c v"   'erlang-compile
;;                 "C-c C-l" 'ublt/erlang-compile-and-display ; was erlang-compile-display
;;                 "C-c C-s" 'erlang-shell-display            ; was erlang-show-syntactic-information
;;                 ))))
(ublt/keys "erlang" erlang-mode-map
  "C-c C-l" 'ublt/erlang-compile-and-display ; was erlang-compile-display
  "C-c C-s" 'erlang-shell-display ; was erlang-show-syntactic-information
  "C-c v"   'erlang-compile)


;;; Other bindings specific to language modes
(ublt/keys "python-mode" py-mode-map
  "'"      'skeleton-pair-insert-maybe)

(ublt/keys "slime-repl" slime-repl-mode-map
  "M-I"   'slime-repl-delete-from-input-history
  "M-TAB" 'company-complete
  "DEL"    nil
  "M-s"    nil
  "C-c p" 'slime-repl-set-package
  "C-c n" 'slime-repl-set-package)

(ublt/keys "markdown-mode" markdown-mode-map
  "<M-down>"  'markdown-move-down
  "<M-up>"    'markdown-move-up
  "<M-right>" 'markdown-demote
  "<M-left>"  'markdown-promote)

(ublt/keys 'debug debugger-mode-map
  "M-." 'elisp-slime-nav-find-elisp-thing-at-point)


;;; Misc

(ublt/keys 'hideshow hs-minor-mode-map
  "M-a"   'hs-toggle-hiding)

(ublt/keys "isearch" isearch-mode-map
  'helm-swoop  'helm-swoop-from-isearch
  "M-<return>" 'ublt/isearch-exit-other-end
  "C-M-w"      'ublt/isearch-yank-symbol)

(ublt/keys 'dired dired-mode-map
  ")"          'dired-hide-details-mode
  "M-RET"      'ublt/dired-open-native
  ;; It makes more sense to search in filenames by default
  "C-s"        'dired-isearch-filenames-regexp
  "C-S-s"      'isearch-forward-regexp
  "C-s-s"      'dired-isearch-filenames
  "C-M-S-s"    'isearch-forward
  "M-l"        'move-to-window-line-top-bottom
  "C-c C-c"    'dired-toggle-read-only
  "M-o"        'dired-omit-mode
  "M-p"        'ublt/dired-preview-previous
  "M-n"        'ublt/dired-preview-next)

(ublt/keys 'rg rg-mode-map
  "C-c C-c" 'wgrep-change-to-wgrep-mode)

(ublt/keys 'rg global-map
  "s-e" rg-global-map)

(ublt/keys 'magit magit-mode-map
  "U"     'magit-unstage-all
  "M-a"   'magit-section-toggle
  "S-SPC" 'magit-diff-show-or-scroll-down
  "C-M-u" 'magit-section-up)
(ublt/keys 'with-editor with-editor-mode-map
  "s-s"     'with-editor-finish
  "C-x C-s" 'with-editor-finish
  "s-k"     'with-editor-cancel)
(ublt/keys 'git-timemachine git-timemachine-mode-map
  "M-p" 'git-timemachine-show-previous-revision
  "M-n" 'git-timemachine-show-next-revision)
(ublt/keys 'git-rebase git-rebase-mode-map
  'undo-tree-undo 'git-rebase-undo)

(ublt/keys "info" Info-mode-map
  "<kp-delete>" 'Info-scroll-up
  "S-SPC"       'Info-scroll-down)

(ublt/keys "woman" woman-mode-map
  "<kp-delete>" 'scroll-up
  "S-SPC"       'scroll-down)

;;; XXX: Why not working?
(ublt/keys "twittering-mode" twittering-mode-map
  "S-SPC" 'twittering-scroll-down)

(ublt/keys 'package package-menu-mode-map
  "S-SPC" 'scroll-down-command)

;; NTA XXX: Their "yank" variations are not as good
(ublt/keys 'ess-mode ess-mode-map
  "C-y" nil)
(ublt/keys 'org org-mode-map
  "C-y" nil)

(ublt/keys 'org org-mode-map
  ;; "M-<return>" 'org-insert-heading-respect-content
  ;; "C-<return>" 'org-insert-heading
  ;; "M-S-<return>" 'org-insert-todo-heading-respect-content
  ;; "C-S-<return>" 'org-insert-todo-heading

  ;; Why org uses these functions for sparse-tree-match navigation I
  ;; don't even know. Well, it's likely there will never be another
  ;; definition of "error" in org buffer, so it's fine.
  ;; TODO: `org-sparse-tree'
  ;; TODO: `org-store-link'
  ;; TODO: `org-insert-link'
  "M-p" 'previous-error
  "M-n" 'next-error
  "M-a" 'org-cycle
  'helm-semantic-or-imenu 'helm-org-in-buffer-headings
  )

(ublt/keys 'org-agenda org-agenda-mode-map
  "C-<down>" 'org-agenda-do-date-later
  "C-<up>"   'org-agenda-do-date-earlier)

(with-eval-after-load 'expand-region
  (setq expand-region-fast-keys-enabled t
        expand-region-contract-fast-key "u"
        expand-region-reset-fast-key "e"))

(provide 'ublt-dvorak)
