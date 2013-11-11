;;; TODO: Maybe functions/macros to create fontspec from scratch?
;;; Maybe TODO: Integration with `ublt-themes'

(eval-when-compile
  (require 'cl))

(defvar ublt/fixed-width-fonts
  (case system-type
    ('darwin '("DejaVu Sans Mono-14" "Menlo-14" "Monaco-14"
               "Consolas-15"))
    ('gnu/linux '("Inconsolata-12" "Fira Mono-11" "DejaVu Sans Mono-10"))
    (t '("Courier New-12" "Arial-12"))))

(defvar ublt/variable-width-fonts
  (case system-type
    ('darwin '("Helvetica-16"))
    ('gnu/linux '("Fira Sans light-13"))
    (t '("Arial-12"))))

(defun ublt/default-fixed-width-font ()
  (first ublt/fixed-width-fonts))

(defun ublt/default-variable-width-font ()
  (first ublt/variable-width-fonts))

;;; TODO: Refactor
(defun ublt/toggle-fonts ()
  (interactive)
  (let* ((fonts ublt/fixed-width-fonts)
         (cur-pos (get this-command 'pos))
         (N (length fonts))
         font)
    (setq cur-pos (if cur-pos (% cur-pos N) 0))
    (setq font (nth cur-pos fonts))
    (modify-all-frames-parameters (list (cons 'font font)))
    ;; FIX: Should be "current theme"
    (color-theme-ubolonton-dark)
    (message "Font: %s" font)
    (put this-command 'pos (% (1+ cur-pos) N))))

;;; Default font
(let ((font (ublt/default-fixed-width-font)))
  (modify-all-frames-parameters `((font . ,font))))


;;; Fontsets

;;; TODO: What is the way to prioritize character sets? This seems to
;; have no effect if both :font/:fontset are set. The priority list
;; must be somewhere else? Or are there 2 lists?
;; (set-language-environment "Vietnamese")

;;; :family => invalid family => no font specified, => non-user-specified default

;; ELISP> (face-attribute 'variable-pitch :font)
;; unspecified
;; ELISP> (face-attribute 'variable-pitch :family)
;; "ubltv"
;; ELISP> (face-attribute 'variable-pitch :fontset)
;; unspecified

;;; :font => weirdest one. The spec's default font is registered, but
;; the overrides work in a funky way (ignored for Vietnamese (even if
;; "preferred charset" is resolved to "viscii" not "unicode"), ok for
;; Russian, (even if "preferred charset" is resolved to "unicode" not
;; "cyrillic-iso8859-5")) (well actually the thing read "(Unicode
;; (ISO10646))" which may explain it (no it does not, it's like that
;; in both cases))

;; ELISP> (face-attribute 'variable-pitch :font)
;; #<font-object "-unknown-Fira Sans-light-normal-normal-*-16-*-*-*-*-0-iso10646-1">
;; ELISP> (face-attribute 'variable-pitch :family)
;; "Fira Sans"
;; ELISP> (face-attribute 'variable-pitch :fontset)
;; unspecified

;;; :fontset => seems to work fine regardless of "preferred charset"
;;; reported for each character. The import thing seems to be what
;;; `fontset-font' reports instead (TODO: not even sure about that)

;; ELISP> (face-attribute 'variable-pitch :font)
;; unspecified
;; ELISP> (face-attribute 'variable-pitch :family)
;; "Sans Serif"
;; ELISP> (face-attribute 'variable-pitch :fontset)
;; "-unknown-fira sans-light-normal-normal--*-*-*-*-m-*-fontset-ubltv"
;; ELISP> (face-attribute 'variable-pitch :fontspec)

;;; Don't do this, it's the same as (set-face-attribute ... :font)
;; which is weird, as described above (probably that's the reason
;; fontsets are excluded from its completion list when calling
;; interactively, duh)
;; (set-frame-font "fontset-ubltf")


(defun ublt/assign-font (fontset &rest mappings)
  (declare (indent 1))
  (dolist (mapping mappings)
    (let ((font (car mapping))
          (charsets (cdr mapping)))
      (dolist (charset charsets)
        (set-fontset-font fontset charset font nil)))))


(defvar ublt/variable-width-fontset
  "-unknown-Fira Sans-light-normal-normal--*-*-*-*-m-*-fontset-ubltv")
(create-fontset-from-fontset-spec ublt/variable-width-fontset)
(ublt/assign-font ublt/variable-width-fontset
  '("DejaVu Sans"
    vietnamese-viscii-upper
    vietnamese-viscii-lower
    viscii
    vscii
    vscii-2
    tcvn-5712))

;;; Don't set :font/:fontset/:family alone. See the long explanation
;;; section above
(set-face-attribute 'variable-pitch nil
                    :fontset "fontset-ubltv"
                    :font "Fira Sans"
                    :height 130
                    :weight 'extra-light
                    ;; :font "Ubuntu Condensed"
                    ;; :height 135
                    ;; :weight 'extra-light
                    )

(dolist
    (rescale '((".*DejaVu Sans-.*" . 0.9)))
  (add-to-list 'face-font-rescale-alist rescale))


;;; The non-uniformity of face/font/fontset handling (normal vs.
;;; default) is so ugly

(set-face-attribute 'default nil
                    :font "Inconsolata"
                    :height 120)

(ublt/assign-font (face-attribute 'default :fontset)
  '("Droid Sans Mono"
    vietnamese-viscii-upper
    vietnamese-viscii-lower
    viscii
    vscii
    vscii-2
    tcvn-5712)
  '("Fira Mono"
    cyrillic-iso8859-5))

(dolist
    (rescale '((".*Droid Sans Mono-.*" . 0.9)
               (".*Fira Mono-.*" . 0.9)))
  (add-to-list 'face-font-rescale-alist rescale))


(provide 'ublt-font)
