;;; TODO: Maybe functions/macros to create fontspec from scratch?
;;; Maybe TODO: Integration with `ublt-themes'

(eval-when-compile
  (require 'cl))

(require 'ublt-util)


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

;;; XXX: When a fontset specifies sizes, Emacs ignores faces' relative
;;; `:height' except for the default font in the fontset.
;;; `face-font-rescale-alist' is used instead, which has the
;;; disadvantage of not allowing rescaling individual characters. For
;;; now this is acceptable, since I haven't needed to use 1 font at 2
;;; different sizes yet.
(dolist (rescale '((".*DejaVu Sans-.*" 0.96)))
  (destructuring-bind (font size) rescale
    (ublt/assoc! 'face-font-rescale-alist font size)))

(defvar ublt/variable-width-fontset
  "-unknown-Fira Sans-light-normal-normal--*-*-*-*-m-*-fontset-ubltv")
(create-fontset-from-fontset-spec ublt/variable-width-fontset)
(ublt/assign-font ublt/variable-width-fontset
  ;; For a more programmatic manipulation, use this and leave the
  ;; FONTSET-NAME blank (see `create-fontset-from-fontset-spec')
  `(,(font-spec :family "Fira Sans"
                :weight 'light
                :size 13.0
                )
    ascii)
  ;; Vietnamese charsets
  `(,(font-spec :family "DejaVu Sans"
                :weight 'extra-light
                ;; :size 11.0              ; points
                )
    ;; ,(font-spec :family "Arial" :weight 'bold)
    vietnamese-viscii-upper
    vietnamese-viscii-lower
    viscii
    vscii
    vscii-2
    tcvn-5712)
  ;; ;; XXX WTF: This would not work, failing with "Can't set a font
  ;; ;; for partial ASCII range"
  ;; `(,(font-spec :family "DejaVu Sans")
  ;;   (? . ? ))
  `(,(font-spec :family "Fira Sans"
                :weight 'light
                ;; :size 13.0
                )
    ;; For Vietnamese characters already covered by extended latin
    latin-iso8859-1
    ;; Russian
    cyrillic-iso8859-5))



;;; Don't set :font/:fontset/:family alone. See the long explanation
;;; section above. And contray to whet the doc says, `font-spec'
;;; cannot take a fontset as its `:family', like `set-face-attribute'
(set-face-attribute 'variable-pitch nil
                    :fontset "fontset-ubltv"
                    ;; This is determined from FONTSET-NAME part (see
                    ;; `create-fontset-from-fontset-spec')
                    :font "fontset-ubltv"
                    ;; :font (font-spec :family "Fira Sans"
                    ;;                  :weight 'light
                    ;;                  :size 13.0)
                    ;; :height 130         ; 1/10 points
                    ;; :font (font-spec :family "Ubuntu Condensed"
                    ;;                  :weight 'extra-light
                    ;;                  :size 13.5)
                    )

;;; The non-uniformity of face/font/fontset handling (normal vs.
;;; default) is so ugly. TODO FIX: Make sure applying theme does not
;;; affect this (or somehow restore this after applying theme, or make
;;; this part of the theme)

;;; TODO: More aggressive prettification and now that we get this
;;; thing and large unicode fonts (Quivira, Gentium, Doulos, Charis...)

;; NOTE: Use (modify-all-frames-parameters nil ...) to reset font if there
;; is "invalid font" error. Such bizarre API.
(set-face-attribute 'default nil
                    ;; :font (font-spec :family "Inconsolata"
                    ;;                  :weight 'normal
                    ;;                  :size 12.0
                    ;;                  )
                    :font (font-spec :family "CosmicSansNeueMono"
                                     :weight 'normal
                                     :size 12.0)
                    ;; :font (font-spec :family "Anonymous Pro"
                    ;;                  :weight 'normal
                    ;;                  :size 12.0)
                    )

(dolist (rescale '((".*Fira Mono-.*" 0.9)
                   (".*Droid Sans Mono-.*" 0.9)))
  (destructuring-bind (font size) rescale
    (ublt/assoc! 'face-font-rescale-alist font size)))

;;; FIX: Somehow variable-pitch font gets messed up after each theme
;;; application as well, and either evaluation this or
;;; `set-face-attribute' on `variable-pitch' again fixes that!?!?!
(ublt/assign-font (face-attribute 'default :fontset)
  `(,(font-spec :family "Droid Sans Mono"
                ;; :size 10.8
                )
    vietnamese-viscii-upper
    vietnamese-viscii-lower
    viscii
    vscii
    vscii-2
    tcvn-5712)
  `(,(font-spec :family "Fira Mono"
                ;; :size 10.8
                )
    cyrillic-iso8859-5)
  `(,(font-spec :family "Ume Mincho"
                ;; :size 12.0
                )
    (?▸ . ?▸))
  `(,(font-spec :family "Symbol"
                ;; :size 12.0
                )
    (?⇒ . ?⇒)
    (?⇐ . ?⇐))
  `(,(font-spec :family "Droid Sans Mono"
                :weight 'normal
                ;; :size 10.8
                )
    (?λ . ?λ))
  `(,(font-spec :family "Inconsolata"
                :weight 'normal)
	(?ƒ . ?ƒ)))

(provide 'ublt-font)
