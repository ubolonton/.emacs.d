(require 'ublt-util)

;;; TODO: Maybe functions/macros to create fontspec from scratch?
;;; Maybe TODO: Integration with `ublt-themes'

;;; emacs-mac-app
(pcase window-system
  ('mac (mac-auto-operator-composition-mode +1)))


;;; Fontsets

;; NOTE: Use (modify-all-frames-parameters nil ...) to reset font if there
;; is "invalid font" error. Such bizarre API.

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
;;; reported for each character. The important thing seems to be what
;;; `fontset-font' reports instead (TODO: not even sure about that).

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
;; interactively, duh).
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
(dolist (rescale '((".*DejaVu Sans-.*" 0.94)))
  (pcase-let ((`(,font ,size) rescale))
    (ublt/assoc! 'face-font-rescale-alist font size)))

;;; XXX: Similar to the above. If a fontset is used, faces' attributes
;;; are ignored in favor of those specified in the fontset, except for
;;; characters displayed by the default font in the fontset (ascii
;;; range). Therefore it's best to specify only `:family' for
;;; non-primary fonts in the fontset.
(defvar ublt/variable-width-fontset
  "-unknown-Lucida Grande-normal-normal-normal--*-*-*-*-m-*-fontset-ubltv")
(create-fontset-from-fontset-spec ublt/variable-width-fontset)
(ublt/assign-font ublt/variable-width-fontset
  ;; For a more programmatic manipulation, use this and leave the
  ;; FONTSET-NAME blank (see `create-fontset-from-fontset-spec')
  `(,(font-spec :family "Lucida Grande"
                :weight 'normal
                :size 14.0)
    ascii)
  ;; Vietnamese charsets.
  `(,(font-spec :family "DejaVu Sans")
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
  `(,(font-spec :family "DejaVu Sans")
    ;; For Vietnamese characters already covered by extended latin
    latin-iso8859-1
    ;; Russian
    cyrillic-iso8859-5)
  `(,(font-spec :family "Lucida Grande")
    (?☑ . ?☑)
    (?☐ . ?☐)))



;; ;;; Don't set :font/:fontset/:family alone. See the long explanation
;; ;;; section above. And contrary to whet the doc says, `font-spec'
;; ;;; cannot take a fontset as its `:family', like `set-face-attribute'
;; (set-face-attribute 'variable-pitch nil
;;                     :fontset "fontset-ubltv"
;;                     ;; This is determined from FONTSET-NAME part (see
;;                     ;; `create-fontset-from-fontset-spec')
;;                     :font "fontset-ubltv"
;;                     ;; :font (font-spec :family "Lucida Grande"
;;                     ;;                  :weight 'light
;;                     ;;                  :size 13.0)
;;                     ;; :height 130         ; 1/10 points
;;                     ;; :font (font-spec :family "Ubuntu Condensed"
;;                     ;;                  :weight 'extra-light
;;                     ;;                  :size 13.5)
;;                     )

(set-face-attribute
 'variable-pitch nil
 :fontset "fontset-ubltv"
 :font "fontset-ubltv")

;;; The non-uniformity of face/font/fontset handling (normal vs.
;;; default) is so ugly. TODO FIX: Make sure applying theme does not
;;; affect this (or somehow restore this after applying theme, or make
;;; this part of the theme)

;;; TODO: More aggressive prettification and now that we get this
;;; thing and large unicode fonts (Quivira, Gentium, Doulos, Charis...)

(defvar ublt/fixed-width-fontset
  (format "-unknown-Fira Code-normal-normal-normal-*-*-*-*-*-m-*-fontset-ubltf"))
(create-fontset-from-fontset-spec ublt/fixed-width-fontset)

(dolist (rescale '((".*Fira Mono-.*" 1.00)
                   (".*Droid Sans Mono-.*" 1.00)
                   (".*DejaVu Sans Mono-.*" 1.00)
                   (".*Fantasque Sans Mono.*" 1.12)
                   (".*Symbol-.*" 1.239)))
  (pcase-let ((`(,font ,size) rescale))
    (ublt/assoc! 'face-font-rescale-alist font size)))

;;; XXX: Not sure why, but we need to set these for the default fontset as well (`t'), not just "ubltf".
(dolist (fontset (list ublt/fixed-width-fontset t))
  (ublt/assign-font fontset
    `(,(font-spec :family "Fira Code"
                  :weight 'normal)
      ascii)
    `(,(font-spec :family "Droid Sans Mono")
      vietnamese-viscii-upper
      vietnamese-viscii-lower
      viscii
      vscii
      vscii-2
      tcvn-5712)
    `(,(font-spec :family "Fira Mono")
      cyrillic-iso8859-5)
    `(,(font-spec :family "DejaVu Sans Mono")
      (?▸ . ?▸))
    `(,(font-spec :family "Symbol")
      (?⇒ . ?⇒)
      (?⇐ . ?⇐)
      (?☑ . ?☑)
      (?☐ . ?☐))
    ;; Fallback.
    `(,(font-spec :family "Arial Unicode MS")
      nil)))

(dolist (face '(default fixed-pitch))
  (set-face-attribute
   face nil
   :fontset "fontset-ubltf"
   :font "fontset-ubltf"))

(provide 'ublt-font-osx)
