;;; TODO: Maybe functions/macros to create fontspec from scratch?

(eval-when-compile
  (require 'cl))


;;; TODO: Refactor
(defun ublt/toggle-fonts ()
  (interactive)
  (let* ((fonts (case system-type
                  ('darwin '("DejaVu Sans Mono-14" "Menlo-14" "Monaco-14"
                             "Consolas-15"))
                  ('gnu/linux '("Inconsolata-12" "Fira Mono-11" "DejaVu Sans Mono-10"))
                  (t '("Courier New-12" "Arial-12"))))
         (cur-pos (get this-command 'pos))
         (N (length fonts))
         font)
    (setq cur-pos (if cur-pos (% cur-pos N) 0))
    (setq font (nth cur-pos fonts))
    (modify-all-frames-parameters (list (cons 'font font) ;; (cons 'height 100)
                                        ))
    ;; TODO: Should be "current theme"
    (color-theme-ubolonton-dark)
    (message "Font: %s" font)
    (put this-command 'pos (% (1+ cur-pos) N))))

;;; Default font
(case system-type
  ('darwin (modify-all-frames-parameters
            '((font . "DejaVu Sans Mono-14"))))
  ('windows-nt (modify-all-frames-parameters
                '((font . "Courier New-12"))))
  ('gnu/linux (modify-all-frames-parameters
               '((font . "Inconsolata-12")))))


(defun ublt/assign-font (fontset &rest mappings)
  (declare (indent 1))
  (dolist (mapping mappings)
    (let ((font (car mapping))
          (charsets (cdr mapping)))
      (dolist (charset charsets)
        (set-fontset-font fontset charset font)))))


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




;;; Maybe TODO: Integration with `ublt-themes'
;; Non-code text reads better in proportional font
;; (when (member window-system '(x ns w32))
;;   (set-face-font 'variable-pitch (case system-type
;;                                    ;; ('gnu/linux "Fira Sans-12")
;;                                    ('gnu/linux "Fira Sans light-13")
;;                                    ;; ('gnu/linux "DejaVu Sans-11")
;;                                    ;; ('gnu/linux "Helvetica")
;;                                    ('darwin "Helvetica-16")
;;                                    (t "Arial"))))


;;; Fontsets


;;; TODO: What is the way to prioritize character sets?
;; (set-language-environment "Vietnamese")


;; (set-fontset-font
;;  ublt/variable-width-fontset 'unicode
;;  "DejaVu Sans" nil)
;; (set-fontset-font
;;  ublt/variable-width-fontset 'ascii
;;  "Fira Sans" nil 'prepend)

;; ;; (set-face-font 'variable-pitch "DejaVu Sans")
;; (set-face-font 'variable-pitch "fontset-ubltv")

;;; XXX: Yes this must be :fontset, not :font, even though the
;;; documentation says nothing about :fontset. Duh!

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
;;; `fontset-font' reports instead

;; ELISP> (face-attribute 'variable-pitch :font)
;; unspecified
;; ELISP> (face-attribute 'variable-pitch :family)
;; "Sans Serif"
;; ELISP> (face-attribute 'variable-pitch :fontset)
;; "-unknown-fira sans-light-normal-normal--*-*-*-*-m-*-fontset-ubltv"
;; ELISP> (face-attribute 'variable-pitch :fontspec)

;;; (:font + :fontset) seem to work

(set-face-attribute 'variable-pitch nil
                    :fontset "fontset-ubltv"
                    :font "Fira Sans light"
                    :height 130)

;;; Don't do this, it's the same as (set-face-attribute ... :font)
;; which is weird, as described above (probably that's the reason
;; fontsets are excluded from its completion list when calling
;; interactively, duh)
;; (set-frame-font "fontset-ubltf")


(provide 'ublt-font)
