;;; ivan-face.el --- setup font faces

(defvar ivan-face-background "black"
  "background color")

(defvar ivan-face-foreground "white"
  "foreground color")

(defvar ivan-face-cache nil
  "cache used to reverse background color")

(defun ivan-face-toggle-background ()
  "Toggle between a black and light grey background."
  (interactive)
  (if (string= ivan-face-background "black")
      (ivan-face-set-background-foreground "#b9babd" "black" t)
    (ivan-face-set-background-foreground "black" "white" nil)))

(defun ivan-face-set-background-foreground (bg fg bright-bg)
  "Set BG background and FG foreground hint.
Set BRIGHT-BG to t if the foreground BG is light."
  (setq ivan-face-background bg
        ivan-face-foreground fg)
  (ivan-face-1 ivan-face-cache nil bright-bg))

;; TDB use a plist
(defun ivan-face-color (color bright-background)
  "For a given COLOR and BRIGHT-BACKGROUND returns an adjusted color."
  (if bright-background
      (cond ((string= color "orange") "orange4")
            ((string= color "yellow") "yellow4")
            ((string= color "purple") "dark magenta")
            ((string= color "blue") "dark blue")
            ((string= color "blue2") "DeepSkyBlue4")
            ((string= color "green") "green4")
            ((string= color "normal") ivan-face-foreground)
            ((string= color "underline") ivan-face-foreground))
    (cond ((string= color "blue") "cyan")
          ((string= color "blue2") "LightSteelBlue")
          ((string= color "normal") ivan-face-foreground)
          ((string= color "purple") "magenta")
          ((string= color "underline") ivan-face-foreground)
          (t color))))

(defmacro ivan-face (&rest list)
  "Take in LIST of face to change.
LIST looks like this (face color face color...)"
  `(ivan-face-1 (quote ,list) t))

(defun ivan-face-1 (list &optional cache bright-background)
  "Go through LIST of faces and apply color.
If CACHE is t, append LIST to the cache for changing background
color. If BRIGHT-BACKGROUND is t the background is light"
  (if cache
      (setq ivan-face-cache (append ivan-face-cache list)))
  (let (face color reverse color-string)
    (while list
      (setq face (pop list)
            color (pop list)
            reverse (string= (substring (symbol-name color) 0 2) "r-"))
      (setq color-string
            (if reverse
                (setq color-string (substring (symbol-name color) 2))
              (symbol-name color)))
      (setq color-string (ivan-face-color color-string bright-background))
      (ivan-face-2 face color-string color reverse))))

(defun ivan-face-2 (face  color-string color reverse)
  "Apply COLOR-STRING to FACE.
When REVERSE is t it indicates the foreground and background are
swapped. It checks COLOR as it might be set underline"
  (make-face face)
  (set-face-attribute
   face nil
   :background (if reverse color-string ivan-face-background)
   :box nil
   :foreground (if reverse ivan-face-background color-string)
   :inherit nil
   :slant 'normal
   :strike-through nil
   :underline (eq color 'underline)
   :weight 'normal))

(defun ivan-face-hex (red green blue)
  "Return string hex of color specified by RED GREEN BLUE."
  (format "#%02x%02x%02x" (lsh red -8) (lsh green -8) (lsh blue -8)))

(defun ivan-face-step (list number color)
  "Return NUMBER of gradient for LIST of index COLOR."
  (let ((start (nth color (car list)))
        (end (nth color (cadr list))))
    (if (= end start)
        (make-list number start)
      (number-sequence start end (/ (- end start) (- number 1))))))

(defun ivan-face-gradient (face-prefix number color-start color-end)
  "Create NUMBER of FACE-PREFIX from COLOR-START to COLOR-END."
  (let* ((list (list (color-values color-start) (color-values color-end)))
         (red (ivan-face-step list number 0))
         (green (ivan-face-step list number 1))
         (blue (ivan-face-step list number 2))
         (num 0))
    (while (< num number)
      (let ((face (make-face
                   (intern
                    (concat face-prefix (number-to-string (1+ num)))))))
        (set-face-attribute face
                            nil :foreground
                            (ivan-face-hex (nth num red)
                                           (nth num green)
                                           (nth num blue))))
      (set 'num (1+ num)))))

(custom-set-faces
 ;; I don't use italic...
 '(italic ((t (:slant normal))))
 ;; Font used for buffer name in modeline
 '(mode-line-buffer-id ((t (:weight normal))))
 '(mode-line-emphasis ((t (:weight normal))))
 '(erc-notice-face ((t (:weight normal)))))

(ivan-face
 bold yellow
 bold-italic blue
 button blue
 blue blue
 cursor r-yellow
 custom-comment-face normal
 custom-comment-tag-face normal
 custom-group-tag blue
 custom-invalid red
 custom-state-face normal
 custom-variable-tag blue
 custom-state normal
 custom-variable-tag normal
 cvs-filename normal
 cvs-handled green
 cvs-header yellow
 default normal
 eshell-ls-archive-face blue
 eshell-ls-backup-face normal
 eshell-ls-clutter-face normal
 eshell-ls-directory-face yellow
 eshell-ls-executable-face green
 eshell-ls-missing-face normal
 eshell-ls-product-face normal
 eshell-ls-readonly-face normal
 eshell-ls-special-face normal
 eshell-ls-symlink-face purple
 eshell-ls-unreadable-face normal
 eshell-prompt-face normal
 excerpt normal
 highlight r-darkseagreen2
 hyper-apropos-documentation yellow
 hyper-apropos-heading normal
 hyper-apropos-hyperlink blue
 hyper-apropos-major-heading normal
 hyper-apropos-section-heading normal
 isearch r-yellow
 italic yellow
 link blue
 link-visited purple
 log-view-message yellow
 log-view-file normal
 nobreak-space normal
 secondary-selection r-yellow
 show-paren-match r-green
 show-paren-mismatch r-red
 svn-status-directory-face yellow
 svn-status-locked-face red
 swbuff-current-buffer-face orange
 swbuff-separator-face normal
 widget-button blue)

(provide 'ivan-face)

;; Copyright (C) 2007 Ivan Kanis
;; Author: Ivan Kanis
;; 
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
