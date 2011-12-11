;;; XXX: Don't look here
(defconst WR 0.299)
(defconst WB 0.114)
(defconst WG (- 1 WR WB))

;; I don't understand the reason behind choosing these coefficients,
;; so I used -0.5..0.5 range for both U and V, for convenience.
(defconst Umax 0.5) ; 0.435
(defconst Vmax 0.5) ; 0.615

(defun clut-rgb->yuv (rgb)
  (apply (lambda (r g b)
           (let* ((y (+ (* WR r) (* WG g) (* WB b)))
                  (u (* Umax (/ (- b y)
                                (- 1 WB))))
                  (v (* Vmax (/ (- r y)
                                (- 1 WR)))))
             (list y u v)))
         rgb))

(defun clut-yuv->rgb (yuv)
  (apply (lambda (y u v)
           (let* ((r (+ y (* (/ v Vmax)
                             (- 1 WR))))
                  (g (- y
                        (* (/ u Umax)
                           (/ WB WG)
                           (- 1 WB))
                        (* (/ v Vmax)
                           (/ WR WG)
                           (- 1 WR))))
                  (b (+ y (* (/ u Umax)
                             (- 1 WB)))))
             (list r g b)))
         yuv))

(defun clut-str->rgb (color)
  (mapcar (lambda (x) (/ x #xFF00 1.0))
          (color-values color)))

(defun clut-rgb->str (rgb)
  (apply 'format "#%02X%02X%02X"
         (mapcar (lambda (x) (* x #xFF 1.0))
                 rgb)))

(defun clut-str->yuv (str)
  (clut-rgb->yuv (clut-str->rgb str)))

(defun clut-yuv->str (yuv)
  (clut-rgb->str (clut-yuv->rgb yuv)))

(defun clut-str-inc-y (str y+)
  (let* ((yuv (clut-str->yuv str))
         (y (car yuv)))
    (setcar yuv (+ y y+))
    (clut-yuv->str yuv)))

(defun clut-str-with-y (str y)
  (let* ((yuv (clut-str->yuv str)))
    (setcar yuv y)
    (clut-yuv->str yuv)))

(provide 'color-util)
