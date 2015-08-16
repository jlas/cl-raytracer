;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

;; infinity
(defconstant +inf+ (exp 20))

(defun clamp-to-rgb (v1)
  (declare (optimize (safety 0)) (type (signed-byte 32) x))
  (vector (round (* (max 0 (min 1 (elt v1 0))) 255))
          (round (* (max 0 (min 1 (elt v1 1))) 255))
          (round (* (max 0 (min 1 (elt v1 2))) 255))))

;; cross product
(defun cross (v1 v2)
  (let ((xl (elt v1 0)) (yl (elt v1 1)) (zl (elt v1 2))
        (xr (elt v2 0)) (yr (elt v2 1)) (zr (elt v2 2)))
    (list (- (* yl zr) (* zl yr))
          (- (* zl xr) (* xl zr))
          (- (* xl yr) (* yl zr)))))

;; dot product
(defun dot (v1 v2)
  (+ (+ (* (elt v1 0) (elt v2 0))
        (* (elt v1 1) (elt v2 1)))
     (* (elt v1 2) (elt v2 2))))

;; weird mult
(defun vmult-alt (v1 v2)
  (list (* (elt v1 0) (elt v2 0))
        (* (elt v1 1) (elt v2 1))
        (* (elt v1 2) (elt v2 2))))

;; magnitude
(defun mag (v1)
  (sqrt (+ (+ (expt (elt v1 0) 2)
              (expt (elt v1 1) 2))
           (expt (elt v1 2) 2))))

;; vector operation
(defmacro vop (v1 v2 f)
  `(list
    (,f (elt ,v1 0) (elt ,v2 0))
    (,f (elt ,v1 1) (elt ,v2 1))
    (,f (elt ,v1 2) (elt ,v2 2))))

;; vector subtraction
(defun vmin (v1 v2)
  (vop v1 v2 -))

;; vector addition
(defun vadd (v1 v2)
  (vop v1 v2 +))

(defmacro vscaleop (s v1 f)
  `(list
    (,f (elt ,v1 0) ,s)
    (,f (elt ,v1 1) ,s)
    (,f (elt ,v1 2) ,s)))

;; vector rotation by quaternion
(defun v-rot-quat (q v)
  (let* ((qv (list (elt q 0) (elt q 1) (elt q 2)))
         (w (elt q 3))
         (uv (cross qv v))
         (uuv (cross qv uv)))
    (vadd (vadd v (vmult (* 2 w) uv)) (vmult 2 uuv))))

;; vector-scalar multiplication
(defun vmult (s v1)
  (vscaleop s v1 *))

;; vector-scalar division
(defun vdiv (s v1)
  (vscaleop s v1 /))

;; vector normalize
(defun normalize (v1)
  (vdiv (sqrt (loop for i from 0 to 2 sum (* i i))) v1))

;;(defconstant +pif+ (coerce pi 'single-float))

(defun to-rad (degrees)
  (/ (* degrees pi) 180))
