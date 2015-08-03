;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :raytracer)

;; infinity
(defconstant +inf+ (exp 20))

;; cross product
(defun cross (v1 v2)
  (let ((xl (nth 0 v1)) (yl (nth 1 v1)) (zl (nth 2 v1))
        (xr (nth 0 v2)) (yr (nth 1 v2)) (zr (nth 2 v2)))
    (list (- (* yl zr) (* zl yr))
          (- (* zl xr) (* xl zr))
          (- (* xl yr) (* yl zr)))))

;; dot product
(defun dot (v1 v2)
  (+ (+ (* (nth 0 v1) (nth 0 v2))
        (* (nth 1 v1) (nth 1 v2)))
     (* (nth 2 v1) (nth 2 v2))))

;; vector operation
(defmacro vop (v1 v2 f)
  `(list
    (,f (nth 0 ,v1) (nth 0 ,v2))
    (,f (nth 1 ,v1) (nth 1 ,v2))
    (,f (nth 2 ,v1) (nth 2 ,v2))))

;; vector subtraction
(defun vmin (v1 v2)
  (vop v1 v2 -))

;; vector addition
(defun vadd (v1 v2)
  (vop v1 v2 +))

(defmacro vscaleop (s v1 f)
  `(list
    (,f (nth 0 ,v1) ,s)
    (,f (nth 1 ,v1) ,s)
    (,f (nth 2 ,v1) ,s)))

;; vector rotation by quaternion
(defun v-rot-quat (q v)
  (let* ((qv (list (nth 0 q) (nth 1 q) (nth 2 q)))
         (w (nth 3 q))
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
  (vdiv v1 (sqrt (loop for i from 0 to 2 sum (* i i)))))

;;(defconstant +pif+ (coerce pi 'single-float))

(defun to-rad (degrees)
  (/ (* degrees pi) 180))
