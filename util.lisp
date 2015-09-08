;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

;; infinity
(defconstant +inf+ (exp 20))

(defun clamp-to-rgb (v1)
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

(defun quat-w (q)
  (cos (* 0.5 (elt q 3))))

(defun to-angle-axis (q)
  (let* ((x (elt q 0))
         (y (elt q 1))
         (z (elt q 2))
         (norm (+ (expt x 2) (expt y 2) (expt z 3))))
    (cond ((> norm 0)
           (list (* 2 (acos (quat-w q)))
                 (vmult (/ 1 (sqrt norm)) (vector x y z))))
          (t '(0 #(1 0 0))))))

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

;; make rotation matrix
;; from https://en.wikipedia.org/wiki/Rotation_formalisms_in_three_dimensions#Euler_angles_.28x-y-z_extrinsic.29_.E2.86.92_Rotation_matrix
(defun to-rot-matrix (x y z)
  (vector (vector (* (cos y) (cos z))
                  (+ (* (cos x) (sin z)) (* (sin x) (sin y) (cos z)))
                  (- (* (sin x) (sin z)) (* (cos x) (sin y) (cos z))))
          (vector (* -1 (cos y) (sin z))
                  (- (* (cos x) (cos z)) (* (sin x) (sin y) (sin z)))
                  (+ (* (sin x) (cos z)) (* (cos x) (sin y) (sin z))))
          (vector (sin y) (* -1 (sin x) (cos y)) (* (cos x) (cos y)))))

;; turn rotation matrix to quaternion
;; from http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/
(defun to-quat (m)
  (let* ((r0 (elt m 0))
         (r1 (elt m 1))
         (r2 (elt m 2))
         (trace (+ (elt r0 0) (elt r1 1) (elt r2 2))))
    (cond ((> trace 0)
           (let* ((s (* (sqrt (1+ trace)) 2))
                  (w (/ s 4))
                  (x (/ (- (elt r2 1) (elt r1 2)) s))
                  (y (/ (- (elt r0 2) (elt r2 0)) s))
                  (z (/ (- (elt r1 0) (elt r0 1)) s)))
             (vector x y z w)))
          ((and (> (elt r0 0) (elt r1 1)) (> (elt r0 0) (elt r2 2)))
           (let* ((s (* (sqrt (1+ (- (elt r0 0) (elt r1 1) (elt r2 2)))) 2))
                  (w (/ (- (elt r2 1) (elt r1 2)) s))
                  (x (/ s 4))
                  (y (/ (+ (elt r0 1) (elt r1 0)) s))
                  (z (/ (+ (elt r0 2) (elt r2 0)) s)))
             (vector x y z w)))
          ((> (elt r1 1) (elt r2 2))
           (let* ((s (* (sqrt (1+ (- (elt r1 1) (elt r0 0) (elt r2 2)))) 2))
                  (w (/ (- (elt r0 2) (elt r2 0)) s))
                  (x (/ (+ (elt r0 1) (elt r1 0)) s))
                  (y (/ s 4))
                  (z (/ (+ (elt r1 2) (elt r2 1)) s)))
             (vector x y z w)))
          (t
           (let* ((s (* (sqrt (1+ (- (elt r2 2) (elt r0 0) (elt r1 1)))) 2))
                  (w (/ (- (elt r1 0) (elt r0 1)) s))
                  (x (/ (+ (elt r0 2) (elt r2 0)) s))
                  (z (/ (+ (elt r1 2) (elt r2 1)) s))
                  (y (/ s 4)))
             (vector x y z w))))))

(defun quat-mult (q1 q2)
  (vector (- (* (elt q1 3) (elt q2 3))
             (* (elt q1 0) (elt q2 0))
             (* (elt q1 1) (elt q2 1))
             (* (elt q1 2) (elt q2 2)))
          (+ (* (elt q1 3) (elt q2 0))
             (* (elt q1 0) (elt q2 3))
             (* (elt q1 1) (elt q2 2))
             (* -1 (elt q1 2) (elt q2 1)))
          (+ (* (elt q1 3) (elt q2 1))
             (* (elt q1 1) (elt q2 3))
             (* (elt q1 2) (elt q2 0))
             (* -1 (elt q1 0) (elt q2 2)))
          (+ (* (elt q1 3) (elt q2 2))
             (* (elt q1 2) (elt q2 3))
             (* (elt q1 0) (elt q2 1))
             (* -1 (elt q1 1) (elt q2 0)))))

;; rotation transformations
(defun rot-x (v degrees)
  (vector (dot v (vector 1 0 0))
          (dot v (vector 0 (cos degrees) (* -1 (sin degrees))))
          (dot v (vector 0 (sin degrees) (cos degrees)))))

(defun rot-y (v degrees)
  (vector (dot v (vector (cos degrees) 0 (sin degrees)))
          (dot v (vector 0 1 0))
          (dot v (vector (* -1 (sin degrees)) 0 (cos degrees)))))

(defun rot-z (v degrees)
  (vector (dot v (vector (cos degrees) (* -1 (sin degrees)) 0))
          (dot v (vector (sin degrees) (cos degrees) 0))
          (dot v (vector 0 0 1))))
