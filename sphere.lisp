;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defclass sphere (shape)
  ((radius
    :initarg :radius
    :initform 1)))

(defmethod draw ((shape sphere))
  (let ((incr (/ pi 15)))
    (do ((long 0 (+ long incr)))
        ((>= long (* 2 pi)))
      (let ((nextLong (min (+ long incr) (* 2 pi))))
        (cl-opengl:with-primitives :triangle-strip
          (do ((lat 0 (+ lat (/ incr 2))))
              ((>= lat pi))
            (apply #'cl-opengl:vertex
                   (vmult
                    (slot-value shape 'radius)
                    (list (* (sin lat) (cos long))
                          (* -1 (cos lat))
                          (* (sin lat) (sin long)))))
            (apply #'cl-opengl:vertex
                   (vmult
                    (slot-value shape 'radius)
                    (list (* (sin lat) (cos nextLong))
                          (* -1 (cos lat))
                          (* (sin lat) (sin nextLong))))))
          (apply #'cl-opengl:vertex
                 (vmult
                  (slot-value shape 'radius)
                  (list (* (sin pi) (cos long))
                        (* -1 (cos pi))
                        (* (sin pi) (sin long)))))
          (apply #'cl-opengl:vertex
                 (vmult
                  (slot-value shape 'radius)
                  (list (* (sin pi) (cos nextLong))
                        (* -1 (cos pi))
                        (* (sin pi) (sin nextLong))))))))))

(defmethod intersect ((shape sphere) direction eye)
  (with-slots (position radius) shape
    (let* ((a (dot direction direction))
      (b (* 2 (dot direction (vmin eye position))))
      (c (- (dot
             (vmin eye position)
             (vmin eye position))
            (* radius radius)))
           (det (- (* b b) (* 4 (* a c)))))
      (cond ((< det 0) +inf+)
            (t
             (min
                (/ (- (* -1 b) (expt det 0.5)) (* 2 a))
                (/ (+ (* -1 b) (expt det 0.5)) (* 2 a))))))))

(defmethod normal ((shape sphere) direction eye vecscale)
  (with-slots (position) shape
    (normalize (vmin (vadd eye (vmult vecscale direction)) position))))

(defmethod ambient ((shape sphere) direction eye vecscale)
  (with-slots (material) shape
    (nth 0 material)))

(defmethod diffuse (shape direction eye vecscale)
  (with-slots (material) shape
    (nth 1 material)))

(defmethod specular ((shape sphere) direction eye vecscale)
  (with-slots (material) shape
    (nth 2 material)))

(defmethod texture (shape direction eye vecscale))

(defmethod refractiveIdx (shape direction eye vecscale)
  (with-slots (material) shape
    (nth 4 material)))
