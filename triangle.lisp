;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defclass triangle (shape)
  ((position
    :initarg :position)
   (orientation
    :initarg :orientation)
   (scale
    :initarg :scale
    :initform '(1 1 1))
   (vertices
    :initarg :vertices)
   (normal
    :initarg :normal)))

(defmethod draw ((shape triangle))
  (destructuring-bind (rotate_angle rotate_axis)
      (to-angle-axis (slot-value shape 'orientation))
    (apply #'cl-opengl:rotate
           (concatenate 'list (list (* (/ 180 pi) rotate_angle)) rotate_axis)))
  (apply #'cl-opengl:scale (slot-value shape 'scale))
  (cl-opengl:with-primitives :triangles
    (mapcar #'(lambda (vertex)
                (apply #'cl-opengl:normal (slot-value shape 'normal))
                (apply #'cl-opengl:vertex vertex))
            (slot-value shape 'vertices))))

(defmethod intersect ((shape triangle) direction eye)
  (let* ((vertA (elt (slot-value shape 'vertices) 0))
         (vertB (elt (slot-value shape 'vertices) 1))
         (vertC (elt (slot-value shape 'vertices) 2)))
    (destructuring-bind ((a b c) (d e f) (g h i) (j k l))
        (list (vmin vertA vertB) (vmin vertA vertC) direction (vmin vertA eye))
      (let ((int
             (* -1 (/ (+ (* f (- (* a k) (* j b)))
                         (* e (- (* j c) (* a l)))
                         (* d (- (* b l) (* k c))))
                      (+ (* a (- (* e i) (* h f)))
                         (* b (- (* g f) (* d i)))
                         (* c (- (* d h) (* e g))))))))
        (if (and (>= int 0) (< int +inf+)) int +inf+)))))

(defmethod normal ((shape triangle) direction eye vecscale)
  (slot-value shape 'normal))
