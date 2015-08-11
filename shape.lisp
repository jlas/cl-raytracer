;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defclass shape ()
  ((position
    :initarg :position
    :initform '(0 0 0))
   ;; ambient, diffuse, specular, shininess
   (material
    :initarg :material
    :initform '((1 1 1 1) (1 1 1 1) (1 1 1 1) 100))))

(defgeneric draw (shape))
(defgeneric intersect (shape direction eye))
(defgeneric normal (shape direction eye vecscale))
(defgeneric specular (shape direction eye vecscale))
(defgeneric ambient (shape direction eye vecscale))
(defgeneric diffuse (shape direction eye vecscale))
(defgeneric texture (shape direction eye vecscale))
(defgeneric refractiveIdx (shape direction eye vecscale))

(defmethod draw :around (shape)
  (with-slots (position material) shape
    (cl-opengl:with-pushed-matrix
      (cl-opengl:translate (nth 0 position) (nth 1 position) (nth 2 position))
      (cl-opengl:material :front :ambient (nth 0 material))
      (cl-opengl:material :front :diffuse (nth 1 material))
      (cl-opengl:material :front :specular (nth 2 material))
      (cl-opengl:material :front :shininess (nth 3 material))
      (call-next-method))))
