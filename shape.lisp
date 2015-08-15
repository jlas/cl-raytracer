;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defclass shape ()
  ((position
    :initarg :position
    :initform '(0 0 0))
   ;; ambient, diffuse, specular, shininess, refractionIdx
   (material
    :initarg :material
    :initform '((.2 .2 .2 1) (.8 .8 .8 1) (.5 .5 .5 1) 0 0))))

(defgeneric draw (shape))
(defgeneric intersect (shape direction eye))
(defgeneric normal (shape direction eye vecscale))
(defgeneric texture (shape direction eye vecscale))
(defgeneric specular (shape))
(defgeneric ambient (shape))
(defgeneric diffuse (shape))
(defgeneric refractiveIdx (shape))

(defmethod draw :around (shape)
  (with-slots (position material) shape
    (cl-opengl:with-pushed-matrix
      (cl-opengl:translate (nth 0 position) (nth 1 position) (nth 2 position))
      (cl-opengl:material :front :ambient (nth 0 material))
      (cl-opengl:material :front :diffuse (nth 1 material))
      (cl-opengl:material :front :specular (nth 2 material))
      (cl-opengl:material :front :shininess (nth 3 material))
      (call-next-method))))

(defmethod ambient (shape)
  (with-slots (material) shape
    (nth 0 material)))

(defmethod diffuse (shape)
  (with-slots (material) shape
    (nth 1 material)))

(defmethod specular (shape)
  (with-slots (material) shape
    (nth 2 material)))

(defmethod refractiveIdx (shape)
  (with-slots (material) shape
    (nth 4 material)))
