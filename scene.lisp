;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defclass scene ()
  ((camera
    :initarg :camera
    :initform (make-instance 'camera))
   (lights
    :initarg :lights)
   (geometries
    :initarg :geometries)))

(defclass camera ()
  ((position
    :initarg :position
    :initform '(0 0 0))
   (orientation
    :initarg :orientation
    :initform '(1 0 0 0))
   (focusdist
    :initarg :focusdist
    :initform 1)
   (fov
    :initarg :fov
    :initform (/ pi 3))
   (aspect
    :initarg :aspect
    :initform 1)
   (nearclip
    :initarg :nearclip
    :initform .1)
   (farclip
    :initarg :farclip
    :initform 100)))

(defun direction (camera)
  (with-slots (orientation) camera
    (v-rot-quat orientation '(0 0 -1))))

(defun up (camera)
  (with-slots (orientation) camera
    (v-rot-quat orientation '(0 1 0))))

(defun fov-degrees (camera)
  (with-slots (fov) camera
    (* (/ 180 pi) fov)))

(defclass light ()
  ((position
    :initarg :position
    :initform '(0 0 0 0))
   (color
    :initarg :color
    :initform '(0 0 0 0))))

(defmethod addlight (light)
  (with-slots (position color) light
    (cl-opengl:light :light0 :position position)
    (cl-opengl:light :light0 :diffuse color)
    (cl-opengl:light :light0 :specular color))
  (cl-opengl:enable :lighting :light0))
