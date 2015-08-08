;;;; -*- Mode: Lisp -*-

(defpackage #:raytracer-asd
  (:use :cl :asdf))

(in-package :raytracer-asd)

(defsystem :raytracer
    :description "Ray tracer in Common Lisp"
    :serial t
    :version "0.0.1"
    :components ((:file "packages")
                 (:file "util")
                 (:file "shape")
                 (:file "sphere")
                 (:file "scene")
                 (:file "tracer"))
    :depends-on (:cl-opengl
                 :cl-glut
                 :cl-glu))
