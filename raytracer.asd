;;;; -*- Mode: Lisp -*-

(defpackage #:raytracer-asd
  (:use :cl :asdf))

(in-package :raytracer-asd)

(defsystem :raytracer
    :description "Ray tracer in Common Lisp"
    :serial t
    :version "0.0.1"
    :entry-point "raytracer:tracer"
    :components ((:file "packages")
                 (:file "util")
                 (:file "shape")
                 (:file "sphere")
                 (:file "triangle")
                 (:file "scene")
                 (:file "tracer")
                 (:file "scene-defs")
                 (:file "app"))
    :depends-on (:cl-opengl
                 :cl-glut
                 :cl-glu))
