;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defun load-scene-foo ()
  (list
   :camera (make-instance 'camera)
   :lights (list
            (make-instance 'light
                           :position '(0 0 0 1)
                           :color '(1 1 1 1)))
   :geometries (list
                ;; red
                (make-instance 'sphere
                               :position '(1 0 -6)
                               :material '((.78 .04 .04 1)
                                           (.78 .04 .04 1)
                                           (.5 .5 .5 1) 0 0))
                ;; crystal
                (make-instance 'sphere
                               :position '(-1 0 -6)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (1 1 1 1) 10 1.6))
                ;; green
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((0 -1 -6) (-1 -2 -6) (1 -2 -6))
                               :normal '(0 0 0)
                               :material '((.08 .57 .09 1)
                                           (.08 .57 .09 1)
                                           (.25 .25 .25 1) 40 0)))))
