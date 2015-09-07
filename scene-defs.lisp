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

(defun load-scene-cornell-box ()
  (list
   :camera (make-instance 'camera :position '(0 0 -6 0))
   :lights (list
            (make-instance 'light
                           :position '(0 4 0 1)
                           :color '(1 1 1 1)))
   :geometries (list
                ;; red
                (make-instance 'sphere
                               :position '(-1.5 2 1.5)
                               :material '((.78 .04 .04 1)
                                           (.78 .04 .04 1)
                                           (.5 .5 .5 1) 0 0))
                ;; crystal
                (make-instance 'sphere
                               :position '(1.5 2 1.5)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (1 1 1 1) 10 1.6))
                ;; back wall
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((-3 3 3) (-3 -3 3) (3 -3 3))
                               :normal '(0 0 0)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (0 0 0 0) 0 0))
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((-3 3 3) (3 3 3) (3 -3 3))
                               :normal '(0 0 0)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (0 0 0 0) 0 0))
                ;; left wall
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((-3 3 -3) (-3 -3 -3) (-3 -3 3))
                               :normal '(0 0 0)
                               :material '((.08 .57 .09 1)
                                           (.08 .57 .09 1)
                                           (.25 .25 .25 1) 0 0))
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((-3 3 -3) (-3 -3 3) (-3 3 3))
                               :normal '(0 0 0)
                               :material '((.08 .57 .09 1)
                                           (.08 .57 .09 1)
                                           (.25 .25 .25 1) 0 0))
                ;; right wall
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((3 3 -3) (3 -3 -3) (3 -3 3))
                               :normal '(0 0 0)
                               :material '((.78 .04 .04 1)
                                           (.78 .04 .04 1)
                                           (.25 .25 .25 1) 00 0))
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((3 3 -3) (3 -3 3) (3 3 3))
                               :normal '(0 0 0)
                               :material '((.78 .04 .04 1)
                                           (.78 .04 .04 1)
                                           (.25 .25 .25 1) 0 0))
                ;; top wall
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((-3 3 -3) (3 3 -3) (-3 3 3))
                               :normal '(0 0 0)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (0 0 0 0) 0 0))
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((3 3 -3) (3 3 3) (-3 3 3))
                               :normal '(0 0 0)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (0 0 0 0) 0 0))
                ;; bottom wall
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((-3 -3 -3) (3 -3 -3) (-3 -3 3))
                               :normal '(0 0 0)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (0 0 0 0) 0 0))
                (make-instance 'triangle
                               :orientation '(0 0 0 0)
                               :vertices '((3 -3 -3) (3 -3 3) (-3 -3 3))
                               :normal '(0 0 0)
                               :material '((1 1 1 1)
                                           (1 1 1 1)
                                           (0 0 0 0) 0 0)))))
