;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

;;(declaim (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))

(defun raytrace (scene direction eyepos)
  (labels ((min-intersect (geoms int geom)
             (cond ((eq geoms nil)
                    (list int geom))
                   (t
                    (let ((newint (intersect (car geoms) direction eyepos)))
                      (cond ((< newint int)
                             (min-intersect (cdr geoms) newint (car geoms)))
                            (t
                             (min-intersect (cdr geoms) int geom))))))))
    (destructuring-bind (int geo)
        (min-intersect (slot-value scene 'geometries) +inf+ nil)
      (cond ((eq +inf+ int)
             ;; TODO get background color if exists
             (vector 0 0 0))
            (t
             (let ((gNormal (normal geo direction eyepos int))
                   (gTexture (texture geo direction eyepos int))
                   (gAmbient (ambient geo direction eyepos int))
                   (gSpecular (specular geo direction eyepos int))
                   (gDiffuse (diffuse geo direction eyepos int))
                   (gRefractiveIdx (refractiveIdx geo direction eyepos int)))
               (vector 0 0 0)))))))

(defun iterpix (width height scene camera)
  (let ((buffer (vector)))
    (with-accessors ((camdir direction) (camup up)) camera
      (with-slots ((eyepos position) aspect nearclip fov) camera
        (dotimes (r height)
          (progn
            (if (eq 0 (mod r 50))
                (progn
                  (format t "r ~a " r)
                  (finish-output nil)))
            (dotimes (c width)
              (let* ((xstep (/ (* 2 c) width)) (ystep (/ (* 2 r) height))
                     (direction
                      (vadd
                       (vadd (vmult nearclip camdir)
                             (vmult
                              (tan (/ fov 2))
                              (vmult (* (* nearclip aspect) (1- xstep))
                                     (cross camdir camup))))
                       (vmult
                        (tan (/ fov 2))
                        (vmult (* nearclip (1- ystep)) camup)))))
                (setf buffer
                      (concatenate 'vector buffer
                                   (raytrace scene direction eyepos)))))))))
    (identity buffer)))
