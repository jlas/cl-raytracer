;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defun raytrace (scene direction eyepos
                 &optional (slop 0) (depth 0) (refIndices (list 0)))
  (labels ((min-intersect (geoms int geom)
             (cond ((eq geoms nil)
                    (list int geom))
                   (t
                    (let ((newint (intersect (car geoms) direction eyepos)))
                      (cond ((and (< newint int)
                                  (> newint slop))
                             (min-intersect (cdr geoms) newint (car geoms)))
                            (t
                             (min-intersect (cdr geoms) int geom))))))))
    (destructuring-bind (int geo)
        (min-intersect (slot-value scene 'geometries) +inf+ nil)
      (cond ((eq +inf+ int)
             ;; TODO get background color if exists
             (vector 0 0 0))
            (t
             (let* ((gNormal (normal geo direction eyepos int))
                    (gTexture (texture geo direction eyepos int))
                    (gAmbient (ambient geo direction eyepos int))
                    (gSpecular (specular geo direction eyepos int))
                    (gDiffuse (diffuse geo direction eyepos int))
                    (gRefractiveIdx (refractiveIdx geo direction eyepos int))
                    (newSrc (vadd eyepos (vmult int direction))))
               (cond ((eq 0 gRefractiveIdx)
                      ;; non-dielectrics
                      (vector 0 0 0))
                     ((< depth 2)
                      ;; dielectrics
                      (let ((refract (vector 0 0 0))
                            (gDot (dot direction gNormal))
                            (curRefIdx (pop refIndices)))
                        (cond ((< gDot 0)
                               (push gRefractiveIdx refIndices))
                              (t
                               (push 0 refIndices)))
                        (let ((root
                               (- 1
                                  (/
                                   (* (expt curRefIdx 2) (- 1 (expt gDot 2)))
                                   (expt (car refIndices) 2)))))
                          (cond ((>= root 0)
                                 (let ((refractDirection
                                        (vmin (vdiv
                                               (car refIndices)
                                               (vmult curRefIdx
                                                      (vmin
                                                       direction
                                                       (vmult gDot gNormal))))
                                              (vmult (sqrt root) gNormal))))
                                   (setf refract (raytrace
                                                  scene
                                                  refractDirection
                                                  newSrc
                                                  0.01
                                                  (1+ depth)
                                                  refIndices)))))
                          (let* ((reflectDirection
                                 (vmin direction
                                       (vmult
                                        (* 2 (dot gNormal direction))
                                        gNormal)))
                                 (reflect
                                  (vmult-alt gSpecular (raytrace
                                                        scene
                                                        reflectDirection
                                                        newSrc
                                                        0.01
                                                        (1+ depth)
                                                        refIndices)))
                                 (r0 (expt
                                      (/ (1- (car refIndices))
                                         (1+ (car refIndices)))
                                      2))
                                 (rtheta (+ r0 (* (1- r0)
                                                 (expt
                                                  (1-
                                                   (cos
                                                    (/ gDot (* (mag direction)
                                                              (mag gNormal)))))
                                                  5)))))
                            (vadd (vmult rtheta reflect)
                                  (vmult (1- rtheta) refract))))))
                     (t
                      (vector 0 0 0)))))))))

(defun iterpix (width height scene camera)
  (let ((buffer (vector)))
    (with-accessors ((camdir direction) (camup up)) camera
      (with-slots ((eyepos position) aspect nearclip fov) camera
        (dotimes (r height)
          (progn
            (if (eq 10 (mod r 50))
                (progn
                  (format t "r ~a " r)
                  (format t "buffer: ~a ~a ~a "
                          (elt buffer 0) (elt buffer 1) (elt buffer 2))
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
                      (concatenate 'vector buffer (clamp-to-rgb
                                                   (raytrace
                                                    scene
                                                    direction
                                                    eyepos))))))))))
    (identity buffer)))
