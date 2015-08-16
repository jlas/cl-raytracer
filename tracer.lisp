;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defun direct_color_from_light (scene newSrc gDiffuse gNormal light)
  (let* ((lightRay (vmin (slot-value light 'position) newSrc))
         (shadowDir (normalize lightRay))
         (toLight (/ (mag lightRay) (mag shadowDir))))
    (if
     (equal
      nil (some
           #'(lambda (geo) (/= +inf+
                               (let ((newInt (intersect geo shadowDir newSrc)))
                                 (cond ((or (> newInt 0.01) (< newInt toLight)) newInt)
                                       (t +inf+)))))
           (remove-if-not
            #'(lambda (geo) (= (refractiveIdx geo) 0))
            (slot-value scene 'geometries))))
     (vmult
      (max 0 (dot gNormal shadowDir))
      (vmult-alt (slot-value light 'color) gDiffuse))
     (vector 0 0 0))))

(defun direct_color_from_lights (scene newSrc gDiffuse gNormal lights)
  (reduce #'vadd
          (mapcar
           #'(lambda (light)
               (direct_color_from_light scene newSrc gDiffuse gNormal light))
           lights)))

(defun rt_nonrefractives (scene newSrc gDiffuse gNormal gAmbient)
  (vadd gAmbient
        (direct_color_from_lights
         scene newSrc gDiffuse gNormal (slot-value scene 'lights))))

(defun rt_refractives (scene newSrc depth direction gNormal refIndices
                       gRefractiveIdx gSpecular)
  (let ((refract (vector 0 0 0))
        (gDot (dot direction gNormal))
        (curRefIdx (pop refIndices)))
    (cond ((< gDot 0)
           (push gRefractiveIdx refIndices))
          (t
           (push 0 refIndices)))
    (let ((root (- 1 (/
                      (* (expt curRefIdx 2) (- 1 (expt gDot 2)))
                      (expt (car refIndices) 2)))))
      (cond ((>= root 0)
             (let ((refractDirection
                    (vmin (vdiv (car refIndices)
                                (vmult curRefIdx (vmin direction
                                                       (vmult gDot gNormal))))
                          (vmult (sqrt root) gNormal))))
               (setf refract (raytrace scene refractDirection newSrc 0.01
                                       (1+ depth) refIndices)))))
      (let* ((reflectDirection
              (vmin direction (vmult
                               (* 2 (dot gNormal direction))
                               gNormal)))
             (reflect
              (vmult-alt gSpecular (raytrace scene reflectDirection newSrc 0.01
                                             (1+ depth) refIndices)))
             (r0 (expt (/ (1- (car refIndices))
                          (1+ (car refIndices)))
                       2))
             (rtheta (+ r0 (* (1- r0)
                              (expt
                               (1-
                                (cos
                                 (/ gDot
                                    (* (mag direction)
                                       (mag gNormal)))))
                               5)))))
        (vadd (vmult rtheta reflect)
              (vmult (1- rtheta) refract))))))

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
                    (gAmbient (ambient geo))
                    (gSpecular (specular geo))
                    (gDiffuse (diffuse geo))
                    (gRefractiveIdx (refractiveIdx geo))
                    (newSrc (vadd eyepos (vmult int direction))))
               (cond ((eq 0 gRefractiveIdx)
                      ;; non-dielectrics
                      (rt_nonrefractives
                       scene newSrc gDiffuse gNormal gAmbient))
                     ((< depth 2)
                      (rt_refractives scene newSrc depth direction gNormal
                                      refIndices gRefractiveIdx gSpecular))
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
