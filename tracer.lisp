;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

;;(declaim (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))

(defclass tracer-window (cl-glut:window)
  ((trackz :initform 0)
   (trackx :initform 0)
   (tracky :initform 0)
   (rotx :initform 0)
   (roty :initform 0)
   (rotz :initform 0)
   (height :initarg :height :initform 300)
   (width :initarg :width :initform 300))
  (:default-initargs :title "Tracer" :mode '(:double :rgb :depth)))

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

(defun iterpix (window scene camera)
  (let ((buffer (vector)))
    (with-slots (width height) window
      (with-accessors ((camdir direction) (camup up)) camera
        (with-slots ((eyepos position) aspect nearclip fov) camera
          (dotimes (r height)
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
                (concatenate 'vector buffer (raytrace scene direction eyepos))))))))))

(defmethod cl-glut:display-window :before ((window tracer-window))
  (cl-opengl:enable :normalize)
  (cl-opengl:enable :depth-test))

(defmethod cl-glut:display ((window tracer-window))
  (cl-opengl:clear :color-buffer :depth-buffer)
  ;; (let ((triangle (cl-opengl:gen-lists 1)))
  ;;   (cl-opengl:with-pushed-matrix
  ;;     (cl-opengl:rotate 0 1 0 0)
  ;;     (cl-opengl:with-new-list (triangle :compile))
  ;;     (draw-triangle)
  ;;     (cl-opengl:call-list triangle)))
  ;;(cl-opengl:shade-model :flat)
  ;;(cl-opengl:normal 0 0 1)

  (let ((scene
         (make-instance
          'scene
          :camera (make-instance 'camera)
          :lights `(,(make-instance
                      'light :position '(0 0 0 1) :color '(1 1 1 1)))
          :geometries `(,(make-instance 'sphere :position '(1 0 -6))
                         ,(make-instance 'sphere :position '(-1 0 -6))))))

    (with-slots (camera lights geometries) scene

      (cl-opengl:matrix-mode :projection)
      (cl-opengl:load-identity)
      (with-accessors ((fov-degrees fov-degrees)) camera
        (with-slots (aspect nearclip farclip) camera
          (cl-glu:perspective fov-degrees aspect nearclip farclip)))

      (cl-opengl:matrix-mode :modelview)
      (cl-opengl:load-identity)
      (with-accessors ((direction direction) (up up)) camera
        (with-slots (position) camera
          (let ((center (vadd position direction)))
            (cl-glu:look-at (nth 0 position) (nth 1 position) (nth 2 position)
                            (nth 0 center) (nth 1 center) (nth 2 center)
                            (nth 0 up) (nth 1 up) (nth 2 up)))))

      (cl-opengl:with-pushed-matrix
        (with-slots (trackx trackz tracky rotx roty rotz) window
          (cl-opengl:rotate rotx 1 0 0)
          (cl-opengl:rotate roty 0 1 0)
          (cl-opengl:rotate rotz 0 0 1)
          (cl-opengl:translate trackx tracky trackz))
        (mapcar #'draw geometries))

      (mapcar #'addlight lights)
      (with-slots (width height) window
        (cl-opengl:draw-pixels width height :rgb :unsigned-byte
                               (iterpix window scene camera)))))
  (cl-glut:swap-buffers))

;;(with-slots (width height) window
    ;;(cl-opengl:draw-pixels width height :rgba :unsigned-byte
        ;;                     (concatenate 'vector (loop for i from 0 to 360000 collect (mod i 255))))))
  ;;(cl-glut:solid-teapot 1))
  ;;(cl-opengl:draw-pixels 50 100 :rgba :unsigned-byte #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(defmethod cl-glut:idle ((window tracer-window))
  (cl-glut:post-redisplay))

(defmethod cl-glut:keyboard ((window tracer-window) key x y)
  (declare (ignore x y))
  (with-slots (rotx roty rotz trackz trackx tracky) window
    (case key
      (#\z (incf (slot-value window 'rotz) 5.0))
      (#\Z (decf (slot-value window 'rotz) 5.0))
      (#\Esc (cl-glut:destroy-current-window))
      (#\w (setf trackz (+ trackz (cos (to-rad rotx))))
           (setf tracky (+ tracky (sin (to-rad rotx))))
           (setf trackx (- trackx (sin (to-rad roty))))
           (setf trackz (+ trackz (cos (to-rad roty)))))
      ;;(setf trackx (+ trackx (cos (to-rad rotz))))
      ;;(setf tracky (- tracky (sin (to-rad rotz)))))
      (#\s (setf trackz (- trackz (cos (to-rad rotx))))
           (setf tracky (- tracky (sin (to-rad rotx))))
           (setf trackx (+ trackx (sin (to-rad roty))))
           (setf trackz (- trackz (cos (to-rad roty)))))
      (#\a (incf trackx))
      (#\d (decf trackx))
      (otherwise
       (setf trackz 0)
       (setf trackx 0)
       (setf tracky 0)
       (setf rotx 0)
       (setf roty 0)
       (setf rotz 0)))))

(defmethod cl-glut:special ((window tracer-window) special-key x y)
  (declare (ignore x y))
  (with-slots (rotx roty rotz) window
    (case special-key
      (:key-up (incf rotx 15))
      (:key-down (decf rotx 15))
      (:key-left (decf roty 15))
      (:key-right (incf roty 15))
    (cl-glut:post-redisplay))))

(defmethod cl-glut:reshape ((w tracer-window) width height)
  (cl-opengl:viewport 0 0 width height)
  (cl-opengl:matrix-mode :projection)
  (cl-opengl:load-identity)
  (let ((h (/ height width)))
    (cl-opengl:frustum -1 1 (- h) h 5 60))
  (cl-opengl:matrix-mode :modelview)
  (cl-opengl:load-identity)
  (cl-opengl:translate 0 0 0))

(defmethod cl-glut:visibility ((w tracer-window) state)
  (case state
    (:visible (cl-glut:enable-event w :idle))
    (t (cl-glut:disable-event w :idle))))

(defun tracer ()
  (cl-glut:display-window (make-instance 'tracer-window)))
