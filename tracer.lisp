;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :raytracer)

;;(declaim (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))

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
    :initform '(0 0 0 1))
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
    :initarg :nearclip
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

(defclass shape ()
  ((position
    :initarg :position
    :initform '(0 0 0))
   ;; ambient, diffuse, specular, shininess
   (material
    :initarg :material
    :initform '((1 0 0 0) (1 0 0 0) (1 0 0 0) 0))))

(defclass sphere (shape)
  ((radius
    :initarg :radius
    :initform 1)))

(defgeneric draw (shape))
(defgeneric intersect (shape direction eye))
(defgeneric normal (shape direction eye vecscale))
(defgeneric specular (shape direction eye vecscale))
(defgeneric ambient (shape direction eye vecscale))
(defgeneric diffuse (shape direction eye vecscale))
(defgeneric texture (shape direction eye vecscale))
(defgeneric refractiveIdx (shape direction eye vecscale))

(defmethod draw :around (shape)
  (with-slots (position material) shape
    (cl-opengl:with-pushed-matrix
      (cl-opengl:translate (nth 0 position) (nth 1 position) (nth 2 position))
      (cl-opengl:material :front :ambient (nth 0 material))
      (cl-opengl:material :front :diffuse (nth 1 material))
      (cl-opengl:material :front :specular (nth 2 material))
      (cl-opengl:material :front :shininess (nth 3 material))
      (call-next-method))))

(defmethod draw ((shape sphere))
  (let ((incr (/ pi 15)))
    (do ((long 0 (+ long incr)))
        ((>= long (* 2 pi)))
      (let ((nextLong (min (+ long incr) (* 2 pi))))
        (cl-opengl:with-primitives :triangle-strip
          (do ((lat 0 (+ lat (/ incr 2))))
              ((>= lat pi))
            (apply #'cl-opengl:vertex
                   (vmult
                    (slot-value shape 'radius)
                    (list (* (sin lat) (cos long))
                          (* -1 (cos lat))
                          (* (sin lat) (sin long)))))
            (apply #'cl-opengl:vertex
                   (vmult
                    (slot-value shape 'radius)
                    (list (* (sin lat) (cos nextLong))
                          (* -1 (cos lat))
                          (* (sin lat) (sin nextLong))))))
          (apply #'cl-opengl:vertex
                 (vmult
                  (slot-value shape 'radius)
                  (list (* (sin pi) (cos long))
                        (* -1 (cos pi))
                        (* (sin pi) (sin long)))))
          (apply #'cl-opengl:vertex
                 (vmult
                  (slot-value shape 'radius)
                  (list (* (sin pi) (cos nextLong))
                        (* -1 (cos pi))
                        (* (sin pi) (sin nextLong))))))))))

(defmethod intersect ((shape sphere) direction eye)
  (with-slots (position radius) shape
    (let* ((a (dot direction direction))
      (b (* 2 (dot direction (vmin eye position))))
      (c (-
          (dot
           (vmin eye position)
           (vmin eye position))
          (* radius radius)))
      (det (- (* b b) (* 4 (* a c)))))
      (if (< det 0) +inf+
          (min
           (/ (- (* -1 b) (expt det 0.5)) (* 2 a))
           (/ (+ (* -1 b) (expt det 0.5)) (* 2 a)))))))

(defmethod normal ((shape sphere) direction eye vecscale)
  (with-slots (position) shape
    (normalize (vmin (vadd eye (vmult vecscale direction)) position))))

(defmethod ambient ((shape sphere) direction eye vecscale)
  (with-slots (material) shape
    (nth 0 material)))

(defmethod diffuse (shape direction eye vecscale)
  (with-slots (material) shape
    (nth 1 material)))

(defmethod specular ((shape sphere) direction eye vecscale)
  (with-slots (material) shape
    (nth 2 material)))

(defmethod texture (shape direction eye vecscale))

(defmethod refractiveIdx (shape direction eye vecscale)
  (with-slots (material) shape
    (nth 3 material)))


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
    (min-intersect (slot-value scene 'geometries) +inf+ nil)))

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
                  (raytrace scene direction eyepos)))))))))

  ;;(with-slots (width height) window

    ;;(cl-opengl:draw-pixels width height :rgba :unsigned-byte
        ;;                     (concatenate 'vector (loop for i from 0 to 360000 collect (mod i 255))))))
  ;;(cl-glut:solid-teapot 1))
  ;;(cl-opengl:draw-pixels 50 100 :rgba :unsigned-byte #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (cl-glut:swap-buffers))

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
