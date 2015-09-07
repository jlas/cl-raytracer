;;;; -*- Mode: Lisp -*-

(in-package :raytracer)

(defclass tracer-window (cl-glut:window)
  ((trackz :initform 0)
   (trackx :initform 0)
   (tracky :initform 0)
   (rotx :initform 0)
   (roty :initform 0)
   (rotz :initform 0)
   (height :initarg :height :initform 300)
   (width :initarg :width :initform 300)
   (rt_start :initform nil)
   (rt_buffer :initform nil))
  (:default-initargs :title "Tracer" :mode '(:double :rgb :depth)))

(defmethod cl-glut:display-window :before ((window tracer-window))
  (cl-opengl:enable :normalize)
  (cl-opengl:enable :depth-test))

(defmethod cl-glut:display ((window tracer-window))
  (cl-opengl:clear :color-buffer :depth-buffer)
  (let ((scene (apply #'make-instance (cons 'scene (load-scene-foo)))))
    (with-slots (camera lights geometries) scene

      (cl-opengl:matrix-mode :projection)
      (cl-opengl:load-identity)
      (with-accessors ((fov-degrees fov-degrees)) camera
        (with-slots (aspect nearclip farclip) camera
          (cl-glu:perspective fov-degrees aspect nearclip farclip)))

      (cl-opengl:matrix-mode :modelview)
      (cl-opengl:load-identity)

      (with-slots (trackx trackz tracky rotx roty rotz) window
        (setf (slot-value camera 'position)
              (vadd (slot-value camera 'position)
                    (vector trackx tracky trackz)))
        (setf (slot-value camera 'orientation)
              (quat-mult (slot-value camera 'orientation)
                         (to-quat (to-rot-matrix rotx roty rotz)))))

      (with-accessors ((direction direction) (up up)) camera
        (with-slots (position) camera
          (let ((center (vadd position direction)))
            (cl-glu:look-at (nth 0 position) (nth 1 position) (nth 2 position)
                            (nth 0 center) (nth 1 center) (nth 2 center)
                            (nth 0 up) (nth 1 up) (nth 2 up)))))

      (cl-opengl:with-pushed-matrix
        (mapcar #'draw geometries))

      (mapcar #'addlight lights)
      (if (slot-value window 'rt_start)
          (with-slots (width height rt_start rt_buffer) window
            (setf rt_buffer (iterpix width height scene camera))
            (setf rt_start nil)))
      (if (slot-value window 'rt_buffer)
          (with-slots (width height rt_buffer) window
            (cl-opengl:draw-pixels
             width height :rgb :unsigned-byte rt_buffer)))))
  (cl-glut:swap-buffers))

(defmethod cl-glut:idle ((window tracer-window))
  (cl-glut:post-redisplay))

(defmethod cl-glut:keyboard ((window tracer-window) key x y)
  (declare (ignore x y))
  (with-slots (rotx roty rotz trackz trackx tracky rt_start rt_buffer) window
    (case key
      (#\z (incf (slot-value window 'rotx) .25))
      (#\x (decf (slot-value window 'rotx) .25))
      (#\Esc (cl-glut:destroy-current-window))
      (#\r (setf rt_start t))
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
       (setf rotz 0)
       (setf rt_buffer nil)))))

(defmethod cl-glut:special ((window tracer-window) special-key x y)
  (declare (ignore x y))
  (with-slots (roty rotz) window
    (case special-key
      (:key-up (incf rotz .25))
      (:key-down (decf rotz .25))
      (:key-left (decf roty .25))
      (:key-right (incf roty .25))
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
  (cl-glut:display-window (make-instance 'tracer-window :width 100 :height 100)))
