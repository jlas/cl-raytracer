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
   (width :initarg :width :initform 300))
  (:default-initargs :title "Tracer" :mode '(:double :rgb :depth)))

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
                         ,(make-instance 'sphere :position '(-1 0 -6))
                         ,(make-instance 'sphere :position '(0 2 -6))))))

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
                               (iterpix width height scene camera)))))
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
  (cl-glut:display-window (make-instance 'tracer-window :width 100 :height 100)))
