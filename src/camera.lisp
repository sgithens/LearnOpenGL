
;; Default camera values
(defconstant +yaw+        -90.0)
(defconstant +pitch+        0.0)
(defconstant +speed+        2.5)
(defconstant +sensitivity+  0.1)
(defconstant +zoom+        45.0)

(defclass camera ()
  (;; camera attributes
   (pos      :initform (3d-vectors:vec3 0.0 0.0  0.0) :initarg :pos       :accessor pos)
   (front    :initform (3d-vectors:vec3 0.0 0.0 -1.0) :initarg :front     :accessor front)
   (up       :initform (3d-vectors:vec3 0.0 1.0  0.0) :initarg :up        :accessor up)
   (right    :initform (3d-vectors:vec3 0.0 0.0  0.0) :initarg :right     :accessor right)
   (world-up :initform (3d-vectors:vec3 0.0 1.0  0.0) :initarg :world-up  :accessor world-up)
  ;; euler angles
   (yaw      :initform +yaw+                          :initarg :yaw       :accessor yaw)
   (pitch    :initform +pitch+                        :initarg :pitch     :accessor pitch)
  ;; camera options
   (movement-speed      :initform +speed+        :initarg :movement-speed      :accessor movement-speed)
   (mouse-sensitivity   :initform +sensitivity+  :initarg :mouse-sensitivity   :accessor mouse-sensitivity)
   (zoom                :initform +zoom+         :initarg :zoom                :accessor zoom)
  ))

(defmethod update-camera-vectors ((self camera))
  "Calculates the front vector from the Camera's (updated) Euler Angles"
  ;; calculate the new Front Vector
  (setf (front self)
        (3d-vectors:vunit
          (3d-vectors:vec3
                (* (cos (degrees->radians (yaw self))) (cos (degrees->radians (pitch self))))
                   (sin (degrees->radians (pitch self)))
                (* (sin (degrees->radians (yaw self))) (cos (degrees->radians (pitch self)))))))
  ;; also re-calculate the right and up vector
  ;; normalize the vectors, because their length gets closer to 0 the more you look up or down which
  ;; results in slower movement.
  (setf (right self)
        (3d-vectors:vunit (3d-vectors:vc (front self) (world-up self))))
  (setf (up self)
        (3d-vectors:vunit (3d-vectors:vc (right self) (front self)))))

(defmethod get-view-matrix ((self camera))
  "Returns the view matrix calculated using Euler Angles and the LookAt Matrix"
  (3d-matrices:mlookat (pos self)
                       (3d-vectors:v+ (pos self) (front self))
                       (up self)))

(defmethod process-keyboard ((self camera) direction delta-time)
  "Processes input received from any keyboard-like input system. Accepts input parameter
   in the form of camera symbols :forward :backward :right :left
  "
  (let ((velocity (* (movement-speed self) delta-time)))
    (cond ((equal direction :forward)
           (setf (pos self) (3d-vectors:v+ (pos self) (3d-vectors:v* (front self) velocity))))
          ((equal direction :backward)
           (setf (pos self) (3d-vectors:v- (pos self) (3d-vectors:v* (front self) velocity))))
          ((equal direction :left)
           (setf (pos self) (3d-vectors:v- (pos self) (3d-vectors:v* (right self) velocity))))
          ((equal direction :right)
           (setf (pos self) (3d-vectors:v+ (pos self) (3d-vectors:v* (right self) velocity)))))))

(defmethod process-mouse-movement ((self camera) x-offset y-offset &optional (constrain-pitch t))
  "Processes input received from a mouse input system. Expects the offset value in both the x and y direction."
  (incf (yaw self)   (* x-offset (mouse-sensitivity self)))
  (incf (pitch self) (* y-offset (mouse-sensitivity self)))

  ;; make sure that when pitch is out of bounds, screen doesn't get flipped
  (when constrain-pitch
    (when (> (pitch self) 89.0)
      (setf (pitch self) 89.0))
    (when (< (pitch self) -89.0)
      (setf (pitch self) -89.0)))

  ;; update Front, Right and Up Vectors using the updated Euler angles
  (update-camera-vectors self))

(defmethod process-mouse-scroll ((self camera) y-offset)
  "Processes input received from a mouse scroll-wheel event. Only requires input on the vertical wheel-axis"
  (decf (zoom self) y-offset)
  (when (< (zoom self) 1.0)
    (setf (zoom self) 1.0))
  (when (> (zoom self) 45.0)
    (setf (zoom self) 45.0)))

(defun make-camera (&optional (pos (3d-vectors:vec3 0.0 0.0 0.0)))
  (let ((togo (make-instance 'camera :pos pos)))
    (update-camera-vectors togo)
    togo))
