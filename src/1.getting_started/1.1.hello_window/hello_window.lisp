(defparameter scr-width 800)
(defparameter scr-height 600)

(cl-glfw3:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (cl-glfw3:set-window-should-close)))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 scr-width scr-height))

(defun hello-window ()
  ;; (with-body-in-main-thread ()
    (cl-glfw3:with-init-window (:title "LearnOpenGL" :width scr-width :height scr-height
                                :context-version-major 3
                                :context-version-minor 3
                                :opengl-profile #x00032001
                                #+os-macosx :opengl-forward-compat #+os-macosx t)
        (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)
        (cl-glfw3:set-key-callback 'quit-on-escape)
        (cl-glfw3:set-window-size-callback 'update-viewport)
        (loop until (cl-glfw3:window-should-close-p)
            do (cl-glfw3:swap-buffers)
            do (cl-glfw3:poll-events))))
  ;; )
