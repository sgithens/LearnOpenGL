(defparameter *scr-width* 800)
(defparameter *scr-height* 600)

(defvar *cffi-float-size* (cffi:foreign-type-size :float))

(defparameter *vertex-shader-source* "#version 330 core
    layout (location = 0) in vec3 aPos;
    void main()
    {
       gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
    }")

(defparameter *fragment-shader-source* "#version 330 core
    out vec4 FragColor;
    void main()
    {
       FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }")

(cl-glfw3:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (cl-glfw3:set-window-should-close)))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun hello-triangle-exercise2 ()
  ;; (with-body-in-main-thread ()
  (cl-glfw3:with-init-window (:title "LearnOpenGL" :width *scr-width* :height *scr-height*
                              :context-version-major 3
                              :context-version-minor 3
                              :opengl-profile #x00032001
                              #+os-macosx :opengl-forward-compat #+os-macosx t)
    (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)

    ;; build and compile our shader program
    ;; ------------------------------------
    (let* ((shader-program  (gl:create-program))
           (vertex-shader   (gl:create-shader :vertex-shader))
           (fragment-shader (gl:create-shader :fragment-shader))
           (vaos            (gl:gen-vertex-arrays 2))
           (vbos            (gl:gen-buffers 2))

           (first-triangle #(-0.9  -0.5 0.0   ; left
                              0.0  -0.5 0.0   ; right
                             -0.45  0.5 0.0)) ; top
           (second-triangle #(0.0  -0.5 0.0   ; left
                              0.9  -0.5 0.0   ; right
                              0.45  0.5 0.0)) ; top

           (arr (gl:alloc-gl-array :float (length first-triangle))))

      ;; vertex shader
      (gl:shader-source vertex-shader *vertex-shader-source*)
      (gl:compile-shader vertex-shader)
      (log:info "~%vertex-shader infolog: ~A"  (gl:get-shader-info-log vertex-shader))

      ;; fragment shader
      (gl:shader-source fragment-shader *fragment-shader-source*)
      (gl:compile-shader fragment-shader)
      (log:info "~%fragment-shader infolog: ~A"  (gl:get-shader-info-log fragment-shader))

      ;; link shaders
      (gl:attach-shader shader-program vertex-shader)
      (gl:attach-shader shader-program fragment-shader)
      (gl:link-program shader-program)
      (log:info "~%program infolog: ~A" (gl:get-program-info-log shader-program))

      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader)

      ;; set up vertex data (and buffer(s)) and configure vertex attributes
      ;; ------------------------------------------------------------------

      ;; Copy the vertices to our C array


      ;; first triangle setup
      ;; --------------------
      (gl:bind-vertex-array (first vaos))
      (gl:bind-buffer :array-buffer (first vbos))
      (dotimes (i (length first-triangle))
          (setf (gl:glaref arr i) (aref first-triangle i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:vertex-attrib-pointer 0 3 :float nil (* 3 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)
      ;; (gl:bind-vertex-array 0) ; no need to unbind at all as we directly bind a different VAO the next few lines

      ;; second triangle setup
      ;; ---------------------
      (gl:bind-vertex-array (second vaos))
      (gl:bind-buffer :array-buffer (second vbos))
      (dotimes (i (length second-triangle))
          (setf (gl:glaref arr i) (aref second-triangle i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:vertex-attrib-pointer 0 3 :float nil (* 3 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)
      ;;  not really necessary as well, but beware of calls that could affect VAOs while
      ;;  this one is bound (like binding element buffer objects, or enabling/disabling
      ;;  vertex attributes)
      ;; (gl:bind-vertex-array 0)

      (gl:free-gl-array arr)

      ;; uncomment this call to draw in wireframe polygons.
      ;; (gl:polygon-mode :front-and-back :line)

      (cl-glfw3:set-key-callback 'quit-on-escape)
      (cl-glfw3:set-window-size-callback 'update-viewport)
      (loop until (cl-glfw3:window-should-close-p)
          do (progn
              ;; render
              ;; ------
              (gl:clear-color 0.2 0.3 0.3 1.0)
              (gl:clear :color-buffer-bit)

              (gl:use-program shader-program)
              ;; draw first triangle using the data from the first VAO
              (gl:bind-vertex-array (first vaos))
              (gl:draw-arrays :triangles 0 3)
              ;; then we draw the second triangle using the data from the second VAO
              (gl:bind-vertex-array (second vaos))
              (gl:draw-arrays :triangles 0 3)

              ;; glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
              ;; -------------------------------------------------------------------------------
              (cl-glfw3:swap-buffers)
              (cl-glfw3:poll-events)))

      ;; optional: de-allocate all resources once they've outlived their purpose:
      ;; ------------------------------------------------------------------------
      (gl:delete-vertex-arrays vaos)
      (gl:delete-buffers vbos)
      (gl:delete-program shader-program))))
  ;; )
