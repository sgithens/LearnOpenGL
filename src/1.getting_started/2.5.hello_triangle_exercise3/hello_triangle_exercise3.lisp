(defparameter *scr-width* 800)
(defparameter *scr-height* 600)

(defvar *cffi-float-size* (cffi:foreign-type-size :float))

(defparameter *vertex-shader-source* "#version 330 core
    layout (location = 0) in vec3 aPos;
    void main()
    {
       gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
    }")

(defparameter *fragment-shader1-source* "#version 330 core
    out vec4 FragColor;
    void main()
    {
       FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }")

(defparameter *fragment-shader2-source* "#version 330 core
    out vec4 FragColor;
    void main()
    {
       FragColor = vec4(1.0f, 1.0f, 0.0f, 1.0f);
    }")

(cl-glfw3:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (cl-glfw3:set-window-should-close)))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun hello-triangle-exercise3 ()
  ;; (with-body-in-main-thread ()
  (cl-glfw3:with-init-window (:title "LearnOpenGL" :width *scr-width* :height *scr-height*
                              :context-version-major 3
                              :context-version-minor 3
                              :opengl-profile #x00032001
                              #+os-macosx :opengl-forward-compat #+os-macosx t)
    (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)

    ;; build and compile our shader program
    ;; ------------------------------------
    (let* ((shader-program-orange  (gl:create-program))
           (shader-program-yellow  (gl:create-program))
           (vertex-shader          (gl:create-shader :vertex-shader))
           (fragment-shader-orange (gl:create-shader :fragment-shader))
           (fragment-shader-yellow (gl:create-shader :fragment-shader))
           (vaos                   (gl:gen-vertex-arrays 2))
           (vbos                   (gl:gen-buffers 2))

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

      ;; fragment shader orange
      (gl:shader-source fragment-shader-orange *fragment-shader1-source*)
      (gl:compile-shader fragment-shader-orange)
      (log:info "~%fragment-shader-orange infolog: ~A"  (gl:get-shader-info-log fragment-shader-orange))

      ;; fragment shader yellow
      (gl:shader-source fragment-shader-yellow *fragment-shader2-source*)
      (gl:compile-shader fragment-shader-yellow)
      (log:info "~%fragment-shader-yellow infolog: ~A"  (gl:get-shader-info-log fragment-shader-yellow))

      ;; link the first program object
      (gl:attach-shader shader-program-orange vertex-shader)
      (gl:attach-shader shader-program-orange fragment-shader-orange)
      (gl:link-program shader-program-orange)
      (log:info "~%orange program infolog: ~A" (gl:get-program-info-log shader-program-orange))

      ;; then link the second program object using a different fragment shader (but same vertex shader)
      ;; this is perfectly allowed since the inputs and outputs of both the vertex and fragment shaders are equally matched.
      (gl:attach-shader shader-program-yellow vertex-shader)
      (gl:attach-shader shader-program-yellow fragment-shader-yellow)
      (gl:link-program shader-program-yellow)
      (log:info "~%yellow program infolog: ~A" (gl:get-program-info-log shader-program-yellow))

      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader-orange)
      (gl:delete-shader fragment-shader-yellow)

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

              ;; now when we draw the triangle we first use the vertex and orange fragment shader from the first program
              (gl:use-program shader-program-orange)
              ;; draw first triangle using the data from the first VAO
              (gl:bind-vertex-array (first vaos))
              (gl:draw-arrays :triangles 0 3)
              ;; then we draw the second triangle using the data from the second VAO
              ;; when we draw the second triangle we want to use a different shader program so we
              ;; switch to the shader program with our yellow fragment shader.
              (gl:use-program shader-program-yellow)
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
      (gl:delete-program shader-program-orange)
      (gl:delete-program shader-program-yellow))))
  ;; )
