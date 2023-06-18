(defparameter *scr-width* 800)
(defparameter *scr-height* 600)

(defvar *cffi-float-size* (cffi:foreign-type-size :float))

(defparameter *vertex-shader-source* "#version 330 core
    layout (location = 0) in vec3 aPos;
    void main()
    {
       gl_Position = vec4(aPos, 1.0);
    }")

(defparameter *fragment-shader-source* "#version 330 core
    out vec4 FragColor;
    uniform vec4 ourColor;
    void main()
    {
       FragColor = ourColor;
    }")

(cl-glfw3:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (cl-glfw3:set-window-should-close)))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun shaders-uniform ()
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
           (vao             (gl:gen-vertex-array))
           (vbo             (gl:gen-buffer))

           (vertices #(-0.5 -0.5 0.0
                        0.5 -0.5 0.0
                        0.0  0.5 0.0))
           (arr (gl:alloc-gl-array :float (length vertices))))

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
      (dotimes (i (length vertices))
          (setf (gl:glaref arr i) (aref vertices i)))

      ;; bind the Vertex Array Object first, then bind and set vertex buffer(s),
      ;; and then configure vertex attributes(s).
      (gl:bind-vertex-array vao)

      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      (gl:vertex-attrib-pointer 0 3 :float nil (* 3 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)

      ;; note that this is allowed, the call to glVertexAttribPointer registered VBO as
      ;; the vertex attribute's bound vertex buffer object so afterwards we can safely unbind
      (gl:bind-buffer :array-buffer 0)

      ;; You can unbind the VAO afterwards so other VAO calls won't accidentally modify this VAO,
      ;; but this rarely happens. Modifying other VAOs requires a call to glBindVertexArray anyways
      ;; so we generally don't unbind VAOs (nor VBOs) when it's not directly necessary.
      (gl:bind-vertex-array 0)

      ;; uncomment this call to draw in wireframe polygons.
      ;; (gl:polygon-mode :front-and-back :line)

      ;; bind the VAO (it was already bound, but just to demonstrate): seeing as we only have a single VAO we can
      ;; just bind it beforehand before rendering the respective triangle; this is another approach.
      (gl:bind-vertex-array vao)

      (cl-glfw3:set-key-callback 'quit-on-escape)
      (cl-glfw3:set-window-size-callback 'update-viewport)
      (loop until (cl-glfw3:window-should-close-p)
          do (progn
              ;; render
              ;; ------
              (gl:clear-color 0.2 0.3 0.3 1.0)
              (gl:clear :color-buffer-bit)

              ;; be sure to activate the shader before any calls to glUniform
              (gl:use-program shader-program)

              ;; update shader uniform
              (let* ((time-value (%cl-glfw3:get-time))
                     (green-value (/ (sin time-value) (+ 2.0 0.5)))
                     (vertex-color-location (gl:get-uniform-location shader-program "ourColor")))
                (gl:uniformf vertex-color-location 0.0 green-value 0.0 1.0))

              ;; render the triangle
              (gl:draw-arrays :triangles 0 3)

              ;; glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
              ;; -------------------------------------------------------------------------------
              (cl-glfw3:swap-buffers)
              (cl-glfw3:poll-events)))

      ;; optional: de-allocate all resources once they've outlived their purpose:
      ;; ------------------------------------------------------------------------
      (gl:delete-vertex-arrays (list vao))
      (gl:delete-buffers (list vbo))
      (gl:delete-program shader-program))))
  ;; )
