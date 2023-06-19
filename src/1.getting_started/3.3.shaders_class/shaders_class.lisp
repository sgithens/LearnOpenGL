(defparameter *scr-width* 800)
(defparameter *scr-height* 600)

(defvar *cffi-float-size* (cffi:foreign-type-size :float))

(defparameter *vertex-shader-source* "#version 330 core
    layout (location = 0) in vec3 aPos;
    layout (location = 1) in vec3 aColor;
    out vec3 ourColor;
    void main()
    {
       gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
       ourColor = aColor;
    }")

(defparameter *fragment-shader-source* "#version 330 core
    out vec4 FragColor;
    in vec3 ourColor;
    void main()
    {
       FragColor = vec4(ourColor, 1.0f);
    }")

(cl-glfw3:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (cl-glfw3:set-window-should-close)))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun shaders-class ()
  (uiop:truename* *compile-file-pathname*)
  *compile-file-pathname* *compile-file-truename*
  *load-pathname* *load-truename*)
  ;; (with-body-in-main-thread ()
  (cl-glfw3:with-init-window (:title "LearnOpenGL" :width *scr-width* :height *scr-height*
                              :context-version-major 3
                              :context-version-minor 3
                              :opengl-profile #x00032001
                              #+os-macosx :opengl-forward-compat #+os-macosx t)
    (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)

    ;; build and compile our shader program
    ;; ------------------------------------
    (let* ((our-shader (make-shader "3.3.shader.vs" "3.3.shader.fs"
                                    :source-dir
                                    (cl-fad:merge-pathnames-as-directory *load-truename*
                                            "src/1.getting_started/3.3.shaders_class/")))
           (vao             (gl:gen-vertex-array))
           (vbo             (gl:gen-buffer))

                      ; positions     ; colors
           (vertices #(-0.5 -0.5 0.0  1.0 0.0 0.0   ; bottom right
                        0.5 -0.5 0.0  0.0 1.0 0.0   ; bottom left
                        0.0  0.5 0.0  0.0 0.0 1.0)) ; top
           (arr (gl:alloc-gl-array :float (length vertices))))

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

      ;; position attribute
      (gl:vertex-attrib-pointer 0 3 :float nil (* 6 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)
      ;; color attribute
      (gl:vertex-attrib-pointer 1 3 :float nil (* 6 *cffi-float-size*) (* 3 *cffi-float-size*))
      (gl:enable-vertex-attrib-array 1)


      ;; note that this is allowed, the call to glVertexAttribPointer registered VBO as
      ;; the vertex attribute's bound vertex buffer object so afterwards we can safely unbind
      (gl:bind-buffer :array-buffer 0)

      ;; You can unbind the VAO afterwards so other VAO calls won't accidentally modify this VAO,
      ;; but this rarely happens. Modifying other VAOs requires a call to glBindVertexArray anyways
      ;; so we generally don't unbind VAOs (nor VBOs) when it's not directly necessary.
      (gl:bind-vertex-array 0)

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

              ;; draw our first triangle
              (use our-shader)
              (gl:bind-vertex-array vao)
              (gl:draw-arrays :triangles 0 3)

              ;; glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
              ;; -------------------------------------------------------------------------------
              (cl-glfw3:swap-buffers)
              (cl-glfw3:poll-events)))

      ;; optional: de-allocate all resources once they've outlived their purpose:
      ;; ------------------------------------------------------------------------
      (gl:delete-vertex-arrays (list vao))
      (gl:delete-buffers (list vbo)))))
  ;; )
