(defparameter *scr-width* 800)
(defparameter *scr-height* 600)

(defvar *cffi-float-size* (cffi:foreign-type-size :float))

(cl-glfw3:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (cl-glfw3:set-window-should-close)))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun coordinate-systems (&key (vertex-shader-file "src/1.getting_started/6.1.coordinate_systems/6.1.coordinate_systems.vs")
                               (fragment-shader-file "src/1.getting_started/6.1.coordinate_systems/6.1.coordinate_systems.fs")
                                          ; positions    ; texture coords
                               (vertices #( 0.5  0.5 0.0   1.0 1.0          ;; top right
                                            0.5 -0.5 0.0   1.0 0.0          ;; bottom right
                                           -0.5 -0.5 0.0   0.0 0.0          ;; bottom left
                                           -0.5  0.5 0.0   0.0 1.0 )))      ;; top left
  ;; (with-body-in-main-thread ()
  (cl-glfw3:with-init-window (:title "LearnOpenGL" :width *scr-width* :height *scr-height*
                              :context-version-major 3
                              :context-version-minor 3
                              :opengl-profile #x00032001
                              #+os-macosx :opengl-forward-compat #+os-macosx t)
    (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)

    ;; build and compile our shader program
    ;; ------------------------------------
    (let* ((our-shader (make-shader vertex-shader-file
                                    fragment-shader-file
                                    :source-dir *load-truename*))
           (vao             (gl:gen-vertex-array))
           (vbo             (gl:gen-buffer))
           (ebo             (gl:gen-buffer))
           (texture1        (gl:gen-texture))
           (texture2        (gl:gen-texture))


           (arr (gl:alloc-gl-array :float (length vertices)))

           (indices #(0 1 3      ;; first triangle
                      1 2 3))    ;; second triangle
           (indices-arr (gl:alloc-gl-array :unsigned-short (length indices))))
      ;; set up vertex data (and buffer(s)) and configure vertex attributes
      ;; ------------------------------------------------------------------

      ;; Copy the vertices to our C array
      (dotimes (i (length vertices))
          (setf (gl:glaref arr i) (aref vertices i)))
      (dotimes (i (length indices))
          (setf (gl:glaref indices-arr i) (aref indices i)))

      ;; bind the Vertex Array Object first, then bind and set vertex buffer(s),
      ;; and then configure vertex attributes(s).
      (gl:bind-vertex-array vao)

      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr)

      (gl:bind-buffer :element-array-buffer ebo)
      (gl:buffer-data :element-array-buffer :static-draw indices-arr)
      (gl:free-gl-array indices-arr)

      ;; position attribute
      (gl:vertex-attrib-pointer 0 3 :float nil (* 5 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)
      ;; texture coord attribute
      (gl:vertex-attrib-pointer 1 2 :float nil (* 5 *cffi-float-size*) (* 3 *cffi-float-size*))
      (gl:enable-vertex-attrib-array 1)

      ;; note that this is allowed, the call to glVertexAttribPointer registered VBO as
      ;; the vertex attribute's bound vertex buffer object so afterwards we can safely unbind
      (gl:bind-buffer :array-buffer 0)

      ;; You can unbind the VAO afterwards so other VAO calls won't accidentally modify this VAO,
      ;; but this rarely happens. Modifying other VAOs requires a call to glBindVertexArray anyways
      ;; so we generally don't unbind VAOs (nor VBOs) when it's not directly necessary.
      (gl:bind-vertex-array 0)

      ;; texture 1
      ;; ---------
      (gl:bind-texture :texture-2d texture1) ;; all upcoming GL_TEXTURE_2D operations now have effect on this texture object
      ;; set the texture wrapping parameters
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat) ;; set texture wrapping to GL_REPEAT (default wrapping method)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      ;; set texture filtering parameters
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      ;; load image, create texture and generate mipmaps
      (gl:tex-image-2d :texture-2d 0 :rgba 512 512 0 :rgba :unsigned-byte
        (gl::gl-array-pointer
          (load-texture (cl-fad:merge-pathnames-as-file *load-truename*
                                "resources/textures/container.png"))))
      (gl:generate-mipmap :texture-2d)

      ;; texture 2
      ;; ---------
      (gl:bind-texture :texture-2d texture2) ;; all upcoming GL_TEXTURE_2D operations now have effect on this texture object
      ;; set the texture wrapping parameters
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat) ;; set texture wrapping to GL_REPEAT (default wrapping method)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      ;; set texture filtering parameters
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      ;; load image, create texture and generate mipmaps
      (gl:tex-image-2d :texture-2d 0 :rgba 476 476 0 :rgba :unsigned-byte
        (gl::gl-array-pointer
          (load-texture (cl-fad:merge-pathnames-as-file *load-truename*
                                "resources/textures/awesomeface.png"))))
      (gl:generate-mipmap :texture-2d)

      ;; tell opengl for each sampler to which texture unit it belongs to (only has to be done once)
      ;; -------------------------------------------------------------------------------------------
      (use our-shader) ;; don't forget to activate/use the shader before setting uniforms!
      ;; either set it manually like so:
      (gl:uniformi (gl:get-uniform-location (id our-shader) "texture1") 0)
      ;; or set it via the texture class
      (set-int our-shader "texture2" 1)


      (cl-glfw3:set-key-callback 'quit-on-escape)
      (cl-glfw3:set-window-size-callback 'update-viewport)
      (loop until (cl-glfw3:window-should-close-p)
          do (progn
              ;; render
              ;; ------
              (gl:clear-color 0.2 0.3 0.3 1.0)
              (gl:clear :color-buffer-bit)

              ;; bind Texture
              (gl:active-texture :texture0)
              (gl:bind-texture :texture-2d texture1)
              (gl:active-texture :texture1)
              (gl:bind-texture :texture-2d texture2)

              ;; activate shader
              (use our-shader)

              ;; create transformations
              (let ((model      (3d-matrices:meye 4))
                    (view       (3d-matrices:meye 4))
                    (projection (3d-matrices:meye 4)))

                (3d-matrices:nmrotate model 3d-vectors:+vx+ (degrees->radians -55.0))
                (3d-matrices:nmtranslate view (3d-vectors:vec 0.0 0.0 -3.0))
                (3d-matrices:nmperspective model (degrees->radians 45.0) (/ *scr-width* *scr-height*) 0.1 100.0)

                ;; set the uniforms a few different ways
                (gl:uniform-matrix-4fv
                    (gl:get-uniform-location (id our-shader) "model")
                    (3d-matrices:marr4 model) t)
                (gl:uniform-matrix-4fv
                    (gl:get-uniform-location (id our-shader) "view")
                    (3d-matrices:marr4 view) t)
                ;; note: currently we set the projection matrix each frame, but since the projection matrix
                ;; rarely changes it's often best practice to set it outside the main loop only once.
                (set-mat4 our-shader "projection" projection))

              ;; render container
              (gl:bind-vertex-array vao)
              (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)

              ;; glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
              ;; -------------------------------------------------------------------------------
              (cl-glfw3:swap-buffers)
              (cl-glfw3:poll-events)))

      ;; optional: de-allocate all resources once they've outlived their purpose:
      ;; ------------------------------------------------------------------------
      (gl:delete-vertex-arrays (list vao))
      (gl:delete-buffers (list vbo))
      (gl:delete-buffers (list ebo)))))
  ;; )
