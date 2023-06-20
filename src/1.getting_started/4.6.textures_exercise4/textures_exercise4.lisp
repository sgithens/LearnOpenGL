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

(defvar *mix-value* 0.2
  "exercise 4 mixValue uniform")
(cl-glfw3:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (cond ((and (eq key :escape) (eq action :press))
         (cl-glfw3:set-window-should-close))
        ((and (eq key :up))
         (setq *mix-value* (min 1.0 (+ *mix-value* 0.005)))) ;; change this value accordingly (might be too slow or too
        ((and (eq key :down))                                ;; fast based on system hardware)
         (setq *mix-value* (max 0.0 (- *mix-value* 0.005))))
        (t nil)))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun textures-exercise4 (&key (vertex-shader-file "src/1.getting_started/4.6.textures_exercise4/4.5.texture.vs")
                                (fragment-shader-file "src/1.getting_started/4.6.textures_exercise4/4.5.texture.fs")
                                           ; positions    ; colors      ; texture coords
                                (vertices #( 0.5  0.5 0.0   1.0 0.0 0.0   1.0 1.0          ;; top right
                                             0.5 -0.5 0.0   0.0 1.0 0.0   1.0 0.0          ;; bottom right
                                            -0.5 -0.5 0.0   0.0 0.0 1.0   0.0 0.0          ;; bottom left
                                            -0.5  0.5 0.0   1.0 1.0 0.0   0.0 1.0 )))      ;; top left
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
      (gl:vertex-attrib-pointer 0 3 :float nil (* 8 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)
      ;; color attribute
      (gl:vertex-attrib-pointer 1 3 :float nil (* 8 *cffi-float-size*) (* 3 *cffi-float-size*))
      (gl:enable-vertex-attrib-array 1)
      ;; texture coord attribute
      (gl:vertex-attrib-pointer 2 2 :float nil (* 8 *cffi-float-size*) (* 6 *cffi-float-size*))
      (gl:enable-vertex-attrib-array 2)

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


      (cl-glfw3:set-key-callback 'key-callback)
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

              ;; exercise 4 change the mix-value with up and down keys
              (set-float our-shader "mixValue" *mix-value*)

              ;; render container
              (use our-shader)
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
