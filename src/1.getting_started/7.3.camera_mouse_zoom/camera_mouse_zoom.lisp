(defparameter *scr-width* 800)
(defparameter *scr-height* 600)

(defvar *cffi-float-size* (cffi:foreign-type-size :float))

;; camera
(defvar camera-pos     (3d-vectors:vec3 0.0 0.0  3.0))
(defvar camera-front   (3d-vectors:vec3 0.0 0.0 -1.0))
(defvar camera-up      (3d-vectors:vec3 0.0 1.0  0.0))

(defvar first-mouse   t)
(defvar yaw         -90.0
  "yaw is initialized to -90.0 degrees since a yaw of 0.0 results in a direction
   vector pointing to the right so we initially rotate a bit to the left.")
(defvar pitch         0.0)
(defvar last-x       (/ *scr-width* 2.0))
(defvar last-y       (/ *scr-height* 2.0))
(defvar fov          45.0)

;; timing
(defvar delta-time 0.0) ;; time between current frame and last frame
(defvar last-frame 0.0)

;; glfw: whenever the mouse moves, this callback is called
;; -------------------------------------------------------
(cffi:defcallback mouse-callback :void ((window :pointer) (x-pos-in :double) (y-pos-in :double))
  (format t "~%mouse-callback x: ~A  y: ~A" x-pos-in y-pos-in)
  (let* ((x-pos (float x-pos-in))
         (y-pos (float y-pos-in))
         (sensitivity 0.1)
         (front nil)
         (x-offset nil)
         (y-offset nil))

    (when first-mouse
      (setf last-x x-pos)
      (setf last-y y-pos)
      (setf first-mouse nil))

    (setf x-offset (- x-pos last-x))
    (setf y-offset (- last-y y-pos)) ;; reversed since y-coordinates go from bottom to top
    (setf last-x x-pos)
    (setf last-y y-pos)

    (setf sensitivity 0.1) ;; change this value to your liking
    (setf x-offset (* sensitivity x-offset))
    (setf y-offset (* sensitivity y-offset))

    (incf yaw x-offset)
    (incf pitch y-offset)

    ;; make sure that when pitch is out of bounds, screen doesn't get flipped
    (when (> pitch 89.0)
      (setf pitch 89.0))
    (when (< pitch -89.0)
      (setf pitch -89.0))

    (setf front (3d-vectors:vec3
                (* (cos (degrees->radians yaw)) (cos (degrees->radians pitch)))
                   (sin (degrees->radians pitch))
                (* (sin (degrees->radians yaw)) (cos (degrees->radians pitch)))))

    (setf camera-front (3d-vectors:vunit front))))

;; glfw: whenever the mouse scroll wheel scrolls, this callback is called
;; ----------------------------------------------------------------------
(cffi:defcallback scroll-callback :void ((window :pointer) (x-offset :double) (y-offset :double))
  (format t "~%scroll-callback x: ~A  y: ~A" x-offset y-offset)
  (decf fov y-offset)
  (when (< fov 1.0)
    (setf fov 1.0))
  (when (> fov 45.0)
    (setf fov 45.0)))

(cl-glfw3:def-key-callback process-input (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (let ((camera-speed (* 2.5 delta-time)))
    (cond
      ((and (eq key :escape) (eq action :press))
       (cl-glfw3:set-window-should-close))
      ((and (eq key :w))
       (setf camera-pos (3d-vectors:v+ camera-pos (3d-vectors:v* camera-front camera-speed))))
      ((and (eq key :s))
       (setf camera-pos (3d-vectors:v- camera-pos (3d-vectors:v* camera-front camera-speed))))
      ((and (eq key :a))
       (setf camera-pos (3d-vectors:v- camera-pos (3d-vectors:v* (3d-vectors:nvunit (3d-vectors:vc camera-front camera-up)) camera-speed))))
      ((and (eq key :d))
       (setf camera-pos (3d-vectors:v+ camera-pos (3d-vectors:v* (3d-vectors:nvunit (3d-vectors:vc camera-front camera-up)) camera-speed))))
    )))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun camera-mouse-zoom (&key (vertex-shader-file "src/1.getting_started/7.3.camera_mouse_zoom/7.3.camera.vs")
                               (fragment-shader-file "src/1.getting_started/7.3.camera_mouse_zoom/7.3.camera.fs")
                                                 ; positions    ; texture coords
                                      (vertices #(-0.5 -0.5 -0.5  0.0 0.0
                                                   0.5 -0.5 -0.5  1.0 0.0
                                                   0.5  0.5 -0.5  1.0 1.0
                                                   0.5  0.5 -0.5  1.0 1.0
                                                  -0.5  0.5 -0.5  0.0 1.0
                                                  -0.5 -0.5 -0.5  0.0 0.0

                                                  -0.5 -0.5  0.5  0.0 0.0
                                                   0.5 -0.5  0.5  1.0 0.0
                                                   0.5  0.5  0.5  1.0 1.0
                                                   0.5  0.5  0.5  1.0 1.0
                                                  -0.5  0.5  0.5  0.0 1.0
                                                  -0.5 -0.5  0.5  0.0 0.0

                                                  -0.5  0.5  0.5  1.0 0.0
                                                  -0.5  0.5 -0.5  1.0 1.0
                                                  -0.5 -0.5 -0.5  0.0 1.0
                                                  -0.5 -0.5 -0.5  0.0 1.0
                                                  -0.5 -0.5  0.5  0.0 0.0
                                                  -0.5  0.5  0.5  1.0 0.0

                                                   0.5  0.5  0.5  1.0 0.0
                                                   0.5  0.5 -0.5  1.0 1.0
                                                   0.5 -0.5 -0.5  0.0 1.0
                                                   0.5 -0.5 -0.5  0.0 1.0
                                                   0.5 -0.5  0.5  0.0 0.0
                                                   0.5  0.5  0.5  1.0 0.0

                                                  -0.5 -0.5 -0.5  0.0 1.0
                                                   0.5 -0.5 -0.5  1.0 1.0
                                                   0.5 -0.5  0.5  1.0 0.0
                                                   0.5 -0.5  0.5  1.0 0.0
                                                  -0.5 -0.5  0.5  0.0 0.0
                                                  -0.5 -0.5 -0.5  0.0 1.0

                                                  -0.5  0.5 -0.5  0.0 1.0
                                                   0.5  0.5 -0.5  1.0 1.0
                                                   0.5  0.5  0.5  1.0 0.0
                                                   0.5  0.5  0.5  1.0 0.0
                                                  -0.5  0.5  0.5  0.0 0.0
                                                  -0.5  0.5 -0.5  0.0 1.0)))
  ;; (with-body-in-main-thread ()
  (cl-glfw3:with-init-window (:title "LearnOpenGL" :width *scr-width* :height *scr-height*
                              :context-version-major 3
                              :context-version-minor 3
                              :opengl-profile #x00032001
                              #+os-macosx :opengl-forward-compat #+os-macosx t)
    (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)

    ;; configure global opengl state
    ;; -----------------------------
    (gl:enable :depth-test);

    ;; build and compile our shader program
    ;; ------------------------------------
    (let* ((our-shader (make-shader vertex-shader-file
                                    fragment-shader-file
                                    :source-dir *load-truename*))
           (vao             (gl:gen-vertex-array))
           (vbo             (gl:gen-buffer))
           (texture1        (gl:gen-texture))
           (texture2        (gl:gen-texture))

           (arr (gl:alloc-gl-array :float (length vertices)))

           (cube-positions (list (3d-vectors:vec3  0.0  0.0  0.0)
                                 (3d-vectors:vec3  2.0  5.0 -15.0)
                                 (3d-vectors:vec3 -1.5 -2.2 -2.5)
                                 (3d-vectors:vec3 -3.8 -2.0 -12.3)
                                 (3d-vectors:vec3  2.4 -0.4 -3.5)
                                 (3d-vectors:vec3 -1.7  3.0 -7.5)
                                 (3d-vectors:vec3  1.3 -2.0 -2.5)
                                 (3d-vectors:vec3  1.5  2.0 -2.5)
                                 (3d-vectors:vec3  1.5  0.2 -1.5)
                                 (3d-vectors:vec3 -1.3  1.0 -1.5))))

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

      (cl-glfw3:set-key-callback 'process-input)
      (cl-glfw3:set-window-size-callback 'update-viewport)
      (cl-glfw3:set-cursor-position-callback 'mouse-callback)
      (cl-glfw3:set-scroll-callback 'scroll-callback)

      ;; pass projection matrix to shader (as projection matrix rarely changes there's no need to do this per frame)
      ;; -----------------------------------------------------------------------------------------------------------
      (set-mat4 our-shader "projection"
        (3d-matrices:mperspective 45.0 (/ *scr-width* *scr-height*) 0.1 100.0))

      (loop until (cl-glfw3:window-should-close-p)
          do (progn
              ;; per-frame time logic
              ;; --------------------
              (let ((current-frame (%cl-glfw3:get-time)))
                (setf delta-time (- current-frame last-frame)
                      last-frame current-frame))

              ;; render
              ;; ------
              (gl:clear-color 0.2 0.3 0.3 1.0)
              (gl:clear :color-buffer-bit :depth-buffer-bit)

              ;; bind Texture
              (gl:active-texture :texture0)
              (gl:bind-texture :texture-2d texture1)
              (gl:active-texture :texture1)
              (gl:bind-texture :texture-2d texture2)

              ;; activate shader
              (use our-shader)

              (let* ((view (3d-matrices:meye 4)))

                ;; camera/view transformation
                (setq view (3d-matrices:nmlookat view
                                                 camera-pos
                                                 (3d-vectors:v+ camera-pos camera-front)
                                                 camera-up))
                (set-mat4 our-shader "view" view)

                ;; render boxes
                (gl:bind-vertex-array vao)
                (dotimes (i (length cube-positions))
                  ;; calculate the model matrix for each object and pass it to shader before drawing
                  (let ((model (3d-matrices:meye 4))
                        (angle (* 20.0 i)))
                    (3d-matrices:nmtranslate model (nth i cube-positions))
                    (3d-matrices:nmrotate model (3d-vectors:vec3 1.0 0.3 0.5) (degrees->radians angle))
                    (set-mat4 our-shader "model" model)
                    (gl:draw-arrays :triangles 0 36)
                )))

              ;; glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
              ;; -------------------------------------------------------------------------------
              (cl-glfw3:swap-buffers)
              (cl-glfw3:poll-events)))

      ;; optional: de-allocate all resources once they've outlived their purpose:
      ;; ------------------------------------------------------------------------
      (gl:delete-vertex-arrays (list vao))
      (gl:delete-buffers (list vbo)))))
  ;; )
