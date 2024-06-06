;; int main()
;; {
;;     [...]
;;     // render loop
;;     while(!glfwWindowShouldClose(window))
;;     {
;;         // per-frame time logic
;;         float currentFrame = glfwGetTime();
;;         deltaTime = currentFrame - lastFrame;
;;         lastFrame = currentFrame;
;;
;;         // input
;;         processInput(window);
;;
;;         // clear the colorbuffer
;;         glClearColor(0.1f, 0.1f, 0.1f, 1.0f);
;;         glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
;;
;;         // change the light's position values over time (can be done anywhere in the render loop actually, but try to do it at least before using the light source positions)
;;         lightPos.x = 1.0f + sin(glfwGetTime()) * 2.0f;
;;         lightPos.y = sin(glfwGetTime() / 2.0f) * 1.0f;
;;
;;         // set uniforms, draw objects
;;         [...]
;;
;;         // glfw: swap buffers and poll IO events
;;         glfwSwapBuffers(window);
;;         glfwPollEvents();
;;     }
;; }

(defparameter *scr-width* 800)
(defparameter *scr-height* 600)

(defvar *cffi-float-size* (cffi:foreign-type-size :float))

;; camera
(defvar camera (make-camera (3d-vectors:vec3 0.0 0.0 3.0)))

(defvar camera-pos     (3d-vectors:vec3 0.0 0.0  3.0))
(defvar camera-front   (3d-vectors:vec3 0.0 0.0 -1.0))
(defvar camera-up      (3d-vectors:vec3 0.0 1.0  0.0))

(defvar first-mouse   t)

(defvar light-pos #(1.2 1.0 2.0))

;; timing
(defvar delta-time 0.0) ;; time between current frame and last frame
(defvar last-frame 0.0)


;; glfw: whenever the mouse moves, this callback is called
;; -------------------------------------------------------
(cffi:defcallback mouse-callback :void ((window :pointer) (x-pos-in :double) (y-pos-in :double))
  (let* ((x-pos (float x-pos-in))
         (y-pos (float y-pos-in))
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

    (process-mouse-movement camera x-offset y-offset)))

;; glfw: whenever the mouse scroll wheel scrolls, this callback is called
;; ----------------------------------------------------------------------
(cffi:defcallback scroll-callback :void ((window :pointer) (x-offset :double) (y-offset :double))
  (process-mouse-scroll camera (float y-offset)))

(cl-glfw3:def-key-callback process-input (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (cond
    ((and (eq key :escape) (eq action :press))
     (cl-glfw3:set-window-should-close))
    ((and (eq key :w))
     (process-keyboard camera :forward delta-time))
    ((and (eq key :s))
     (process-keyboard camera :backward delta-time))
    ((and (eq key :a))
     (process-keyboard camera :left delta-time))
    ((and (eq key :d))
     (process-keyboard camera :right delta-time))))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 *scr-width* *scr-height*))

(defun basic-lighting-exercise1 (&key (lighting-vertex-shader-file  "src/2.lighting/2.2.basic_lighting_specular/2.2.basic_lighting.vs")
                              (lighting-fragment-shader-file      "src/2.lighting/2.2.basic_lighting_specular/2.2.basic_lighting.fs")
                              (light-cube-vertex-shader-file      "src/2.lighting/2.2.basic_lighting_specular/2.2.light_cube.vs")
                              (light-cube-fragment-shader-file    "src/2.lighting/2.2.basic_lighting_specular/2.2.light_cube.fs")
                              (vertices #(
                                 ; positions    ; normals
                                -0.5 -0.5 -0.5  0.0  0.0 -1.0
                                 0.5 -0.5 -0.5  0.0  0.0 -1.0
                                 0.5  0.5 -0.5  0.0  0.0 -1.0
                                 0.5  0.5 -0.5  0.0  0.0 -1.0
                                -0.5  0.5 -0.5  0.0  0.0 -1.0
                                -0.5 -0.5 -0.5  0.0  0.0 -1.0

                                -0.5 -0.5  0.5  0.0  0.0  1.0
                                 0.5 -0.5  0.5  0.0  0.0  1.0
                                 0.5  0.5  0.5  0.0  0.0  1.0
                                 0.5  0.5  0.5  0.0  0.0  1.0
                                -0.5  0.5  0.5  0.0  0.0  1.0
                                -0.5 -0.5  0.5  0.0  0.0  1.0

                                -0.5  0.5  0.5 -1.0  0.0  0.0
                                -0.5  0.5 -0.5 -1.0  0.0  0.0
                                -0.5 -0.5 -0.5 -1.0  0.0  0.0
                                -0.5 -0.5 -0.5 -1.0  0.0  0.0
                                -0.5 -0.5  0.5 -1.0  0.0  0.0
                                -0.5  0.5  0.5 -1.0  0.0  0.0

                                 0.5  0.5  0.5  1.0  0.0  0.0
                                 0.5  0.5 -0.5  1.0  0.0  0.0
                                 0.5 -0.5 -0.5  1.0  0.0  0.0
                                 0.5 -0.5 -0.5  1.0  0.0  0.0
                                 0.5 -0.5  0.5  1.0  0.0  0.0
                                 0.5  0.5  0.5  1.0  0.0  0.0

                                -0.5 -0.5 -0.5  0.0 -1.0  0.0
                                 0.5 -0.5 -0.5  0.0 -1.0  0.0
                                 0.5 -0.5  0.5  0.0 -1.0  0.0
                                 0.5 -0.5  0.5  0.0 -1.0  0.0
                                -0.5 -0.5  0.5  0.0 -1.0  0.0
                                -0.5 -0.5 -0.5  0.0 -1.0  0.0

                                -0.5  0.5 -0.5  0.0  1.0  0.0
                                 0.5  0.5 -0.5  0.0  1.0  0.0
                                 0.5  0.5  0.5  0.0  1.0  0.0
                                 0.5  0.5  0.5  0.0  1.0  0.0
                                -0.5  0.5  0.5  0.0  1.0  0.0
                                -0.5  0.5 -0.5  0.0  1.0  0.0
)))
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
    (let* ((lighting-shader (make-shader lighting-vertex-shader-file
                                         lighting-fragment-shader-file
                                         :source-dir *load-truename*))
           (light-cube-shader (make-shader light-cube-vertex-shader-file
                                           light-cube-fragment-shader-file
                                           :source-dir *load-truename*))
           (cube-vao          (gl:gen-vertex-array))
           (light-cube-vao    (gl:gen-vertex-array))
           (vbo             (gl:gen-buffer))

           (arr (gl:alloc-gl-array :float (length vertices)))
           (lighting-pos (3d-vectors:vec3 1.2 1.0 2.0))
          )

      ;; set up vertex data (and buffer(s)) and configure vertex attributes
      ;; ------------------------------------------------------------------

      ;; Copy the vertices to our C array
      (dotimes (i (length vertices))
          (setf (gl:glaref arr i) (aref vertices i)))

      ;; first, configure the cube's VAO (and VBO)
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw arr)

      (gl:bind-vertex-array cube-vao)

      ;; position attribute
      (gl:vertex-attrib-pointer 0 3 :float nil (* 6 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)
      ;; normal attribute
      (gl:vertex-attrib-pointer 1 3 :float nil (* 6 *cffi-float-size*) (* 3 *cffi-float-size*))
      (gl:enable-vertex-attrib-array 1)

      ;; second, configure the light's VAO (VBO stays the same; the vertices are the same for the light object which is also a 3D cube)
      (gl:bind-vertex-array light-cube-vao)

      ;; we only need to bind to the VBO (to link it with glVertexAttribPointer), no need to fill it;
      ;; the VBO's data already contains all we need (it's already bound, but we do it again for educational purposes)
      (gl:bind-buffer :array-buffer vbo)

      ;; note that we update the lamp's position attribute's stride to reflect the updated buffer data
      (gl:vertex-attrib-pointer 0 3 :float nil (* 6 *cffi-float-size*) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)

      (gl:free-gl-array arr)


      (cl-glfw3:set-key-callback 'process-input)
      (cl-glfw3:set-window-size-callback 'update-viewport)
      (cl-glfw3:set-cursor-position-callback 'mouse-callback)
      (cl-glfw3:set-scroll-callback 'scroll-callback)


      (loop until (cl-glfw3:window-should-close-p)
          do (progn
              ;; per-frame time logic
              ;; --------------------
              (let ((current-frame (%cl-glfw3:get-time)))
                (setf delta-time (- current-frame last-frame)
                      last-frame current-frame))

              ;; render
              ;; ------
              (gl:clear-color 0.1 0.1 0.1 1.0)
              (gl:clear :color-buffer-bit :depth-buffer-bit)

              ;; Exercise 1 changes
              ;; change the light's position values over time (can be done anywhere in the render loop actually, but try to do it at least before using the light source positions)
              (setf (aref light-pos 0) (* (+ 1.0 (sin (%cl-glfw3:get-time))) 2.0))
              (setf (aref light-pos 1) (* (sin (/ (%cl-glfw3:get-time) 2.0)) 1.0))

              ;; be sure to activate shader when setting uniforms/drawing objects
              (use lighting-shader)
              (set-vec3 lighting-shader "objectColor" #(1.0 0.5 0.31))
              (set-vec3 lighting-shader "lightColor" #(1.0 1.0 1.0))
              (set-vec3 lighting-shader "lightPos" light-pos)
              (set-vec3 lighting-shader "viewPos" (3d-vectors:with-vec3 (x y z) (pos camera)
                                                    `#(,x ,y ,z)))

              ;; view/projection transformations
              (set-mat4 lighting-shader "projection"
                (3d-matrices:mperspective (zoom camera) (/ *scr-width* *scr-height*) 0.1 100.0))
              (set-mat4 lighting-shader "view" (get-view-matrix camera))

              ;; world transformation
              (set-mat4 lighting-shader "model" (3d-matrices:meye 4))

              ;; render the cube
              (gl:bind-vertex-array cube-vao)
              (gl:draw-arrays :triangles 0 36)

              ;; also draw the lamp object
              (use light-cube-shader)
              (set-mat4 light-cube-shader "projection"
                (3d-matrices:mperspective (zoom camera) (/ *scr-width* *scr-height*) 0.1 100.0))
              (set-mat4 light-cube-shader "view" (get-view-matrix camera))
              (let ((model (3d-matrices:meye 4)))
                (3d-matrices:nmtranslate model lighting-pos)
                (3d-matrices:nmscale model (3d-vectors:vec 0.2 0.2 0.2))
                (set-mat4 light-cube-shader "model" model))

              (gl:bind-vertex-array light-cube-vao)
              (gl:draw-arrays :triangles 0 36)

              ;; glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
              ;; -------------------------------------------------------------------------------
              (cl-glfw3:swap-buffers)
              (cl-glfw3:poll-events)))

      ;; optional: de-allocate all resources once they've outlived their purpose:
      ;; ------------------------------------------------------------------------
      (gl:delete-vertex-arrays (list cube-vao light-cube-vao))
      (gl:delete-buffers (list vbo)))))
  ;; )
