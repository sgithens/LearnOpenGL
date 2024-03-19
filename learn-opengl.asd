(asdf:defsystem :learn-opengl
  :version "0.1"
  :license "CC BY-NC 4.0"
  :author "Steven W Githens <steve@githens.org>"
  :maintainer "Steven W Githens <steve@githens.org>"
  :description "A common lisp version of the example source from Joey de Vries book
Learning OpenGL. This code is derived from the C++ versions in repo JoeyDeVries:master"
  :homepage "https://learnopengl.com"
  :bug-tracker "https://github.com/sgithens/LearnOpenGL/issues"
  :source-control (:git "git@github.com:sgithens/LearnOpenGL.git")
  :serial T
  :components ((:file "package")
               (:file "src/shaders_s")
               (:file "src/camera")
               (:file "src/texture-utils")
               (:file "src/1.getting_started/1.1.hello_window/hello_window")
               (:file "src/1.getting_started/1.2.hello_window_clear/hello_window_clear")
               (:file "src/1.getting_started/2.1.hello_triangle/hello_triangle")
               (:file "src/1.getting_started/2.2.hello_triangle_indexed/hello_triangle_indexed")
               (:file "src/1.getting_started/2.3.hello_triangle_exercise1/hello_triangle_exercise1")
               (:file "src/1.getting_started/2.4.hello_triangle_exercise2/hello_triangle_exercise2")
               (:file "src/1.getting_started/2.5.hello_triangle_exercise3/hello_triangle_exercise3")
               (:file "src/1.getting_started/3.1.shaders_uniform/shaders_uniform")
               (:file "src/1.getting_started/3.2.shaders_interpolation/shaders_interpolation")
               (:file "src/1.getting_started/3.3.shaders_class/shaders_class")
               (:file "src/1.getting_started/3.4.shaders_exercise1/shaders_exercise1")
               (:file "src/1.getting_started/3.5.shaders_exercise2/shaders_exercise2")
               (:file "src/1.getting_started/3.6.shaders_exercise3/shaders_exercise3")
               (:file "src/1.getting_started/4.1.textures/textures")
               (:file "src/1.getting_started/4.2.textures_combined/textures_combined")
               (:file "src/1.getting_started/4.3.textures_exercise1/textures_exercise1")
               (:file "src/1.getting_started/4.4.textures_exercise2/textures_exercise2")

               (:file "src/1.getting_started/4.6.textures_exercise4/textures_exercise4")
               (:file "src/1.getting_started/5.1.transformations/transformations")
               (:file "src/1.getting_started/5.2.transformations_exercise1/transformations_exercise1")
               (:file "src/1.getting_started/6.1.coordinate_systems/coordinate_systems")
               (:file "src/1.getting_started/6.2.coordinate_systems_depth/coordinate_systems_depth")
               (:file "src/1.getting_started/6.3.coordinate_systems_multiple/coordinate_systems_multiple")
               (:file "src/1.getting_started/6.4.coordinate_systems_exercise3/coordinate_systems_exercise3")
               (:file "src/1.getting_started/7.1.camera_circle/camera_circle")
               (:file "src/1.getting_started/7.2.camera_keyboard_dt/camera_keyboard_dt")
               (:file "src/1.getting_started/7.3.camera_mouse_zoom/camera_mouse_zoom")
               (:file "src/1.getting_started/7.4.camera_class/camera_class")

               (:file "src/2.lighting/1.colors/colors")
               )
  :depends-on (:cl-glfw3
               :cl-opengl
               :cl-fad
               :log4cl
               :pngload
               :3d-matrices
               :3d-vectors)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-vectors-test))))
