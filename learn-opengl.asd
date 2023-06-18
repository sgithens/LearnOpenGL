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
               (:file "src/1.getting_started/1.1.hello_window/hello_window")
               (:file "src/1.getting_started/1.2.hello_window_clear/hello_window_clear")
               (:file "src/1.getting_started/2.1.hello_triangle/hello_triangle")
               (:file "src/1.getting_started/2.2.hello_triangle_indexed/hello_triangle_indexed")
               (:file "src/1.getting_started/2.3.hello_triangle_exercise1/hello_triangle_exercise1")
               (:file "src/1.getting_started/2.4.hello_triangle_exercise2/hello_triangle_exercise2")
               (:file "src/1.getting_started/2.5.hello_triangle_exercise3/hello_triangle_exercise3")
               (:file "src/1.getting_started/3.1.shaders_uniform/shaders_uniform")
               (:file "src/1.getting_started/3.2.shaders_interpolation/shaders_interpolation")

               )
  :depends-on (:cl-glfw3
               :cl-opengl
               :cl-fad
               :log4cl
               :pngload
               :3d-matrices
               :3d-vectors)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-vectors-test))))
