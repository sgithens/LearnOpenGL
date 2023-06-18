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
               )
  :depends-on (:cl-glfw3
               :cl-opengl
               :cl-fad
               :log4cl
               :pngload
               :3d-matrices
               :3d-vectors)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-vectors-test))))
