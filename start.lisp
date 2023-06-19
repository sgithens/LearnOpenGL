(require "asdf")
(require "uiop")

(ql:quickload :cl-fad)
(ql:quickload :log4cl)
(ql:quickload :cffi)

(defvar *project-dir* (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(setf asdf:*central-registry*
      (list* '*default-pathname-defaults*
              *project-dir*
      asdf:*central-registry*))

(ql:quickload :learn-opengl)
;; (hello-triangle)
;; (hello-triangle-exercise1)
;; (hello-triangle-exercise2)
;; (hello-triangle-exercise3)
;; (shaders-uniform)
;; (shaders-interpolation)
;; (shaders-class)
;; (shaders-exercise1)
;; (shaders-exercise2)
(shaders-exercise3)
