(defclass shader ()
  ((id :initform 0 :initarg :id :accessor id
       :documentation "Shader program integer ID")))

(defun make-shader (vertex-source fragment-source
                    &key (source-dir (make-pathname :directory (pathname-directory (uiop:truename* *load-truename*)))))
  (let ((vertex-shader   (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader))
        (program (gl:create-program)))

    (gl:shader-source vertex-shader
                      (uiop:read-file-string (cl-fad:merge-pathnames-as-file source-dir vertex-source)))
    (gl:compile-shader vertex-shader)
    (log:info "~%shader infolog: ~A" (gl:get-shader-info-log vertex-shader))

    (gl:shader-source fragment-shader
                      (uiop:read-file-string (cl-fad:merge-pathnames-as-file source-dir fragment-source)))
    (gl:compile-shader fragment-shader)
    (log:info "~%shader infolog: ~A" (gl:get-shader-info-log fragment-shader))

    (gl:attach-shader program vertex-shader)
    (gl:attach-shader program fragment-shader)
    (gl:link-program program)
    (log:debug "~%program infolog: ~A" (gl:get-program-info-log program))
    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)
    (make-instance 'shader :id program)))

(defmethod use ((self shader))
  (gl:use-program (id self))
)

(defmethod set-bool ((self shader) name value)
  (error "TODO")
)

(defmethod set-int ((self shader) name value)
  (gl:uniformi (gl:get-uniform-location (id self) name) value)
)

(defmethod set-float ((self shader) name value)
  (gl:uniformf (gl:get-uniform-location (id self) name) value))

(defmethod set-mat4 ((self shader) name value)
  (gl:uniform-matrix-4fv (gl:get-uniform-location (id shader) name)
          (3d-matrices:marr4 value) t))
