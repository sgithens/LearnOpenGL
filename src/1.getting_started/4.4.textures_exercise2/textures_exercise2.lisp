(defun textures-exercise2 ()
                                ; positions    ; colors      ; texture coords
  (textures-combined :vertices #( 0.5  0.5 0.0   1.0 0.0 0.0   2.0 2.0          ;; top right
                                  0.5 -0.5 0.0   0.0 1.0 0.0   2.0 0.0          ;; bottom right
                                 -0.5 -0.5 0.0   0.0 0.0 1.0   0.0 0.0          ;; bottom left
                                 -0.5  0.5 0.0   1.0 1.0 0.0   0.0 2.0)         ;; top left
                     :vertex-shader-file "src/1.getting_started/4.4.textures_exercise2/4.3.texture.vs"
                     :fragment-shader-file "src/1.getting_started/4.4.textures_exercise2/4.3.texture.fs"))

