(defun shaders-exercise3 ()
  (shaders-class :vertex-shader-file "src/1.getting_started/3.6.shaders_exercise3/shaders_exercise3.vs"
                 :fragment-shader-file "src/1.getting_started/3.6.shaders_exercise3/shaders_exercise3.fs"))


;; Answer to the question: Do you know why the bottom-left side is black?
;; -- --------------------------------------------------------------------
;; Think about this for a second: the output of our fragment's color is equal to the (interpolated) coordinate of
;; the triangle. What is the coordinate of the bottom-left point of our triangle? This is (-0.5f, -0.5f, 0.0f). Since the
;; xy values are negative they are clamped to a value of 0.0f. This happens all the way to the center sides of the
;; triangle since from that point on the values will be interpolated positively again. Values of 0.0f are of course black
;; and that explains the black side of the triangle.
