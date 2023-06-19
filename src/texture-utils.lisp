;; From Trial
(defun infer-pixel-type (depth type)
  (ecase depth
    ((1 8) (ecase type
             (:signed :byte)
             (:unsigned :unsigned-byte)))
    (16 (ecase type
          (:signed :short)
          (:unsigned :unsigned-short)
          (:float :half-float)))
    (32 (ecase type
          (:signed :int)
          (:unsigned :unsigned-int)
          (:float :float)))))

(defmethod load-image (path (type (eql :png)) &key)
  (let ((png (pngload:load-file path :flatten T :flip-y T)))
    (values (pngload:data png)
            (pngload:width png)
            (pngload:height png)
            (infer-pixel-type (pngload:bit-depth png) :unsigned)
            (ecase (pngload:color-type png)
              (:greyscale :red)
              (:greyscale-alpha :rg)
              (:truecolour :rgb)
              (:truecolour-alpha :rgba)))))
;; End From Trial

(defun load-texture (path)
  (multiple-value-bind (data width height pixel-type pixel-format)
                       (load-image path :png)
    (let ((arr (gl:alloc-gl-array :unsigned-char (length data))))
      (dotimes (i (length data))
        (setf (gl:glaref arr i) (aref data i)))
      arr
    )))
