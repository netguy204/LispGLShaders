(in-package :gltest)

(defmacro with-pixel-and-format (((img pix fmt) file) &body body)
  `(let ((,img (sdl-image:load-image ,file)))
     (sdl-base::with-pixel (,pix (sdl:fp ,img))
       (let ((,fmt (ecase (sdl-base::pixel-bpp ,pix)
		     (3 :rgb)
		     (4 :rgba))))
	 (assert (and (= (sdl-base::pixel-pitch ,pix)
			 (* (sdl:width ,img)
			    (sdl-base::pixel-bpp ,pix)))
		      (zerop (rem (sdl-base::pixel-pitch ,pix) 4))))

	 (progn . ,body)))))

(defmacro with-texture ((tex file) &body body)
  `(let ((,tex (make-texture ,file)))
     (load-texture ,tex)
     (progn
       . ,body)

     (unload-texture ,tex)))

(defun slurp-lines (file)
  (let ((lines nil))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do (push line lines)))
    (nreverse lines)))

(defmacro with-shader ((name file type) &body body)
  (let ((lines (gensym)))
    `(let ((,name (gl:create-shader ,type))
	   (,lines (slurp-lines ,file)))
       (gl:shader-source ,name ,lines)
       (gl:compile-shader ,name)
       (assert (gl:get-shader ,name :compile-status))

       (progn . ,body)

       (gl:delete-shader ,name))))

(defun emit-attach-shader (prg shader)
  `(gl:attach-shader ,prg ,shader))

(defmacro with-program ((program shaders) &body body)
  `(let ((,program (gl:create-program)))
     ,@(mapcar #'(lambda (shader) (emit-attach-shader program shader)) shaders)
     (gl:link-program ,program)
     (assert (gl:get-program ,program :link-status))

     (progn . ,body)

     (gl:delete-program ,program)))

(defmacro with-program-texture ((variable texture) &body body)
  (let ((handle (gensym)))
    `(let ((,handle (gl:get-uniform-location *program* ,variable)))

       (when (null (tex-unit ,texture))
	 (hardware-bind-texture ,texture))

       (gl:uniformi ,handle (glname ,texture))

       (progn . ,body)
       
       (when (tex-unit ,texture)
	 (hardware-unbind-texture ,texture)))))
