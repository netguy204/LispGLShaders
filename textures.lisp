(in-package :gltest)

(defclass btexture ()
  ((glname :accessor glname)))

(defun make-texture (file)
  (with-pixel-and-format ((img pixels fmt) file)
    (let ((name (car (gl:gen-textures 1)))
	  (tex (make-instance 'btexture)))

      (format t "~&texture size: ~a x ~a" (sdl:width img) (sdl:height img))

      (gl:bind-texture :texture-2d name)

      ;; do stuff
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)

      (gl:tex-image-2d :texture-2d
		       0 ;; base LOD
		       :rgba ;; how gl should store this
		       (sdl:width img)
		       (sdl:height img)
		       0 ;; border, always 0
		       fmt :unsigned-byte ;; how we're providing it
		       (sdl-base::pixel-data pixels))

      (setf (glname tex) name)
      tex)))

