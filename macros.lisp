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

