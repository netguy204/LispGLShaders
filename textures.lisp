(in-package :gltest)

(defclass texture ()
  ((filename :reader filename :initarg :filename)
   (tex-unit :accessor tex-unit :initform nil)
   (glname :accessor glname :initform nil)))

(defun make-texture (file)
  (make-instance 'texture :filename file))

(defvar *available-texture-units*
  (list :texture0 :texture1 :texture2 :texture3
	:texture4 :texture5 :texture6 :texture7))

(defun load-texture (tex)
  (with-pixel-and-format ((img pixels fmt) (filename tex))
    (let ((name (car (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d name)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgba
		       (sdl:width img)
		       (sdl:height img)
		       0
		       fmt
		       :unsigned-byte (sdl-base::pixel-data pixels))
      (setf (glname tex) name))))

(defun unload-texture (tex)
  (assert (glname tex))
  (gl:delete-textures (list (glname tex))))

(defun hardware-bind-texture (tex)
  (assert (null (tex-unit tex)))
  (assert (glname tex))
  (let ((unit (pop *available-texture-units*)))
    (assert unit)
    (gl:active-texture unit)
    (gl:bind-texture :texture-2d (glname tex))
    (setf (tex-unit tex) unit)))

(defun hardware-unbind-texture (tex)
  (assert (tex-unit tex))
  (push (tex-unit tex) *available-texture-units*)
  (setf (tex-unit tex) nil))
