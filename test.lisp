(in-package :gltest)

(defun rpath (resource)
  (merge-pathnames resource *sys-path*))

(defparameter +vshader+ (rpath "test.v.glsl"))
(defparameter +fshader+ (rpath "test.f.glsl"))
(defparameter +texture+ (rpath "testmap.jpg"))
(defparameter +mesh+ (rpath "apple.mesh"))
(defparameter *teapot-rotation* 0.0)
(defparameter *last-update-time* nil)

(defparameter *initial-width* 300)
(defparameter *initial-height* 300)
(defparameter *use-shader* 0)

(defclass gltest-window (bwindow)
  ((obj :accessor obj
	:initform (load-bobj +mesh+))
;;   (tex :accessor tex
;;	:initform (make-texture +texture+))
   (shader :accessor shader
	   :initform nil)
   (tex-uniform :accessor tex-uniform
		:initform nil)))

(defmethod reload-shader ((w gltest-window))
  (when *use-shader*
    (unless (null (shader w))
      (release (shader w)))
    (setf (shader w) (make-shader (list (list :vert +vshader+ :vertex-shader)
					(list :frag +fshader+ :fragment-shader))))))

(defmethod prepare-window :after ((w gltest-window))
  (when *use-shader*
    (reload-shader w)
    (setf (tex-uniform w) (get-uniform-loc (shader w) "texture"))))

(defmethod render ((w gltest-window))
  (gl:translate 0 0 -5)
  (gl:light :light0 :position '(0 1 1 0))
  (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
  
  (gl:rotate *teapot-rotation* 1 1 0)

  ;; enable program and bind our texture to it
  (when *use-shader*
    (gl:use-program (program (shader w)))
    ;; (gl:active-texture :texture0)
    ;; (gl:bind-texture :texture-2d (glname (tex w)))
    (gl:uniformi (tex-uniform w) 0))

  (render (obj w)))

(defmethod animate ((window gltest-window))
  (let* ((time-now (sdl:sdl-get-ticks))
	 (delta-t (/ (- time-now *last-update-time*) 1000.0)))
    (setf *last-update-time* time-now)
    (setf *teapot-rotation* (+ *teapot-rotation* (* 36 delta-t)))))

(defmethod release ((w gltest-window))
  (release (obj w))
  (when *use-shader*
    (release (shader w))))

(defun create-gl-window (width height title class &rest instance-args)
    (sdl:window width height
		:title-caption title
		:icon-caption title
		:flags sdl:sdl-opengl)

    (setf cl-opengl-bindings:*gl-get-proc-address*
	  #'sdl-cffi::sdl-gl-get-proc-address)
  
    (let ((w (apply #'make-instance class
		    :width *initial-width*
		    :height *initial-height*
		    instance-args)))
      (prepare-window w)
      (reshape w width height)
      w))

(defun run-test ()
  (setf *last-update-time* (sdl:sdl-get-ticks))
  (sdl:with-init ()
    (let ((w (create-gl-window *initial-width* *initial-height*
			       "OpenGL Test"
			       'gltest-window)))
      (setf (sdl:frame-rate) 10)
      (sdl:with-events ()
	(:quit-event ()
		     (release w)
		     t)

	(:key-down-event (:key key)
			 (when (sdl:key= key :sdl-key-r)
			   (reload-shader w))

			 (when (sdl:key= key :sdl-key-escape)
			   (sdl:push-quit-event)))
	
	(:idle ()
	       (animate w)
	       (render w))))))

(run-test)
