(in-package :gltest)

(defun rpath (resource)
  (merge-pathnames resource *sys-path*))

(defparameter +vshader+ (rpath "test.v.glsl"))
(defparameter +fshader+ (rpath "test.f.glsl"))
(defparameter +texture+ (rpath "lisplogo_alien_256.png"))
(defparameter +mesh+ (rpath "test.mesh"))
(defparameter +num-points+ 5)

(defclass bshader ()
  ((program :accessor program)
   (components :accessor components
	       :initform nil)))

(defmacro assert-with-info (test item type)
  `(unless ,test
     (progn
       ,(case type
	      (:shader `(format t "~&SHADER_INFO_LOG: ~a"
				(gl:get-shader-info-log ,item)))
	      
	      (:program `(format t "~&PROGRAM_INFO_LOG: ~a"
				 (gl:get-program-info-log ,item))))
       (assert nil))))

(defun make-shader (components)
  (let ((shader (make-instance 'bshader)))
    (dolist (centry components)
      (let ((handle (first centry))
	    (file (second centry))
	    (type (third centry)))
	(let ((name (gl:create-shader type))
	      (lines (slurp-lines file)))
	  (gl:shader-source name lines)
	  (gl:compile-shader name)
	  (assert-with-info (gl:get-shader name :compile-status) name :shader)
	  (push (cons handle name) (components shader)))))
    (let ((program (gl:create-program)))
      (dolist (comp (components shader))
	(gl:attach-shader program (cdr comp)))
      (gl:link-program program)
      (assert-with-info (gl:get-program program :link-status) program :program)
      (setf (program shader) program))

    shader))

(defun get-uniform-loc (shader name)
  (gl:get-uniform-location (program shader) name))

(defmethod release ((o bshader))
  (dolist (comp (components o))
    (gl:delete-shader (cdr comp)))
  (setf (components o) nil)

  (gl:delete-program (program o))
  (setf (program o) nil))

(defclass btexture ()
  ((glname :accessor glname)))

(defun make-texture (file)
  (with-pixel-and-format ((img pixels fmt) file)
    (let ((name (car (gl:gen-textures 1)))
	  (tex (make-instance 'btexture)))

      (gl:bind-texture :texture-2d name)

      ;; do stuff
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)

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

(defclass gltest-window (glut:window)
  ((obj :accessor obj
	:initform (load-bobj +mesh+))
   (tex :accessor tex
	:initform (make-texture +texture+))
   (shader :accessor shader
	   :initform nil)
   (tex-uniform :accessor tex-uniform
		:initform nil))
  (:default-initargs :width 250 :height 250
		     :title "mytest"
		     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((w gltest-window))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test)

  (setf (shader w) (make-shader (list (list :vert +vshader+ :vertex-shader)
				      (list :frag +fshader+ :fragment-shader))))
  (setf (tex-uniform w) (get-uniform-loc (shader w) "texture")))

(defparameter *teapot-rotation* 0.0)
(defparameter *last-update-time* nil)
(defvar *program* nil)

(defmethod glut:display ((w gltest-window))
  (gl:load-identity)
  (gl:translate 0 0 -5)
  (gl:light :light0 :position '(5 0 0 0))
  (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
  (gl:clear :color-buffer :depth-buffer)
  
  (gl:rotate *teapot-rotation* 1 1 0)

  ;; enable program and bind our texture to it
  (gl:use-program (program (shader w)))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (glname (tex w)))
  (gl:uniformi (tex-uniform w) 0)

  (render (obj w))

  (gl:color 1.0 1.0 1.0 1.0)
 
  (glut:swap-buffers))

(defmethod glut:reshape ((window gltest-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 50 (/ width height) 0.5 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun animate ()
  (let* ((time-now (sdl:sdl-get-ticks))
	 (delta-t (/ (- time-now *last-update-time*) 1000.0)))
    (setf *last-update-time* time-now)
    (setf *teapot-rotation* (+ *teapot-rotation* (* 36 delta-t)))))

(defmethod glut:idle ((w gltest-window))
  (animate)
  (glut:post-redisplay))

(defmethod glut:close ((w gltest-window))
  (release (obj w))
  (release (shader w))
  (sdl-image:quit-image))

(defun run-test ()
  (setf *last-update-time* (sdl:sdl-get-ticks))
  (sdl:with-init ()
    (sdl:window 400 400
		:title-caption "fake"
		:icon-caption "fake")
    (sdl:initialise-default-font)
    (sdl-image:init-image :jpg :png)
    (glut:display-window (make-instance 'gltest-window))))

(run-test)
