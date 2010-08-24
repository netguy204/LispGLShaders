(in-package :gltest)

(defparameter +vshader+ (merge-pathnames "test.v.glsl" *sys-path*))
(defparameter +fshader+ (merge-pathnames "test.f.glsl" *sys-path*))

(defclass gltest-window (glut:window)
  ()
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
  (gl:enable :light0 :lighting :cull-face :depth-test))

(defparameter *teapot-rotation* 0.0)
(defparameter *last-update-time* nil)
(defvar *program* nil)

(defmethod glut:display ((window gltest-window))
  (gl:load-identity)
  (gl:translate 0 0 -5)
  (gl:light :light0 :position '(0 1 1 0))
  (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
  (gl:clear :color-buffer :depth-buffer)
  (gl:color 1 1 1)
  (gl:front-face :cw)
  (gl:rotate *teapot-rotation* 1 1 0)
  (glut:solid-teapot 1.3)
  (gl:front-face :ccw)
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

(defmethod glut:idle ((window gltest-window))
  (animate)
  (glut:post-redisplay))

(defun run-test ()
  (setf *last-update-time* (sdl:sdl-get-ticks))
  (glut:display-window (make-instance 'gltest-window)))

(run-test)
