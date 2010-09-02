(in-package :gltest)

(defclass bwindow ()
  ((width :accessor width
	  :initarg :width)
   (height :accessor height
	   :initarg :height)))

(defmethod prepare-window ((w bwindow))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test :color-material))

(defmethod render :before ((w bwindow))
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity))

(defmethod render :after ((w bwindow))
  (gl:flush)
  (sdl:update-display))

(defmethod reshape ((w bwindow) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 50 (/ width height) 0.5 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (setf (width w) width
	(height w) height))

