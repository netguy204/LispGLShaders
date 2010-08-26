(in-package :gltest)

(defun rpath (resource)
  (merge-pathnames resource *sys-path*))

(defparameter +vshader+ (rpath "test.v.glsl"))
(defparameter +fshader+ (rpath "test.f.glsl"))
(defparameter +mesh+ (rpath "test.mesh"))
(defparameter +num-points+ 5)

(defun read-from-file (file)
  (with-open-file (s file)
    (read s)))

(defun verts (data)
  (second (find :vert data :key #'first)))

(defun faces (data)
  (second (find :face data :key #'first)))

(gl:define-gl-array-format position3d
  (gl:vertex :type :float :components (x y z))
  (gl:normal :type :float :components (nx ny nz)))

(defun verts-to-array (verts)
  (let* ((nverts (length verts))
	 (arr (gl:alloc-gl-array 'position3d nverts)))
    (loop for v in verts
	 for i from 0 do
	 (setf (gl:glaref arr i 'x) (first v))
	 (setf (gl:glaref arr i 'y) (second v))
	 (setf (gl:glaref arr i 'z) (third v))
	 (setf (gl:glaref arr i 'nx) (fourth v))
	 (setf (gl:glaref arr i 'ny) (fifth v))
	 (setf (gl:glaref arr i 'nz) (sixth v)))
    arr))

(defun faces-to-array (faces)
  (let* ((nelems (reduce #'+ (mapcar #'length faces)))
	 (flat (reduce #'append faces))
	 (arr (gl:alloc-gl-array :unsigned-short nelems)))
    (loop for v in flat
	 for i from 0 do
	 (setf (gl:glaref arr i) v))
    arr))

(defun data-style (data)
  (case (length (first (faces data)))
    (3 :triangles)))

(defclass bmesh ()
  ((vertices :accessor vertices
	     :initarg :vertices)
   (elements :accessor elements
	     :initarg :elements)
   (style :accessor style
	  :initarg :style)))

(defclass bobj ()
  ((meshes :accessor meshes
	   :initform nil)))

(defun make-bmesh (data)
  (make-instance 'bmesh
		 :style (data-style data)
		 :vertices (verts-to-array (verts data))
		 :elements (faces-to-array (faces data))))

(defun load-bobj (file)
  (let ((data (read-from-file file))
	(obj (make-instance 'bobj)))
    (dolist (mesh data)
      (push (make-bmesh (second mesh)) (meshes obj)))
    obj))

(defmethod release ((m bmesh))
  (when (vertices m)
    (gl:free-gl-array (vertices m))
    (setf (vertices m) nil))
  (when (elements m)
    (gl:free-gl-array (elements m))
    (setf (elements m) nil)))

(defmethod release ((o bobj))
  (dolist (mesh (meshes o))
    (release mesh))
  (setf (meshes o) nil))

(defmethod render ((m bmesh))
  (gl:enable-client-state :vertex-array)
  (gl:enable-client-state :normal-array)
  (gl:bind-gl-vertex-array (vertices m))
  (gl:draw-elements (style m) (elements m)))

(defmethod render ((o bobj))
  (dolist (mesh (meshes o))
    (render mesh)))

(gl:define-gl-array-format position-color
  (gl:vertex :type :float :components (x y))
  (gl:color :type :unsigned-char :components (r g b)))

(defclass gltest-window (glut:window)
  ((vertex-array :accessor vertex-array
		 :initform (gl:alloc-gl-array 'position-color +num-points+))
   (indices-array :accessor indices-array
		  :initform (gl:alloc-gl-array :unsigned-short 10))
   (obj :accessor obj
	:initform (load-bobj +mesh+)))

  (:default-initargs :width 250 :height 250
		     :title "mytest"
		     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((w gltest-window))
  (dotimes (i +num-points+)
    (let ((phi (float (+ (/ pi 2) (* (/ i 5) (* 2 pi))) 0.0)))
      ;; vertices
      (setf (gl:glaref (vertex-array w) i 'x) (cos phi))
      (setf (gl:glaref (vertex-array w) i 'y) (sin phi))
      ;; indices
      (setf (gl:glaref (indices-array w) (* 2 i)) i)
      (setf (gl:glaref (indices-array w) (1+ (* 2 i))) (mod (+ i 2) 5))
      ;; colors
      (setf (gl:glaref (vertex-array w) i 'r) 255)
      (setf (gl:glaref (vertex-array w) i 'g) 0)
      (setf (gl:glaref (vertex-array w) i 'b) 0)))
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

(defmethod glut:display ((w gltest-window))
  (gl:load-identity)
  (gl:translate 0 0 -5)
  (gl:light :light0 :position '(1 0 0 0))
  (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
  (gl:clear :color-buffer :depth-buffer)
  
  (gl:rotate *teapot-rotation* 1 1 0)
  (gl:front-face :cw)
  (with-shader (vertex +vshader+ :vertex-shader)
    (with-shader (frag +fshader+ :fragment-shader)
      (with-program (*program* (vertex frag))
	(gl:use-program *program*)
	(render (obj w)))))
;	(glut:solid-teapot 1.3))))
  (gl:front-face :ccw)

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

(defmethod glut:idle ((window gltest-window))
  (animate)
  (glut:post-redisplay))

(defmethod glut:close ((w gltest-window))
  (gl:free-gl-array (vertex-array w))
  (gl:free-gl-array (indices-array w))
  (release (obj w)))

(defun run-test ()
  (setf *last-update-time* (sdl:sdl-get-ticks))
  (glut:display-window (make-instance 'gltest-window)))

(run-test)
