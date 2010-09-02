(in-package :gltest)

(defun rpath (resource)
  (merge-pathnames resource *sys-path*))

(defparameter +vshader+ (rpath "test.v.glsl"))
(defparameter +fshader+ (rpath "test.f.glsl"))
(defparameter +texture+ (rpath "testmap.jpg"))
(defparameter +mesh+ (rpath "apple.mesh"))
(defparameter *teapot-rotation* 0.0)
(defparameter *last-update-time* nil)

(defparameter *initial-width* 400)
(defparameter *initial-height* 400)

(defparameter *grid-width* 50)
(defparameter *grid-height* 50)

(defparameter *use-shader* nil)

;; game of life stuff
(defclass board ()
  ((num-rows :reader num-rows
	     :initarg :num-rows)
   (now-cols :reader num-cols
	     :initarg :num-cols)
   (board-data :accessor board-data
	       :initarg :board-data)))

(defun make-board (nx ny)
  (make-instance 'board
		 :num-rows ny :num-cols nx
		 :board-data (make-array (* nx ny)
					 :element-type '(unsigned-byte 1)
					 :initial-element 0)))

(defun index (x y nx)
  (+ x (* y nx)))

(defmethod boardref ((b board) x y)
  (aref (board-data b) (index x y (num-cols b))))

(defmethod (setf boardref) (val (b board) x y)
  (setf (aref (board-data b) (index x y (num-cols b))) val))

(defmethod occupied? ((b board) x y)
  (= (boardref b x y) 1))

(defun wrapped (idx max)
  (abs (mod idx max)))

(defmethod neighbors ((b board) x y)
  (list (cons (wrapped (- x 1) (num-cols b))
	      y)
	(cons (wrapped (+ x 1) (num-cols b))
	      y)
	(cons x
	      (wrapped (- y 1) (num-rows b)))
	(cons x
	      (wrapped (+ y 1) (num-rows b)))))

(defmethod num-neighbors ((b board) x y)
  (reduce #'+ (mapcar (lambda (c) (if (occupied? b (car c)
						 (cdr c))
				      1
				      0))
		      (neighbors b x y))))

(defmethod evolve ((b board))
  (let ((e (make-board (num-cols b) (num-rows b))))
    (dogrid (x y) ((num-cols b) (num-rows b))
      (if (occupied? b x y)
	  (case (num-neighbors b x y)
	    ((0 1 4) (setf (boardref e x y) 0))
	    (otherwise (setf (boardref e x y) 1)))

	  (when (= (num-neighbors b x y) 3)
	    (setf (boardref e x y) 1))))
    e))

(defun make-random-board (nx ny)
  (let ((b (make-board nx ny)))
    (dogrid (x y) (nx ny)
      (setf (boardref b x y) (random 2)))
    b))

(defmethod display ((b board))
  (loop for y from 1 to (num-rows b) collecting
       (loop for x from 1 to (num-cols b) collecting
	    (boardref b (- x 1) (- y 1)))))
	
(defclass gltest-window (bwindow)
  ((obj :accessor obj
	:initform (make-grid-plane *grid-width*
				   *grid-height*));(load-bobj +mesh+))
;;   (tex :accessor tex
;;	:initform (make-texture +texture+))
   (shader :accessor shader
	   :initform nil)
   (tex-uniform :accessor tex-uniform
		:initform nil)
   (world :accessor world
	  :initform (make-random-board *grid-width*
				       *grid-height*))))

(defmethod reload-shader ((w gltest-window))
  (setf (world w) (make-random-board (num-cols (world w))
				     (num-rows (world w))))

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
  (gl:translate 0.5 -0.5 -1)
  (gl:light :light0 :position '(0 1 1 0))
  (gl:light :light0 :diffuse '(0.8 0.8 0.8 0))
  
  (gl:rotate 180.0 0 1 0)

  ;; enable program and bind our texture to it
  (when *use-shader*
    (gl:use-program (program (shader w)))
    ;; (gl:active-texture :texture0)
    ;; (gl:bind-texture :texture-2d (glname (tex w)))
    (gl:uniformi (tex-uniform w) 0))

  (render (obj w)))

(defmethod animate ((w gltest-window))
  (let* ((time-now (sdl:sdl-get-ticks))
	 (delta-t (/ (- time-now *last-update-time*) 1000.0)))
    (setf *last-update-time* time-now)
    (setf *teapot-rotation* (+ *teapot-rotation* (* pi delta-t)))

    ;; modulate the colors in obj
    (let ((varr (vertices (obj w)))
	  (idx (make-idxer (num-cols (world w)))))

      (dogrid (xx yy) ((num-cols (world w)) (num-rows (world w)))
	(setf (gl:glaref varr (funcall idx xx yy) 'b)
	      (if (occupied? (world w) xx yy)
		  255
		  0))))
    (setf (world w) (evolve (world w)))))


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
      (setf (sdl:frame-rate) 3)
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
