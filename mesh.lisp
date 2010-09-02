(in-package :gltest)

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

(defmacro with-client-state (states &body body)
  "execute body with the client states enabled"
  (let ((svar (gensym)))
    `(progn
       (dolist (,svar ,states)
	 (gl:enable-client-state ,svar))
       (progn . ,body)
       (dolist (,svar ,states)
	 (gl:disable-client-state ,svar)))))

(defmethod render ((m bmesh))
  (with-client-state '(:vertex-array :normal-array :color-array)
    (gl:bind-gl-vertex-array (vertices m))
    (gl:draw-elements (style m) (elements m))))

(defmethod render ((o bobj))
  (dolist (mesh (meshes o))
    (render mesh)))

(gl:define-gl-array-format position-color
  (gl:color :type :unsigned-char :components (r g b))
  (gl:vertex :type :float :components (x y z))
  (gl:normal :type :float :components (nx ny nz)))

(defun make-idxer (nx)
  (lambda (x y) (+ x (* y nx))))

(defun make-eidxer (nx)
  (lambda (x y tnum vnum) (+ (* x 6) (* tnum 3) vnum
			     (* y (- nx 1) 6))))

(defmacro dogrid ((x y) (nx ny) &body body)
  "iterate over each grid point in row major order"
  `(dotimes (,y ,ny)
     (dotimes (,x ,nx)
       (progn . ,body))))

(defclass grid-plane (bmesh)
  ((num-rows :reader num-rows
	     :initarg :num-rows)
   (num-cols :reader num-cols
	     :initarg :num-cols)))

(defun make-grid-plane (nx ny)
  (let* ((nverts (* nx ny))

	 ;; 2 triangles at each center
	 (nelems (* (- nx 1) (- ny 1) 6))

	 (varr (gl:alloc-gl-array 'position-color nverts))
	 (earr (gl:alloc-gl-array :unsigned-short nelems))

	 (uscale (float (/ (- nx 1))))
	 (vscale (float (/ (- ny 1))))
	 (get-idx (make-idxer nx))
	 (get-eidx (make-eidxer nx)))

    ;; build a row major array of nx X ny verts
    ;; that is 1 by 1 in object space
    (dogrid (xx yy) (nx ny)
      (let ((idx (funcall get-idx xx yy)))
	(setf (gl:glaref varr idx 'x) (* xx uscale)
	      (gl:glaref varr idx 'y) (* yy vscale)
	      (gl:glaref varr idx 'z) 0.0
	      (gl:glaref varr idx 'nx) 0.0
	      (gl:glaref varr idx 'ny) 0.0
	      (gl:glaref varr idx 'nz) -1.0
	      (gl:glaref varr idx 'r) 0
	      (gl:glaref varr idx 'g) 0
	      (gl:glaref varr idx 'b) 0)))

    ;; now wind triangles across those verts in
    ;; successive strips (but stored as individual
    ;; triangles
    ;; Now our index coordinates represent the centers
    ;; of cells on our grid (not verts)
    ;;
    ;; 0,0---------------1,0
    ;;  |                 |
    ;;  |                 |
    ;;  |       0,0       |
    ;;  |                 |
    ;;  |                 |
    ;; 0,1---------------1,1
    (dogrid (xc yc) ((- nx 1) (- ny 1))
      (setf (gl:glaref earr (funcall get-eidx xc yc 0 0)) (funcall get-idx xc yc)
	    (gl:glaref earr (funcall get-eidx xc yc 0 1)) (funcall get-idx xc (+ yc 1))
	    (gl:glaref earr (funcall get-eidx xc yc 0 2)) (funcall get-idx (+ xc 1) yc)
	    
	    (gl:glaref earr (funcall get-eidx xc yc 1 0)) (funcall get-idx xc (+ yc 1))
	    (gl:glaref earr (funcall get-eidx xc yc 1 1)) (funcall get-idx (+ xc 1) (+ yc 1))
	    (gl:glaref earr (funcall get-eidx xc yc 1 2)) (funcall get-idx (+ xc 1) yc)))

    (make-instance 'grid-plane
		   :num-rows ny
		   :num-cols nx
		   :vertices varr
		   :elements earr
		   :style :triangles)))
