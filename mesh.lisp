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

