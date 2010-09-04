(in-package :gltest)

(defclass change-list ()
  ((hashed :initform (make-hash-table :test #'equal)
	   :accessor hashed)
   (listed :initform nil
	   :accessor listed)))

(defmethod add-item ((l change-list) item)
  (unless (gethash item (hashed l))
    (setf (gethash item (hashed l)) t)
    (push item (listed l))))

(defmacro with-items ((val list) &body body)
  `(dolist (,val (listed ,list)) . ,body))
    
(defclass board ()
  ((num-rows :reader num-rows
	     :initarg :num-rows)
   (now-cols :reader num-cols
	     :initarg :num-cols)
   (board-data :accessor board-data
	       :initarg :board-data)
   (changed :reader changed
	    :initform (make-instance 'change-list))))

(defun make-board (nx ny)
  (make-instance 'board
		 :num-rows ny :num-cols nx
		 :board-data (make-array (* nx ny)
					 :element-type '(unsigned-byte 1)
					 :initial-element 0)))

(defun index (x y nx)
  (+ x (* y nx)))

(defun wrapped (idx max)
  (abs (mod idx max)))

(defmethod neighbors ((b board) x y)
  (let* ((nx (num-cols b))
	 (ny (num-rows b))
	 (x-1 (wrapped (- x 1) nx))
	 (x+1 (wrapped (+ x 1) nx))
	 (y-1 (wrapped (- y 1) ny))
	 (y+1 (wrapped (+ y 1) ny)))

    (list (cons x-1 y)
	  (cons x+1 y)
	  (cons x y-1)
	  (cons x y+1)
	  (cons x-1 y+1)
	  (cons x+1 y+1)
	  (cons x-1 y-1)
	  (cons x+1 y-1))))

(defmethod boardref ((b board) x y)
  (aref (board-data b) (index x y (num-cols b))))

(defmethod (setf boardref) (val (b board) x y)
  (setf (aref (board-data b) (index x y (num-cols b))) val)
  (dolist (n (neighbors b x y))
    (add-item (changed b) n)))

(defmacro with-changed ((x y) b &body body)
  (let ((c (gensym)))
    `(dolist (,c (listed (changed ,b)))
       (let ((,x (car ,c))
	     (,y (cdr ,c)))
	 . ,body))))

(defmethod occupied? ((b board) x y)
  (= (boardref b x y) 1))

(defmethod num-neighbors ((b board) x y)
  (reduce #'+ (mapcar (lambda (c) (if (occupied? b (car c)
						 (cdr c))
				      1
				      0))
		      (neighbors b x y))))

(defmethod evolve ((b board))
  (let ((e (make-board (num-cols b) (num-rows b))))
    (with-changed (x y) b
    ;;(dogrid (x y) ((num-cols b) (num-rows b))
      (if (occupied? b x y)
	  (case (num-neighbors b x y)
	    ((0 1 4) (setf (boardref e x y) 0))
	    (otherwise (setf (boardref e x y) 1)))

	  (when (= (num-neighbors b x y) 3)
	    (setf (boardref e x y) 1))))
    e))

(defun random-zero-or-one (prob-one)
  (if (< (random 1.0) prob-one)
      1
      0))

(defun make-random-board (nx ny)
  (let ((b (make-board nx ny)))
    (dogrid (x y) (nx ny)
      (setf (boardref b x y) (random-zero-or-one 0.05)))
    b))

(defmethod display ((b board))
  (loop for y from 1 to (num-rows b) collecting
       (loop for x from 1 to (num-cols b) collecting
	    (boardref b (- x 1) (- y 1)))))
	
(defun make-line-board (b)
  (let* ((mx (floor (/ (num-cols b) 2)))
	 (my (floor (/ (num-rows b) 2)))
	 (xhw (floor (* (num-cols b) .1)))
	 (minx (- mx xhw))
	 (maxx (+ mx xhw)))
    (dogrid (x y) ((num-cols b) (num-rows b))
      (setf (boardref b x y)
	    (if (and (= y my)
		     (>= x minx)
		     (<= x maxx)) 1 0)))))

(defun benchmark ()
  (let ((b (make-random-board 40 40)))
    (dotimes (gen 40)
      (setf b (evolve b)))))
