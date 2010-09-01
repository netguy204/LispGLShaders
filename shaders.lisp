(in-package :gltest)

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

