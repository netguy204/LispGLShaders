(defpackage :gltest
  (:use :cl))

(in-package :gltest)

(defvar *sys-path*
  (make-pathname :host (pathname-host 
			#.(or *compile-file-truename*
			      *load-truename*))

		 :directory (pathname-directory
			     #.(or *compile-file-truename*
				   *load-truename*))))
