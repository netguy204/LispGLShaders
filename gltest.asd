(asdf:defsystem "gltest"
  :description "me playing around with opengl"
  :version "0.2"
  :author "Brian Taylor <el.wubo@gmail.com>"
  :licence "GPL"
  :depends-on (:cl-opengl :lispbuilder-sdl :lispbuilder-sdl-image
			  :cl-glu :cl-glut)
  :serial t
  :components ((:file "packages")
	       (:file "utils")
	       (:file "macros")
	       (:file "mesh")
	       (:file "test")))
