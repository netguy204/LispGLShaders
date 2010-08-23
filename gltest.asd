(asdf:defsystem "gltest"
  :description "me playing around with opengl"
  :version "0.2"
  :author "Brian Taylor <el.wubo@gmail.com>"
  :licence "GPL"
  :depends-on (:cl-opengl :lispbuilder-sdl :lispbuilder-sdl-image
			  :cl-glu :cl-glut)
  :components ((:file "packages")
	       (:file "textures" :depends-on ("packages"))
	       (:file "resources" :depends-on ("packages"))
	       (:file "macros" :depends-on ("packages" "textures" "resources"))
	       (:file "test" :depends-on ("macros"))))
