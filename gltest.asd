(asdf:defsystem "gltest"
  :description "me playing around with opengl"
  :version "0.2"
  :author "Brian Taylor <el.wubo@gmail.com>"
  :licence "GPL"
  :components ((:file "packages")
	       (:file "textures")
	       (:file "resources")
	       (:file "macros" :depends-on ("packages" "textures" "resources"))
	       (:file "test" :depends-on ("macros"))))
