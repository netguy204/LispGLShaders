(defpackage :gltest
  (:use :cl))

(in-package :gltest)

(dolist (sys '(:cl-opengl :lispbuilder-sdl :lispbuilder-sdl-image :cl-glu :cl-glut))
  (asdf:load-system sys))

