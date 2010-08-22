(in-package :gltest)

(defvar *resources* nil)

(defun add-resource (name res)
  (push (cons name res) *resources*))

(defun get-resource (name)
  (cdr (assoc name *resources*)))

