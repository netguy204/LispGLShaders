(in-package :gltest)

(defun read-from-file (file)
  (with-open-file (s file)
    (read s)))
