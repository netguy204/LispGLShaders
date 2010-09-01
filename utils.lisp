(in-package :gltest)

(defun read-from-file (file)
  (with-open-file (s file)
    (read s)))

(defun slurp-lines (file)
  (let ((lines nil))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do (push line lines)))
    (nreverse lines)))

