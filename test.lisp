

(defun reload-tests ()
  (mapc (lambda (name)
	  (load (format nil "t/~a.lisp" name)))
	'("pcl-tests"
	  "test-odm"
	  "test-define")))

(defun run-tests ()
  (test.discworld:run-tests))
