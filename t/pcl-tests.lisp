(defpackage :test.discworld
  (:use :cl :odm :define)
  (:export #:run-tests))

(in-package :test.discworld)

(defun run-tests ()
  (asdf:operate 'asdf:load-op :discworld)
  (test/odm)
  (test/define))

(defvar *test-name* nil)

(defun test-data-directory ()
  (make-pathname :name nil
		 :type nil
		 :defaults
    (merge-pathnames #p"data/"
      (load-time-value
       (or #.*compile-file-pathname* *load-pathname*)))))

(defun find-test-file (name)
  (merge-pathnames name (test-data-directory)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

