
(in-package :dw.api)

(defparameter *meta-schema*
  (list "
create table _codelists (
 oid, 
 coded_value, 
 decode,
 primary key (oid, coded_value))"))

(defun schema (mdv)
  (labels ((domain-tbl (domain)
	     (format nil "
create table ~a (
  ~{~a~^,~%~},
  primary key (~a)
);"
		     (name domain)
		     (mapcar #'column-def (variables domain))
		     (domain-keys domain)))
	   (domain-keys (domain)
	     (format nil "~{~a~^,~}"
		     (mapcar #'name (remove-if-not #'key-p 
						   (variables domain)))))
	   (column-def (var)
	     (format nil "~a ~:[default ''~;not null~]"
		     (name var)
		     (key-p var))))

    (append *meta-schema*
	    (mapcar #'domain-tbl (groups mdv)))))

