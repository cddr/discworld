
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

(defun load-schema (mdv)
  (mapc (lambda (item)
	  (sql> item))
	(schema mdv)))

(defun load-codelists (mdv)
  (flet ((codelist-data (cl)
	   (loop for item in (kids-like 'codelistitem :in cl)
	      collect
		(list (property cl :|Name|)
		      (property item :|CodedValue|)
		      (value-of (find-one item :test 
					  (of-elem-type 'translatedtext)))))))
    (mapc (lambda (row)
	    (sql> "insert into _codelists (oid, coded_value, decode)
                      values (:oid, :coded_value, :decode)"
		  (list :oid (first row)
			:coded-value (second row)
			:decode (third row))))
	  (loop for cl in (codelists mdv)
	     nconc (codelist-data cl)))))

	  
