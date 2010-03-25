
(defpackage :odm
  (:use :cl :s-xml)
  (:export #:oid 
	   #:name #:property #:properties

	   #:study #:metadata #:events #:forms #:groups #:items
	   #:question

	   #:odm-root
	   #:odm-type

	   #:kids #:find-def
	   #:items #:groups #:forms #:events #:metadata

	   #:parse-odm
	   #:parse-odm-stream))

(defpackage :define
  (:use :cl :odm))
