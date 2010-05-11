
(defpackage :odm
  (:use :cl :s-xml)
  (:export #:oid #:name #:purpose
	   #:property #:properties

	   #:study #:metadata #:events #:forms #:groups #:items
	   #:question

	   #:odm-root
	   #:odm-type

	   #:kids-like #:name= #:find-one #:gather
	   #:of-elem-type #:value-of

	   #:kids #:find-def
	   #:items #:groups #:forms #:events #:metadata

	   #:odm-parse))

(defpackage :define
  (:use :cl :s-xml :odm)
  (:export #:find-define #:find-domain
	   #:domains #:variables
	     #:name
	     #:variables
	     #:description
	     #:data-structure
	     #:keys
	     #:location
	     #:purpose
	     #:schema

	     #:label
	     #:data-type
	     #:terminology
	     #:origin
	     #:role
	     #:comment
	     #:key-p))
	   
