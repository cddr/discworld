
(defpackage :dw.odm
  (:use :cl :s-xml)
  (:export #:odm-object #:oid #:name #:purpose
	   #:property #:properties

	   #:study #:metadata #:events #:forms #:groups #:items
	   #:question
	   #:repeating-p

	   #:root
	   #:odm-type
	   #:odm-path

	   #:parent #:kids-like #:oid= #:name= #:find-one #:gather
	   #:of-elem-type #:value-of
	   #:fint

	   #:kids #:find-def
	   #:items #:groups #:forms #:events #:metadata

	   #:odm-parse #:odm-write))

(defpackage :dw.define
  (:use :cl :s-xml :dw.odm)
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
	   
(defpackage :dw.api
  (:use :cl :lisp-db :dw.odm :dw.define)
  (:export #:*db-root*
	   #:put
	   #:post
	   #:serve))


