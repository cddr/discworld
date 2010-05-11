;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
#|

    discworld -- The world of CDISC (in Lisp)

Copyright (C) 2010 by Andy Chambers

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :odm)

(register-namespace "http://www.cdisc.org/ns/odm/v1.2" "odm" :odm)
(register-namespace "http://www.cdisc.org/ns/odm/v1.3" "odm" :odm)

(defvar *odm-index* (make-hash-table :test 'equal)
  "An index of ODM \"defs\" is maintained to avoid traversing
the entire tree when looking up references

see `odm-index' and `odm-lookup' for usage
")

(defclass odm-object ()
  ((xml :initarg :xml :accessor xml)
   (parent :initarg :parent :accessor parent))
  (:documentation "An odm-object can be any type of object described by
the CDISC ODM model (e.g. ItemDef, CodeList, ItemGroupDef etc)"))

(defmethod print-object ((self odm-object) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~a: ~a"
	    (odm-type self)
	    (oid self))))

(defmethod odm-path ((self odm-object))
  "Returns the xpath address of this object (only applicable for
objects which respond to the `oid' generic function)"
  (cond 
    ((root-p self) (format nil "document('~a')/ODM"
			   (oid self)))
    ((oid self) (format nil "~a/~a[@OID='~a']"
			(odm-path (parent self))
			(symbol-name (xml-element-name (xml self)))
			(oid self)))))

(defun property (self name)
  "Returns the named property of `self'"
  (when (xml-element-p (xml self))
    (xml-element-attribute (xml self) name)))

(defun oid (self)
  "Returns the `oid' of `self'"
  (property self (oid-attr
		  (odm-type self))))

(defun def-p (self)
  (member (odm-type self)
	  '(codelist itemdef itemgroupdef formdef studyeventdef)))

(defun ref-p (self)
  (member (odm-type self)
	  '(codelistref itemref itemgroupref formref studyeventref)))

(defun root-p (self)
  (not (parent self)))

(defun mdv-p (self)
  (eq 'metadataversion (odm-type self)))

(defun study-p (self)
  (eq 'study (odm-type self)))

(defun name (self)
  (flet ((%name (self)
	   (cond ((def-p self) (property self :|Name|))
		 ((ref-p self) (name (find-def self)))
		 ((study-p self) (value-of
				  (find-one self :test 
					    (of-elem-type 'studyname)))))))
    (%name self)))

(defun purpose (self)
  (property self ':|Purpose|))

(defun properties (self)
  (when (xml-element-p (xml self))
    (xml-element-attributes
     (xml self))))

(defun root (self)
  "Returns the root to which this object belongs"
  (if (null (parent self))
      self
      (root (parent self))))

(defun odm-type (self)
  "Returns the type of this object (will correspond with
one of the 'General Elements' defined by CDISC ODM"
  (typecase (xml self)
    (string 'odm-text)
    (s-xml::xml-element (intern (string-upcase
			  (format nil "~a" (xml-element-name
					    (xml self))))
			 :odm))
    (t 'unknown)))

(defun kids (self)
  "Returns this object's kids"
  (when (xml-element-p (xml self))
    (mapcar (odm-object-factory self)
	    (xml-element-children
	     (xml self)))))

(defun gather (root &key (test (constantly t)))
  "Traverses the ODM model looking for nodes that pass `test'

NB. This method traverses the whole tree and as such is pretty expensive
Don't use it in a loop"
  (flatten
   (cons (when (funcall test root)
	   root)
	 (mapcar (lambda (root)
		   (gather root :test test))
		 (kids root)))))

(defun value-of (odm-object)
  "Concatenates all the textual elements beneath `odm-object' (like
<xsl:value-of../>"
  (with-output-to-string (s)
    (mapc (lambda (item)
	    (format s "~a" (xml item)))
	  (gather odm-object :test (of-elem-type 'odm-text)))))

(defun find-one (root &key test)
  "Traverses the ODM model looking for the first node (in document
order) that passes `test'"
  (if (funcall test root)
      root
      (some (lambda (kid)
	      (find-one kid :test test))
	    (kids root))))

(defun of-elem-type (name)
  "Given the name of an ODM type, returns a function that takes one
argument (an odm-object) and returns true if that argument is an
odm-object of the named type"
  (lambda (odm-obj)
    (when (eq (intern (symbol-name name) :odm) 
	      (odm-type odm-obj))
      odm-obj)))

(defun kids-like (name &key in)
  "Returns kids of `in', of odm-type `name'"
  (mapcar (odm-object-factory in)
    (mapcar 'xml
	  (remove-if-not (of-elem-type name)
			 (kids in)))))

(defun kid-like (name &key in)
  "Returns the first kid of `in', of odm-type `name'"
  (first (kids-like name :in in)))

(defun oid-attr (odm-type)
  "Returns the property which should be used to obtain the OID
of an odm-object of `odm-type'"
  (case (intern (symbol-name odm-type) :odm)
    (odm :|FileOID|)
    (studyeventref :|StudyEventOID|)
    (formref :|FormOID|)
    (itemgroupref :|ItemGroupOID|)
    (itemref :|ItemOID|)
    (codelistref :|CodeListOID|)
    (otherwise :|OID|)))

(defun p= (property-name value)
  "Given a property-name, and a corresponding value, returns a
function which takes one argument (an odm-object) and returns true
if that argument has a property named `property-name' with value
`value'"
  (lambda (obj)
    (string= (property obj property-name)
	     value)))

(defun oid= (name)
  "Given an OID `name', returns a function which takes one argument
\(an odm-object) and returns true if (oid obj) is `string=' to `name'"
  (lambda (obj)
    (string= (oid obj) name)))

(defun name= (name)
  "Given an name `name', returns a function which takes one argument
\(an odm-object) and returns true if (name obj) is `string=' to `name'"
  (lambda (obj)
    (string= (name obj) name)))

(defun flatten (node)
  "Flattens nested lists into a single list"
  (labels ((rec (node accumulation)
	     (cond ((null node) accumulation)
		   ((atom node) (cons node
				      accumulation))
		   (t (rec (car node)
			   (rec (cdr node) accumulation))))))
    (rec node nil)))


;; (defun ttrav (rec &optional (base #'identity))
;;   (labels ((self (tree)
;;              (if (atom tree)
;;                  (if (functionp base)
;;                      (funcall base tree)
;;                      base)
;;                  (funcall rec (self (car tree))
;;                               (if (cdr tree) 
;;                                   (self (cdr tree)))))))
;;     #'self))

(defun fint (fn &rest fns)
  "functional intersection (from 'On Lisp')

\(fint 'signed 'sealed 'delivered)
=> (lambda (x)
     (and (signed x)
          (sealed x)
          (delivered x)))
"
  (if (null fns)
      fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
	  (and (funcall fn x) (funcall chain x))))))

(defun flip (sym)
  "Flips a symbol from either def to ref, or ref to def depending
on what was passed in

XXX Needs a better name.  I'm open to suggestions"
  (let* ((mapping '((studyeventref studyeventdef)
		    (formref formdef)
		    (itemgroupref itemgroupdef)
		    (itemref itemdef)
		    (codelistref codelist))))
    (loop for (ref def) in mapping
       when (eq ref sym) return def
       when (eq def sym) return ref
       finally (return nil))))


(defun find-def (ref)
  "Finds the \"def\" referenced by `ref'

XXX This only really works in the simplest of cases.  For example,
it ignores the possibility of <odm:Include/> files and where some
ref might reference a def in another MetaDataVersion.  It is
reasonably efficent though, using as it does the index through
odm-lookup"
  (odm-lookup
   :type (flip (odm-type ref))
   :metadata (metadata ref)
   :ref ref))

(defun odm-index (mdv def)
  (setf (gethash (list (odm-type def)
		       (oid mdv)
		       (oid def))
		 *odm-index*)
	def))

(defun odm-lookup (&key type metadata ref)
  (gethash (list type
		 (oid metadata)
		 (oid ref))
	   *odm-index*))

;;; These next functions are for creating/reusing odm-objects

(defun cache-hit (cache key)
  (gethash key cache))

(defun cache-miss (cache key val)
  (setf (gethash key cache)
	val))

;; Using an object cache allows us to make use of object
;; equality for objects that are, well, equal.
(let ((obj-cache (make-hash-table :test 'equal)))

  (defun index-defs (root)
    (let ((mdv (metadata root)))
      (mapc (lambda (def)
	      (odm-index mdv def))
	    (kids mdv))))

  (defun odm-parse (filename &key (into 'odm-object))
    (with-open-file (s filename)
      (let ((doc (parse-xml-dom s :xml-struct)))
	;; parsing a new file clears the object cache
	(clrhash obj-cache)
	(when doc
	  (let ((root (funcall (odm-object-factory nil into)
			       doc)))
	    (index-defs root)
	    root)))))

  (defun odm-object-factory (parent-node &optional 
			     (object-type 'odm-object))
    (lambda (child-node)
      (or (cache-hit obj-cache 
            (list parent-node child-node))
	  (cache-miss obj-cache
            (list parent-node child-node)
            (make-instance object-type
	      :xml child-node
	      :parent parent-node)))))
)

;;; Handy ODM accessors

;; XXX assumes an ODM with a single study
(defun study (self)
  (first (kids-like 'study :in (root self))))

;; XXX assumes an ODM with a single MetaDataVersion
(defun metadata (self)
  (first (kids-like 'metadataversion :in (study self))))

(defun events (self)
  (case (odm-type self)
    (protocol (mapcar 'find-def (kids-like 'studyeventref :in self)))
    (t (kids-like 'studyeventdef :in (metadata self)))))

(defun forms (self)
  (case (odm-type self)
    (studyevent (mapcar 'find-def (kids-like 'formref :in self)))
    (t (kids-like 'formdef :in (metadata self)))))

(defun groups (self)
  (case (odm-type self)
    (formdef (mapcar 'find-def (kids-like 'itemgroupref :in self)))
    (t (kids-like 'itemgroupdef :in (metadata self)))))

(defun items (self)
  (case (odm-type self)
    (itemgroupdef (mapcar 'find-def (kids-like 'itemref :in self)))
    (t (kids-like 'itemdef :in (metadata self)))))

(defun question (self)
  (let ((q (find-one self 
             :test (of-elem-type 'question))))
    (when q
      ;; XXX must add support for multiple TranslatedText items
      (string-trim 
       '(#\Space #\Tab #\Newline)
       (xml (find-one q
		      :test (of-elem-type 'odm-text)))))))
