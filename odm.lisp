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

(defvar *odm-index* (make-hash-table :test 'equal)
  "An index of ODM \"defs\" is maintained to avoid traversing
the entire tree when looking up references

see `odm-index' and `odm-lookup' for usage
")

(defclass odm-object ()
  ((xml :initarg :xml :accessor xml)
   (parent :initarg :parent :accessor parent)))

(defmethod print-object ((self odm-object) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~a: ~a"
	    (odm-type self)
	    (oid self))))

(defmethod property ((self odm-object) name)
  (when (xml-element-p (xml self))
    (xml-element-attribute (xml self) name)))

(defmethod oid ((self odm-object))
  (property self (oid-attr
		  (odm-type self))))

(defmethod name ((self odm-object))
  (property self :|Name|))

(defmethod properties ((self odm-object))
  (when (xml-element-p (xml self))
    (xml-element-attributes
     (xml self))))

(defmethod odm-root ((self odm-object))
  "Returns the root to which this object belongs"
  (if (null (parent self))
      self
      (odm-root (parent self))))

(defmethod odm-type ((self odm-object))
  "Returns the type of this object (will correspond with
one of the 'General Elements' defined by CDISC ODM"
  (typecase (xml self)
    (string 'odm-text)
    (s-xml::xml-element (intern (string-upcase
			  (format nil "~a" (xml-element-name
					    (xml self))))
			 :odm))
    (t 'unknown)))

(defmethod kids ((self odm-object))
  "Returns this object's kids"
  (when (xml-element-p (xml self))
    (mapcar (odm-object-factory self)
	    (xml-element-children
	     (xml self)))))

(defun odm-gather (root &key (test (constantly t)))
  "Traverses the ODM model looking for nodes that pass `test'"
  (flatten
   (cons (when (funcall test root)
	   root)
	 (mapcar (lambda (root)
		   (odm-gather root :test test))
		 (kids root)))))

(defun odm-find-one (root &key test)
  (if (funcall test root)
      root
      (some (lambda (kid)
	      (odm-find-one kid :test test))
	    (kids root))))

(defun of-elem-type (name)
  (lambda (odm-obj)
    (when (eq name (odm-type odm-obj))
      odm-obj)))

(defun kids-like (name &key in)
  (mapcar (odm-object-factory in)
    (mapcar 'xml
	  (remove-if-not (of-elem-type name)
			 (kids in)))))

(defun kid-like (name &key in)
  (first (kids-like name :in in)))

(defun oid-attr (odm-type)
  (case (intern (symbol-name odm-type) :odm)
    (odm :|FileOID|)
    (studyeventref :|StudyEventOID|)
    (formref :|FormOID|)
    (itemgroupref :|ItemGroupOID|)
    (itemref :|ItemOID|)
    (codelistref :|CodeListOID|)
    (otherwise :|OID|)))

(defun p= (property-name value)
  (lambda (obj)
    (string= (property obj property-name)
	     value)))

(defun oid= (name)
  (lambda (obj)
    (string= (oid obj) name)))

(defun name= (name)
  (lambda (obj)
    (string= (name obj) name)))

(defun flatten (node)
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
  "functional intersection (from 'On Lisp')"
  (if (null fns)
      fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
	  (and (funcall fn x) (funcall chain x))))))

(defun odm-flip (sym)
  "Flips a symbol from either def to ref, or ref to def depending
on what was passed in"
  (let* ((mapping '((studyeventref studyeventdef)
		    (formref formdef)
		    (itemgroupref itemgroupdef)
		    (itemref itemdef)
		    (codelistref codelist))))
    (loop for (ref def) in mapping
       when (eq ref sym) return def
       when (eq def sym) return ref
       finally (return nil))))


(defmethod find-def ((ref odm-object))
  (let ((ref-type (odm-type ref)))
    (odm-find-one (metadata ref)
      :test (fint (oid= (oid ref))
		  (of-elem-type (odm-flip ref-type))))))



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

  (defun parse-odm-stream (stream)
    (let ((doc (parse-xml stream :output-type :xml-struct)))
      (clrhash obj-cache)
      (funcall (odm-object-factory nil) doc)))

  (defun index-defs (root)
    (let ((mdv (metadata root)))
      (mapc (lambda (def)
	      (odm-index mdv def))
	    (kids mdv))))

  (defun parse-odm (filename &key (into 'odm-object))
    (let ((doc (parse-xml-file filename :output-type :xml-struct)))
      ;; parsing a new file clears the object cache
      (clrhash obj-cache)
      (when doc
	(let ((root (funcall (odm-object-factory nil into)
			     doc)))
	  (index-defs root)
	  root))))

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
(defmethod study ((self odm-object))
  (first (kids-like 'study :in (odm-root self))))

;; XXX assumes an ODM with a single MetaDataVersion
(defmethod metadata ((self odm-object))
  (first (kids-like 'metadataversion :in (study self))))

(defmethod events ((self odm-object))
  (odm-gather (metadata self)
    :test (of-elem-type 'studyeventdef)))

(defmethod forms ((self odm-object))
  (case (odm-type self)
    (studyevent (mapcar 'find-def (kids-like 'formref :in self)))
    (t (kids-like 'formdef :in (metadata self)))))

(defmethod groups ((self odm-object))
  (case (odm-type self)
    (formdef (mapcar 'find-def (kids-like 'itemgroupref :in self)))
    (t (kids-like 'itemgroupdef :in (metadata self)))))

(defmethod items ((self odm-object))
  (case (odm-type self)
    (itemgroupdef (mapcar 'find-def (kids-like 'itemref :in self)))
    (t (kids-like 'itemdef :in (metadata self)))))

(defmethod question ((self odm-object))
  (let ((q (odm-find-one self 
             :test (of-elem-type 'question))))
    (when q
      ;; XXX must add support for multiple TranslatedText items
      (string-trim 
       '(#\Space #\Tab #\Newline)
       (xml (odm-find-one q
			  :test (of-elem-type 'odm-text)))))))
