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

(in-package :define)

(defclass study (odm::odm-object)
  ((domains :initarg :domains :accessor domains :initform nil)))

(defclass domain (odm::odm-object)
  ((vars :initarg :vars :accessor vars)))

(defclass var (odm::odm-object)
  ())

(defun find-study (study-id)
  (declare (ignore study-id))
  (parse-odm #p"/usr/src/discworld/t/data/define-cdiscpilot01.xml"
	     :into 'study))

(defun find-domain (study-id domain-id)
  (let* ((study (find-study study-id))
	 (domains (domains study)))
    (find-if (name= domain-id) domains)))

;;; study accessors

(defmethod name ((self study))
  (value-of 
   (odm-find-one self :test (of-elem-type 'studyname))))

(defmethod domains ((self study))
  (mapcar (lambda (odm)
	    (change-class odm 'domain))
	  (groups self)))

;;; domain accessors

(defmethod name ((self domain))
  (property self :|Name|))

(defmethod variables ((self domain))
  (mapcar (lambda (odm)
	    (change-class odm 'var))
	  (kids-like 'itemref :in self)))

(defmethod description ((self domain))
  (property self '|def|:|Label|))

(defmethod data-structure ((self domain))
  (property self '|def|:|Structure|))

(defmethod keys ((self domain))
  (property self '|def|:|DomainKeys|))

(defmethod location ((self domain))
  (property self '|def|:|ArchiveLocationID|))

(defmethod purpose ((self domain))
  (property self :|Purpose|))

(defmethod vars ((self domain))
  (kids-like 'itemref :in self))

;; variable accessors

(defmethod name ((self var))
  (name (find-def self)))

(defmethod label ((self var))
  (property (find-def self)
	    '|def|:|Label|))

(defmethod data-type ((self var))
  (property (find-def self)
	    :|DataType|))

(defmethod terminology ((self var))
  nil)

(defmethod origin ((self var))
  (property (find-def self)
	    :|Origin|))

(defmethod role ((self var))
  (property (find-def self)
	    :|Role|))

(defmethod comment ((self var))
  (property (find-def self)
	    :|Comment|))

