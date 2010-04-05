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

(defun find-define (study-id)
  (declare (ignore study-id))
  (parse-odm #p"/usr/src/discworld/t/data/define-cdiscpilot01.xml"))

(defun find-domain (study-id domain-id)
  (let* ((study (find-define study-id))
	 (domains (domains study)))
    (find-if (name= domain-id) domains)))

;;; study accessors

(defun domains (study)
  (groups study))

;;; domain accessors

(defun variables (domain)
  (items domain))

(defun description (domain)
  (property domain '|:def:Label|))

(defun data-structure (domain)
  (property domain '|:def:Structure|))

(defun keys (domain)
  (property domain '|:def:DomainKeys|))

(defun location (domain)
  (property domain '|:def:ArchiveLocationID|))

(defun purpose (domain)
  (property domain :|Purpose|))

;; variable accessors

(defun label (var)
  (property (find-def var)
	    '|:def:Label|))

(defun data-type (var)
  (property (find-def var)
	    :|DataType|))

(defun terminology (var)
  (declare (ignore var))
  nil)

(defun origin (var)
  (property (find-def var)
	    :|Origin|))

(defun role (var)
  (property (find-def var)
	    :|Role|))

(defun comment (var)
  (property (find-def var)
	    :|Comment|))

