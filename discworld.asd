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

(defpackage #:discworld-system
  (:use #:cl #:asdf))
(in-package #:discworld-system)

(defsystem :discworld
  :description "The world of CDISC (in Lisp)"
  :author "Andy Chambers <achambers.home@gmail.com>"
  :license "LLGPL"
  :version "0.2.1"
  :name "discworld"
  :depends-on (:s-xml)
  :serial t
  :components
  (
   (:file "packages")
   (:file "odm")
   (:file "define")
   ))
