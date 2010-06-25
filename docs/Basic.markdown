# Basic Operations 

From the specifications....

> The Operational Data Model (ODM) is a vendor neutral, platform
> independent format for interchange and archive of clinical study
> data. The model includes the clinical data along with its associated
> metadata, administrative data, reference data and audit
> information. All of the information that needs to be shared among
> different software systems during the setup, operation, analysis,
> submission or for long term retention as part of an archive is
> included in the model.

So ODM is just a data model.  We define a base-class `odm-object' to
abstract away the implementation details, and just provide a functional
interface to the concepts described in the model.

# Read an ODM file

To read in an odm file, use odm-parse.  The result of calling odm-parse
is also a "root" odm-object.

    (defparameter *odm-test* (odm-parse "/home/andy/studies/001.xml"))
    (root-p *odm-test*)
    => t

# Access metadata elements

Accessors have been defined for all the various types of metadata.  The
names from the standard are "lispified" versions of the corresponding
names in the CDISC specifications

    (codelists *odm-test*)
    => list of <CodeList/> odm-objects

    (items *odm-test*)
    => list of <ItemDef/> odm-objects

    (groups *odm-test*)
    => list of <ItemGroupDef/> odm-objects

    (mapcar #'question (items *odm-test*))
    => list of <Question/> odm-objects

# Defs and Refs

In the metadata section of the model, we introduce the concept of "defs"
and "refs". def-p returns true if the object is a def.  ref-p returns
true if the object is a ref.  An object cannot be both a ref and a def.

    (let ((items (items *odm-test*)))
      (def-p (first items)))
    => t

    (let* ((groups (groups *odm-test*)))
      (ref-p (first (kids-like 'itemref :in (first groups)))))
    => t

# access element properties

The properties defined in the specifications can be accessed using
the `property' function.  In addition, convenience functions are defined
to access a few commonly used properties

    (mapcar (lambda (item)
              (property item :|DataType|))
            (items *odm-test*))
    => (list "text" "integer" "boolean" ..)

    (mapcar #'oid (items *odm-test*))
    => (list "AETERM" "SAE" "AESEV" ..)
      
