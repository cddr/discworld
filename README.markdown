
# discworld -- The World of CDISC (in Lisp)

# Summary

The clinical trial community have gathered together to
produce an extensive set of standards for data transfer,
dataset structures, and eCRF content guides.  This
project implements some of those specifications.  As
far as I know, this is the *only* project which implements
CDISC in a programming language standardized by ANSI :-)

# Required

* [s-xml](http://common-lisp.net/project/s-xml/)

# Installation

    (push #p"/path/to/discworld" asdf:*central-registry*)
    (asdf:operate 'asdf:load-op :discworld)

# Documentation

Not yet I'm afraid, although there is decent doc string coverage


# See also

* [common-lisp-stat](http://github.com/blindglobe/common-lisp-stat)
* [CDISC](http://www.cdisc.org/standards)
* [OpenCDISC](http://www.opencdisc.org/)
* [OpenClinica](http://www.openclinica.org/)

