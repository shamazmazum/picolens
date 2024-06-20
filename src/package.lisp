(defpackage picolens
  (:use #:cl)
  (:shadow #:set
           #:car
           #:cdr
           #:caar
           #:cadr
           #:cdar
           #:cddr)
  (:export #:over
           #:set
           #:view
           #:compose
           #:empty
           #:fold
           #:lens
           #:to
           #:gen-lenses
           #:car
           #:cdr
           #:cadr
           #:caar
           #:cdar
           #:cddr))
