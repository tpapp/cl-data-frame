(asdf:defsystem #:cl-data-frame
  :version "0"
  :description "Data frames for Common Lisp."
  :long-description "FIXME"
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :licence "MIT"
  :depends-on (#:alexandria
               #:anaphora
               #:array-operations
               #:cl-num-utils
               #:cl-slice
               #:let-plus)
  :serial t
  :components ((:file "cl-data-frame")))

(asdf:defsystem #:cl-data-frame-tests
  :version "0"
  :description "Unit tests for CL-DATA-FRAME."
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :licence "MIT"
  :depends-on (#:cl-data-frame
               #:clunit)
  :serial t
  :components ((:file "cl-data-frame-tests")))
