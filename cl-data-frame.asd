(asdf:defsystem cl-data-frame
  :version "0"
  :description "Data frames for Common Lisp."
  :long-description "FIXME"
  :maintainer "Tamas Papp <tkpapp@gmail.com>"
  :author "Tamas Papp <tkpapp@gmail.com>"
  :licence "BOOST"
  :depends-on (alexandria anaphora let-plus cl-slice)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "cl-data-frame")))
