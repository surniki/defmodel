;;;; defmodel.asd

(asdf:defsystem #:defmodel
  :description "A tool for describing physical, dynamical systems."
  :author "Scott Urnikis <surniki@ilstu.edu>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "digraph")
	       (:file "model-utils")
               (:file "defmodel")
	       (:file "object-utils")))
