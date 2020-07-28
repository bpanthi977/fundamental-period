;;;; MOI.asd

(asdf:defsystem #:fundamental-period
  :description "Fundamnetal period of buildings"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:trivia :numcl :magicl)
  :components ((:file "package")
	       (:file "utils")
	       (:file "report")
               (:file "moi")))
