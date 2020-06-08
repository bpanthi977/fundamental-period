;;;; MOI.asd

(asdf:defsystem #:MOI
  :description "Describe MOI here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:trivia :numcl :magicl)
  :components ((:file "package")
			   (:file "utils")
			   (:file "report")
               (:file "moi")))
