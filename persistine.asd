(asdf:defsystem #:persistine
  :description "Persistent data structures"
  :author "Mikhail Novikov <freiksenet@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "utils")
             (:file "vector")))))

(asdf:defsystem #:persistine.tests
  :depends-on (#:cl-pds
               #:fiveam)
  :components
  ((:module "tests"
            :serial t
            :components
            ())))
