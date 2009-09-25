;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem chanl
  :name "chanl"
  :maintainer "Josh Marchan"
  :author "Josh Marchan"
  :description "Communicating Sequential Process support for Common Lisp"
  :depends-on (:bordeaux-threads)
  :components
  ((:file "chanl")))

(asdf:defsystem chanl-examples
  :name "chanl examples"
  :maintainer "Adlai Chandrasekhar"
  :author "Josh Marchan"
  :description "Examples of how to use chanl"
  :depends-on (:chanl)
  :components ((:module "examples"
                        :components ((:file "package")
                                     (:file "utils" :depends-on ("package"))
                                     (:file "conditions" :depends-on ("utils"))
                                     (:file "sieve" :depends-on ("utils"))))))

