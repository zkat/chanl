;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem chanl
  :name "chanl"
  :maintainer "Kat Marchan"
  :author "Kat Marchan"
  :description "Communicating Sequential Process support for Common Lisp"
  :depends-on (:bordeaux-threads)
  :components
  ((:file "chanl")))

(asdf:defsystem chanl-examples
  :name "chanl examples"
  :maintainer "Adlai Chandrasekhar"
  :author "Kat Marchan"
  :description "Examples of how to use chanl"
  :depends-on (:chanl)
  :components ((:module "examples"
                        :components ((:file "package")
                                     (:file "utils" :depends-on ("package"))
                                     (:file "conditions" :depends-on ("utils"))
                                     (:file "sieve" :depends-on ("utils"))))))

(asdf:defsystem chanl-tests
  :name "chanl tests"
  :description "Unit Tests for the ChanL library and its examples"
  :depends-on (:chanl :eos :chanl-examples)
  :components ((:module "tests"
                        :components ((:file "setup-tests")
                                     (:file "chanl" :depends-on ("setup-tests"))
                                     (:file "examples" :depends-on ("setup-tests"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :chanl))))
  (format t "~2&*******************~@
                ** Loading tests **~@
                *******************~%")
  (asdf:oos 'asdf:load-op :chanl-tests)
  (asdf:oos 'asdf:test-op :chanl-tests))
