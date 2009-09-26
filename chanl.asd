;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem chanl
  :name "chanl"
  :maintainer "Kat Marchan"
  :author "Kat Marchan"
  :description "Communicating Sequential Process support for Common Lisp"
  :depends-on (:bordeaux-threads)
  :components ((:module "src"
                        :components ((:file "channels" :depends-on ("package")))))

(asdf:defsystem chanl.examples
  :name "chanl examples"
  :maintainer "Adlai Chandrasekhar"
  :author "Kat Marchan"
  :description "Examples of how to use chanl"
  :depends-on (:chanl)
  :components ((:module "examples"
                        :components ((:file "package")
                                     (:file "utils" :depends-on ("package"))))))

(asdf:defsystem chanl.examples.conditions
  :name "ChanL cross-thread conditions example"
  :depends-on (:chanl.examples)
  :components
  ((:module "examples"
            :components ((:file "conditions")))))

(asdf:defsystem chanl.examples.sieve
  :name "ChanL parallel sieve example"
  :depends-on (:chanl-examples)
  :components
  ((:module "examples"
            :components ((:file "sieve")))))

(asdf:defsystem chanl.examples.ltk
  :name "ChanL LTK example"
  :depends-on (:ltk :chanl-examples)
  :components
  ((:module "examples"
            :components ((:file "tk")))))

(asdf:defsystem chanl.tests
  :name "chanl tests"
  :description "Unit Tests for thim ChanL library and its examples"
  :depends-on (:chanl :eos :chanl.examples)
  :components ((:module "tests"
                        :components ((:file "setup-tests")
                                     (:file "chanl" :depends-on ("setup-tests"))
                                     (:file "examples" :depends-on ("setup-tests"))))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :chanl))))
  (format t "~2&*******************~@
                ** Loading tests **~@
                *******************~%")
  (asdf:oos 'asdf:load-op :chanl.tests)
  (asdf:oos 'asdf:test-op :chanl.tests))
