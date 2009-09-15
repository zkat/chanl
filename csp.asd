;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem csp
  :name "csp"
  :maintainer "Roger Peppe"
  :author "Roger Peppe"
  :description "Communicating Sequential Process support for Common LISP"
  :depends-on ("bordeaux-threads")
  :components ((:file "csp")))

