;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem chanl
  :name "chanl"
  :maintainer "Kat Marchan"
  :author "Roger Peppe"
  :description "Communicating Sequential Process support for Common LISP"
  :depends-on ("bordeaux-threads")
  :components ((:file "chanl")))

