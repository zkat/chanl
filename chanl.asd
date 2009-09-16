;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem chanl
  :name "chanl"
  :maintainer "Kat Marchan"
  :author "Kat Marchan"
  :description "Communicating Sequential Process support for Common Lisp"
  :depends-on (:bordeaux-threads)
  :components
  ((:file "chanl")))

