;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

(defpackage #:csp-asd
  (:use :cl :asdf))

(in-package :csp-asd)

(defsystem csp
  :name "csp"
  :maintainer "Roger Peppe"
  :author "Roger Peppe"
  :description "Communicating Sequential Process support for Common LISP"
  :components ((:file "csp"))
  :depends-on ("bordeaux-threads"))
