;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2015 Adlai Chandrasekhar
;;;;
;;;; tatft!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:chanl.actors.tests
  (:import-from :5am #:def-suite #:run! #:is #:in-suite #:signals #:pass)
  (:import-from :chanl #:test #:chanl)
  (:use :cl :chanl :chanl.actors) (:export #:run-all-tests))
(in-package :chanl.actors.tests)

(def-suite actors :in chanl)     ; even worse
(in-suite actors)

(test actors-sanity
  (let ((actor (make-instance 'actor)))
    (is (not (null (name actor))))
    (is (not (null (slot-value actor 'tasks))))
    (is (channelp (slot-channel actor 'command)))
    (let ((channel (make-instance 'channel)))
      (send (slot-channel actor 'command)
            (lambda (actor) (send channel actor)))
      (is (eq actor (recv channel))))
    (halt actor) (pass)))
