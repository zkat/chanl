;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan, Adlai Chandrasekhar
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite select :in chanl)
(in-suite select)

(test select-basic
  (is (null (select)))
  (is (eq 'foo (select (otherwise 'foo))))
  (is (eq 'foo (select (else 'foo))))
  (is (eq 'foo (select (t 'foo)))))

(def-suite select-unbuffered :in select)
(in-suite select-unbuffered)

;;; The following tests depend on SEND, RECV, PEXEC, & co.

(test select-unbuffered-recv
  (let ((channel (make-instance 'channel)))
    (select ((recv channel x) x (5am:fail "SELECT ran a blocking clause"))
            (otherwise (5am:pass)))
    (pexec () (send channel (recv channel))) (send channel 'foo)
    (select ((recv channel x y)
             (is (eq 'foo x) "SELECT didn't bind the RECVed value")
             (is (eq channel y) "SELECT didn't bind the recved-from chanl"))
            (otherwise (5am:fail "SELECT didn't RECV when it could've")))))

(test select-unbuffered-send
  (let ((channel (make-instance 'channel)))
    (select ((send channel t) (5am:fail "SELECT ran a blocking clause"))
            (otherwise (5am:pass)))
    (pexec () (recv channel)) (sleep 0.5)
    (select ((send channel t x)
             (is (eq channel x) "SELECT didn't bind the sent-to chanl"))
            (otherwise (5am:fail "SELECT didn't SEND when it could've")))))
