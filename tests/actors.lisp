;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2015 Adlai Chandrasekhar
;;;;
;;;; tatft!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:chanl.actors.tests
  (:import-from :5am #:def-suite #:run! #:is #:in-suite #:signals #:pass)
  (:import-from :chanl #:test #:chanl) (:import-from :chanl.actors #:tubes)
  (:use :cl :chanl :chanl.actors) (:export #:run-all-tests))
(in-package :chanl.actors.tests)

(def-suite actors :in chanl)
(def-suite action :in actors)
(in-suite action)

(test actors-sanity
  (let ((actor (make-instance 'actor)))
    (is (not (null (name actor))))
    (is (eq 'perform (slot-value actor 'state)))
    (is (threadp (slot-value actor 'boss)))
    (is (channelp (slot-channel actor 'command)))
    (let ((channel (make-instance 'channel)))
      (send (slot-channel actor 'command)
            (lambda (actor) (send channel actor)))
      (is (eq actor (recv channel))))
    (halt actor) (sleep 1)
    (is (not (thread-alive-p (slot-value actor 'boss))))))

(test actors-insanity
  #.(let ((class-name (gensym "actor")))
      `(progn
         (defclass ,class-name (actor) (input (output :initform ())))
         (defmethod compute-tubes list ((actor ,class-name)) '(input output))
         (defmethod perform recv input ((actor ,class-name))
           (setf (slot-value actor 'output) (slot-value actor 'input)))
         (defmethod perform send output ((actor ,class-name)))
         (let ((actor (make-instance ',class-name)))
           (is (equal '(command input output)
                      (mapcar 'car (slot-value actor 'tubes))))
           (is (null (recv (slot-channel actor 'output))))
           (is (null (recv (slot-channel actor 'output))))
           (send (slot-channel actor 'input) ())
           (is (null (recv (slot-channel actor 'output))))
           (send (slot-channel actor 'input) 'tubes)
           (is (eq 'tubes (recv (slot-channel actor 'output))))
           (is (eq 'tubes (recv (slot-channel actor 'output))))
           (halt actor) (sleep 1)
           (is (not (thread-alive-p (slot-value actor 'boss)))))
         (flet ((remmethod (genfun quals specs)
                  (remove-method genfun (find-method genfun quals specs))))
           (mapcar #'remmethod (list #'perform #'perform #'compute-tubes)
                   '((send output) (recv input) (list))
                   '(#1=(,class-name) #1# #1#)))
         (setf (find-class ',class-name) ()))))

(def-suite bossing :in actors)
(in-suite bossing)

(test bossing-sanity
  (let ((n (length (all-threads))))
    (let ((boss (make-instance 'boss)))
      (is (not (null (name boss))))
      (is (eq 'perform (slot-value boss 'state)))
      (is (threadp (slot-value boss 'boss)))
      (is (channelp (slot-channel boss 'command)))
      (let ((channel (make-instance 'channel)))
        (send (slot-channel boss 'command)
              (lambda (boss) (send channel boss)))
        (is (eq boss (recv channel))))
      (let ((worker (make-instance 'actor :boss boss)))
        (let ((channel (make-instance 'channel)))
          (send (slot-channel worker 'command)
                (lambda (worker) (send channel worker)))
          (is (eq worker (recv channel))))
        (send (slot-channel boss 'chanl.actors::to-halt) worker) (sleep 2)
        (is (= (1+ n) (length (all-threads))))
        (send (slot-channel boss 'chanl.actors::to-run) worker) (sleep 1)
        (let ((channel (make-instance 'channel)))
          (send (slot-channel worker 'command)
                (lambda (worker) (send channel worker)))
          (is (eq worker (recv channel))))
        (halt boss) (sleep 2)
        (is (not (thread-alive-p (slot-value boss 'boss))))
        (is (= n (length (all-threads))))))))
