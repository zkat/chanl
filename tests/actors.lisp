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
    (halt actor) (sleep 1)
    (is (null (find :alive (slot-value actor 'tasks) :key #'task-status)))))

(test actors-insanity
  #.(let ((class-name (gensym "actor")))
      `(progn
         (defclass ,class-name (actor) (input (output :initform ())))
         (defmethod compute-tubes append ((actor ,class-name))
           `((input  . ,(make-instance 'channel))
             (output . ,(make-instance 'channel))))
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
           (halt actor))
         (flet ((remmethod (genfun quals specs)
                  (remove-method genfun (find-method genfun quals specs))))
           (mapcar #'remmethod (list #'perform #'perform #'compute-tubes)
                   '((send output) (recv input) (append))
                   '(#1=(,class-name) #1# #1#)))
         (setf (find-class ',class-name) ()))))
