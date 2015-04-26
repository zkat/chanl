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

(defun divulge-thread (actor &aux (channel (slot-channel actor 'command)))
  (recv (send channel (lambda (actor) actor (send channel (current-thread))))))

(def-suite actors :in chanl)
(def-suite action :in actors)
(in-suite action)

(test actors-sanity
  (let ((actor (make-instance 'actor)))
    (is (not (null (name actor))))
    (is (eq 'perform (slot-value actor 'state)))
    (is (channelp (slot-channel actor 'command)))
    (let ((thread (divulge-thread actor)))
      (is (threadp thread))
      (fire actor) (sleep 1)
      (is (not (thread-alive-p thread))))))

(test actors-insanity
  #.(let ((class-name (gensym "actor")))
      `(progn
         (defclass ,class-name (actor) (input (output :initform ())))
         (defmethod compute-tubes list ((actor ,class-name)) '(input output))
         (defmethod perform recv input ((actor ,class-name))
           (setf (slot-value actor 'output) (slot-value actor 'input)))
         (defmethod perform send output ((actor ,class-name)))
         (let ((actor (make-instance ',class-name)))
           (is (intersection '(command input output)
                             (mapcar 'car (slot-value actor 'tubes))))
           (is (null (recv (slot-channel actor 'output))))
           (is (null (recv (slot-channel actor 'output))))
           (send (slot-channel actor 'input) ())
           (is (null (recv (slot-channel actor 'output))))
           (send (slot-channel actor 'input) 'tubes)
           (is (eq 'tubes (recv (slot-channel actor 'output))))
           (is (eq 'tubes (recv (slot-channel actor 'output))))
           (let ((thread (divulge-thread actor)))
             (is (threadp thread))
             (fire actor) (sleep 1)
             (is (not (thread-alive-p thread)))))
         (flet ((remmethod (genfun quals specs)
                  (remove-method genfun (find-method genfun quals specs))))
           (mapcar #'remmethod (list #'perform #'perform #'compute-tubes)
                   '((send output) (recv input) (list))
                   '(#1=(,class-name) #1# #1#)))
         (setf (find-class ',class-name) ()))))

(def-suite bossing :in actors)
(in-suite bossing)

(test bossing-sanity
  (let ((boss (make-instance 'boss :name "test boss")))
    (is (not (null (name boss))))
    (is (eq 'perform (slot-value boss 'state)))
    (is (channelp (slot-channel boss 'command)))
    (let ((boss-thread (divulge-thread boss)))
      (is (threadp boss-thread))
      (let ((worker (make-instance 'actor :boss boss :name "test worker")))
        (let ((worker-thread (divulge-thread worker)))
          (is (thread-alive-p worker-thread))
          (halt worker) (sleep 1)
          (is (not (thread-alive-p worker-thread)))
          (send (slot-channel boss 'chanl.actors::to-run) worker)
          (setf worker-thread (divulge-thread worker))
          (is (thread-alive-p worker-thread))
          (halt boss) (sleep 1)
          (is (not (thread-alive-p worker-thread)))
          (is (not (thread-alive-p boss-thread)))
          (ensure-running boss)
          (is (thread-alive-p (divulge-thread worker)))
          (is (thread-alive-p (divulge-thread boss)))
          (fire worker) (sleep 1)
          (is (not (thread-alive-p worker-thread)))
          (halt boss) (sleep 1)
          (is (not (thread-alive-p boss-thread)))
          (ensure-running boss)
          (is (thread-alive-p (divulge-thread boss)))
          (is (null (assoc worker (slot-value boss 'chanl.actors::workers))))
          (fire boss)
          (is (not (thread-alive-p boss-thread))))))))
