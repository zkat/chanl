;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2015 Adlai Chandrasekhar
;;;;
;;;; tatft!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:chanl.actors.tests
  (:import-from :5am #:def-suite #:run! #:is #:in-suite #:signals #:pass #:fail)
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
         (defmethod perform recv input ((actor ,class-name) &key)
           (setf (slot-value actor 'output) (slot-value actor 'input)))
         (defmethod perform send output ((actor ,class-name) &key))
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

(test actors-unsanity
  #.(let ((class-name (gensym "actor")))
      `(progn
         (defclass ,class-name (actor)
           ((in :initarg :in) input (out :initarg :out)
            (output :initform ())))     ; slot-boundp as default guard?
         (defmethod compute-tubes list ((actor ,class-name))
           '((input :from in) (output :to out)))
         (defmethod perform recv input ((actor ,class-name) &key)
           (setf (slot-value actor 'output) (slot-value actor 'input)))
         (defmethod perform send output ((actor ,class-name) &key))
         (let* ((bread (gensym "crumb"))
                (in (make-instance 'channel))
                (out (make-instance 'channel))
                (actor (make-instance ',class-name :in in :out out)))
           (is (eq in (send in bread)))
           (is (eq bread (recv out)))
           (fire actor))
         (flet ((remmethod (genfun quals specs)
                  (remove-method genfun (find-method genfun quals specs))))
           (mapcar #'remmethod (list #'perform #'perform #'compute-tubes)
                   '((send output) (recv input) (list))
                   '(#1=(,class-name) #1# #1#)))
         (setf (find-class ',class-name) ()))))

(def-suite bossing :in actors)
(in-suite bossing)

(test bossing-sanity
  (macrolet ((with-actor ((symbol &key (class 'actor) (name (gensym)) thread)
                          &body body)
               `(let* ((,symbol (make-instance ',class))
                       ,@(when thread `((,thread (divulge-thread ,symbol)))))
                  ,@body))
             (warns (warnspec (&optional test) &body body
                              &aux (warnedp (gensym)))
               `(let (,warnedp)
                  (handler-bind         ; FIXME: this belongs as 5am:warns
                      ((,warnspec
                        #'(lambda (warning)
                            (when ,(or test t)
                              (pass "warned of ~S" ',warnspec)
                              (setf ,warnedp t) (muffle-warning warning)))))
                    ,@body (unless ,warnedp
                             (fail "didn't warn ~S" ',warnspec))))))
    (with-actor (*boss* :class boss :name "test boss" :thread boss-thread)
      (is (not (null (name *boss*))))
      (is (eq 'perform (slot-value *boss* 'state)))
      (is (channelp (slot-channel *boss* 'command)))
      (is (threadp boss-thread))
      (with-actor (worker :name "test worker" :thread worker-thread)
        (is (thread-alive-p worker-thread))
        (halt worker) (sleep 1)
        (is (not (thread-alive-p worker-thread)))
        (send (slot-channel *boss* 'chanl.actors::to-run) worker)
        (setf worker-thread (divulge-thread worker))
        (is (thread-alive-p worker-thread))
        (halt *boss*) (sleep 1)
        (is (not (thread-alive-p worker-thread)))
        (is (not (thread-alive-p boss-thread)))
        (warns simple-warning
               ((string= "races ahoy!" (simple-condition-format-control warning)))
               (ensure-running *boss*))
        (is (thread-alive-p (divulge-thread worker)))
        (is (thread-alive-p (divulge-thread *boss*)))
        (fire worker) (sleep 1)
        (is (not (thread-alive-p worker-thread)))
        (halt *boss*) (sleep 1)
        (is (not (thread-alive-p boss-thread)))
        (warns simple-warning
               ((string= "races ahoy!" (simple-condition-format-control warning)))
               (ensure-running *boss*))
        (is (thread-alive-p (divulge-thread *boss*)))
        (is (null (assoc worker (slot-value *boss* 'chanl.actors::workers))))
        (fire *boss*)
        (is (not (thread-alive-p boss-thread)))))))
