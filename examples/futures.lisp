;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; ChanL example implementation of doing concurrency using futures instead of channels.
;;;;
;;;; Copyright © 2009 Josh Marchan, Adlai Chandrasekhar
;;;;
;;;; This file is derived from 'Eager Future'; see the file COPYRIGHT, in the top directory,
;;;; for the license information for that project.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl.examples)

;;; This example is similar to Eager Future's API.
;;; It demonstrates the value of channels as concurrency primitives.

(defstruct (future (:print-object (lambda (f s) (print-unreadable-object (f s :type t :identity t)))))
  (channel (make-instance 'buffered-channel :size 1) :read-only t))

(define-condition execution-error (error)
  ((cause :initarg :cause :reader execution-error-cause)
   (future :initarg :future :reader execution-error-future))
  (:report (lambda (condition stream)
             (format stream "~A errored during execution.~%Cause: ~A"
                     (execution-error-future condition)
                     (execution-error-cause condition)))))

(let ((sentinel (make-symbol (format nil "The future has performed an illegal ~
                                          operation and will have to be shut down"))))
  (defun yield (future)
    "Yield the values returned by FUTURE. If FUTURE isn't ready to yield yet, block until it is."
    (let ((yielded-values (recv (future-channel future))))
      (send (future-channel future) yielded-values)
      (if (eq sentinel (car yielded-values))
          (error (cdr yielded-values))
          (values-list yielded-values))))

  (defun future-call (function &key (initial-bindings *default-special-bindings*))
    "Executes FUNCTION in parallel and returns a future that will yield the return value of
that function. INITIAL-BINDINGS may be provided to create dynamic bindings inside the thread."
    (let ((future (make-future)))
      (pcall (lambda ()
               (send (future-channel future)
                     (handler-case
                         (multiple-value-list (funcall function))
                       (condition (cause)
                         (cons sentinel (make-condition 'execution-error
                                                        :cause cause :future future))))))
             :initial-bindings initial-bindings)
      future))
  ) ; End sentinel closure

(defmacro future-exec ((&key initial-bindings) &body body)
  "Convenience macro that makes the lambda for you."
  `(future-call (lambda () ,@body) ,@(when initial-bindings `(:initial-bindings ,initial-bindings))))

(defun future-select (&rest futures)
  "Blocks until one of the futures in FUTURES (a sequence) is ready to yield,
then returns that future."
  ;; This is an improvement. However, we should try to find some way of not "thrashing". - Adlai
  (setf futures (sort futures (lambda (a b) a b (zerop (random 2)))))
  (loop for future = (find-if 'send-blocks-p futures :key 'future-channel)
     when future return future))

(defmacro future-let ((&rest bindings) &body body)
  (loop for (symbol . forms) in bindings
     for future = (make-symbol (string symbol))
     collect `(,future (future-exec () ,@forms)) into futures
     collect `(,symbol (yield ,future)) into variables
     finally (return `(let ,futures (symbol-macrolet ,variables ,@body)))))

;; EXAMPLES> (defparameter *future* (future-exec () 'success))
;; *FUTURE*
;; EXAMPLES> (yield *future*)
;; SUCCESS
;; EXAMPLES> (yield (future-select (future-exec () (sleep 10) 'long)
;;                                 (future-exec () (sleep 2) 'short)))
;; SHORT
;; EXAMPLES> (defparameter *future* (future-exec () (error "OHNOES")))
;; *FUTURE*
;; EXAMPLES> (yield *future*)
;; ...
;; #<FUTURE #x14FFE71E> errored during execution.
;; Cause: OHNOES
;;    [Condition of type EXECUTION-ERROR]
;; ...
;; Invoking restart: Return to SLIME's top level.
;; ; Evaluation aborted.
