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
  (channel (make-channel)) values-yielded returned-p ready-p error)

(define-condition execution-error (error)
  ((cause :initarg :cause :reader execution-error-cause)
   (future :initarg :future :reader execution-error-future))
  (:report (lambda (condition stream)
             (format stream "~A errored during execution.~%Cause: ~A"
                     (execution-error-future condition)
                     (execution-error-cause condition)))))

(defun yield (future)
  "Yield the values returned by FUTURE. If FUTURE isn't ready to yield yet, block until it is."
  (cond ((future-returned-p future)     ; if we've already returned, just keep returning the value
         (values-list (future-values-yielded future)))
        ((future-error future)
         (error (future-error future)))
        (t
         (let ((yielded-values (recv (future-channel future)))) ;otherwise, wait on the channel
           (setf (future-values-yielded future) yielded-values
                 (future-returned-p future) t)
           (values-list yielded-values)))))

(defun future-call (function &key (initial-bindings *default-special-bindings*))
  "Executes FUNCTION in parallel and returns a future that will yield the return value of
that function. INITIAL-BINDINGS may be provided to create dynamic bindings inside the thread."
  (let ((future (make-future)))
    (pcall (lambda () (handler-case (send (future-channel future)
                                          (prog1 (multiple-value-list (funcall function))
                                            (setf (future-ready-p future) t)))
                        (condition (cause)
                          (setf (future-error future)
                                (make-condition 'execution-error
                                                :cause cause :future future)))))
           :initial-bindings initial-bindings)
    future))

(defmacro future-exec ((&key initial-bindings) &body body)
  "Convenience macro that makes the lambda for you."
  `(future-call (lambda () ,@body) ,@(when initial-bindings `(:initial-bindings ,initial-bindings))))

(defun future-select (&rest futures)
  "Blocks until one of the futures in FUTURES (a sequence) is ready to yield,
then returns that future."
  ;; This could be much better. It thrashes hardcore until something yields.
  (loop for future = (find-if #'future-ready-p futures)
     when future return future))

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
