;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;; The infamous parallel prime sieve from NewSqueak
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:chanl-examples
  (:use :cl :chanl)
  (:export :first-n-primes))

(in-package :chanl-examples)

;;;
;;; Utils
;;;
(defmacro cleanup-leftovers (&body body)
  (let ((procs (gensym)))
    `(let ((,procs (all-procs)))
       (unwind-protect (progn ,@body)
         (mapc 'kill (set-difference (all-procs) ,procs))))))

;;;
;;; Parallel Prime Sieve
;;;
(defun counter (channel)
  (loop for i from 2 do (send channel i)))

(defun filter (prime in out)
  (loop for i = (recv in)
     when (plusp (mod i prime))
     do (send out i)))

(defun sieve (output-channel)
  (let ((input-channel (make-channel :name "1")))
    (proc-exec (:name "Counter") (counter input-channel))
    (proc-exec (:name "Sieve")
      (labels ((next-prime (input-channel)
                 (let* ((prime (recv input-channel))
                        (new-input (make-channel :name (format nil "~D" prime))))
                   (send output-channel prime)
                   (proc-exec (:name (format nil "Filter ~D" prime))
                     (filter prime input-channel new-input))
                   (next-prime new-input))))
        (next-prime input-channel)))
    output-channel))

(defun first-n-primes (n)
  (let ((prime-channel (make-channel :name "Prime output channel")))
    (cleanup-leftovers
      (sieve prime-channel)
      (loop repeat n collect (recv prime-channel)))))
