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

(defun counter (channel)
  (loop for i from 2 do (send channel i)))

(defun filter (prime in out)
  (loop for i = (recv in)
     when (plusp (mod i prime))
     do (send out i)))

(defun sieve ()
  (let* ((c (make-channel))
         (prime-chan (make-channel)))
    (proc-exec (:name "Counter") (counter c))
    (proc-exec (:name "Sieve")
      (labels ((next-prime (input-channel)
                 (let ((prime (recv input-channel))
                       (new-input (make-channel)))
                   (send prime-chan prime)
                   (proc-exec ()
                     (filter prime input-channel new-input))
                   (next-prime new-input))))
        (next-prime c)))
    prime-chan))

(defun first-n-primes (n &optional (cleanupp t))
  (let* ((procs (all-procs))
         (prime-chan (sieve)))
    (unwind-protect (loop repeat n collect (recv prime-chan))
      (when cleanupp
        (mapc 'kill (set-difference (all-procs) procs))))))
