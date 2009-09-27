;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;; The infamous parallel prime sieve from NewSqueak
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl.examples)

(export '(first-n-primes))

;;;
;;; Parallel Prime Sieve
;;;

(defvar *terminate* nil)

(defun counter (channel)
  (loop for i from 2 do (if *terminate*
                            (progn (send channel *terminate*)
                                   (loop-finish))
                            (send channel i))))

(defun filter (prime in out)
  (loop for i = (recv in) do
       (if (numberp i)
           (when (plusp (mod i prime))
             (send out i) #- (and) ; Enable for wavyness [TODO: channel-dynamic on/off] - Adlai
             (syncout t "~&~A~D~%" (make-string i :initial-element #\Space) prime))
           (progn (send out nil)
                                        ;(syncout t "~&Filter #~D terminating." prime)
                  (loop-finish)))))

(defun sieve (output-channel buffer-size)
  (let ((input-channel (make-channel buffer-size)))
    (pexec (:name "Counter") (counter input-channel))
    (pexec (:name "Sieve")
      (labels ((next-prime (input-channel)
                 (let* ((prime (recv input-channel))
                        (new-input (make-channel buffer-size)))
                   (send output-channel prime)
                   (when (numberp prime)
                     (pexec (:name (format nil "Filter ~D" prime))
                       (filter prime input-channel new-input))
                     (next-prime new-input)))))
        (next-prime input-channel)))
    output-channel))

(defun first-n-primes (n &optional (buffer-size 0))
  (let ((prime-channel (make-channel buffer-size)))
    (setf *terminate* nil)
    (sieve prime-channel buffer-size)
    (prog1 (loop repeat n collect (recv prime-channel))
      (setf *terminate* t)
      (pexec (:name "Cleanup")
        (loop while (numberp (recv prime-channel)))))))

(defun eratosthenes (n)
  (declare (optimize speed (safety 0) (debug 0))
           (fixnum n))
  (let ((bit-vector (make-array n :initial-element 1 :element-type 'bit)))
    (loop for i from 2 upto (isqrt n) do
         (loop for j fixnum from i
            for index fixnum = (* i j)
            until (>= index n) do
            (setf (sbit bit-vector index) 0)))
    (loop for i from 2 below (length bit-vector)
       unless (zerop (sbit bit-vector i)) collect i)))
