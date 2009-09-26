;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;; Simple Queues
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

(setf *print-circle* t)                 ; Horrible hack, just in case... - Adlai

(defun make-queue (length)
  "Creates a new queue of maximum size LENGTH"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
           (type (integer 1 #.(- array-total-size-limit 2)) length))
  (let ((queue (the simple-vector
                 (make-array (the (integer 3 #.array-total-size-limit)
                               (+ 2 length))))))
    (setf (svref queue 0) 2
          (svref queue 1) 2
          (svref queue 2) queue)))

(defun queue-peek (queue)
  "Dereference QUEUE's head pointer"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector queue))
  (svref queue (the (integer 2 #.(1- array-total-size-limit))
                 (svref queue 0))))

(defun queue-zero-p (queue)
  "Checks whether QUEUE's theoretical length is zero"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector queue))
  (= (the (integer 2 #.(1- array-total-size-limit))
       (svref queue 0))
     (the (integer 2 #.(1- array-total-size-limit))
       (svref queue 1))))

(defun queue-empty-p (queue)
  "Checks whether QUEUE's effective length is zero"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector queue))
  (and (queue-zero-p queue)
       (eq (queue-peek queue) queue)))

(defun queue-full-p (queue)
  "Checks whether QUEUE is effectively full"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector queue))
  (and (queue-zero-p queue)
       (not (eq (queue-peek queue) queue))))

(defun queue-count (queue)
  "Returns QUEUE's effective length"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector queue))
  (let ((length (mod (- (svref queue 1)
                        (svref queue 0))
                     (- (length queue) 2))))
    (if (zerop length)
        (if (eq (queue-peek queue) queue) 0
            (- (length queue) 2))
        length)))

(defun enqueue (object queue)
  "Sets QUEUE's head to OBJECT and increments QUEUE's head pointer"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
           (type simple-vector queue))
  (setf (svref queue (the (integer 2 #.(1- array-total-size-limit))
                       (svref queue 1)))
        object
        (svref queue 1)
        (+ 2 (mod (1- (svref queue 1))
                  (- (length queue) 2))))
  object)

(defun dequeue (queue)
  "Sets QUEUE's tail to QUEUE, increments QUEUE's tail pointer, and returns the previous tail ref"
  (declare ;(optimize (speed 3) (safety 0) (debug 0))
          (type simple-vector queue))
  (prog1 (svref queue (svref queue 0))
    (setf (svref queue (svref queue 0)) queue
          (svref queue 0) (+ 2 (mod (1- (svref queue 0)) (- (length queue) 2))))))
